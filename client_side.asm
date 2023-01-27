;;
;; Embedded Systems Course Term Project
;;
;; Created: 1/15/2023 11:34:00 AM
;; Author : Özgür Kuzucu
;;
;;
;; Client side code of a bank simulation between two Easy Avr 7 devices. 
;;
;; General process outline: User Identificiation => Authorization => Basic Bank Operations With Display (Loop)
;;
;; After it is ensured that the other (bank) device has been switched on, the device (client) that has this code can also be switched on. 
;; Overall, the bank is in the position of a slave, always waiting for the input of a client to process and send the required outputs. SPI is used for
;; the communication processes between the bank and the client. Initially, when the client is first switched on, it sends the id of the user (user_id) 
;; to the bank that is waiting for a signal. After the id is sent, it is processed by the bank, and decided whether it is a valid id. If it is not, the client
;; receives an error code that blocks further process, and bank does not continue further with the requests of the client. If it is a valid id, client device 
;; waits for the 4-digit password input from the user and a confirmation button push. When a confirmation is received after a 4-digit password has been entered, 
;; client sends the arranged password signal to the bank. If an error code signal is received that indicates wrong password, user can continue with another try.
;; If not, bank starts waiting for the operation signals. 
;;
;; There are 4 operations and 3 currency types defined in the system. 
;;
;; 3 currency types are: 
;; Turkish Lira (TL), United States Dollar (USD) and Euro (EUR)
;;
;; 4 operation types are:
;; Fetch: Fetch the current amount of money that is hold in the bank and display (User can only choose to display. Fetch operations are done internally)
;; Deposit:	Deposit the amount of money that is chosen by the user in a certain currency type to the bank account
;; Withdraw: Withdraw the amount of money that is chosen by the user in a certain currency type from the bank account
;; Exchange: Exchange the amount of money that is chosen by the user between two currency types
;;
;; Signals for these various operations and identification/authorization processes are arranged in the following manner:
;; Identification: Meet Signal / 8-bits / $55(error_code_zero) => Id Signal / 8-bits / $01(user_id)
;; Authorization: Password Signal / 8-bits / n'th entered digit (can be 1-4) covers the bits 2*n-2 and 2*n-1
;; Operations: Operation Signal / 8-bits / [3 bits empty|2 bits operation type|2 bits currency/exchange type|1 bit reverse order (for exchange)]
;;			   Amount Signal(Not used in fetch) / 8-bits / 8-bit amount
;;
;; Buttons for these various operations and identification/authorization processes are arranged in the following manner:
;; Authorization: Pin D / Least significant 4 bits and Most significant 1 bit / Password digit value = button pin index + 1, confirmation on most significant index
;; Operations: Pin D / All bits except 6'th / Currency selection on bits 0-2, Operation selection (except display) on bits 3-5, Reverse switch on most significant index
;;			   Pin A / Least significant 2 bits / Display switch on bit 1, Confirmation on least significant bit
;;
;; User can enter the amounts from the potentiometer with the help of built-in ADC
;; Operations, amounts and various prompts are presented in the LCD Display

.equ user_id						= $01   ;; for valid identification and bank to compare the users prompt with the password associated with id
.equ confirm_password_buttons_d		= $80   ;; button index   on Pin D for the confirmation that the password has entered
.equ enter_password_buttons_d		= $0f   ;; button indices on Pin D for entering password sequentially
.equ currency_type_buttons_d		= $07   ;; button indices on Pin D for currency  type selection (100 => EUR/USD-EUR, 010 => USD/TL-EUR, 001 => TL/TL-USD)
.equ operation_type_buttons_d		= $38   ;; button indices on Pin D for operation type selection (100 => exchange, 010 => withdraw, 001 => deposit)
.equ operation_reverse_buttons_d	= $80   ;; button index   on Pin D for reversing exchange direction (1 => reverse)
.equ confirmation_buttons_a			= $01   ;; button index   on Pin A for the confirmation of the current operation
.equ view_saving_buttons_a			= $02   ;; button index	  on Pin A for viewing the current amount of savings in a particular currency type
.equ error_code_zero				= $55   ;; signal that is received from the bank if there is no error
.equ sufficient_money				= $00   ;; signal that is received from the bank if there is sufficient amount of money to process the requested operation
.equ max_currency_display_byte		= 5		;; max amount of bytes required to store the savings' display values in SRAM
.equ max_currency_amount_byte		= 2     ;; max amount of bytes required to store the actual saving values	 in SRAM
.equ signal_size_max				= 2	    ;; max amount of bytes required to store the SPI signals			 in SRAM
.equ password_size					= 1	    ;;     amount of bytes required to store the user's password	     in SRAM
.equ error_code_size				= 1	    ;;	   amount of bytes required to store the received error codes	 in SRAM
.equ currency_type_signal_tl		= 0<<1  ;; id of tl		    in deposit/withdrawal signal that will be sent via SPI
.equ currency_type_signal_usd		= 1<<1  ;; id of usd		in deposit/withdrawal signal that will be sent via SPI
.equ currency_type_signal_eur		= 2<<1  ;; id of eur		in deposit/withdrawal signal that will be sent via SPI
.equ exchange_type_signal_tl_usd	= 0<<1  ;; id of tl to usd  in exchange			  signal that will be sent via SPI
.equ exchange_type_signal_tl_eur	= 1<<1  ;; id of tl to eur  in exchange			  signal that will be sent via SPI
.equ exchange_type_signal_usd_eur	= 2<<1  ;; id of usd to eur in exchange			  signal that will be sent via SPI
.equ operation_type_fetch			= 0<<3  ;; id of fetching (savings) operation	  signal that will be sent via SPI
.equ operation_type_deposit			= 1<<3  ;; id of deposit		    operation	  signal that will be sent via SPI
.equ operation_type_withdraw		= 2<<3  ;; id of withdraw		    operation	  signal that will be sent via SPI
.equ operation_type_exchange		= 3<<3  ;; id of exchange		    operation	  signal that will be sent via SPI
.equ menu_buttons_d					= (operation_reverse_buttons_d)|(operation_type_buttons_d)|(currency_type_buttons_d) ;; menu button locations in Pin D
.equ menu_buttons_a					= (confirmation_buttons_a)|(view_saving_buttons_a)									 ;; menu button locations in Pin A
.equ right_most_operation_button	= (operation_type_buttons_d) & (-operation_type_buttons_d) ;; location of the right-most "operation type" button
.equ right_most_currency_button		= (currency_type_buttons_d)  & (-currency_type_buttons_d)  ;; location of the right-most "currency type"  button
.equ operation_signal_adder			= (1<<3)  ;; adder for "operation type" to get the next operation type signal
.equ currency_type_signal_adder		= (1<<1)  ;; adder for "currency type"  to get the next currency  type signal
.equ exchange_reversed_signal		= (1<<0)  ;; location of "exchange reverse" signal

.def amount_display1 = R2	    ;; holds the right-most digit of the amount that is selected 
.def amount_display2 = R3	    ;; holds the middle	   digit of the amount that is selected 	
.def amount_display3 = R4	    ;; holds the left-most  digit of the amount that is selected 	
.def temp  = R16			    ;; temporary register 1
.def temp2 = R17			    ;; temporary register 2
.def temp3 = R18			    ;; temporary register 3
.def temp4 = R19			    ;; temporary register 4
.def temp5 = R20			    ;; temporary register 5
.def data_size = R21		    ;; holds the data size (byte) of the next load/store operation that will be done in SRAM
.def login_fsm_state = R22	    ;; fsm state for login, changes with the password button pushes
.def current_operation = R23    ;; holds current operation		   type  (has same id numbers with operation_type_*)
.def current_currency = R24     ;; holds current currency/exchange type  (has same id numbers with currency_type_signal_*/exchange_type_signal_*)
.def current_is_reversed = R25  ;; holds current is reversed	   state (1 => reversed exchange, 0 => default exchange)
.def current_is_view_mode = R30	;; holds the boolean value of whether the system is currently displaying the saving amounts
.def current_amount = R26	    ;; holds current amount of money to deposit/withdraw/exchange (Also holds the entered password at the beginning)
.def released_previously = R27  ;; holds the information whether buttons are released previously

				    .dseg
password_cache:	    .byte password_size				;; allocate enough space for user's password prompt (also used for sending bank id at the beginning)
error_code_cache:   .byte error_code_size			;; allocate enough space for error codes received via SPI
signal_cache:	    .byte signal_size_max			;; allocate enough space for signals that will be sent to bank
saving_amount_tl:   .byte max_currency_amount_byte	;; allocate enough space to fetch   current saving amount in TL
saving_amount_usd:  .byte max_currency_amount_byte	;; allocate enough space to fetch   current saving amount in USD
saving_amount_eur:  .byte max_currency_amount_byte	;; allocate enough space to fetch   current saving amount in EUR
saving_display_tl:  .byte max_currency_display_byte ;; allocate enough space to display current saving amount in TL
saving_display_usd: .byte max_currency_display_byte ;; allocate enough space to display current saving amount in EUR
saving_display_eur: .byte max_currency_display_byte ;; allocate enough space to display current saving amount in EUR

.cseg
.org  $000
	jmp RESET
.org $0020
	jmp	ADC_COMPLETE ;; AD conversion handle

RESET:

	;; Initialize stack pointer
	ldi		temp, low(RAMEND)
	out		SPL, temp
	ldi		temp, high(RAMEND)
	out		SPH, temp

	;; Initialize in/out registers' directions
	;; Initialize Port D
	ldi		temp, $40		;; Location of LCD pulse enable is output
	out		DDRD, temp		;; while every other location   is input

	;; Initialize Port C
	ldi		temp, $ff		;; Location of LCD data registers are output
	out		DDRC, temp		

	;; Initialize Port A
	ldi		temp, $04		;; Location of LCD command/data selection bit is output
	out		DDRA, temp		;; while every other location      is input (only right-most 2 bits used)

	; Set SS@PB4, MOSI@PB5, SCK@PB7 output, all others input
	ldi		temp, (1<<DDB4)|(1<<DDB5)|(1<<DDB7)
	out		DDRB,temp

	; Pull SS pin to HIGH at the beginning
	in		temp, PINB
	ori		temp, (1<<PB4)
	out		PORTB, temp

	; Enable SPI, Master, set clock rate fck/16
	ldi		temp,(1<<SPE)|(1<<MSTR)|(1<<SPR0)
	out		SPCR,temp

	; Enable ADC and set according registers
	ldi     temp,(1<<MUX2)|(1<<MUX0)				; Set referance voltage as PinA 5
	out     ADMUX,temp 						
	ldi		temp,(1<<ADEN)|(1<<ADATE)|(1<<ADIE)	
	out		ADCSRA,temp								; Enable conversion, auto-trigger and interrupt for conversion			

	clr		login_fsm_state	
	clr		temp3
	clr		current_amount

	ldi		current_operation,	   operation_signal_adder 	;; operation      is deposit,
	ldi     current_currency,  currency_type_signal_adder	;; currency       is TL,
	clr		current_is_reversed								;; is_reversed    is false,
	ser		released_previously								;; button release is true at the beginning of menu entrance
	ser		current_is_view_mode

	call	INIT_LCD		 ;; initialize LCD
	call	DISPLAY_WELCOME	 ;; display welcome message
	call	DELAY_100_MS

	sei


;; Sends the id of the user to the bank directly when the device is first initialized 
SEND_ID:
	ldi		YL, low(signal_cache)
	ldi		YH, high(signal_cache)

	ldi		temp, user_id				;; store the id first, even though the meet signal will be sent first,
	st		Y+, temp					;; because SEND_SPI_ITEMS sends items like a stack

	ldi		temp, error_code_zero		;; store the signal informing that our device will be sending an id
	st		Y+, temp					
	
	ldi		data_size, 2				;; total size is 2 bytes (signal: 1 byte, user id: 1 byte)

	call	SEND_SPI_ITEMS				;; signal that user id will be sent and send user id via SPI

	call	DELAY_100_MS				;; delay before trying to receive error code, for bank to process the sent data

	ldi		YL, low(error_code_cache)
	ldi		YH, high(error_code_cache)
	ldi		data_size, 1				;; load Y with the location of the beginning of the space that is allocated for error code

	call	RECEIVE_SPI_ITEMS			;; receive error code

	ld		temp, -Y					;; load temp with error code
	cpi		temp, error_code_zero		;; check whether there is an error
	breq	LOGIN_FSM_0_4				;; if not, continue with login process
SEND_ID_ERROR:		
	call	DISPLAY_RESET					
	call	DISPLAY_WRONG
	call	DISPLAY_SPACE
	call	DISPLAY_ID					;; Displays: [WRONG ID]
SEND_ID_ERROR_LOOP:						
	out		PORTC, temp					;; infinite loop (error)
	call	DELAY_400_MS

	com		temp

	out		PORTC, temp					
	call	DELAY_400_MS				;; flash out
	rjmp	SEND_ID_ERROR_LOOP			;; reached if a user with the provided id does not exist 


;; requires Y to hold SRAM location (end+1) of the data and data_size to hold data size
SEND_SPI_ITEMS:
	in		temp, PINB
	andi	temp, ~(1<<PB4)
	out		PORTB, temp					;; pull SS pin to low, to start the transmission
SEND_SPI_ITEMS_LOOP:
	ld		temp, -Y					;; decrement SRAM location and load data to temp
	out		SPDR, temp					;; send char via SPI
WAIT_TRANSMIT:
	sbis	SPSR, SPIF					;; Wait for transmittion to complete
	rjmp	WAIT_TRANSMIT

	rcall	DELAY_10_MS					;; delay between consecutive transmittions

	dec		data_size					;; decrement (remaining) data size
	brne	SEND_SPI_ITEMS_LOOP			;; if data size did not reach zero, continue

	in		temp, PINB
	ori		temp, (1<<PB4)
	out		PORTB, temp					;; pull SS pin to high, to end the transmission
	
	ret									;; return


;; requires Y to hold SRAM location (start) of the data and data_size to hold data size
RECEIVE_SPI_ITEMS:
	in		temp, PINB
	andi	temp, ~(1<<PB4)
	out		PORTB, temp					;; pull SS pin to low, to start the transmission
RECEIVE_SPI_ITEMS_LOOP:
	rcall	DELAY_10_MS					;; delay between consecutive transmittion data
	clr		temp					
	out		SPDR, temp					;; send empty data to receive data from SPI
WAIT_RECEIVE:
	sbis	SPSR, SPIF					;; Wait for transmittion to complete
	rjmp	WAIT_RECEIVE

	in		temp, SPDR					;; retrieve char from SPI register
	st		Y+, temp					;; store data in temp to SRAM and increment SRAM location

	dec		data_size					;; decrement (remaining) data size
	brne	RECEIVE_SPI_ITEMS_LOOP		;; if data size did not reach zero, continue

	in		temp, PINB
	ori		temp, (1<<PB4)
	out		PORTB, temp					;; pull SS pin to high, to end the transmission

	ret									;; return
		
;; Login Finite State Machine, that requires user to enter correct sequence of button pushes and then press enter
LOGIN_FSM_0_4:
	in		temp, PIND
	call	DELAY_1_MS
	in		temp2, PIND
	cp		temp, temp2
	brne	LOGIN_FSM_0_4				;; to eliminate issues related with bouncing (debounce)

	andi	temp, enter_password_buttons_d	
	tst		temp						;; check whether any password button is pushed
	breq	LOGIN_FSM_0_4				;; if not, continue until there is a button push
FOUR_BIT_LOG2:							;; 0001 => 0000, 0010 => 0001, 0100 => 0010, 1000 => 0011 (2^n) To fit 4-times 4-bit button push inside 1 byte
	lsr		temp
	ldi		temp2, $0c
	and		temp2, temp
	breq	LOGIN_ARRANGE_PASSWORD
	dec		temp
LOGIN_ARRANGE_PASSWORD:	
	mov		temp3, temp					;; move current digit to temp3, for later use
	inc		temp3						;; and increment it, for it to become a digit between 1-4
	mov		temp2, login_fsm_state	
	lsl		temp2						;; temp2 holds the information that how many times we should shift the incoming 2-bit part of the password, to fit the whole in 1 byte	
LOGIN_ARRANGE_PASSWORD_LOOP:
	tst		temp2
	breq	LOGIN_FSM_0_4_CONTINUE		;; if the current 2-bit part is shifted enough, continue with fsm
	dec		temp2						;; decrement remaining shift count
	lsl		temp						;; else shift again,
	rjmp	LOGIN_ARRANGE_PASSWORD_LOOP ;; and continue with the loop
LOGIN_FSM_0_4_CONTINUE:
	or		current_amount, temp		;; current_amount holds the current arranged password

	in		temp2, PIND
	tst		temp2
	brne	LOGIN_FSM_0_4_CONTINUE		;; if the last applied push still persist (button is hold), wait until it is off

	call	STEP_DISPLAY_PASSWORD		;; display the current entered digit

	inc		login_fsm_state
	cpi		login_fsm_state, 4
	brne	LOGIN_FSM_0_4				;; if not captured all 4 button pushes, continue with fsm
LOGIN_WAIT_CONFIRM:						;; else wait for confirmation button push
	in		temp, PIND
	andi	temp, confirm_password_buttons_d	
	cpi		temp, $00
	breq	LOGIN_WAIT_CONFIRM			;; if confirm button is not pushed, wait until it is pushed
LOGIN_WAIT_CONFIRM_RELEASE:
	in		temp2, PIND
	tst		temp2
	brne	LOGIN_WAIT_CONFIRM_RELEASE	;; if confirm button is not released, wait until it is
	
	ldi		YL, low(password_cache)		
	ldi		YH, high(password_cache)
	st		Y+, current_amount
	ldi		data_size, password_size	;; else store password in password cache location in SRAM

	rcall	SEND_SPI_ITEMS				;; and call SPI transmission subroutine

	call	DELAY_100_MS				;; delay before trying to receive error code, for bank to process the sent data
	
	ldi		YL, low(error_code_cache)
	ldi		YH, high(error_code_cache)
	ldi		data_size, 1				;; load Y with the location of the beginning of the space that is allocated for error code

	rcall	RECEIVE_SPI_ITEMS			;; receive error code

	ld		temp, -Y					;; load temp with error code
	cpi		temp, error_code_zero		;; check whether there is an error
	breq	LOGIN_SUCCESSFUL			;; if not, continue with successfull login process

	call	DISPLAY_RESET
	call	DISPLAY_WRONG
	call	DISPLAY_SPACE
	call	DISPLAY_PASSWORD			;; Displays: [WRONG PASSWORD]

	clr		current_amount				;; clear the password holder (current_amount),
	clr		login_fsm_state				;; and the state that the login fsm is in (login_fsm_state),
	rjmp	LOGIN_FSM_0_4				;; and return to the beginning of authentication process
LOGIN_SUCCESSFUL:
	call	DELAY_10_MS					;; delay for bank to change its state from authentication to menu operations

	rcall	FETCH_SAVING_AMOUNTS		;; fetch the savings of the user at the beginning,
	call	DISPLAY_SAVING_AMOUNTS		;; and display TL (displays the currency with id current_currency)

	in		temp, ADCSRA
	ori		temp, (1<<ADSC)
	out		ADCSRA, temp				;; start adc conversion for analog amount input,
	rjmp	MENU_MAIN_LOOP				;; then jump to the operations main menu

;; Operates the operation with id "operation_type_fetch" (0) for all currency types
;; Fetches the amount of all types of savings from the bank via SPI, and stores them in SRAM
FETCH_SAVING_AMOUNTS:

	//////////// FETCH TL //////////
	rcall	FETCH_SAVING_AMOUNT_TL		;; fetch amount of savings in TL
	////////////////////////////////
	//////// SET DISPLAYS TL ///////
	////////////////////////////////
	ldi		YL, low(saving_amount_tl)
	ldi		YH, high(saving_amount_tl)
	ld		temp4, Y+					;; resultH = temp4
	ld		temp5, Y					;; resultL = temp5

	ldi		YL, low(saving_display_tl)
	ldi		YH, high(saving_display_tl)
	call	SET_SAVING_DISPLAYS			
	////////////////////////////////
	////////////////////////////////

	call	DELAY_10_MS					;; delay between requests
	
	//////////// FETCH USD //////////
	rcall	FETCH_SAVING_AMOUNT_USD		;; fetch amount of savings in USD
	/////////////////////////////////
	//////// SET DISPLAYS USD ///////
	/////////////////////////////////
	ldi		YL, low(saving_amount_usd)
	ldi		YH, high(saving_amount_usd)
	ld		temp4, Y+					;; resultH = temp4
	ld		temp5, Y					;; resultL = temp5

	ldi		YL, low(saving_display_usd)
	ldi		YH, high(saving_display_usd)
	call	SET_SAVING_DISPLAYS		
	////////////////////////////////
	////////////////////////////////
		
	call	DELAY_10_MS					;; delay between requests

	//////////// FETCH EUR //////////
	rcall	FETCH_SAVING_AMOUNT_EUR		;; fetch amount of savings in EUR
	/////////////////////////////////
	//////// SET DISPLAYS EUR ///////
	/////////////////////////////////
	ldi		YL, low(saving_amount_eur)
	ldi		YH, high(saving_amount_eur)
	ld		temp4, Y+					;; resultH = temp4
	ld		temp5, Y					;; resultL = temp5

	ldi		YL, low(saving_display_eur)
	ldi		YH, high(saving_display_eur)
	call	SET_SAVING_DISPLAYS		
	////////////////////////////////
	////////////////////////////////

	ret

;; Operates the operation with id "operation_type_fetch" (0) and currency type "currency_type_signal_tl"(0)
;; Fetches the amount of TL savings in the bank via SPI, and stores them in SRAM
FETCH_SAVING_AMOUNT_TL:
	ldi		temp, (operation_type_fetch)|(currency_type_signal_tl)
	rjmp	FETCH_SAVING_AMOUNT

;; Operates the operation with id "operation_type_fetch" (0) and currency type "currency_type_signal_usd"(1)
;; Fetches the amount of USD savings in the bank via SPI, and stores them in SRAM
FETCH_SAVING_AMOUNT_USD:
	ldi		temp, (operation_type_fetch)|(currency_type_signal_usd)
	rjmp	FETCH_SAVING_AMOUNT

;; Operates the operation with id "operation_type_fetch" (0) and currency type "currency_type_signal_eur"(2)
;; Fetches the amount of EUR savings in the bank via SPI, and stores them in SRAM
FETCH_SAVING_AMOUNT_EUR:
	ldi		temp, (operation_type_fetch)|(currency_type_signal_eur)
	rjmp	FETCH_SAVING_AMOUNT

;; requires temp to have fetch type (TL, USD, EUR) with user id signal
FETCH_SAVING_AMOUNT:
	ldi		YL, low(signal_cache)
	ldi		YH, high(signal_cache)
	st		Y+, temp				;; store fetch signal to SRAM

	mov		temp4, temp				;; save fetch signal in temp4 to be able to compare later

	ldi		data_size, 1
	rcall	SEND_SPI_ITEMS			;; call SPI transmission subroutine to send signal of fetch

	rcall	DELAY_100_MS			;; delay for bank to process fetching operation

	;;	Continue the fetch operation (receive result) according to saving type
	cpi		temp4, (operation_type_fetch)|(currency_type_signal_tl)	
	breq	FETCH_SAVING_AMOUNT_TL_CONTINUE	

	cpi		temp4, (operation_type_fetch)|(currency_type_signal_usd)
	breq	FETCH_SAVING_AMOUNT_USD_CONTINUE

FETCH_SAVING_AMOUNT_EUR_CONTINUE:
	ldi		YL, low(saving_amount_eur)
	ldi		YH, high(saving_amount_eur)		;; give SRAM location as EUR saving location to RECEIVE_SPI_ITEMS
	rjmp	FETCH_SAVING_AMOUNT_CONTINUE

FETCH_SAVING_AMOUNT_USD_CONTINUE:
	ldi		YL, low(saving_amount_usd)
	ldi		YH, high(saving_amount_usd)		;; give SRAM location as USD saving location to RECEIVE_SPI_ITEMS
	rjmp	FETCH_SAVING_AMOUNT_CONTINUE

FETCH_SAVING_AMOUNT_TL_CONTINUE:
	ldi		YL, low(saving_amount_tl)
	ldi		YH, high(saving_amount_tl)		;; give SRAM location as TL	 saving location to RECEIVE_SPI_ITEMS

FETCH_SAVING_AMOUNT_CONTINUE:
	ldi		data_size, max_currency_amount_byte
	rcall	RECEIVE_SPI_ITEMS				;; retrieve amount of savings of a specific currency type which has "max_currency_amount_byte" bytes of size via SPI

	ret

;; Interrupt handler of adc conversion for analog deposit/withdrawal/exchange amount input
ADC_COMPLETE:
	push	temp3
	push	temp2
	push	temp
	in		temp, SREG
	push	temp

	in		temp3, ADCH		;; Get the result for conversion
	in		temp2, ADCL		;; both for higher and lower part

	lsr		temp3			;; by shifting temp3 normally,
	ror		temp2			;; and temp2 with carry, divide the total number by 2
	lsr		temp3			;; applying this operation twice,
	ror		temp2			;; will let us get the result of division by 4

	mov		current_amount, temp2 ;; as the maximum value of the ADC result can get is 1023, division by 4 will make temp2 1 byte, thus we can store it in register

	pop		temp
	out		SREG, temp
	pop		temp
	pop		temp2
	pop		temp3
	reti


;; Main loop that is entered after the authentication is complete
MENU_MAIN_LOOP:
	in		temp, PIND
	andi	temp, menu_buttons_d			;; get button push in Pin D

	mov		temp2, temp
	neg		temp2
	and		temp, temp2						;; get the right-most button push

	in		temp2, PINA
	andi	temp2, menu_buttons_a			;; get button push in Pin A

	mov		temp3, temp2
	neg		temp3
	and		temp2, temp3					;; get the right-most button push

	rcall	APPLY_MENU_BUTTONS				;; if any of the valid buttons has been pushed, apply its functionality

	cpi		current_is_view_mode, $00
	brne	MENU_MAIN_LOOP

	call	set_display_regs
	call	DISPLAY_AMOUNT_IN_PLACE

	rjmp	MENU_MAIN_LOOP					;; infinite loop



;; requires temp to have menu button push in Pin D and temp2 to have menu button push in Pin A
APPLY_MENU_BUTTONS:
	
	mov		temp3, temp
	or		temp3, temp2					   ;; if any of the menu buttons is not pushed,
	breq	APPLY_MENU_BUTTONS_NO_PUSH_RET	   ;; return through no push branch

	tst		released_previously				   ;; if there is no release since the last applied push
	breq	APPLY_MENU_BUTTONS_RET			   ;; return immediately 

;; There is a valid menu button push
	mov		temp3, temp
	andi	temp3, operation_reverse_buttons_d ;; if reverse exchange direction button is pushed,
	brne	CHANGE_EXCHANGE_DIRECTION		   ;; apply

	mov		temp3, temp
	andi	temp3, operation_type_buttons_d	   ;; if change operation button is pushed,
	brne	CHANGE_OPERATION_TYPE			   ;; apply

	mov		temp3, temp
	andi	temp3, currency_type_buttons_d	   ;; if change currency type button is pushed,
	brne	CHANGE_CURRENCY_TYPE			   ;; apply
   
	mov		temp3, temp2	
	andi	temp3, confirmation_buttons_a	   ;; if confirmation button is pushed,
	brne	REQUEST_OPERATION				   ;; apply

	jmp		DISPLAY_SAVING_AMOUNTS			   ;; if reached, display saving amount button is pushed, thus apply


;; Reached if there is an applied button push in the current frame
APPLY_MENU_BUTTONS_VALID_RET:
	clr		released_previously	 ;; button is NOT released previously (for upcoming frames) if there is an applied button push in the current frame
	call	DISPLAY_OPERATION
	ret
;; Reached if there is no menu button push in the current frame
APPLY_MENU_BUTTONS_NO_PUSH_RET:
	ser		released_previously
;; Reached if there is button hold (there is no frame with all menu buttons released since the last applied push)
APPLY_MENU_BUTTONS_RET:
	ret
	
;; Subroutine to change exchange direction
;; Reached if there is a valid "direction reverse" button push
CHANGE_EXCHANGE_DIRECTION:
	ldi		temp3, exchange_reversed_signal
	eor		current_is_reversed, temp3	;; switch least-significant bit of "current_is_reversed" to the opposite of the current one
	rjmp	APPLY_MENU_BUTTONS_VALID_RET

;; Subroutine to change operation type
;; Reached if there is a valid "change operation" button push
CHANGE_OPERATION_TYPE:
	clr		current_is_view_mode						;; if an operation is selected, switch from the view mode
	ldi		current_operation, operation_signal_adder	;; set current operation as deposit

	cpi		temp, right_most_operation_button			;; if the button push is the right_most, 
	breq	APPLY_MENU_BUTTONS_VALID_RET				;; leave current operation as deposit and return

	ldi		temp3, operation_signal_adder				;; if not, load temp3 temp3 with 1, that is shifted with required amount
	add		current_operation, temp3					;; and add it with current operation. This will equate to changing the current operation to next one (withdraw),
	lsr		temp										;; and right-shift temp

	cpi		temp, right_most_operation_button			;; if currently temp is equal to right-most button
	breq	APPLY_MENU_BUTTONS_VALID_RET				;; then leave the current operation as withdraw and return
	
	ldi		temp3, operation_signal_adder				;; else the operation is exchange,
	add		current_operation, temp3					;; so add the shifted 1, to get exchange operation, and set the current operation as exchange,
	rjmp	APPLY_MENU_BUTTONS_VALID_RET				;; then return

;; Subroutine to change currency type
;; Reached if there is a valid "change currency" button push
CHANGE_CURRENCY_TYPE:
	clr		current_currency							;; set the current currency as TL

	cpi		temp, right_most_currency_button			;; if the button push is right-most
	breq	CHANGE_CURRENCY_TYPE_RET				;; leave current currency as TL and return

	ldi		temp3, currency_type_signal_adder			;; if not, load temp3 with 1, that is shifted with required amount
	add		current_currency, temp3						;; and add it with current currency. This will equate to changing current currency to next one (USD),
	lsr		temp										;; and right-shift temp

	cpi		temp, right_most_currency_button			;; if currently temp is equal to right-most button
	breq	CHANGE_CURRENCY_TYPE_RET				;; then leave the current currency as USD and return

	ldi		temp3, currency_type_signal_adder			;; else the currency is EUR,
	add		current_currency, temp3						;; so add the shifted 1, to get EUR currency, and set the current currency as EUR
	rjmp	CHANGE_CURRENCY_TYPE_RET				;; then return

;; Changing currency type does not cause switching from the view mode, 
;; thus do not return from the "usual" branch and display the amount of the selected currency type
CHANGE_CURRENCY_TYPE_RET:
	call	DISPLAY_SAVING_AMOUNTS
	clr		released_previously
	ret

;; Subroutine to request an operation from the bank
;; Reached if the confirm button is pushed after the operation selection is done
REQUEST_OPERATION:

	ser		temp
	out		PORTC,	temp
	call	DELAY_400_MS				;; signal the user

	ldi		YL, low(signal_cache)
	ldi		YH, high(signal_cache)
	st		Y+, current_amount			;; store current amount in SRAM
	
	clr		temp
	or		temp, current_operation
	or		temp, current_currency
	or		temp, current_is_reversed	;; combine all of the operation type related part of the signal (first byte)
	st		Y+, temp					;; and store the operation signal in SRAM

	ldi		data_size, signal_size_max	;; load data size with 2 (operation signal: 1 byte, amount: 1 byte)

	call	SEND_SPI_ITEMS				;; send the operation signal to the bank
	rcall	DELAY_100_MS				;; delay for the bank to process the operation

	ldi		YL, low(error_code_cache)
	ldi		YH, high(error_code_cache)	;; load Y with the location of the beginning of the space that is allocated for error code
	ldi		data_size, error_code_size	;; load data size with error_code_size to retrieve error code

	call	RECEIVE_SPI_ITEMS			;; receive error code

	ld		temp, -Y					;; load temp with error code

	cpi		temp, sufficient_money		;; check whether there is an error
	brne	INSUFFICIENT_MONEY_RET		;; if there is, return

	call	DISPLAY_TRANSACTION_DONE
	rcall	DELAY_10_MS					;; if there is not, wait some time before the next operation

	call	FETCH_SAVING_AMOUNTS		;; and fetch the changes that occured due to the operation
	rcall	DELAY_10_MS

	rjmp	APPLY_MENU_BUTTONS_VALID_RET


DISPLAY_TRANSACTION_DONE:
	call	DISPLAY_RESET

	ldi		temp, $54
	rcall	SEND_LCD_CHAR	; ascii: T
	ldi		temp, $52
	rcall	SEND_LCD_CHAR	; ascii: R
	ldi		temp, $41
	rcall	SEND_LCD_CHAR	; ascii: A
	ldi		temp, $4e
	rcall	SEND_LCD_CHAR	; ascii: N
	ldi		temp, $53
	rcall	SEND_LCD_CHAR	; ascii: S
	ldi		temp, $41
	rcall	SEND_LCD_CHAR	; ascii: A
	ldi		temp, $43
	rcall	SEND_LCD_CHAR	; ascii: C
	ldi		temp, $54
	rcall	SEND_LCD_CHAR	; ascii: T
	ldi		temp, $2E
	rcall	SEND_LCD_CHAR	; ascii: [.]

	call	DISPLAY_SPACE

	ldi		temp, $44
	rcall	SEND_LCD_CHAR	; ascii: D
	ldi		temp, $4f
	rcall	SEND_LCD_CHAR	; ascii: O
	ldi		temp, $4e
	rcall	SEND_LCD_CHAR	; ascii: N
	ldi		temp, $45
	rcall	SEND_LCD_CHAR	; ascii: E

	call	DELAY_400_MS
	call	DELAY_400_MS
	call	DELAY_400_MS
	call	DELAY_400_MS	; Wait to make the text be visible

	call	DISPLAY_RESET
	ret

INSUFFICIENT_MONEY_RET:
	call	DISPLAY_RESET

	call	DISPLAY_INSUFFICIENT
	call	DISPLAY_SPACE
	call	DISPLAY_MONEY		; Displays: [INSUFFICIENT MONEY]

	call	DELAY_400_MS
	call	DELAY_400_MS
	call	DELAY_400_MS
	call	DELAY_400_MS		; Wait to make the text be visible

	call	DISPLAY_RESET

	call	DISPLAY_OPERATION	; Return the usual process where the operation is displayed
	ret

DISPLAY_INSUFFICIENT:
	ldi		temp, $49
	rcall	SEND_LCD_CHAR	; ascii: I
	ldi		temp, $4e
	rcall	SEND_LCD_CHAR	; ascii: N
	ldi		temp, $53
	rcall	SEND_LCD_CHAR	; ascii: S
	ldi		temp, $55
	rcall	SEND_LCD_CHAR	; ascii: U
	ldi		temp, $46
	rcall	SEND_LCD_CHAR	; ascii: F
	ldi		temp, $2E
	rcall	SEND_LCD_CHAR	; ascii: [.]
	ret

DISPLAY_MONEY:
	ldi		temp, $4d
	rcall	SEND_LCD_CHAR	; ascii: M
	ldi		temp, $4F
	rcall	SEND_LCD_CHAR	; ascii: O
	ldi		temp, $4e
	rcall	SEND_LCD_CHAR	; ascii: N
	ldi		temp, $45
	rcall	SEND_LCD_CHAR	; ascii: E
	ldi		temp, $59
	rcall	SEND_LCD_CHAR	; ascii: Y
	ret


DISPLAY_OWNED:
	ldi		temp, $4f
	rcall	SEND_LCD_CHAR	; ascii: O
	ldi		temp, $57
	rcall	SEND_LCD_CHAR	; ascii: W
	ldi		temp, $4e
	rcall	SEND_LCD_CHAR	; ascii: N
	ldi		temp, $45
	rcall	SEND_LCD_CHAR	; ascii: E
	ldi		temp, $44
	rcall	SEND_LCD_CHAR	; ascii: D
	ret

;; Displays the amount of savings in a certain type of currency
DISPLAY_SAVING_AMOUNTS:
	ser		current_is_view_mode	;; set the current mode as view mode to be able to surpass the display operations for entered ADC amount

	call	DISPLAY_RESET
	call	DISPLAY_OWNED
	call	DISPLAY_SPACE			; Displays: [OWNED ] 

	ldi		temp5, max_currency_display_byte			;; load temp5 with the amount of bytes required to store the display values

	cpi		current_currency, currency_type_signal_tl	;; if current selected currency type is tl,
	breq	DISPLAY_SAVING_AMOUNTS_TL					;; branch to "DISPLAY_SAVING_AMOUNTS_TL"

	cpi		current_currency, currency_type_signal_usd	;; if current selected currency type is usd,
	breq	DISPLAY_SAVING_AMOUNTS_USD					;; branch to "DISPLAY_SAVING_AMOUNTS_USD"

DISPLAY_SAVING_AMOUNTS_EUR:								;; else branch to "DISPLAY_SAVING_AMOUNTS_EUR"
	call	DISPLAY_EUR				; Displays: [EUR]

	ldi		YL, low(saving_display_eur+max_currency_display_byte)	 
	ldi		YH, high(saving_display_eur+max_currency_display_byte)	;; load Y with the location of highest EUR digit + 1
	rjmp	DISPLAY_SAVING_AMOUNTS_CONTINUE

DISPLAY_SAVING_AMOUNTS_USD:
	call	DISPLAY_USD				; Displays: [USD]

	ldi		YL, low(saving_display_usd+max_currency_display_byte)
	ldi		YH, high(saving_display_usd+max_currency_display_byte)	;; load Y with the location of highest USD digit + 1
	rjmp	DISPLAY_SAVING_AMOUNTS_CONTINUE

DISPLAY_SAVING_AMOUNTS_TL:
	call	DISPLAY_TL				; Displays: [TL]

	ldi		YL, low(saving_display_tl+max_currency_display_byte)
	ldi		YH, high(saving_display_tl+max_currency_display_byte)	;; load Y with the location of highest TL digit + 1

DISPLAY_SAVING_AMOUNTS_CONTINUE:
	call	DISPLAY_COLON
	call	DISPLAY_SPACE			; Displays: [: ]

;; Displays the saving digits sequentially, from higher to lower digits
DISPLAY_SAVING_AMOUNTS_LOOP:
	ld		temp, -Y						;; load temp with next digit
	rcall	SEND_LCD_CHAR					;; display it
	dec		temp5							;; decrement remaining digits
	brne	DISPLAY_SAVING_AMOUNTS_LOOP		;; loop if any digit remains
	ret


DISPLAY_WELCOME:
	ldi		temp, $57
	rcall	SEND_LCD_CHAR	; ascii: W
	ldi		temp, $45
	rcall	SEND_LCD_CHAR	; ascii: E
	ldi		temp, $4c
	rcall	SEND_LCD_CHAR	; ascii: L
	ldi		temp, $43
	rcall	SEND_LCD_CHAR	; ascii: C
	ldi		temp, $4f
	rcall	SEND_LCD_CHAR	; ascii: O
	ldi		temp, $4d
	rcall	SEND_LCD_CHAR	; ascii: M
	ldi		temp, $45
	rcall	SEND_LCD_CHAR	; ascii: E
	ret

STEP_DISPLAY_PASSWORD:
	cpi		login_fsm_state, 0
	brne	STEP_DISPLAY_PASSWORD_CONTINUE	;; if not in first state of the login fsm, continue with displaying the digits

	rcall	DISPLAY_RESET					;; else, reset the display and display the required text, then continue with displaying digits
	rcall	DISPLAY_PASSWORD			
	rcall	DISPLAY_COLON				
	rcall	DISPLAY_SPACE					; [DISPLAYS] PASSWORD: 

STEP_DISPLAY_PASSWORD_CONTINUE:
	ldi		temp, $30
	add		temp, temp3		;; add the offset to the digit, for it to become an ascii character
	rcall	SEND_LCD_CHAR   ;; then display it
	ret

DISPLAY_WRONG:
	ldi		temp, $57
	rcall	SEND_LCD_CHAR	; ascii: W
	ldi		temp, $52
	rcall	SEND_LCD_CHAR	; ascii: R
	ldi		temp, $4f
	rcall	SEND_LCD_CHAR	; ascii: O
	ldi		temp, $4e
	rcall	SEND_LCD_CHAR	; ascii: N
	ldi		temp, $47
	rcall	SEND_LCD_CHAR	; ascii: G
	ret

DISPLAY_ID:
	ldi		temp, $49
	rcall	SEND_LCD_CHAR	; ascii: I
	ldi		temp, $44
	rcall	SEND_LCD_CHAR	; ascii: D
	ret

DISPLAY_PASSWORD:
	ldi		temp, $50
	rcall	SEND_LCD_CHAR	; ascii: P
	ldi		temp, $41
	rcall	SEND_LCD_CHAR	; ascii: A
	ldi		temp, $53
	rcall	SEND_LCD_CHAR	; ascii: S
	ldi		temp, $53
	rcall	SEND_LCD_CHAR	; ascii: S
	ldi		temp, $57
	rcall	SEND_LCD_CHAR	; ascii: W
	ldi		temp, $4f
	rcall	SEND_LCD_CHAR	; ascii: O
	ldi		temp, $52
	rcall	SEND_LCD_CHAR	; ascii: R
	ldi		temp, $44
	rcall	SEND_LCD_CHAR	; ascii: D
	ret

DISPLAY_OPERATION:
	rcall	DISPLAY_RESET
	rcall	DISPLAY_OPERATION_TYPE
	rcall	DISPLAY_SPACE			; Displays: [<operation type> ]

	cpi		current_operation, operation_type_exchange
	breq	DISPLAY_OPERATION_EXCHANGE					;; if the current operation is exchange, display currency part accordingly

	rcall	DISPLAY_CURRENCY_TYPE						;; else directly display the current currency,
	rjmp	DISPLAY_OPERATION_CONTINUE					;; and continue

DISPLAY_OPERATION_EXCHANGE:
	rcall	DISPLAY_EXCHANGE_TYPE						;; display exchange type

DISPLAY_OPERATION_CONTINUE:
	rcall	DISPLAY_COLON
	rcall	DISPLAY_SPACE			; Displays: [: ]
	ret

DISPLAY_TL:
	ldi		temp, $54
	rcall	SEND_LCD_CHAR	; ascii: T
	ldi		temp, $4C
	rcall	SEND_LCD_CHAR	; ascii: L
	ret

DISPLAY_USD:
	ldi		temp, $55
	rcall	SEND_LCD_CHAR	; ascii: U
	ldi		temp, $53
	rcall	SEND_LCD_CHAR	; ascii: S
	ldi		temp, $44
	rcall	SEND_LCD_CHAR	; ascii: D
	ret

DISPLAY_EUR:
	ldi		temp, $45
	rcall	SEND_LCD_CHAR	; ascii: E
	ldi		temp, $55
	rcall	SEND_LCD_CHAR	; ascii: U
	ldi		temp, $52
	rcall	SEND_LCD_CHAR	; ascii: R
	ret

DISPLAY_COLON:
	ldi		temp, $3a
	rcall	SEND_LCD_CHAR	; ascii: [:]
	ret

DISPLAY_DASH:
	ldi		temp, $2d
	rcall	SEND_LCD_CHAR	; ascii: [-]
	ret

DISPLAY_TL_USD:
	rcall	DISPLAY_TL
	rcall	DISPLAY_DASH
	rcall	DISPLAY_USD		; Displays: [TL-USD]
	ret

DISPLAY_USD_TL:
	rcall	DISPLAY_USD
	rcall	DISPLAY_DASH
	rcall	DISPLAY_TL		; Displays: [USD-TL]
	ret

DISPLAY_TL_EUR:
	rcall	DISPLAY_TL
	rcall	DISPLAY_DASH
	rcall	DISPLAY_EUR		; Displays: [TL-EUR]
	ret

DISPLAY_EUR_TL:
	rcall	DISPLAY_EUR
	rcall	DISPLAY_DASH
	rcall	DISPLAY_TL		; Displays: [EUR-TL]
	ret

DISPLAY_USD_EUR:
	rcall	DISPLAY_USD
	rcall	DISPLAY_DASH
	rcall	DISPLAY_EUR		; Displays: [USD-EUR]
	ret

DISPLAY_EUR_USD:
	rcall	DISPLAY_EUR
	rcall	DISPLAY_DASH
	rcall	DISPLAY_USD		; Displays: [EUR-USD]
	ret

DISPLAY_OPERATION_TYPE:
	cpi		current_operation, operation_type_deposit		;; if the current operation is deposit
	breq	DISPLAY_DEPOSIT									;; display "deposit"

	cpi		current_operation, operation_type_withdraw		;; if the current operation is withdraw
	breq	DISPLAY_WITHDRAW								;; display "withdraw"

	cpi		current_operation, operation_type_exchange		;; if the current operation is exchange
	breq	DISPLAY_EXCHANGE								;; display "exchange"

	ret

DISPLAY_CURRENCY_TYPE:
	cpi		current_currency, currency_type_signal_tl		;; if the current currency is tl
	breq	DISPLAY_TL										;; display "tl"

	cpi		current_currency, currency_type_signal_usd		;; if the current currency is usd
	breq	DISPLAY_USD										;; display "usd"

	cpi		current_currency, currency_type_signal_eur		;; if the current currency is eur
	breq	DISPLAY_EUR										;; display "eur"

	ret

;; Display the exchange type (ex. TL-USD)
DISPLAY_EXCHANGE_TYPE:
	cpi		current_is_reversed, exchange_reversed_signal	;; if currently, the exchange types are reversed
	breq	DISPLAY_EXCHANGE_TYPE_REVERSE					;; branch accordingly

DISPLAY_EXCHANGE_TYPE_FORWARD:								;; else, display normally 
	cpi		current_currency, exchange_type_signal_tl_usd	;; if current exchange type is tl-usd
	breq	DISPLAY_TL_USD									;; display "tl-usd"

	cpi		current_currency, exchange_type_signal_tl_eur	;; if current exchange type is tl-eur
	breq	DISPLAY_TL_EUR									;; display "tl-eur"

	cpi		current_currency, exchange_type_signal_usd_eur	;; if current exchange type is usd-eur
	breq	DISPLAY_USD_EUR									;; display "usd-eur"
		
	ret

DISPLAY_EXCHANGE_TYPE_REVERSE:								;; display in reverse order
	cpi		current_currency, exchange_type_signal_tl_usd	;; if current exchange type is tl-usd
	breq	DISPLAY_USD_TL									;; display "usd-tl"

	cpi		current_currency, exchange_type_signal_tl_eur	;; if current exchange type is tl-eur
	breq	DISPLAY_EUR_TL									;; display "eur-tl"

	cpi		current_currency, exchange_type_signal_usd_eur	;; if current exchange type is usd-eur
	breq	DISPLAY_EUR_USD									;; display "eur-usd"

	ret

DISPLAY_DEPOSIT:
	ldi		temp, $44
	rcall	SEND_LCD_CHAR	; ascii: D
	ldi		temp, $45
	rcall	SEND_LCD_CHAR	; ascii: E
	ldi		temp, $50
	rcall	SEND_LCD_CHAR	; ascii: P
	ldi		temp, $4f
	rcall	SEND_LCD_CHAR	; ascii: O
	ldi		temp, $53
	rcall	SEND_LCD_CHAR	; ascii: S
	ldi		temp, $49
	rcall	SEND_LCD_CHAR	; ascii: I
	ldi		temp, $54
	rcall	SEND_LCD_CHAR	; ascii: T
	ret

DISPLAY_WITHDRAW:
	ldi		temp, $57
	rcall	SEND_LCD_CHAR	; ascii: W
	ldi		temp, $49
	rcall	SEND_LCD_CHAR	; ascii: I
	ldi		temp, $54
	rcall	SEND_LCD_CHAR	; ascii: T
	ldi		temp, $48
	rcall	SEND_LCD_CHAR	; ascii: H
	ldi		temp, $2E
	rcall	SEND_LCD_CHAR	; ascii: [.]
	ret

DISPLAY_EXCHANGE:
	ldi		temp, $45
	rcall	SEND_LCD_CHAR	; ascii: E
	ldi		temp, $58
	rcall	SEND_LCD_CHAR	; ascii: X
	ldi		temp, $2E
	rcall	SEND_LCD_CHAR	; ascii: [.]
	ret	

DISPLAY_SPACE:
	ldi		temp, $20
	rcall	SEND_LCD_CHAR	; ascii: [SPACE]
	ret
	
DISPLAY_RESET:
	ldi		temp, $01
	rcall	SEND_LCD_CMD

	rcall	DELAY_1_MS
	rcall	DELAY_1_MS		;; Delay 2 milliseconds after clearing display
	ret

;; Displays the amount selected on the LCD, and returns to the initial cursor position
DISPLAY_AMOUNT_IN_PLACE:

	mov		temp, amount_display3
	rcall	SEND_LCD_CHAR			;; Display the left-most digit

	mov		temp, amount_display2
	rcall	SEND_LCD_CHAR			;; Display the middle digit

	mov		temp, amount_display1
	rcall	SEND_LCD_CHAR			;; Display the right-most digit

	;; Shift the cursor to the left 3 times to get to the initial position
	ldi		temp, $10
	rcall	SEND_LCD_CMD
	rcall	DELAY_50_US
	ldi		temp, $10
	rcall	SEND_LCD_CMD
	rcall	DELAY_50_US
	ldi		temp, $10
	rcall	SEND_LCD_CMD
	rcall	DELAY_50_US

	ret

;; Sets the display registers according to the entered ADC amount
;; Applies a division/modulo process by subtracting succesively
set_display_regs:
	ldi		temp, $30					;; initialize temp with $30, to later add it with the digit and get the ascii code
	ldi		temp2, 0					;; temp2 holds the quotient for the nth step in determining the digits
	ldi		temp3, 0					;; temp3 holds n (digit index, from right to left)
	mov		temp4, current_amount
division_loop:
	cpi		temp4, 10					
	brlo	set_remainder_and_continue	;; if temp4 < 10, then set the nth display and continue if necessary
bigger_than_10:
	subi	temp4, 10		
	inc		temp2						;; while result is bigger than 10, increment the division result
	rjmp	division_loop				;; loop
set_remainder_and_continue:
	inc		temp3						;; as we obtained the remainder, to change the register of the current display we are working on,
	cpi		temp3, 2					;; compare in which state we are in
	brlo	set_display1				;; continue with setting display 1
	breq	set_display2				;; continue with setting display 2
set_display3:							;; continue with setting display 3
	mov		amount_display3, temp4		;; assign remainder to display 3,
	add		amount_display3, temp		;; add the ascii offset,
	ret									;; and return
set_display2:
	mov		amount_display2, temp4		;; assign remainder to display 2,
	add		amount_display2, temp		;; add the ascii offset,
	rjmp	set_display_1_2_common		;; and continue with setting the next display
set_display1:
	mov		amount_display1, temp4		;; assign remainder to display 1,
	add		amount_display1, temp		;; add the ascii offset and continue with setting the next display
set_display_1_2_common:
	mov		temp4, temp2				;; set the new amount as the found quotient
	clr		temp2						;; set the initial quotient as 0
	rjmp	division_loop				;; and loop


;; Stores the display digits of the raw saving amounts in the SRAM 
;; Applies a division/modulo process by subtracting succesively
SET_SAVING_DISPLAYS:
	cli											;; reset the global interrupt enable
	ldi		temp, 0								;; temp  holds the higher byte of the quotient for the nth step in division
	ldi		temp2, 0							;; temp2 holds the lower  byte of the quotient for the nth step in division
	ldi		temp3, 0							;; temp3 holds n (digit index, from right to left)
SAVING_DIVISION_LOOP:
	cpi		temp4, 0							
	brne	SAVING_BIGGER_THAN_10				;; if resultH(temp4) is > 0 then remainder is > 10, thus continue subtracting
	cpi		temp5, 10							;; if resultL(temp5) is > 10 then remainder is > 10, thus continue subtracting
	brlo	SAVING_SET_REMAINDER_AND_CONTINUE	;; if resultH = 0 and resultL < 10, then set the nth display and continue if necessary
SAVING_BIGGER_THAN_10:
	subi	temp5, 10		
	sbci	temp4, 0							;; Subtract 10 from results[1:0] to reach remainder,
	inc		temp2								;;  while the result is bigger than 10, incrementing the division result
	brne	SAVING_BIGGER_THAN_10_CONTINUE
	inc		temp								;; if the lower byte of the quotient has reached 255 + 1, increment the higher byte of the quotient 
SAVING_BIGGER_THAN_10_CONTINUE:
	rjmp	SAVING_DIVISION_LOOP				;; loop
SAVING_SET_REMAINDER_AND_CONTINUE:
	ldi		temp4, $30
	add		temp5, temp4						;; add the ascii offset to the found remainder
	st		Y+, temp5							;; store it into the given location
	inc		temp3								;; and increment the current state (digit)
	cpi		temp3, 5							;; if the current state is not 5
	brne	SET_SAVING_DISPLAY_1_4_COMMON		;; continue with the loop operations
	sei											;; else set the global interrupt enable,
	ret											;; and return
SET_SAVING_DISPLAY_1_4_COMMON:
	mov		temp4, temp							;; load the higher amount byte with the higher byte of the quotient
	mov		temp5, temp2						;; load the lower  amount byte with the lower  byte of the quotient
	clr		temp								;; set the higher part of the initial quotient as 0
	clr		temp2								;; set the lower  part of the initial quotient as 0
	rjmp	SAVING_DIVISION_LOOP				;; and loop
	

;; Initializes LCD with respect to the specified procedure in the datasheet
INIT_LCD:
	rcall	DELAY_10_MS
	rcall	DELAY_10_MS		;; init-delay (20ms)

	ldi		temp, $30
	rcall	SEND_LCD_PULSE	;; init-signal (Power-On)

	rcall	DELAY_1_MS
	rcall	DELAY_1_MS
	rcall	DELAY_1_MS	
	rcall	DELAY_1_MS	
	rcall	DELAY_1_MS		;; init-delay (5ms)

	ldi		temp, $30
	rcall	SEND_LCD_PULSE  ;; init-signal (4-bit)

	rcall	DELAY_50_US
	rcall	DELAY_50_US
	rcall	DELAY_50_US		;; init-delay (150us)

	ldi		temp, $30
	rcall	SEND_LCD_PULSE  ;; init-signal (4-bit)

	rcall	DELAY_50_US
	rcall	DELAY_50_US
	rcall	DELAY_50_US		;; init-delay (150us)

	ldi		temp, $20
	rcall	SEND_LCD_PULSE	;; init-signal (controller)

	rcall	DELAY_50_US
	rcall	DELAY_50_US
	rcall	DELAY_50_US		;; init-delay (150us)

	ldi		temp, $28
	rcall	SEND_LCD_CMD	;; 2-Line 5x8 display mode

	ldi		temp, $06
	rcall	SEND_LCD_CMD	;; Increment mode with no shift

	ldi		temp, $0f
	rcall	SEND_LCD_CMD	;; init-command

	ldi		temp, $01
	rcall	SEND_LCD_CMD	;; Clear

	rcall	DELAY_1_MS
	rcall	DELAY_1_MS		;; last delay
	ret

;; Sends a message as pulse to LCD
;; Requires temp to have the data to be sent
SEND_LCD_PULSE:
	clr		temp4			;; clear temp4
	ldi		temp2, $40		;; load temp2 with LCD enable bit

	out		PORTC, temp		;; output the given pulse char in temp to PORTC
	out		PORTD, temp2	;; enable pulse
	out		PORTD, temp4	;; disable to finish it

	ret

;; Sends ascii char to LCD, to display it where the cursor is currently located
;; Requires temp to have the ascii character
SEND_LCD_CHAR:
	ldi		temp2, $04		;; load temp2 with data enable bit
	out		PORTA, temp2	;; output data enable bit to Port A

	rcall	SEND_LCD_CMD	;; send command to LCD, command will be interpreted as ascii character

	clr		temp2			;; then clear temp2,
	out		PORTA, temp2	;; to change the command interpretation from data to command
	rcall	DELAY_50_US		;; lastly delay

	ret

;; Sends LCD command assuming 4-bit mode
;; Requires temp to have the command to be sent
SEND_LCD_CMD:
	
	rcall	SEND_LCD_PULSE	;; first send the most-significant 4-bits of temp as a pulse
	rcall	DELAY_50_US		;; delay a bit before sending the second command (least significant 4 bits) 

	swap	temp			;; swap the nibbles of temp

	rcall	SEND_LCD_PULSE  ;; send the least-significant 4-bits of temp as a pulse
	rcall	DELAY_50_US		;; lastly delay before returning

	ret

; Delays for 100 miliseconds. The algorithm is similar to the example in homework 2.
DELAY_100_MS:   
	cli        
	push temp
	push temp2

    ldi temp, $a6           
    ldi temp2, $61   
DELAY_100_MS_LOOP:    
    subi temp, $01    
    sbci temp2, $00                 
	brcc DELAY_100_MS_LOOP  

	pop temp2
	pop temp
	sei            
	ret 

; Delays for approximately 400 milliseconds
DELAY_400_MS:         
	cli       
	push temp
	push temp2
	push temp3

    ldi temp, $9e           
    ldi temp2, $86
	ldi	temp3, $01   
DELAY_400_MS_LOOP:    
    subi temp, $01    
    sbci temp2, $00   
	sbci temp3, $00              
	brcc DELAY_400_MS_LOOP  

	pop temp3
	pop temp2
	pop temp   
	sei         
	ret 

; Delays for approximately 1 millisecond
DELAY_1_MS:              
	cli  
	push temp
	push temp2
	push temp3

    ldi temp, $f8           
    ldi temp2, $00
DELAY_1_MS_LOOP:    
    subi temp, $01    
    sbci temp2, $00           
	brcc DELAY_1_MS_LOOP  

	pop temp3
	pop temp2
	pop temp
	sei            
	ret 

; Delays for approximately 10 milliseconds
DELAY_10_MS:           
	cli     
	push temp
	push temp2

    ldi temp, $c2        
    ldi temp2, $09
DELAY_10_MS_LOOP:    
    subi temp, $01    
    sbci temp2, $00              
	brcc DELAY_10_MS_LOOP 

	pop temp2
	pop temp
	sei             
	ret 

; Delays for approximately 50 microseconds
DELAY_50_US:      
	cli         
	push temp
	push temp2
	 
    ldi temp, $0a        
    ldi temp2, $00
DELAY_50_US_LOOP:    
    subi temp, $01    
    sbci temp2, $00             
	brcc DELAY_50_US_LOOP   

	pop temp2
	pop temp
	sei           
	ret 

