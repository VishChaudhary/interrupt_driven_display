; subroutine_polled_based_display
; Author: vish75000
; Target: AVR128DB48 @ 4MHZ

.dseg

	bcd_entries: .byte 4		;creating the bcd_entries array x4000 to x4003
	led_display: .byte 4		;creating the led_display array x4004 to x4007
	digit_num:   .byte 1			;creating the digit_num starts at x4008

.cseg					;start of code segment

.equ PERIOD_EXAMPLE_VALUE = 60 ;each period is 64us and we what a delay of 25ms so we need 390 cycles

reset:
 	jmp setup			;reset vector executed a power ON

.org TCA0_OVF_vect
	jmp multiplex_display

.org PORTE_PORT_vect
	jmp porte_isr		;vector for all PORTE pin change IRQs

setup:
	;configure TCA0
	ldi r16, TCA_SINGLE_WGMODE_NORMAL_gc	;WGMODE normal
	sts TCA0_SINGLE_CTRLB, r16

	ldi r16, TCA_SINGLE_OVF_bm		;enable overflow interrupt
	sts TCA0_SINGLE_INTCTRL, r16

	;load period low byte then high byte
	ldi r16, LOW(PERIOD_EXAMPLE_VALUE)		;set the period
	sts TCA0_SINGLE_PER, r16
	ldi r16, HIGH(PERIOD_EXAMPLE_VALUE)
	sts TCA0_SINGLE_PER + 1, r16

	;set clock and start timer
	ldi r16, TCA_SINGLE_CLKSEL_DIV256_gc | TCA_SINGLE_ENABLE_bm
	sts TCA0_SINGLE_CTRLA, r16

	ldi r16, 0x00					
	sts digit_num, r16				;on reset making digit_num = 0
	sts PORTC_DIR, r16				;port C is inputs		
				
	sts PORTE_DIR, r16				;load port E with pin 0 as input 
	ldi r16, 0x02
	sts PORTE_PIN0CTRL, r16			;port E pin 0 intrrupt is triggered by a rising	edge

	ldi r16, 0xFF
	sts PORTD_DIR, r16				;PORT D is outputs
	sts PORTA_DIR, r16				;PORT A is outputs
	rcall pull_enable				;enable pullups and inven on all pins of port C

	ldi ZH, HIGH(led_display)
	ldi ZL, LOW(led_display)
	ldi r16, 0x01
	st Z+, r16
	st Z+, r16
	st Z+, r16
	st Z+, r16						;loading lcd_display with all 0s

	sei								;enable global intrreput
	
main_loop:
	nop
	rjmp main_loop

porte_ISR:
	cli				;clear global interrupt enable, I = 0       ;I should have push my regs for poll_digit_entry in here
	push r16		;save r16 then SREG, note I = 0
	in r16, CPU_SREG
	push r16

	;Determine which pins of PORTE have IRQs
	lds r16, PORTE_INTFLAGS	;check for PE0 IRQ flag set
	sbrc r16, 0
	rcall poll_digit_entry			;execute subroutine for PE0

	pop r16				;restore SREG then r16
	out CPU_SREG, r16	;note I in SREG now = 0
	pop r16
	sei				;SREG I = 1
	reti			;return from PORTE pin change ISR

pull_enable:
	push r16
	ldi r16, 0x88					;turns on the inven and pullup
	sts PORTC_PINCONFIG, r16		;loading x88 into pinConfig
	ldi r16, 0xFF
	sts PORTC_PINCTRLUPD, r16		;where there is a 1, the corresponding pin will have PINCONFIG in it
	pop r16
	ret
	
multiplex_display:
ISR_push:
	push r16			;save register
	in r16, CPU_SREG
	push r16
	push r17


multiplex_display_push:
	push r16					;push the registers that I used
	push r17					
	push r18

turn_off:
	ldi r16, 0xFF			;1111 1111 
	sts PORTA_OUT, r16

	lds r17, digit_num		;loading digit_num into r17
	inc r17					;increment digit_num
	cpi r17, 0x04			;if next digit is 4 
	breq overflow			;then next digit will be 0
	sts digit_num, r17		;save new digit_num

	output:
	ldi ZH, HIGH(led_display)	;make pointer to index of led_display
	ldi ZL, LOW(led_display)
	ldi r18, 0x00				
	add ZL, r17					;add digit num to low byte of led_display address
	adc ZH, r18					;add carry to high byte of led_display
	ld r16, Z					;load r16 with what the pointer is aiming atoutput:
	sts PORTD_OUT, r16			;outputing that number

checking_what_digit:
	cpi r17, 0x00			;if digit_num = 0
	breq digit0
	cpi r17, 0x01			;if digit_num = 1
	breq digit1
	cpi r17, 0x02			;if digit_num = 2
	breq digit2
	cpi r17, 0x03			;if digit_num = 3
	breq digit3

digit0:
	ldi r18, 0xE0			;0111 0000 turning on leftmost digit aka digit 0 PA7
	sts PORTA_OUT, r18
	rjmp multiplex_display_clear_ISR
 
digit1:
	ldi r18, 0xD0			;1011 0000 turning on second leftmost digit aka digit 1 PA6
	sts PORTA_OUT, r18
	rjmp multiplex_display_clear_ISR

digit2:
	ldi r18, 0xB0			;1101 0000 turning on second rightmost digit aka digit 2 PA5
	sts PORTA_OUT, r18
	rjmp multiplex_display_clear_ISR

digit3:
	ldi r18, 0x70			;1110 0000 turning on rightmost digit aka digit 3 PA4
	sts PORTA_OUT, r18
	rjmp multiplex_display_clear_ISR

multiplex_display_clear_ISR:
	ldi r16, TCA_SINGLE_OVF_bm	;clear OVF flag
	sts TCA0_SINGLE_INTFLAGS, r16

multiplex_display_pop:
	pop r18					;pop the registers that I used
	pop r17
	pop r16
ISR_pop:
	pop r17				;restore registers
	pop r16
	out CPU_SREG, r16
	pop r16

	reti

overflow:
	ldi r17, 0x00			;digit_num will be 0
	sts digit_num, r17
	rjmp output

poll_digit_entry:
push_regs:
	push r18			;push
	push r19

getting_input_from_C:
	lds r19, PORTC_IN	;Getting the least 4 sig bits in PORTC_IN
	andi r19, 0xF0
	reverse_bits:
	ldi r20, 8
reverse:
	lsl r19
	ror r21
	dec r20
	brne reverse
	mov r19, r21
			;masking bits
	cpi r19, 0x0A		;if least 4 sig bits in C reg >= 10 then
	brge clear			;go to clear

shift:
	ldi r18, 0x04		;loop control var, it is 4 since bcd_entries is 4 long
	push r19			;pushing least 4 sig bits in PORTC_IN
	ldi ZL, LOW(bcd_entries)		
	ldi ZH, HIGH(bcd_entries)	;pointing at the first index of array
push_loop:
	ld r19, Z+			;load element of array
	push r19			;push that element in stack, this will make first element at the bottom of the stack, last element at the top of the stack
	dec r18				;do this for the length of the array
	brne push_loop
	ldi r18, 0x04		;loop control var
	pop r19				;taking the last element out of the stack and do nothing with it
pop_loop:
	pop r19				;take the second last element
	st -Z, r19			;it will point to the last index
	dec r18				;do this for the length of the array
	brne pop_loop

moving_into_led:
	ldi ZL, LOW(bcd_entries)		;pointing at the first index of array
	ldi ZH, HIGH(bcd_entries)
	ldi YL, LOW(led_display)		;pointing at the first index of led array
	ldi YH, HIGH(led_display)
	ldi r19, 0x04
led_loop:
	ld r18, Z+						;take first element
	rcall hex_to_7seg				;make that into 7seg
	st Y+, r18						;put that in led_display first index
	dec r19
	brne led_loop

clear:
	ldi r18, 0x01
	sts PORTE_INTFLAGS, r18		;clearing pin 0 interrupt

pop_regs:
	pop r19				;pop
	pop r18
	ret
hex_to_7seg:
	push ZL
	push ZH
    ldi ZH, HIGH(hextable * 2)  ;set Z to point to start of table
    ldi ZL, LOW(hextable * 2)
    ldi r16, $00                ;add offset to Z pointer
	andi r18, 0x0F				;mask for low nibble
    add ZL, r18
    adc ZH, r16
    lpm r18, Z					;load byte from table pointed to by Z
	pop ZH
	pop ZL             
	ret

    ;Table of segment values to display digits 0 - F
    ;!!! seven values must be added
hextable: .db $01, $4F, $12, $06, $4C, $24, $20, $0F, $00, $04, $08, $60, $31, $42, $30, $38



