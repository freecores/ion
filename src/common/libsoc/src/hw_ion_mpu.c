/**
    @file hw_ion_mpu.c
    @brief Hardware-dependent code for the default Ion MPU.
    
    
    This is the file that needs to be modified in order to use the library in
    an FPGA project other than the default one supplied with the Ion core.
    
    This code targets the 'default' MPU built around the Ion core. That is, the 
    MPU built from vhdl template 'mips_mpu1_template.vhdl' which includes the
    CPU, cache module, a timer and a simple UART.
    
    @note The functionality of the functions is not entirely compatible to the
    libc standard. For example, neither getchar nor putchar update errno, etc.
*/
#include <stdint.h>

#include "hw.h"

/** Replacement for the standard C library putchar. */
int putchar(int c){
   while( !((*((volatile uint32_t *)UART_STATUS) ) & UART_TXRDY_MASK));
    *((volatile uint32_t *)UART_TX) = (uint32_t)c;
    return c;
}

/** Replacement for the standard C library getchar. */
int getchar(void){
    uint32_t uart;
    
    while(1){
        uart = *((volatile uint32_t *)UART_STATUS);
        if(uart & UART_RXRDY_MASK) break;
    }
    uart = *((volatile uint32_t *)UART_RX);
    uart = (uart >> 24) & 0x0ff;
    return (int)uart;
}
