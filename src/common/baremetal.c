#include <stdint.h>
#include <stdio.h>

#include "baremetal.h"



#define UART_TX             (0x20000000)
#define UART_RX             (0x20000000)
#define UART_STATUS         (0x20000020)
#define UART_RXRDY_MASK     (0x00000001)
#define UART_TXRDY_MASK     (0x00000002)


extern char data_start [];
extern char data_size [];
extern char data_load_start [];


void po_char(uint32_t value);




void print_num(unsigned long num){
    char buffer[32], d;
    int32_t negative=0, i=0;

    if(num<0){
        negative = 1;
        num = -num;
    }
    
    while(1){
        d = num % 10;
        buffer[i++] = '0' + d;
        num = num / 10;
        if(num==0 || i>=32) break;
    }
    i--;
    
    if(negative){
        po_char('-');
    }
    
    while(i>=0){
        po_char(buffer[i--]);
    }
}



void po_num(uint32_t num){
    print_num(num);
}


void copy_data_sections(void){
#ifndef RUN_FROM_BRAM
    uint32_t i;
    /* Move data section image from flash to RAM */
    if (data_start != data_load_start){
        for(i=0;i<(uint32_t)data_size;i++){
            data_start[i] = data_load_start[i];
        }
    }
#endif
}


void print_hex(uint32_t num){
    uint32_t i, j;
    for(i = 28; i >= 0; i -= 4){
        j = (num >> i) & 0xf;
        if(j < 10){
            po_char('0' + j);
        }
        else {
            po_char('a' - 10 + j);
        }
    }
}


void po_char(uint32_t value){
   while( !((*((volatile uint32_t *)UART_STATUS) ) & UART_TXRDY_MASK));
    *((volatile uint32_t *)UART_TX) = value;
}

char get_char(void){
    uint32_t uart;
    
    while(1){
        uart = *((volatile uint32_t *)UART_STATUS);
        if(uart & UART_RXRDY_MASK) break;
    }
    uart = *((volatile uint32_t *)UART_RX);
    uart = (uart >> 24) & 0x0ff;
    return uart;
}

int puts(const char *string){
    while(*string){
        /* Implicit CR with every NL as usual */
        if(*string == '\n'){
            po_char('\r');
        }
        po_char(*string++);
    }
    return 0;
}

void trap_service(void){
}

char *gets (char *str){
    uint32_t i=0;
    char c;

    while(1){
        c = get_char();
        
        if(c=='\0'){
            break;
        }
        else if(c=='\n' || c=='\r'){
            break;
        }
        else{
            str[i++] = c;
        }
    }
    str[i] = '\0';
    return str;
} 
