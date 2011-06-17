/**
 @file  soc.c
 @brief Supporting functions that do not warrant their own file.

 @bug   Any attempt to use gets() or puts() triggers a linker error (undefined
        reference to '_impure_ptr') in this file, despite the fact that this
        file does not reference those symbols; newlib's gets and puts DO, and
        the linker is somehow getting confused.
        Right now I have no idea how to sort that out.
*/

#include <stdint.h>
#include <stdio.h>

#include "soc.h"

/* Linker-defined symbols usd to access the data section */
extern char data_start [];          /**< Where .data section should be in RAM */
extern char data_load_start [];     /**< Where .data section is in ROM */
extern char data_size [];           /**< Size of .data section */


/*-- Non-standard utility functions ------------------------------------------*/

/** Return time elapsed since the last HW reset in clock cycles */
unsigned ctime(void){
    unsigned cycles;
    
    cycles = *((volatile unsigned *)0x20000100);
    return cycles;
}

/** Copies .data sections (initialized data) from ROM to RAM.
   Will be called from the startup code IIF the program was linked to run
   from FLASH. */
void copy_data_sections(void){
    uint32_t i;
    /* Move data section image from flash to RAM */
    if (data_start != data_load_start){
        for(i=0;i<(uint32_t)data_size;i++){
            data_start[i] = data_load_start[i];
        }
    }
}


/*-- Libc replacement functions ----------------------------------------------*/

/** Write character to console; replacement for standard puts. 
    Uses no buffering. */
int puts(const char *string){
    while(*string){
        /* Implicit CR with every NL if requested */
        if(IMPLICIT_CR_WITH_NL & (*string == '\n')){
            putchar('\r');
        }
        putchar(*string++);
    }
    /* A newline character is appended to the output. */
    if(IMPLICIT_CR_WITH_NL & (*string == '\n')){
        putchar('\r');
    }
    putchar('\n');
    
    return 0; /* on success return anything non-negative */
}

/** Read character from console, blocking; replacement for standard puts. 
    Uses no buffering. */
char *gets (char *str){
    uint32_t i=0;
    char c;

    while(1){
        c = getchar();
        
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
