/*******************************************************************************
* baremetal.h -- Replacement for some common libc functions
*
* Meant for use in systems where no underlying OS or even a libc is available.
*******************************************************************************/

#ifndef BAREMETAL_H_INCLUDED
#define BAREMETAL_H_INCLUDED

#include <stdint.h>



int puts(const char *string);
void po_num(uint32_t num);
char *gets (char *str);
void print_hex(uint32_t num);






#endif // BAREMETAL_H_INCLUDED
