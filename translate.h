#ifndef TRANSLATE_H
#define TRANSLATE_H

#include <stdio.h>
#include <stdint.h>

#include "emulator.h"

char* concat(const char*, const char*);

void copy_str(char*, const char*);

void copy_path(char*, char**);

void write_data_hex(uint8_t*, FILE*);

void translate_to_machine_code(uint8_t*, instr*, char*);

#endif
