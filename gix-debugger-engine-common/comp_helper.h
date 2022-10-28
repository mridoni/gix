#pragma once

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

class CompHelper {

public:

	static char* comp3_to_display(int total_len, int scale, int has_sign, uint8_t* addr);
	static char* comp5_to_display(int total_len, int scale, int has_sign, uint8_t* addr, bool is_native_binary);
};
