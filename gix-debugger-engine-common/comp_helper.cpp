#include "comp_helper.h"

#define ASCII_ZERO 0x30

#if defined(unix) || defined(__unix__) || defined(__unix) || defined(__linux__)
#include <byteswap.h>
#define COB_BSWAP_16(val) (bswap_16 (val))
#define COB_BSWAP_32(val) (bswap_32(val))
#define COB_BSWAP_64(val) (bswap_64 (val))
#elif defined(__APPLE__)
#include <libkern/OSByteOrder.h>
#define COB_BSWAP_16(val) (OSSwapInt16(val))
#define COB_BSWAP_32(val) (OSSwapInt32(val))
#define COB_BSWAP_64(val) (OSSwapInt64(val))
#else
#define COB_BSWAP_16(val) (_byteswap_ushort (val))
#define COB_BSWAP_32(val) (_byteswap_ulong (val))
#define COB_BSWAP_64(val) (_byteswap_uint64 (val))
#endif

char* CompHelper::comp3_to_display(int total_len, int scale, int has_sign, uint8_t* addr)
{
	int display_len = total_len;

	int bfrlen = display_len + (has_sign ? 1 : 0) + (scale > 0 ? 1 : 0);

	char* copy = (char*)malloc(bfrlen + 1);
	memset(copy, ASCII_ZERO, display_len);
	uint8_t* ptr = (uint8_t*)copy;

	int storage_len = (total_len / 2) + 1;

	if (has_sign) {
		uint8_t sign_byte = addr[storage_len - 1] & 0x0f;
		char sign = (sign_byte == 0x0d) ? '-' : '+';
		*(ptr++) = sign;

	}

	int ndigits = total_len;

	int skip_first_nibble = ((ndigits + 1) % 2) > 0;
	int ndigit = 0;
	int decsep_pos = (total_len - scale) + (has_sign ? 1 : 0) - 1;
	for (int i = 0; i < storage_len; i++) {

		uint8_t b = addr[i];

		if (i > 0 || !skip_first_nibble) {
			uint8_t digit_l = (b & 0xf0) >> 4;
			digit_l += ASCII_ZERO;
			*(ptr++) = digit_l;
			ndigit++;
		}


		if (ndigit == decsep_pos) {
			*(ptr++) = '.';
		}

		uint8_t digit_r = (b & 0x0f);
		digit_r += ASCII_ZERO;
		*(ptr++) = digit_r;

		ndigit++;
		if (ndigit == decsep_pos) {
			*(ptr++) = '.';
		}
	}

	copy[bfrlen] = '\0';

	return copy;
}

char* CompHelper::comp5_to_display(int total_len, int scale, int has_sign, uint8_t* addr, bool is_native_binary)
{
	int display_len = total_len;

	int bfrlen = display_len + (has_sign ? 1 : 0) + (scale > 0 ? 1 : 0);

	char* bfr = (char*)malloc(bfrlen + 1);
	memset(bfr, ASCII_ZERO, display_len);
	uint8_t* ptr = (uint8_t*)bfr;

	bool is_negative = false;

	if (!has_sign) {
		if (total_len == 1) {	// 1 byte
			uint8_t n8 = *((uint8_t*)addr);
			snprintf((char*)bfr, total_len, "%d", n8);
		}
		else {
			if (total_len == 2) {	// 1 byte
				uint8_t n8 = *((uint8_t*)addr);
				snprintf((char*)bfr, total_len, "%d", n8);
			}
			else {
				if (total_len == 3 || total_len == 4) {	// 2 bytes
					uint16_t n16 = *((uint16_t*)addr);
					if (!is_native_binary)
						n16 = COB_BSWAP_16(n16);
					snprintf((char*)bfr, total_len, "%d", n16);
				}
				else {
					if (total_len >= 5 || total_len <= 9) {	// 4 bytes
						uint32_t n32 = *((uint32_t*)addr);
						if (!is_native_binary)
							n32 = COB_BSWAP_32(n32);
						snprintf((char*)bfr, total_len, "%d", n32);
					}
					else {
						if (total_len >= 10 || total_len <= 18) {	// 8 bytes
							uint64_t n64 = *((uint64_t*)addr);
							if (!is_native_binary)
								n64 = COB_BSWAP_64(n64);
							snprintf((char*)bfr, total_len, "%d", n64);
						}
						else {
							// Should never happen
						}
					}
				}
			}
		}
	}
	else {
		if (total_len == 1) {	// 1 byte
			int8_t n8 = *((int8_t*)addr);
			is_negative = n8 < 0;
			snprintf((char*)bfr, total_len, "%d", abs(n8));
		}
		else {
			if (total_len == 2) {	// 1 byte
				int8_t n8 = *((int8_t*)addr);
				is_negative = n8 < 0;
				snprintf((char*)bfr, total_len, "%d", abs(n8));
			}
			else {
				if (total_len == 3 || total_len == 4) {	// 2 bytes
					int16_t n16 = *((int16_t*)addr);
					if (!is_native_binary)
						n16 = COB_BSWAP_16(n16);
					is_negative = n16 < 0;
					snprintf((char*)bfr, total_len, "%d", abs(n16));
				}
				else {
					if (total_len >= 5 || total_len <= 9) {	// 4 bytes
						int32_t n32 = *((int32_t*)addr);
						if (!is_native_binary)
							n32 = COB_BSWAP_32(n32);
						is_negative = n32 < 0;
						snprintf((char*)bfr, total_len, "%d", labs(n32));
					}
					else {
						if (total_len >= 10 || total_len <= 18) {	// 8 bytes
							int64_t n64 = *((int64_t*)addr);
							if (!is_native_binary)
								n64 = COB_BSWAP_64(n64);
							is_negative = n64 < 0;
							snprintf((char*)bfr, total_len, "%d", llabs(n64));
						}
						else {
							// Should never happen
						}
					}
				}
			}
		}
	}

	bfr[bfrlen] = '\0';

	char* final_bfr = (char*)malloc(bfrlen + 1);
	memset(final_bfr, ASCII_ZERO, display_len);
	char* fptr = (char*)final_bfr + (has_sign ? 1 : 0) + (display_len - strlen(bfr));
	strcpy(fptr, bfr);

	if (has_sign)
		final_bfr[0] = is_negative ? '-' : '+';

	free(bfr);

	return final_bfr;
}