#include <stdlib.h>
#include <math.h>
#include <string>
#include <cstring>

#include "SqlVar.h"
#include "utils.h"
#include "Logger.h"

#define SIGN_LENGTH ((int)1)
#define TERMINAL_LENGTH ((int)1)
#define DECIMAL_LENGTH 1

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

#define CBL_FIELD_FLAG_NONE	0x0
#define CBL_FIELD_FLAG_VARLEN	0x80

#define ASCII_ZERO ((unsigned char)0x30)

SqlVar::SqlVar()
{
	type = 0;
	length = 0;
	power = 0;
	addr = NULL;
	realdata = NULL;
	is_variable_length = false;
}


SqlVar::~SqlVar()
{
	if (realdata)
		free(realdata);
}

SqlVar* SqlVar::copy()
{
	SqlVar* v = new SqlVar();

	v->type = type;
	v->length = length;
	v->power = power;
	v->addr = addr;
	v->is_variable_length = is_variable_length;

	v->realdata = realdata != NULL ? strdup(realdata) : NULL;

	return v;
}

char* SqlVar::getRealData()
{
	return realdata;
}

void SqlVar::setType(int t)
{
	type = t;
}

void SqlVar::setLength(int l)
{
	length = l;
}

void SqlVar::setPower(int p)
{
	power = p;
}

void SqlVar::setAddr(void* a)
{
	addr = a;
}

void SqlVar::setFlags(uint32_t flags)
{
	is_variable_length = (flags & CBL_FIELD_FLAG_VARLEN);
}

void SqlVar::setRealData(char* r)
{
	realdata = r;
}

void SqlVar::createRealData()
{
	DECLARE_LOGGER(logger);

	int type = this->type;
	int length = this->length;
	int power = this->power;
	void* addr = this->addr;

	int realdata_length = 0;

	if (realdata) {
		free(realdata);
		realdata = NULL;
	}

	switch (type) {
		case COBOL_TYPE_UNSIGNED_NUMBER:
		{
			/* set real data */
			realdata_length = length;

			// 小数点
			if (power < 0) {
				realdata_length++;
			}
			realdata = (char*)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
			memcpy(realdata, addr, realdata_length);

			if (power < 0) {
				insert_decimal_point(realdata, realdata_length, power);
			}

			LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			break;
		}
		case COBOL_TYPE_SIGNED_NUMBER_TC:
		{
			/* set real data */
			// 符号部分
			realdata_length = SIGN_LENGTH + length;
			// 小数点
			if (power < 0) {
				realdata_length++;
			}

			realdata = (char*)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
			memcpy(realdata + SIGN_LENGTH, addr, length);

			// 符号は最後の1桁で判別
			if (type_tc_is_positive(realdata + SIGN_LENGTH + length - 1)) {
				realdata[0] = '+';
			}
			else {
				realdata[0] = '-';
			}

			if (power < 0) {
				insert_decimal_point(realdata, realdata_length, power);
			}

			LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			break;
		}
		case COBOL_TYPE_SIGNED_NUMBER_LS:
		{

			/* set real data */
			// 符号部分
			realdata_length = SIGN_LENGTH + length;
			// 小数点
			if (power < 0) {
				realdata_length++;
			}

			realdata = (char*)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
			memcpy(realdata, addr, realdata_length);

			if (power < 0) {
				insert_decimal_point(realdata, realdata_length, power);
			}

			LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			break;
		}
		case COBOL_TYPE_UNSIGNED_NUMBER_PD:
		{
			double dlength;
			int skip_first;
			int realdata_length;

			dlength = ceil(((double)length + 1) / 2);
			skip_first = (length + 1) % 2; // 1 -> skip first 4 bits

			//sv_tmp.data = (char*)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
			//memcpy(sv_tmp.data, addr, (int)dlength);

			/* set real data */
			int i;
			int index = 0;
			char* ptr;
			unsigned char tmp;
			unsigned char ubit = 0xF0;
			unsigned char lbit = 0x0F;

			realdata_length = length;
			// 小数点
			if (power < 0) {
				realdata_length++;
			}

			realdata = (char*)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
			for (i = 0; i < (int)dlength; i++) {
				char val[3];

				ptr = ((char*)addr) + i * sizeof(char);
				tmp = (unsigned char)*ptr;

				if (i != 0 || !skip_first) {
					//sprintf(val, "%d", (tmp & ubit) >> 4);
					unsigned char c = ((tmp & ubit) >> 4) + ASCII_ZERO;
					realdata[index] = c;
					index++;
				}
				if (i != dlength - 1) {
					//sprintf(val, "%d", tmp & lbit);
					unsigned char c = (tmp & lbit) + ASCII_ZERO;
					realdata[index] = c;
					index++;
				}
			}

			if (power < 0) {
				insert_decimal_point(realdata, realdata_length, power);
			}

			LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			break;
		}
		case COBOL_TYPE_SIGNED_NUMBER_PD:
		{
			double dlength, dlengthbuf;
			int skip_first;

			dlength = ceil(((double)length + 1) / 2);
			skip_first = (length + 1) % 2; // 1 -> skip first 4 bits

			//sv_tmp.data = (char*)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
			//memcpy(sv_tmp.data, addr, (int)dlength);

			/* set real data */
			int i;
			int index = SIGN_LENGTH;
			char* ptr;
			unsigned char tmp;
			unsigned char ubit = 0xF0;
			unsigned char lbit = 0x0F;

			// 符号部分
			realdata_length = SIGN_LENGTH + length;
			// 小数点
			if (power < 0) {
				realdata_length++;
			}

			realdata = (char*)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
			for (i = 0; i < (int)dlength; i++) {
				char val[3];

				ptr = ((char*)addr) + i * sizeof(char);
				tmp = (unsigned char)*ptr;

				if (i != 0 || !skip_first) {
					//sprintf(val, "%d", (tmp & ubit) >> 4);
					unsigned char c = ((tmp & ubit) >> 4) + 48;
					realdata[index] = c;
					index++;
				}
				if (i != dlength - 1) {
					//sprintf(val, "%d", tmp & lbit);
					unsigned char c = (tmp & lbit) + 48;
					realdata[index] = c;
					index++;
				}
				else {
					if ((tmp & lbit) == 0x0C) {
						realdata[0] = '+';
					}
					else {
						realdata[0] = '-';
					}
				}
			}

			if (power < 0) {
				insert_decimal_point(realdata, realdata_length, power);
			}

			LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			break;
		}

		case COBOL_TYPE_JAPANESE:
			length = length * 2;
			/* no break */
		case COBOL_TYPE_ALPHANUMERIC:
		{
			if (!is_variable_length) {
				realdata = (char*)calloc(length + TERMINAL_LENGTH, sizeof(char));
				memcpy(realdata, (char*)addr, length);
				LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			}
			else {
				void* actual_addr = (char*)addr + 2;
				uint16_t* len = (uint16_t*)addr;

				realdata = (char*)calloc((*len) + TERMINAL_LENGTH, sizeof(char));
				memcpy(realdata, (char*)actual_addr, (*len));
				LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			}
		}
		break;

		case COBOL_TYPE_UNSIGNED_BINARY:
			realdata = (char*)calloc(length + TERMINAL_LENGTH, sizeof(char));

			if (this->length == 1) {	// 1 byte
				uint8_t n8 = *((uint8_t*)addr);
				snprintf((char*)realdata, length, "%d", n8);
			}
			else {
				if (this->length == 2) {	// 1 byte
					uint8_t n8 = *((uint8_t*)addr);
					snprintf((char*)realdata, length, "%d", n8);
				}
				else {
					if (this->length == 3 || this->length == 4) {	// 2 bytes
						uint16_t n16 = *((uint16_t*)addr);
						n16 = COB_BSWAP_16(n16);
						snprintf((char*)realdata, length, "%d", n16);
					}
					else {
						if (this->length >= 5 || this->length <= 9) {	// 4 bytes
							uint32_t n32 = *((uint32_t*)addr);
							n32 = COB_BSWAP_32(n32);
							snprintf((char*)realdata, length, "%d", n32);
						}
						else {
							if (this->length >= 10 || this->length <= 18) {	// 8 bytes
								uint64_t n64 = *((uint64_t*)addr);
								n64 = COB_BSWAP_64(n64);
								snprintf((char*)realdata, length, "%d", n64);
							}
							else {
								// Should never happen
							}
						}
					}
				}
			}

			LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			break;

		case COBOL_TYPE_SIGNED_BINARY:
			realdata = (char*)calloc(length + TERMINAL_LENGTH, sizeof(char));

			if (this->length == 1) {	// 1 byte
				int8_t n8 = *((int8_t*)addr);
				snprintf((char*)realdata, length, "%d", n8);
			}
			else {
				if (this->length == 2) {	// 1 byte
					//int8_t  s_byte_number = (int8_t)strtoll(retstr, NULL, 0);
					int8_t n8 = *((int8_t*)addr);
					snprintf((char*)realdata, length, "%d", n8);
				}
				else {
					if (this->length == 3 || this->length == 4) {	// 2 bytes
						int16_t n16 = *((int16_t*)addr);
						n16 = COB_BSWAP_16(n16);
						snprintf((char*)realdata, length, "%d", n16);
					}
					else {
						if (this->length >= 5 || this->length <= 9) {	// 4 bytes
							int32_t n32 = *((int32_t*)addr);
							n32 = COB_BSWAP_32(n32);
							snprintf((char*)realdata, length, "%d", n32);
						}
						else {
							if (this->length >= 10 || this->length <= 18) {	// 8 bytes
								int64_t n64 = *((int64_t*)addr);
								n64 = COB_BSWAP_64(n64);
								snprintf((char*)realdata, length, "%d", n64);
							}
							else {
								// Should never happen
							}
						}
					}
				}
			}

			LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			break;

		default:
			realdata = (char*)calloc(length + TERMINAL_LENGTH, sizeof(char));

			memcpy(realdata, (char*)addr, length);
			LOG_DEBUG(__FILE__, __func__, "%d %d->#data:%s#realdata:%s\n", type, length, addr, realdata);
			break;
	}
}

void* SqlVar::getAddr()
{
	return addr;
}

void SqlVar::createCobolData(char* retstr)
{
	void* addr = this->addr;

	DECLARE_LOGGER(logger);

	switch (type) {
		case COBOL_TYPE_UNSIGNED_NUMBER:
		{
			char* ptr;

			int fillzero;
			//int zcount;
			//char* final;

			// fill zero
			//final = (char*)calloc(length + TERMINAL_LENGTH, sizeof(char));

			// before decimal point
			int beforedp = 0;
			for (ptr = retstr; *ptr != '\0'; ptr++) {
				if (*ptr == '.') {
					break;
				}
				else {
					beforedp++;
				}
			}

			fillzero = length - beforedp + power;
			if (fillzero < 0)
				fillzero = 0;

			//for (zcount = 0; zcount < fillzero; zcount++) {
			//	strcat(final, "0");
			//}
			memset(addr, ASCII_ZERO, fillzero);

			memcpy((uint8_t *)addr + fillzero, retstr, beforedp);

			if (power < 0) {
				int afterdp = 0;

				if (*ptr != '\0') {
					ptr++;

					// after decimal point
					for (; *ptr != '\0'; ptr++) {
						afterdp++;
					}

					// fill zero
					memcpy((uint8_t*)addr + fillzero + beforedp,
						retstr + beforedp + DECIMAL_LENGTH, afterdp);
				}

				fillzero = -power - afterdp;
				uint8_t* ptr = ((uint8_t*)addr + fillzero + beforedp) + afterdp;
				//for (zcount = 0; zcount < fillzero; zcount++) {
				//	strcat(final, "0");
				//}
				memset(ptr, ASCII_ZERO, fillzero);
			}

			//memcpy(addr, final, length);
			//free(final);
			break;
		}
		case COBOL_TYPE_SIGNED_NUMBER_TC:
		{
			char* value;
			char* ptr;
			int is_negative = false;

			int fillzero;
			//int zcount;
			//char* final;
			int final_length;

			// fill zero
			//final = (char*)calloc(length + TERMINAL_LENGTH, sizeof(char));

			if (retstr[0] == '-') {
				is_negative = true;
				value = retstr + 1;
			}
			else {
				value = retstr;
			}

			// before decimal point
			int beforedp = 0;
			for (ptr = value; *ptr != '\0'; ptr++) {
				if (*ptr == '.') {
					break;
				}
				else {
					beforedp++;
				}
			}

			fillzero = length - beforedp + power;
			if (fillzero < 0)
				fillzero = 0;

			//for (zcount = 0; zcount < fillzero; zcount++) {
			//	strcat(final, "0");
			//}
			memset(addr, ASCII_ZERO, fillzero);

			//memcpy(final + fillzero, value, beforedp);
			memcpy((uint8_t *)addr + fillzero, value, beforedp);

			if (power < 0) {
				int afterdp = 0;

				if (*ptr != '\0') {
					ptr++;

					// after decimal point
					for (; *ptr != '\0'; ptr++) {
						afterdp++;
					}
					memcpy((uint8_t*)addr + fillzero + beforedp, value +
						beforedp + DECIMAL_LENGTH, afterdp);
				}

				// fill zero
				fillzero = -power - afterdp;
				//for (zcount = 0; zcount < fillzero; zcount++) {
				//	strcat(final, "0");
				//}
				uint8_t* ptr = ((uint8_t*)addr + fillzero + beforedp) + afterdp;
				memset(ptr, ASCII_ZERO, fillzero);

			}

			final_length = strlen((const char *)addr);
			uint8_t* addr_ptr = (uint8_t*)addr;
			if (is_negative) {
				int index = *(addr_ptr + (final_length - 1)) - '0';
				addr_ptr[final_length - 1] = type_tc_negative_final_number[index];
			}

			//memcpy(addr, final, length + SIGN_LENGTH);
			//free(final);
			break;
		} 
		case COBOL_TYPE_SIGNED_NUMBER_LS:
		{
			unsigned char* value;
			unsigned char* ptr;

			int fillzero;
			//int zcount;
			//char* final;

			// fill zero
			//final = (char*)calloc(SIGN_LENGTH + length + TERMINAL_LENGTH, sizeof(char));

			if (retstr[0] == '-') {
				((uint8_t *)addr)[0] = '-';
				value = (unsigned char *)retstr + 1;
			}
			else {
				((uint8_t*)addr)[0] = '+';
				value = (unsigned char*)retstr;
			}

			// before decimal point
			int beforedp = 0;
			for (ptr = value; *ptr != '\0'; ptr++) {
				if (*ptr == '.') {
					break;
				}
				else {
					beforedp++;
				}
			}

			fillzero = length - beforedp + power;
			//for (zcount = 0; zcount < fillzero; zcount++) {
			//	strcat(final, "0");
			//}
			memset(addr, ASCII_ZERO, fillzero);

			memcpy((uint8_t *)addr + SIGN_LENGTH + fillzero, value, beforedp);

			if (power < 0) {
				int afterdp = 0;

				if (*ptr != '\0') {
					ptr++;

					// after decimal point
					for (; *ptr != '\0'; ptr++) {
						afterdp++;
					}

					// fill zero
					memcpy((uint8_t*)addr + SIGN_LENGTH + fillzero + beforedp,
						value + beforedp + DECIMAL_LENGTH, afterdp);
				}

				fillzero = -power - afterdp;
				//for (zcount = 0; zcount < fillzero; zcount++) {
				//	strcat(final, "0");
				//}
				ptr = ((uint8_t*)addr + SIGN_LENGTH + fillzero + beforedp) + afterdp;
				memset(ptr, ASCII_ZERO, fillzero);
			}

			//memcpy(addr, final, length + SIGN_LENGTH);
			//free(final);
			break;
		} 

		case COBOL_TYPE_UNSIGNED_NUMBER_PD:
		{
			//char* value = retstr;
			char* ptr;
			int is_negative = false;

			int fillzero;
			//int zcount;
			char* pre_final;
			//char* final;

			double dlength;
			int skip_first;
			int i;
			unsigned char ubit = 0xF0;
			unsigned char lbit = 0x0F;

			dlength = ceil(((double)length + 1) / 2);
			skip_first = ((int)length + 1) % 2; // 1 -> skip first 4 bits

			int pre_final_sz = (int)(length + 1) + TERMINAL_LENGTH;
			pre_final = (char*)calloc(pre_final_sz, sizeof(char));

			// before decimal point
			int beforedp = 0;
			for (ptr = retstr; *ptr != '\0'; ptr++) {
				if (*ptr == '.') {
					break;
				}
				else {
					beforedp++;
				}
			}

			fillzero = length - beforedp + power;
			//for (zcount = 0; zcount < fillzero; zcount++) {
			//	strcat(pre_final, "0");
			//}
			memset(pre_final, ASCII_ZERO, fillzero);

			memcpy(pre_final + fillzero, retstr, beforedp);

			if (power < 0) {
				int afterdp = 0;

				if (*ptr != '\0') {
					ptr++;

					// after decimal point
					for (; *ptr != '\0'; ptr++) {
						afterdp++;
					}
					memcpy(pre_final + fillzero + beforedp,
						retstr + beforedp + DECIMAL_LENGTH, afterdp);
				}

				// fill zero
				fillzero = -power - afterdp;
				//for (zcount = 0; zcount < fillzero; zcount++) {
				//	strcat(pre_final, "0");
				//}
				uint8_t* pptr = ((uint8_t *)pre_final + fillzero + beforedp) + afterdp;
				memset(pptr, ASCII_ZERO, fillzero);
			}

			// format setting
			//final = (char*)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
			ptr = pre_final;
			uint8_t* addr_ptr = (uint8_t*)addr;
			for (i = 0; i < dlength; i++) {
				unsigned char vubit = 0x00;
				unsigned char vlbit = 0x00;

				if (i == 0 && skip_first) {
					vubit = 0x00;
				}
				else {
					vubit = (*ptr) << 4;
					vubit = vubit & ubit;
					ptr++;
				}

				if (i != dlength - 1) {
					vlbit = *ptr;
					vlbit = vlbit & lbit;
					ptr++;
				}
				else {
					vlbit = 0x0F;
				}

				addr_ptr[i] = vubit | vlbit;
			}

			//memcpy(addr, final, (int)dlength);
			free(pre_final);
			//free(final);
			break;
		} // *********************** FINO A QUI

		case COBOL_TYPE_SIGNED_NUMBER_PD:
		{
			char* value;
			char* ptr;
			int is_negative = false;

			int fillzero;
			//int zcount;
			char* pre_final;
			//char* final;

			double dlength;
			int skip_first;
			int i;
			unsigned char ubit = 0xF0;
			unsigned char lbit = 0x0F;

			dlength = ceil((double)(length + 1) / 2);
			skip_first = ((int)length + 1) % 2; // 1 -> skip first 4 bits

			if (retstr[0] == '-') {
				is_negative = true;
				value = retstr + 1;
			}
			else {
				value = retstr;
			}


			//int pre_final_sz = (int)dlength + TERMINAL_LENGTH;
			int pre_final_sz = (int)(length + 1) + TERMINAL_LENGTH;
			pre_final = (char*)calloc(pre_final_sz, sizeof(char));

			// before decimal point
			int beforedp = 0;
			for (ptr = value; *ptr != '\0'; ptr++) {
				if (*ptr == '.') {
					break;
				}
				else {
					beforedp++;
				}
			}

			fillzero = length - beforedp + power;
			//for (zcount = 0; zcount < fillzero; zcount++) {
			//	strcat(pre_final, "0");
			//}
			memset(pre_final, ASCII_ZERO, fillzero);

			memcpy(pre_final + fillzero, value, beforedp);

			if (power < 0) {
				int afterdp = 0;

				if (*ptr != '\0') {
					ptr++;

					// after decimal point
					for (; *ptr != '\0'; ptr++) {
						afterdp++;
					}
					memcpy(pre_final + fillzero + beforedp,
						value + beforedp + DECIMAL_LENGTH, afterdp);
				}

				// fill zero
				fillzero = -power - afterdp;
				//for (zcount = 0; zcount < fillzero; zcount++) {
				//	strcat(pre_final, "0");
				//}
				uint8_t* pptr = ((uint8_t * )pre_final + fillzero + beforedp) + afterdp;
				memset(pptr, ASCII_ZERO, fillzero);

			}

			// format setting
			//final = (char*)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
			ptr = pre_final;
			uint8_t* addr_ptr = (uint8_t*)addr;
			for (i = 0; i < dlength; i++) {
				unsigned char vubit = 0x00;
				unsigned char vlbit = 0x00;

				if (i == 0 && skip_first) {
					vubit = 0x00;
				}
				else {
					vubit = (*ptr) << 4;
					vubit = vubit & ubit;
					ptr++;
				}

				if (i != dlength - 1) {
					vlbit = *ptr;
					vlbit = vlbit & lbit;
					ptr++;
				}
				else {
					if (is_negative) {
						vlbit = 0x0D;
					}
					else {
						vlbit = 0x0C;
					}
				}

				addr_ptr[i] = vubit | vlbit;
			}

			//memcpy(addr, final, (int)dlength);
			free(pre_final);
			//free(final);
			break;
		}
		case COBOL_TYPE_ALPHANUMERIC:
			// 文字の長さだけメモリコピー
			if (!is_variable_length) {
				if (strlen(retstr) >= length) {
					memcpy(addr, retstr, length);
				}
				else {
					memset(addr, ' ', length);
					memcpy(addr, retstr, strlen(retstr));
				}
			}
			else {
				void* actual_addr = (uint8_t*)addr + 2;
				int actual_len = length - 2;
				if (strlen(retstr) >= length) {
					memcpy(actual_addr, retstr, actual_len);
				}
				else {
					memset(actual_addr, ' ', actual_len);
					memcpy(actual_addr, retstr, strlen(retstr));
				}
				uint16_t* fld_len = (uint16_t*)addr;
				*fld_len = strlen(retstr);
			}
			break;
		case COBOL_TYPE_JAPANESE:
			// 文字の長さだけメモリコピー
			if (strlen(retstr) >= length * 2) {
				memcpy(addr, retstr, length * 2);
			}
			else {
				int i;
				char* tmp = (char*)addr;
				for (i = 0; i + 1 < length * 2; i = i + 2) {
					tmp[i] = 0x81;
					tmp[i + 1] = 0x40;
				}
				memcpy(addr, retstr, strlen(retstr));
			}
			break;
			//std::stoi, std::stol, std::stoll
		case COBOL_TYPE_UNSIGNED_BINARY:

			memset(addr, 0, length);

			if (this->length == 1) {	// 1 byte
				*((uint8_t*)addr) = (uint8_t)strtoull(retstr, NULL, 0);
			}
			else {
				if (this->length == 2) {	// 1 byte
					uint8_t  u_byte_number = (uint8_t)strtoull(retstr, NULL, 0);
					*((uint8_t*)addr) = u_byte_number;
				}
				else {
					if (this->length == 3 || this->length == 4) {	// 2 bytes
						uint16_t  u_short_number = (uint16_t)strtoull(retstr, NULL, 0);
						*((uint16_t*)addr) = COB_BSWAP_16(u_short_number);
					}
					else {
						if (this->length >= 5 || this->length <= 9) {	// 4 bytes
							uint32_t  u_int_number = (uint32_t)strtoull(retstr, NULL, 0);
							uint32_t t = COB_BSWAP_32(u_int_number);
							*((uint32_t*)addr) = COB_BSWAP_32(u_int_number);
						}
						else {
							if (this->length >= 10 || this->length <= 18) {	// 8 bytes
								uint64_t  u_long_number = (uint64_t)strtoull(retstr, NULL, 0);
								*((uint64_t*)addr) = COB_BSWAP_64(u_long_number);
							}
							else {
								// Should never happen
							}
						}
					}
				}
			}
			break;

		case COBOL_TYPE_SIGNED_BINARY:
			memset(addr, 0, length);

			if (this->length == 1) {	// 1 byte
				*((int8_t*)addr) = (int8_t)strtoll(retstr, NULL, 0);
			}
			else {
				if (this->length == 2) {	// 1 byte
					int8_t  s_byte_number = (int8_t)strtoll(retstr, NULL, 0);
					*((int8_t*)addr) = s_byte_number;
				}
				else {
					if (this->length == 3 || this->length == 4) {	// 2 bytes
						int16_t  s_short_number = (int16_t)strtoll(retstr, NULL, 0);
						*((int16_t*)addr) = COB_BSWAP_16(s_short_number);
					}
					else {
						if (this->length >= 5 || this->length <= 9) {	// 4 bytes
							int32_t  s_int_number = (int32_t)strtoll(retstr, NULL, 0);
							*((int32_t*)addr) = COB_BSWAP_32(s_int_number);
						}
						else {
							if (this->length >= 10 || this->length <= 18) {	// 8 bytes
								int64_t  s_long_number = (int64_t)strtoll(retstr, NULL, 0);
								*((int64_t*)addr) = COB_BSWAP_64(s_long_number);
							}
							else {
								// Should never happen
							}
						}
					}
				}
			}
			break;

		default:
			break;
	}
//#ifndef NDEBUG
//	char* tmp;
//	if (type == COBOL_TYPE_JAPANESE) {
//		tmp = oc_strndup((char*)addr, length * 2);
//	}
//	else {
//		tmp = oc_strndup((char*)addr, length);
//	}
//	LOG_DEBUG(__FILE__, __func__, "%d %d#%s#%s#\n", type, length, retstr, tmp);
//	if (tmp) free(tmp);
//#endif
}

void SqlVar::createCobolDataLowValue()
{
	memset(addr, 0, length);
}

int SqlVar::getType()
{
	return type;
}

int SqlVar::getLength()
{
	return length;
}

