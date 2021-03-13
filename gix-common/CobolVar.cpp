/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#include <stdlib.h>
#include <math.h>
#include <string>
#include <cstring>
#include <QString>

#include "QLogger.h"
#include "CobolVar.h"
#include "utils.h"


#define SIGN_LENGTH 1
#define TERMINAL_LENGTH 1
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

CobolVar::CobolVar()
{
	type = 0;
	length = 0;
	power = 0;
	addr = NULL;
	data = NULL;
	realdata = NULL;
}


CobolVar::~CobolVar()
{
	if (data)
		free(data);

	if (realdata)
		free(realdata);
}

CobolVar *CobolVar::copy()
{
	CobolVar *v = new CobolVar();

	v->type = type;
	v->length = length;
	v->power = power;
	v->addr = addr;

	v->data = calloc(sizeof(v->data) + TERMINAL_LENGTH, 1);
	memcpy(v->data, data, sizeof(data));

	v->realdata = realdata != NULL ? strdup(realdata) : NULL;

	return v;
}

char * CobolVar::getRealData()
{
	return realdata;
}

void CobolVar::setType(int t)
{
	type = t;
}

void CobolVar::setLength(int l)
{
	length = l;
}

void CobolVar::setPower(int p)
{
	power = p;
}

void CobolVar::setAddr(void *a)
{
	addr = a;
}

void CobolVar::setData(void *d)
{
	data = d;
}

void CobolVar::setRealData(char *r)
{
	realdata = r;
}

void CobolVar::createRealData()
{
	QString log_msg;

	int type = this->type;
	int length = this->length;
	int power = this->power;
	void *addr = this->addr;

	CobolVar sv_tmp;
	sv_tmp.type = type;
	sv_tmp.length = length;
	sv_tmp.power = power;
	sv_tmp.addr = addr;

	switch (sv_tmp.type) {
	case COBOL_TYPE_UNSIGNED_NUMBER:
	{
		int realdata_length;

		sv_tmp.data = (char *)calloc(sv_tmp.length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, sv_tmp.addr, sv_tmp.length);

		/* set real data */
		realdata_length = sv_tmp.length;
		// 小数点
		if (sv_tmp.power < 0) {
			realdata_length++;
		}
		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.realdata, sv_tmp.data, realdata_length);

		if (sv_tmp.power < 0) {
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
								sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;
	}
	case COBOL_TYPE_SIGNED_NUMBER_TC:
	{
		int realdata_length;

		sv_tmp.data = (char *)calloc(sv_tmp.length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, sv_tmp.addr, sv_tmp.length);

		/* set real data */
		// 符号部分
		realdata_length = SIGN_LENGTH + sv_tmp.length;
		// 小数点
		if (sv_tmp.power < 0) {
			realdata_length++;
		}

		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.realdata + SIGN_LENGTH, sv_tmp.data, sv_tmp.length);

		// 符号は最後の1桁で判別
		if (type_tc_is_positive(sv_tmp.realdata + SIGN_LENGTH + sv_tmp.length - 1)) {
			sv_tmp.realdata[0] = '+';
		}
		else {
			sv_tmp.realdata[0] = '-';
		}

		if (sv_tmp.power < 0) {
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;
	}
	case COBOL_TYPE_SIGNED_NUMBER_LS:
	{
		int realdata_length;

		sv_tmp.data = (char *)calloc(SIGN_LENGTH + sv_tmp.length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, sv_tmp.addr, sv_tmp.length + SIGN_LENGTH);

		/* set real data */
		// 符号部分
		realdata_length = SIGN_LENGTH + sv_tmp.length;
		// 小数点
		if (sv_tmp.power < 0) {
			realdata_length++;
		}
		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.realdata, sv_tmp.data, realdata_length);

		if (sv_tmp.power < 0) {
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;
	}
	case COBOL_TYPE_UNSIGNED_NUMBER_PD:
	{
		double dlength;
		int skip_first;
		int realdata_length;

		dlength = ceil(((double)sv_tmp.length + 1) / 2);
		skip_first = (sv_tmp.length + 1) % 2; // 1 -> skip first 4 bits
		sv_tmp.data = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, addr, (int)dlength);

		/* set real data */
		int i;
		int index = 0;
		char *ptr;
		unsigned char tmp;
		unsigned char ubit = 0xF0;
		unsigned char lbit = 0x0F;

		realdata_length = sv_tmp.length;
		// 小数点
		if (sv_tmp.power < 0) {
			realdata_length++;
		}

		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		for (i = 0; i < dlength; i++) {
			char val[3];

			ptr = ((char *)sv_tmp.data) + i * sizeof(char);
			tmp = (unsigned char)*ptr;

			if (i != 0 || !skip_first) {
				sprintf(val, "%d", (tmp & ubit) >> 4);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
			if (i != dlength - 1) {
				sprintf(val, "%d", tmp & lbit);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
		}

		if (sv_tmp.power < 0) {
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;
	}
	case COBOL_TYPE_SIGNED_NUMBER_PD:
	{
		double dlength, dlengthbuf;
		int skip_first;
		int realdata_length;

		dlength = ceil(((double)sv_tmp.length + 1) / 2);
		skip_first = (sv_tmp.length + 1) % 2; // 1 -> skip first 4 bits
		sv_tmp.data = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, addr, (int)dlength);

		/* set real data */
		int i;
		int index = SIGN_LENGTH;
		char *ptr;
		unsigned char tmp;
		unsigned char ubit = 0xF0;
		unsigned char lbit = 0x0F;

		// 符号部分
		realdata_length = SIGN_LENGTH + sv_tmp.length;
		// 小数点
		if (sv_tmp.power < 0) {
			realdata_length++;
		}

		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		for (i = 0; i < dlength; i++) {
			char val[3];

			ptr = ((char *)sv_tmp.data) + i * sizeof(char);
			tmp = (unsigned char)*ptr;

			if (i != 0 || !skip_first) {
				sprintf(val, "%d", (tmp & ubit) >> 4);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
			if (i != dlength - 1) {
				sprintf(val, "%d", tmp & lbit);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
			else {
				if ((tmp & lbit) == 0x0C) {
					sv_tmp.realdata[0] = '+';
				}
				else {
					sv_tmp.realdata[0] = '-';
				}
			}
		}

		if (sv_tmp.power < 0) {
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;
	}
	case COBOL_TYPE_JAPANESE:
		length = length * 2;
		/* no break */
	case COBOL_TYPE_ALPHANUMERIC:
		sv_tmp.data = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));
		sv_tmp.realdata = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, (char *)addr, length);
		memcpy(sv_tmp.realdata, (char *)addr, length);
#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;

	case COBOL_TYPE_UNSIGNED_BINARY:
		sv_tmp.data = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));
		sv_tmp.realdata = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));

		memset(addr, 0, length);

		if (this->length == 1) {	// 1 byte
			uint8_t n8 = *((uint8_t *)addr);
			snprintf((char *)sv_tmp.data, length, "%d", n8);
		}
		else {
			if (this->length == 2) {	// 1 byte
				uint8_t n8 = *((uint8_t *)addr);
				snprintf((char *)sv_tmp.data, length, "%d", n8);
				snprintf((char *)sv_tmp.realdata, length, "%d", n8);
			}
			else {
				if (this->length == 3 || this->length == 4) {	// 2 bytes
					uint16_t n16 = *((uint16_t *)addr);
					n16 = COB_BSWAP_16(n16);
					snprintf((char *)sv_tmp.data, length, "%d", n16);
					snprintf((char *)sv_tmp.realdata, length, "%d", n16);
				}
				else {
					if (this->length >= 5 || this->length <= 9) {	// 4 bytes
						uint32_t n32 = *((uint32_t *)addr);
						n32 = COB_BSWAP_32(n32);
						snprintf((char *)sv_tmp.data, length, "%d", n32);
						snprintf((char *)sv_tmp.realdata, length, "%d", n32);
					}
					else {
						if (this->length >= 10 || this->length <= 18) {	// 8 bytes
							uint64_t n64 = *((uint64_t *)addr);
							n64 = COB_BSWAP_64(n64);
							snprintf((char *)sv_tmp.data, length, "%d", n64);
							snprintf((char *)sv_tmp.realdata, length, "%d", n64);
						}
						else {
							// Should never happen
						}
					}
				}
			}
		}

#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;

	case COBOL_TYPE_SIGNED_BINARY:
		sv_tmp.data = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));
		sv_tmp.realdata = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));

		memset(addr, 0, length);

		if (this->length == 1) {	// 1 byte
			int8_t n8 = *((int8_t *)addr);
			snprintf((char *) sv_tmp.data, length, "%d", n8);
		}
		else {
			if (this->length == 2) {	// 1 byte
				//int8_t  s_byte_number = (int8_t)strtoll(retstr, NULL, 0);
				int8_t n8 = *((int8_t *)addr);
				snprintf((char *)sv_tmp.data, length, "%d", n8);
				snprintf((char *)sv_tmp.realdata, length, "%d", n8);
			}
			else {
				if (this->length == 3 || this->length == 4) {	// 2 bytes
					int16_t n16 = *((int16_t *)addr);
					n16 = COB_BSWAP_16(n16);
					snprintf((char *)sv_tmp.data, length, "%d", n16);
					snprintf((char *)sv_tmp.realdata, length, "%d", n16);
				}
				else {
					if (this->length >= 5 || this->length <= 9) {	// 4 bytes
						int32_t n32 = *((int32_t *)addr);
						n32 = COB_BSWAP_32(n32);
						snprintf((char *)sv_tmp.data, length, "%d", n32);
						snprintf((char *)sv_tmp.realdata, length, "%d", n32);
					}
					else {
						if (this->length >= 10 || this->length <= 18) {	// 8 bytes
							int64_t n64 = *((int64_t *)addr);
							n64 = COB_BSWAP_64(n64);
							snprintf((char *)sv_tmp.data, length, "%d", n64);
							snprintf((char *)sv_tmp.realdata, length, "%d", n64);
						}
						else {
							// Should never happen
						}
					}
				}
			}
		}

#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;

	default:
		sv_tmp.data = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));
		sv_tmp.realdata = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));

		memcpy(sv_tmp.data, (char *)addr, length);
		memcpy(sv_tmp.realdata, (char *)addr, length);
#ifdef _DEBUG
		log_msg.sprintf("%s@%s: %d %d->%d#data:%s#realdata:%s\n", __FILE__, __func__, sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
		break;
	}
	if (realdata) {
		free(realdata);
		realdata = NULL;
	}
	realdata = strdup(sv_tmp.realdata);
	data = strdup((const char *)sv_tmp.data);
}

void CobolVar::createCobolData(char *retstr) 
{
	void *addr = this->addr;

	switch (type) {
	case COBOL_TYPE_UNSIGNED_NUMBER:
	{
		char *ptr;

		int fillzero;
		int zcount;
		char *final;

		// fill zero
		final = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));

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
		for (zcount = 0; zcount < fillzero; zcount++) {
			strcat(final, "0");
		}
		memcpy(final + fillzero, retstr, beforedp);

		if (power < 0) {
			int afterdp = 0;

			if (*ptr != '\0') {
				ptr++;

				// after decimal point
				for (; *ptr != '\0'; ptr++) {
					afterdp++;
				}

				// fill zero
				memcpy(final + fillzero + beforedp,
					retstr + beforedp + DECIMAL_LENGTH, afterdp);
			}

			fillzero = -power - afterdp;
			for (zcount = 0; zcount < fillzero; zcount++) {
				strcat(final, "0");
			}
		}

		memcpy(addr, final, length);
		free(final);
		break;
	}
	case COBOL_TYPE_SIGNED_NUMBER_TC:
	{
		char *value;
		char *ptr;
		int is_negative = false;

		int fillzero;
		int zcount;
		char *final;
		int final_length;

		// fill zero
		final = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));

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
		for (zcount = 0; zcount < fillzero; zcount++) {
			strcat(final, "0");
		}
		memcpy(final + fillzero, value, beforedp);

		if (power < 0) {
			int afterdp = 0;

			if (*ptr != '\0') {
				ptr++;

				// after decimal point
				for (; *ptr != '\0'; ptr++) {
					afterdp++;
				}
				memcpy(final + fillzero + beforedp, value +
					beforedp + DECIMAL_LENGTH, afterdp);
			}

			// fill zero
			fillzero = -power - afterdp;
			for (zcount = 0; zcount < fillzero; zcount++) {
				strcat(final, "0");
			}
		}

		final_length = strlen(final);
		if (is_negative) {
			int index = *(final + (final_length - 1)) - '0';
			final[final_length - 1] = type_tc_negative_final_number[index];
		}

		memcpy(addr, final, length + SIGN_LENGTH);
		free(final);
		break;
	}
	case COBOL_TYPE_SIGNED_NUMBER_LS:
	{
		char *value;
		char *ptr;

		int fillzero;
		int zcount;
		char *final;

		// fill zero
		final = (char *)calloc(SIGN_LENGTH + length + TERMINAL_LENGTH, sizeof(char));

		if (retstr[0] == '-') {
			final[0] = '-';
			value = retstr + 1;
		}
		else {
			final[0] = '+';
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
		for (zcount = 0; zcount < fillzero; zcount++) {
			strcat(final, "0");
		}
		memcpy(final + SIGN_LENGTH + fillzero, value, beforedp);

		if (power < 0) {
			int afterdp = 0;

			if (*ptr != '\0') {
				ptr++;

				// after decimal point
				for (; *ptr != '\0'; ptr++) {
					afterdp++;
				}

				// fill zero
				memcpy(final + SIGN_LENGTH + fillzero + beforedp,
					value + beforedp + DECIMAL_LENGTH, afterdp);
			}

			fillzero = -power - afterdp;
			for (zcount = 0; zcount < fillzero; zcount++) {
				strcat(final, "0");
			}
		}

		memcpy(addr, final, length + SIGN_LENGTH);
		free(final);
		break;
	}
	case COBOL_TYPE_UNSIGNED_NUMBER_PD:
	{
		char *value = retstr;
		char *ptr;
		int is_negative = false;

		int fillzero;
		int zcount;
		char *pre_final;
		char *final;

		double dlength;
		int skip_first;
		int i;
		unsigned char ubit = 0xF0;
		unsigned char lbit = 0x0F;

		dlength = ceil(((double)length + 1) / 2);
		skip_first = ((int)length + 1) % 2; // 1 -> skip first 4 bits

		int pre_final_sz = (int)(length + 1) + TERMINAL_LENGTH;
		pre_final = (char *)calloc(pre_final_sz, sizeof(char));

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
		for (zcount = 0; zcount < fillzero; zcount++) {
			strcat(pre_final, "0");
		}
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
			for (zcount = 0; zcount < fillzero; zcount++) {
				strcat(pre_final, "0");
			}
		}

		// format setting
		final = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
		ptr = pre_final;
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

			final[i] = vubit | vlbit;
		}

		memcpy(addr, final, (int)dlength);
		free(pre_final);
		free(final);
		break;
	}
	case COBOL_TYPE_SIGNED_NUMBER_PD:
	{
		char *value;
		char *ptr;
		int is_negative = false;

		int fillzero;
		int zcount;
		char *pre_final;
		char *final;

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
		pre_final = (char *)calloc(pre_final_sz, sizeof(char));

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
		for (zcount = 0; zcount < fillzero; zcount++) {
			strcat(pre_final, "0");
		}
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
			for (zcount = 0; zcount < fillzero; zcount++) {
				strcat(pre_final, "0");
			}
		}

		// format setting
		final = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
		ptr = pre_final;
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

			final[i] = vubit | vlbit;
		}

		memcpy(addr, final, (int)dlength);
		free(pre_final);
		free(final);
		break;
	}
	case COBOL_TYPE_ALPHANUMERIC:
		// 文字の長さだけメモリコピー
		if (strlen(retstr) >= length) {
			memcpy(addr, retstr, length);
		}
		else {
			memset(addr, ' ', length);
			memcpy(addr, retstr, strlen(retstr));
		}
		break;
	case COBOL_TYPE_JAPANESE:
		// 文字の長さだけメモリコピー
		if (strlen(retstr) >= length * 2) {
			memcpy(addr, retstr, length * 2);
		}
		else {
			int i;
			char *tmp = (char *)addr;
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
			*((uint8_t *)addr) = (uint8_t) strtoull(retstr, NULL, 0);
		}
		else {
			if (this->length == 2) {	// 1 byte
				uint8_t  u_byte_number = (uint8_t)strtoull(retstr, NULL, 0);
				*((uint8_t *)addr) = u_byte_number;
			}
			else {
				if (this->length == 3 || this->length == 4) {	// 2 bytes
					uint16_t  u_short_number = (uint16_t)strtoull(retstr, NULL, 0);
					*((uint16_t *)addr) = COB_BSWAP_16(u_short_number);
				}
				else {
					if (this->length >= 5 || this->length <= 9) {	// 4 bytes
						uint32_t  u_int_number = (uint32_t)strtoull(retstr, NULL, 0);
						uint32_t t = COB_BSWAP_32(u_int_number);
						*((uint32_t *)addr) = COB_BSWAP_32(u_int_number);
					}
					else {
						if (this->length >= 10 || this->length <= 18) {	// 8 bytes
							uint64_t  u_long_number = (uint64_t)strtoull(retstr, NULL, 0);
							*((uint64_t *)addr) = COB_BSWAP_64(u_long_number);
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
			*((int8_t *)addr) = (int8_t)strtoll(retstr, NULL, 0);
		}
		else {
			if (this->length == 2) {	// 1 byte
				int8_t  s_byte_number = (int8_t)strtoll(retstr, NULL, 0);
				*((int8_t *)addr) = s_byte_number;
			}
			else {
				if (this->length == 3 || this->length == 4) {	// 2 bytes
					int16_t  s_short_number = (int16_t)strtoll(retstr, NULL, 0);
					*((int16_t *)addr) = COB_BSWAP_16(s_short_number);
				}
				else {
					if (this->length >= 5 || this->length <= 9) {	// 4 bytes
						int32_t  s_int_number = (int32_t)strtoll(retstr, NULL, 0);
						*((int32_t *)addr) = COB_BSWAP_32(s_int_number);
					}
					else {
						if (this->length >= 10 || this->length <= 18) {	// 8 bytes
							int64_t  s_long_number = (int64_t)strtoll(retstr, NULL, 0);
							*((int64_t *)addr) = COB_BSWAP_64(s_long_number);
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
#ifndef NDEBUG
	char *tmp;
	if (type == COBOL_TYPE_JAPANESE) {
		tmp = oc_strndup((char *)addr, length * 2);
	}
	else {
		tmp = oc_strndup((char *)addr, length);
	}

#ifdef _DEBUG
	QString log_msg;
	log_msg.sprintf("%s@%s: %d %d#%s#%s#\n", __FILE__, __func__, type, length, retstr, tmp);
	QLogger::QLog_Trace(GIX_CONSOLE_LOG, log_msg);
#endif
	if (tmp) free(tmp);
#endif
}

void CobolVar::createCobolDataLowValue() {
	void *_addr = addr;

	memset(_addr, 0, length);

	return;
}

int CobolVar::getType()
{
	return type;
}
