/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#include "SymbolBufferReader.h"

SymbolBufferReader::SymbolBufferReader(uint8_t *_data, int _datalen)
{
	data = _data;
	datalen = _datalen;
}

QString SymbolBufferReader::readString()
{
	QByteArray qba;

	uint8_t b = 0;

	while (*(data + curpos) != 0 && curpos < datalen) {
		b = *(data + curpos);
		curpos++;
		qba.append(b);
	}

	curpos++;

	return QString::fromLocal8Bit(qba);
}

int SymbolBufferReader::readInt()
{
	if (curpos + 4 >= datalen)
		return false;

	uint8_t b0 = *(data + curpos); curpos++;
	uint8_t b1 = *(data + curpos); curpos++;
	uint8_t b2 = *(data + curpos); curpos++;
	uint8_t b3 = *(data + curpos); curpos++;

	uint32_t res = b0 + (b1 << 8) + (b2 << 16) + (b3 << 24);
	
	return res;
}
