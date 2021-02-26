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
