#pragma once

#include <stdint.h>
#include <QString>

#include "gixcommon_global.h"

class GIXCOMMON_EXPORT SymbolBufferReader
{
public:
	SymbolBufferReader(uint8_t *_data, int _datalen);

	QString readString();
	int readInt();

private:
	uint8_t *data = 0;
	int datalen = 0;
	int curpos = 0;
};

