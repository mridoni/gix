#pragma once

#include <QString>

#define _GIX_VER_GIXIDEMAJ 1
#define _GIX_VER_GIXIDEMIN 0
#define _GIX_VER_GIXIDEREL 0
#define _GIX_VER_GIXIDEBLD 0


QString getGixIdePrintableVersion()
{
	return QString("%1.%2.%3-%4").arg(_GIX_VER_GIXIDEMAJ).arg(_GIX_VER_GIXIDEMIN).arg(_GIX_VER_GIXIDEREL).arg(_GIX_VER_GIXIDEBLD);
}
