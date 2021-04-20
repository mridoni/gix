#pragma once

#include <QString>

#define _GIX_VER_GIXIDEMAJ {GIXIDEMAJ}
#define _GIX_VER_GIXIDEMIN {GIXIDEMIN}
#define _GIX_VER_GIXIDEREL {GIXIDEREL}
#define _GIX_VER_GIXIDEBLD {GIXIDEBLD}


QString getGixIdePrintableVersion()
{
	return QString("%1.%2.%3-%4").arg(_GIX_VER_GIXIDEMAJ).arg(_GIX_VER_GIXIDEMIN).arg(_GIX_VER_GIXIDEREL).arg(_GIX_VER_GIXIDEBLD);
}
