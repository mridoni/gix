#include "GixVersion.h"

QString getGixIdePrintableVersion()
{
	return QString("%1.%2.%3-%4").arg(_GIX_VER_GIXIDEMAJ).arg(_GIX_VER_GIXIDEMIN).arg(_GIX_VER_GIXIDEREL).arg(_GIX_VER_GIXIDEBLD);
}
