#pragma once

#include <QObject>

enum class EolMode
{
	PlatformDefault = -1,
	Windows = 0,		// CRLF
	Unix = 2,			// LF
	ClassicMacOS = 1,	// CR

	Mixed = 999
};

Q_DECLARE_METATYPE(EolMode)
