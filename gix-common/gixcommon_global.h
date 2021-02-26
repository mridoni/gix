#pragma once

#include <QtCore/qglobal.h>

#ifndef BUILD_STATIC
# if defined(GIXCOMMON_LIB)
#  define GIXCOMMON_EXPORT Q_DECL_EXPORT
# else
#  define GIXCOMMON_EXPORT Q_DECL_IMPORT
# endif
#else
# define GIXCOMMON_EXPORT
#endif

#define GIX_CONSOLE_LOG "GIX-CONSOLE"
