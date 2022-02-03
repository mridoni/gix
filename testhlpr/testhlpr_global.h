#pragma once

#include <QtCore/qglobal.h>

#ifndef BUILD_STATIC
# if defined(TESTHLPR_LIB)
#  define TESTHLPR_EXPORT Q_DECL_EXPORT
# else
#  define TESTHLPR_EXPORT Q_DECL_IMPORT
# endif
#else
# define TESTHLPR_EXPORT
#endif
