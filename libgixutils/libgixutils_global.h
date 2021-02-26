#pragma once

#include <QtCore/qglobal.h>

#ifndef BUILD_STATIC
# if defined(LIBGIXUTILS_LIB)
#  define LIBGIXUTILS_EXPORT Q_DECL_EXPORT
# else
#  define LIBGIXUTILS_EXPORT Q_DECL_IMPORT
# endif
#else
# define LIBGIXUTILS_EXPORT
#endif
