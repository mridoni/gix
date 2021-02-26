#pragma once

#include <QtCore/qglobal.h>

#ifndef BUILD_STATIC
# if defined(LIBGIXPP_LIB)
#  define LIBGIXPP_EXPORT Q_DECL_EXPORT
# else
#  define LIBGIXPP_EXPORT Q_DECL_IMPORT
# endif
#else
# define LIBGIXPP_EXPORT
#endif
