#pragma once


#ifndef BUILD_STATIC
#ifdef _WIN32
# if defined(GIX_DEBUGGER_LIB) || defined(GIX_IDE)
#  define GIX_DEBUGGER_EXPORT __declspec(dllexport)
# else
#  define GIX_DEBUGGER_EXPORT __declspec(dllimport)
# endif
#else
# define GIX_DEBUGGER_EXPORT
#endif
#else
# define GIX_DEBUGGER_EXPORT
#endif
