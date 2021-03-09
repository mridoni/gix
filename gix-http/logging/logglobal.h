/*
This file is part of QtWebApp
Copyright (C) 2010 Stefan Frings

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

/**
  @file
  @author Stefan Frings
*/

#ifndef LOGGLOBAL_H
#define LOGGLOBAL_H

#include <QtGlobal>

// This is specific to Windows dll's
#if defined(Q_OS_WIN)
    #if defined(QTWEBAPPLIB_EXPORT)
        #define DECLSPEC Q_DECL_EXPORT
    #elif defined(QTWEBAPPLIB_IMPORT)
        #define DECLSPEC Q_DECL_IMPORT
    #endif
#endif
#if !defined(DECLSPEC)
    #define DECLSPEC
#endif

#endif // LOGGLOBAL_H

