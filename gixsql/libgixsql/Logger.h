/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 2.1,
* or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; see the file COPYING.LIB.  If
* not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
* Boston, MA 02110-1301 USA
*/

#pragma once

#include "ILogger.h"

#if _DEBUG
class Logger : public ILogger
{
public:
	Logger();
	~Logger();

	void log_error(const char * format, ...) override;
	void log_debug(const char *_file, const char *_func, const char * format, ...) override;
	bool isDebugLogActive() override;
	ILogger * get() override;

private:
	char *get_err_log_file_name();
	char *get_debug_log_file_name();
	bool check_debug_log();
	
	char *__debug_log_file;
	char *__err_log_file;
	bool __use_debug_log;

};

#define LOG_DEBUG(file, func, format, ...) logger.log_debug(file, func, format, __VA_ARGS__)
#define LOG_ERROR(format, ...) logger.log_error(format, __VA_ARGS__)

#define DECLARE_LOGGER(_l) Logger _l
#define DECLARE_LOGGER_STATIC(_l) static Logger _l

#else

#define LOG_DEBUG(file, func, format, ...)
#define LOG_ERROR(format, ...) 

#define DECLARE_LOGGER(_l) 
#define DECLARE_LOGGER_STATIC(_l)

#endif


