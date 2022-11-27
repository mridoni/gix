/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#pragma once


#include <QString>

#include "spdlog/spdlog.h"
#include "spdlog/common.h"

#include "QStringFormatter.h"


// standard log sources
#define LOG_DEFAULT		0
#define LOG_IDE			1
#define LOG_BUILD		2
#define LOG_NETWORK		3
#define LOG_DB			4
#define LOG_DEBUG		5
#define LOG_METADATA	6

#define LOG_CUSTOM_BASE	100

class IGixLogManager : public QObject
{
public:

	template<typename... Args>
	inline void log(int source, spdlog::level::level_enum level, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		spdlog::logger* logger = get_logger();
		if (logger)
			logger->log(level, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void trace(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		spdlog::logger* logger = get_logger(source);
		if (logger)
			logger->trace(fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void debug(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		spdlog::logger* logger = get_logger();
		if (logger)
			logger->debug(fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void info(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		spdlog::logger* logger = get_logger();
		if (logger)
			logger->info(fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void warn(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		spdlog::logger* logger = get_logger();
		if (logger)
			logger->warn(fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void error(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		spdlog::logger* logger = get_logger();
		if (logger)
			logger->error(fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void critical(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		spdlog::logger* logger = get_logger();
		if (logger)
			logger->critical(fmt, std::forward<Args>(args)...);
	};



private:
	virtual spdlog::logger* get_logger(int source) = 0;
};	

