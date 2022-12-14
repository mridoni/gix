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


#include <QObject>
#include <QString>

#include <spdlog/spdlog.h>
#if SPDLOG_VERSION < 11000
//#include <fmt/format.h>
#endif

#include "QStringFormatter.h"


// standard log sources
#define LOG_DEFAULT		0
#define LOG_IDE			1
#define LOG_BUILD		2
#define LOG_NETWORK		3
#define LOG_DB			4
#define LOG_DEBUG		5
#define LOG_METADATA	6
#define LOG_CONFIG		7

#define LOG_TEST		99

#define LOG_CUSTOM_BASE	100

class IGixLogManager : public QObject
{
public:

#if SPDLOG_VERSION >= 11000    
	template<typename... Args>
	inline void log(int source, spdlog::level::level_enum level, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		std::shared_ptr<spdlog::logger> logger = get_logger(source);
		if (logger.get()) {

			logger->log(level, fmt, std::forward<Args>(args)...);
		}
		else {
			spdlog::memory_buf_t buf;
			spdlog::string_view_t f = fmt;
			fmt::detail::vformat_to(buf, f, fmt::make_format_args(std::forward<Args>(args)...));
			std::string s(buf.data(), buf.size());
			add_to_backlog(source, level, s);
		}
	};

	template<typename... Args>
	inline void trace(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		log(source, spdlog::level::trace, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void debug(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		log(source, spdlog::level::debug, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void info(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		log(source, spdlog::level::info, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void warn(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		log(source, spdlog::level::warn, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void error(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		log(source, spdlog::level::err, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void critical(int source, spdlog::format_string_t<Args...> fmt, Args &&... args)
	{
		this->log(source, spdlog::level::critical, fmt, std::forward<Args>(args)...);
	};
#elif SPDLOG_VERSION >= 10900
    template<typename... Args>
	inline void log(int source, spdlog::level::level_enum level, fmt::format_string<Args...> fmt, Args &&...args)
	{
		std::shared_ptr<spdlog::logger> logger = get_logger(source);
		if (logger.get()) {

			logger->log(level, fmt, std::forward<Args>(args)...);
		}
		else {
			spdlog::memory_buf_t buf;
			spdlog::string_view_t f = fmt;
			fmt::detail::vformat_to(buf, f, fmt::make_format_args(std::forward<Args>(args)...));
			std::string s(buf.data(), buf.size());
			add_to_backlog(source, level, s);
		}
	};

    template<typename... Args>
    inline void trace(int source, fmt::format_string<Args...> fmt, Args &&...args)
	{
		log(source, spdlog::level::trace, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void debug(int source, fmt::format_string<Args...> fmt, Args &&...args)
	{
		log(source, spdlog::level::debug, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void info(int source, fmt::format_string<Args...> fmt, Args &&...args)
	{
		log(source, spdlog::level::info, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void warn(int source, fmt::format_string<Args...> fmt, Args &&...args)
	{
		log(source, spdlog::level::warn, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void error(int source, fmt::format_string<Args...> fmt, Args &&...args)
	{
		log(source, spdlog::level::err, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void critical(int source, fmt::format_string<Args...> fmt, Args &&...args)
	{
		this->log(source, spdlog::level::critical, fmt, std::forward<Args>(args)...);
	};    

#else
    template<typename... Args>
	inline void log(int source, spdlog::level::level_enum level, const char *fmt, Args &&...args)
	{
		std::shared_ptr<spdlog::logger> logger = get_logger(source);
		if (logger.get()) {

			logger->log(level, fmt, std::forward<Args>(args)...);
		}
		else {
#if FMT_VERSION >= 80000
			spdlog::memory_buf_t buf;
			spdlog::string_view_t f = fmt;
			fmt::detail::vformat_to(buf, f, fmt::make_format_args(std::forward<Args>(args)...));
#else
			fmt::memory_buffer buf;
			fmt::format_to(buf, fmt, args...);
#endif
			std::string s(buf.data(), buf.size());
			add_to_backlog(source, level, s);
		}
	};

    template<typename... Args>
    inline void trace(int source, const char *fmt, Args &&...args)
	{
		log(source, spdlog::level::trace, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void debug(int source, const char *fmt, Args &&...args)
	{
		log(source, spdlog::level::debug, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void info(int source, const char *fmt, Args &&...args)
	{
		log(source, spdlog::level::info, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void warn(int source, const char *fmt, Args &&...args)
	{
		log(source, spdlog::level::warn, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void error(int source, const char *fmt, Args &&...args)
	{
		log(source, spdlog::level::err, fmt, std::forward<Args>(args)...);
	};

	template<typename... Args>
	inline void critical(int source, const char *fmt, Args &&...args)
	{
		this->log(source, spdlog::level::critical, fmt, std::forward<Args>(args)...);
	};  
#endif    
	template<typename T>
	void trace(int source, const T& msg)
	{
		log(source, spdlog::level::trace, msg);
	}

	template<typename T>
	void debug(int source, const T& msg)
	{
		log(source, spdlog::level::debug, msg);
	}

	template<typename T>
	void info(int source, const T& msg)
	{
		log(source, spdlog::level::info, msg);
	}

	template<typename T>
	void warn(int source, const T& msg)
	{
		log(source, spdlog::level::warn, msg);
	}

	template<typename T>
	void error(int source, T& msg)
	{
		log(source, spdlog::level::err, msg);
	}

	template<typename T>
	void critical(int source, const T& msg)
	{
		log(source, spdlog::level::critical, msg);
	}

private:
	virtual std::shared_ptr<spdlog::logger> get_logger(int source) = 0;
	virtual void add_to_backlog(int source, spdlog::level::level_enum, std::string msg) = 0;
};	

