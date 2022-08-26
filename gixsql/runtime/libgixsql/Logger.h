/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 3,
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

#include <memory>

#include "spdlog/spdlog.h"
#include "spdlog/sinks/basic_file_sink.h"


#define FMT_FILE_FUNC				"{}>{}: "
#define DEFAULT_GIXSQL_LOG_FILE		"gixsql.log"
#define DEFAULT_GIXSQL_LOG_LEVEL	spdlog::level::err

extern std::shared_ptr<spdlog::logger> gixsql_logger;
