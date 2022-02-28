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

#include "DbInterfaceFactory.h"

#include <stdlib.h>
#include <cstring>

#include "gixsql.h"
#include "IDbInterface.h"
#include "Logger.h"

static std::map<IDbInterface *, LIBHANDLE> lib_map;

DECLARE_LOGGER_STATIC(logger);

typedef IDbInterface *(*DBLIB_PROVIDER_FUNC)(void);

#if defined(_WIN32)
//Returns the last Win32 error, in string format. Returns an empty string if there is no error.
std::string GetLastErrorAsString()
{
	//Get the error message ID, if any.
	DWORD errorMessageID = ::GetLastError();
	if (errorMessageID == 0) {
		return std::string(); //No error message has been recorded
	}

	LPSTR messageBuffer = nullptr;

	//Ask Win32 to give us the string version of that message ID.
	//The parameters we pass in, tell Win32 to create the buffer that holds the message for us (because we don't yet know how long the message string will be).
	size_t size = FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&messageBuffer, 0, NULL);

	//Copy the error message into a std::string.
	std::string message(messageBuffer, size);

	//Free the Win32's string's buffer.
	LocalFree(messageBuffer);

	return message;
}
#endif

IDbInterface *DbInterfaceFactory::getInterface(int type)
{
	switch (type) {
		case DB_PGSQL:
			return load_dblib("pgsql");

		case DB_ODBC:
			return load_dblib("odbc");

		case DB_MYSQL:
			return load_dblib("mysql");

		default:
			return NULL;
	}
}

IDbInterface* DbInterfaceFactory::getInterface(std::string t)
{
		if (t == "pgsql")
			return load_dblib("pgsql");

		if (t == "odbc")
			return load_dblib("odbc");

		if (t == "mysql")
			return load_dblib("mysql");

			return NULL;
}

IDbManagerInterface* DbInterfaceFactory::getManagerInterface(int type)
{
	return dynamic_cast<IDbManagerInterface *>(getManagerInterface(type));
}

IDbManagerInterface* DbInterfaceFactory::getManagerInterface(std::string type)
{
	return dynamic_cast<IDbManagerInterface *>(getManagerInterface(type));
}

IDbInterface *DbInterfaceFactory::load_dblib(const char *lib_id)
{
	char bfr[256];
	IDbInterface *dbi = NULL;
	LIBHANDLE libHandle = NULL;
	DBLIB_PROVIDER_FUNC dblib_provider;

	sprintf(bfr, "libgixsql-");
	strcat(bfr, lib_id);

#if defined(_WIN32)

	strcat(bfr, ".dll");
	LOG_DEBUG(__FILE__, __func__, "loading DB provider: %s\n", bfr);

	libHandle = LoadLibrary(bfr);

	if (libHandle != NULL)
	{
		dblib_provider = (DBLIB_PROVIDER_FUNC)GetProcAddress(libHandle, "get_dblib");
		LOG_DEBUG(__FILE__, __func__, "Accessing DB provider: %s\n", bfr);
		// If the function address is valid, call the function. 
		if (dblib_provider != NULL)
		{
#if _DEBUG
			char dll_path[MAX_PATH];
			int rc = GetModuleFileName(libHandle, dll_path, MAX_PATH);
			if (rc) {
				LOG_DEBUG(__FILE__, __func__, "DB provider loaded from: %s", dll_path);
			}
#endif
			dbi = dblib_provider();
			lib_map[dbi] = libHandle;
		}
		else {
			LOG_ERROR("ERROR while accessing DB provider: %s\n", bfr);
		}

		// Library not freed here
	}
	else {
		auto err = GetLastErrorAsString();
		LOG_ERROR("ERROR while loading DB provider %s: %s", bfr, err.c_str());
#if _DEBUG
		LOG_ERROR("PATH is: %s\n", getenv("PATH"));
#endif
	}

#else

	strcat(bfr, ".so");
	LOG_DEBUG(__FILE__, __func__, "loading DB provider: %s\n", bfr);

	libHandle = dlopen(bfr, RTLD_NOW);
	if (libHandle != NULL)
	{
		dblib_provider = (DBLIB_PROVIDER_FUNC)dlsym(libHandle, "get_dblib");
		LOG_DEBUG(__FILE__, __func__, "Accessing DB provider: %s\n", bfr);
		// If the function address is valid, call the function. 
		if (dblib_provider != NULL)
		{
			dbi = dblib_provider();
			lib_map[dbi] = libHandle;
		}
		else {
			LOG_ERROR("ERROR while accessing DB provider: %s\n", bfr);
		}

		// Library not freed here
	}
	else {
        LOG_ERROR("ERROR while loading DB provider: %s (%s)\n", bfr, dlerror());
	}

#endif

	if (dbi != NULL) {
#if _DEBUG
		ILogger *logger = (ILogger *) new Logger();
#else
		ILogger* logger = NULL;
#endif
		dbi->init(logger);
	}
	return dbi;
}

int DbInterfaceFactory::removeInterface(IDbInterface *dbi)
{
	if (dbi == NULL)
		return 1;


	if (lib_map.find(dbi) == lib_map.end())
		return 1;

	LIBHANDLE lib_ptr = lib_map[dbi];

#if defined(_WIN32) || defined(_WIN64)
	FreeLibrary(lib_ptr);
#else
	dlclose(lib_ptr),
#endif
	
	lib_map.erase(dbi);

	delete (dbi);

	return 0;
}

std::vector<std::string> DbInterfaceFactory::getAvailableDrivers()
{
	return std::vector<std::string> { "odbc", "mysql", "pgsql" } ;
}

