#include "DbInterfaceFactory.h"

#include <stdlib.h>
#include <cstring>

#include "gixsql.h"
#include "IDbInterface.h"
#include "Logger.h"

static std::map<IDbInterface *, LIBHANDLE> lib_map;

DECLARE_LOGGER_STATIC(logger);

typedef IDbInterface *(*DBLIB_PROVIDER_FUNC)(void);

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

#if defined(_WIN32) || defined(_WIN64)

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
			dbi = dblib_provider();
			lib_map[dbi] = libHandle;
		}
		else {
			LOG_ERROR("ERROR while accessing DB provider: %s\n", bfr);
		}

		// Library not freed here
	}
	else {
		LOG_ERROR("ERROR while loading DB provider: %s\n", bfr);
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
		LOG_ERROR("ERROR while loading DB provider: %s\n", bfr);
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

