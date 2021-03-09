/*
Copyright (C) 2021 Marco Ridoni

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


#include "RuntimeHelper.h"

bool RuntimeHelper::loadRuntime(ServerConfig *svr)
{

	__cob_init = nullptr;
	__cob_resolve = nullptr;
	__cob_tidy = nullptr;
	__cob_stop_run = nullptr;

#if defined(_WIN32) || defined(_WIN64)

	libHandle = LoadLibrary("libcob.dll");

	if (libHandle != NULL)
	{
		__cob_init = (cob_init_func_t) GetProcAddress(libHandle, "cob_init");
		__cob_resolve = (cob_resolve_func_t)GetProcAddress(libHandle, "cob_resolve");
		__cob_tidy = (cob_tidy_func_t)GetProcAddress(libHandle, "cob_tidy");
		__cob_stop_run = (cob_stop_run_func_t)GetProcAddress(libHandle, "cob_stop_run");
		__cob_setenv = (cob_setenv_t)GetProcAddress(libHandle, "cob_setenv");

		if (!__cob_init || !__cob_resolve || !__cob_tidy || !__cob_stop_run || !__cob_setenv) {
			return false;
		}
		else {
			cob_setenv("COB_LIBRARY_PATH", svr->getBasePath().toLocal8Bit().constData(), 1);
			cob_init(0, NULL);
			return true;
		}
	}
	else {
		return false;
	}

#else

strcat(bfr, ".so");
logger.log_debug(__FILE__, __func__, "loading DB provider: %s\n", bfr);

libHandle = dlopen(bfr, RTLD_NOW);
if (libHandle != NULL)
{
	dblib_provider = (DBLIB_PROVIDER_FUNC)dlsym(libHandle, "get_dblib");
	logger.log_debug(__FILE__, __func__, "Accessing DB provider: %s\n", bfr);
	// If the function address is valid, call the function. 
	if (dblib_provider != NULL)
	{
		dbi = dblib_provider();
		lib_map[dbi] = libHandle;
	}
	else {
		logger.log_error("ERROR while accessing DB provider: %s\n", bfr);
	}

	// Library not freed here
}
else {
	logger.log_error("ERROR while loading DB provider: %s\n", bfr);
}

#endif

	return false;
}

void RuntimeHelper::cleanup()
{
	cob_tidy();

#if defined(_WIN32) || defined(_WIN64)
	if (libHandle)
		FreeLibrary(libHandle);
#else

#endif
}

void RuntimeHelper::cob_init(int argc, char* argv)
{
	__cob_init(argc, argv);
}

void* RuntimeHelper::cob_resolve(char* name)
{
	return __cob_resolve(name);
}

int RuntimeHelper::cob_tidy()
{
	return __cob_tidy();
}

void RuntimeHelper::cob_stop_run(int i)
{
	__cob_stop_run(i);
}

void RuntimeHelper::cob_setenv(const char* n, const char* v, int ov)
{
	__cob_setenv(n, v, ov);
}

