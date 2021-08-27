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

#if defined(_WIN32) || defined(_WIN64)
#define GETSYMBOL GetProcAddress
#else
#define GETSYMBOL dlsym
#endif

bool RuntimeHelper::loadRuntime(ServerConfig *svr)
{
	__cob_init = nullptr;
	__cob_resolve = nullptr;
	__cob_tidy = nullptr;
	__cob_stop_run = nullptr;
	__cob_setenv = nullptr;
	__cob_call = nullptr;

#if defined(_WIN32) || defined(_WIN64)
	libHandle = LoadLibrary("libcob.dll");
#else
    libHandle = dlopen("libcob.so", RTLD_NOW);
#endif

	if (libHandle != NULL) {
		__cob_init = (cob_init_func_t)GETSYMBOL(libHandle, "cob_init");
		__cob_resolve = (cob_resolve_func_t)GETSYMBOL(libHandle, "cob_resolve");
		__cob_tidy = (cob_tidy_func_t)GETSYMBOL(libHandle, "cob_tidy");
		__cob_stop_run = (cob_stop_run_func_t)GETSYMBOL(libHandle, "cob_stop_run");
		__cob_setenv = (cob_setenv_func_t)GETSYMBOL(libHandle, "cob_setenv");
		__cob_call = (cob_call_func_t)GETSYMBOL(libHandle, "cob_call");

		if (!__cob_init || !__cob_resolve || !__cob_tidy || !__cob_stop_run || !__cob_setenv || !__cob_call) {
			return false;
		}
		else {
			//            cob_setenv("COB_LIBRARY_PATH", svr->getBasePath().toLocal8Bit().constData(), 1);

			return true;
		}
	}
	else {
		return false;
	}
	// Library not freed here
}

void RuntimeHelper::init()
{
#if defined(_DEBUG) && defined(_WIN32)
	fprintf(stderr, "gix-http: initializing GnuCOBOL runtime\n");
#endif
	cob_init(0, NULL);
}

void RuntimeHelper::cleanup()
{
	cob_tidy();

#if defined(_WIN32) || defined(_WIN64)
	if (libHandle)
		FreeLibrary(libHandle);
#else
	if (libHandle)
		dlclose(libHandle);

#endif
}

void RuntimeHelper::cob_init(int argc, char *argv)
{
	__cob_init(argc, argv);
}

void *RuntimeHelper::cob_resolve(const char *name)
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

void RuntimeHelper::cob_setenv(const char *n, const char *v, int ov)
{
	__cob_setenv(n, v, ov);
}

int RuntimeHelper::cob_call(const char *n, const int argc, void **argv)
{
	return __cob_call(n, argc, argv);
}

