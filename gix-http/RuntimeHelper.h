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

#pragma once

#include "ServerConfig.h"

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#define LIBHANDLE HINSTANCE
#else
#include <dlfcn.h>
#define LIBHANDLE void *
#define CALLBACK 
#endif

#include <QString>

typedef void(*CALLBACK cob_init_func_t)(int, char*);
typedef void *(*CALLBACK cob_resolve_func_t)(char*);
typedef int (*CALLBACK cob_tidy_func_t)();
typedef void (*CALLBACK cob_stop_run_func_t)(int);
typedef void (*CALLBACK cob_setenv_t)(const char* n, const char* v, int ov);

class RuntimeHelper
{
public:
	bool loadRuntime(ServerConfig *svr);

	void cleanup();

	void cob_init(int argc, char* argv);
	void *cob_resolve(char* name);
	int cob_tidy();
	void cob_stop_run(int);
	void cob_setenv(const char* n, const char* v, int ov);

private:
	cob_init_func_t __cob_init = NULL;
	cob_resolve_func_t __cob_resolve = NULL;
	cob_tidy_func_t __cob_tidy = NULL;
	cob_stop_run_func_t __cob_stop_run = NULL;
	cob_setenv_t __cob_setenv = NULL;

	LIBHANDLE libHandle = NULL;
};

