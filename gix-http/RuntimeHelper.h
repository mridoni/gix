#pragma once

#include "ServerConfig.h"

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#define LIBHANDLE HINSTANCE
#else
#include <dlfcn.h>
#define LIBHANDLE void *
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

