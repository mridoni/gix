#pragma once

#include <string>



class ILogger
{

public:
	virtual void log_error(const char * format, ...) = 0;
	virtual void log_debug(const char *_file, const char *_func, const char * format, ...) = 0;
	virtual ILogger *get() = 0;
	virtual bool isDebugLogActive() = 0;
};