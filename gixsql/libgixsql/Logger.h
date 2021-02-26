#pragma once

#include "ILogger.h"

#if _DEBUG
class Logger : public ILogger
{
public:
	Logger();
	~Logger();

	void log_error(const char * format, ...) override;
	void log_debug(const char *_file, const char *_func, const char * format, ...) override;
	bool isDebugLogActive() override;
	ILogger * get() override;

private:
	char *get_err_log_file_name();
	char *get_debug_log_file_name();
	bool check_debug_log();
	
	char *__debug_log_file;
	char *__err_log_file;
	bool __use_debug_log;

};

#define LOG_DEBUG(file, func, format, ...) logger.log_debug(file, func, format, __VA_ARGS__)
#define LOG_ERROR(format, ...) logger.log_error(format, __VA_ARGS__)

#define DECLARE_LOGGER(_l) Logger _l
#define DECLARE_LOGGER_STATIC(_l) static Logger _l

#else

#define LOG_DEBUG(file, func, format, ...)
#define LOG_ERROR(format, ...) 

#define DECLARE_LOGGER(_l) 
#define DECLARE_LOGGER_STATIC(_l)

#endif


