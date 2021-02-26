#if _DEBUG
#include "Logger.h"
#include <stdio.h>
#include <stdarg.h>
#include <time.h>
#include <string>
#include <cstring>

Logger::Logger()
{
	__debug_log_file = NULL;
	__err_log_file = NULL;
	__use_debug_log = false;
}


Logger::~Logger()
{
}

void Logger::log_error(const char * format, ...)
{
	int is_std = 1;
	FILE* log = stdout;
	va_list args;
	va_list c_args;

	time_t rawtime;
	struct tm * timeinfo;
	char logts[80];

	time(&rawtime);
	timeinfo = localtime(&rawtime);
	strftime(logts, 80, "%Y-%m-%d %H:%M:%S", timeinfo);

	log = fopen(get_err_log_file_name(), "a+");

	if (!log)
		return;

	
	va_start(args, format);
	//va_copy(c_args, args);

	//char * fmt = va_arg(c_args, char *);

	fprintf(log, "%s - ", logts);
	vfprintf(log, format, args);

	if (format[strlen(format) - 1] != '\n')
		fprintf(log, "\n");

	va_end(args);


	fclose(log);
}

void Logger::log_debug(const char * _file, const char * _func, const char * format, ...)
{
	FILE* log;
	va_list args;

	time_t rawtime;
	struct tm * timeinfo;
	char logts[80];

	if (!__use_debug_log) {
		__use_debug_log = check_debug_log();
		//fprintf(stderr, "Debug logging is %s\n", (__use_debug_log) ? "ON" : "OFF");
	}

	if (!__use_debug_log)
		return;

	time(&rawtime);
	timeinfo = localtime(&rawtime);
	strftime(logts, 80, "%Y-%m-%d %H:%M:%S", timeinfo);

	log = fopen(get_debug_log_file_name(), "a+");

	if (!log)
		return;

	va_start(args, format);

#if defined(_WIN32) || defined(_WIN64)
	char *filename = strrchr((char *)_file, '\\');
#else
	char *filename = strrchr((char *)_file, '/');
#endif

	if (filename)
		filename += 1;
	else
		filename = (char *)_file;

	fprintf(log, "%s - %s@%s - ", logts, _func, filename);
	vfprintf(log, format, args);

	va_end(args);

	if (format[strlen(format) - 1] != '\n')
		fprintf(log, "\n");

	fclose(log);
}

bool Logger::isDebugLogActive()
{
	return __use_debug_log;
}

ILogger *Logger::get()
{
	return new Logger();
}

char * Logger::get_err_log_file_name()
{
	if (!__err_log_file) {
		if (getenv("GIXSQL_ERR_LOG")) {
			__err_log_file = strdup(getenv("GIXSQL_ERR_LOG"));
		}
		else {
			__err_log_file = (char *)calloc(1024, 1);

#if defined(_WIN32) || defined(_WIN64)
			strcpy(__err_log_file, getenv("TEMP"));
			strcat(__err_log_file, "\\");
#else
			strcpy(__err_log_file, "/tmp/");

#endif

			strcat(__err_log_file, "gixsql_err.log");
		}
	}
	return __err_log_file;
}

char * Logger::get_debug_log_file_name()
{
	if (!__debug_log_file) {
		if (getenv("GIXSQL_DEBUG_LOG")) {
			__debug_log_file = strdup(getenv("GIXSQL_DEBUG_LOG"));
		}
		else {
			__debug_log_file = (char *)calloc(1024, 1);

#if defined(_WIN32) || defined(_WIN64)
			strcpy(__debug_log_file, getenv("TEMP"));
			strcat(__debug_log_file, "\\");
#else
			strcpy(__debug_log_file, "/tmp/");

#endif

			strcat(__debug_log_file, "gixsql.log");
		}
		//fprintf(stderr, "Debug log filename is %s\n", __debug_log_file);
	}
	return __debug_log_file;
}

bool Logger::check_debug_log()
{
	return getenv("GIXSQL_DEBUG_LOG_ON") != NULL && strcmp(getenv("GIXSQL_DEBUG_LOG_ON"), "1") == 0;
}

#endif
