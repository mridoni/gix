#ifndef __PLATFORM_H__

#if (defined(_WIN32) || defined(_WIN64)) 

char *strndup(const char *s1, size_t n);

#define strcasecmp _stricmp 
#define strncasecmp _strnicmp 

#endif

#if defined(_WIN32) || defined(_WIN64)

char *get_log_file_name();
#endif

#endif
