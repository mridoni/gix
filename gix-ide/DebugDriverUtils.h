#pragma once

#define DBG_DRVR_STATUS_PORT "50002"

void dbg_status_print(const char* fmt, ...);
int dbg_status_init();
void dbg_status_cleanup();

long long get_sys_time();

#if defined(_WIN32) || defined(_WIN64)
int gettimeofday(struct timeval* trans_pipeline, struct timezone* tzp);
#endif
