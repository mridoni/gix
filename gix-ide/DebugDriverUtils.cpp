/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#include "DebugDriverUtils.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>
#include <math.h>


#if defined(_WIN32) || defined(_WIN64)

#if defined(__MINGW32__)
typedef unsigned char byte;
#endif

#pragma comment (lib, "Ws2_32.lib")
#pragma comment (lib, "Mswsock.lib")
#pragma comment (lib, "AdvApi32.lib")

#define _WINSOCKAPI_

#define close closesocket

#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>

#else

#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/time.h>

#endif

#ifdef _GCDEBUGGER_NET_DEBUG
static int dbg_status_socket = 0;

void dbg_status_print(const char* fmt, ...)
{
    char bfr[2048];

    sprintf(bfr, "%012d - ", get_sys_time());

    va_list args;
    va_start(args, fmt);
    vsprintf(bfr + strlen(bfr), fmt, args);
    va_end(args);

    int len = strlen(bfr);
    if (!len)
        return;

    // if (len > 0 && bfr[len - 1] == '\n') {
        // bfr[len - 1] = '\0';
        // len--;
    // }

    if (send(dbg_status_socket, (const char *)&bfr, len, 0) != len) {
        fprintf(stderr, "partial/failed write\n");
    }
    else {
        if (len > 1 && bfr[len - 1] != '\n') {
            bfr[0] = '\n';
            bfr[1] = '\0';
            send(dbg_status_socket, (const char *)&bfr, 2, 0);
        }

    }

}

int dbg_status_init()
{
    const char* hostname = "127.0.0.1";
    struct addrinfo hints;
    struct addrinfo *result, *rp;

#if defined(_WIN32) || defined(_WIN64)
    WSADATA wsaData;

    int wsaret = WSAStartup(0x101, &wsaData);
    if (wsaret != 0) {
        return 1;
    }
#endif

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;    /* Allow IPv4 */
    hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
    hints.ai_flags = 0;
    hints.ai_protocol = IPPROTO_UDP;          /* Any protocol */

    int err = getaddrinfo(0, DBG_DRVR_STATUS_PORT, &hints, &result);
    if (err != 0) {
        fprintf(stderr, "failed to resolve remote socket address: %d (%s)\n", err, strerror(err));
        return 2;
    }

    for (rp = result; rp != NULL; rp = rp->ai_next) {
        dbg_status_socket = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (dbg_status_socket == -1)
            continue;

        if (connect(dbg_status_socket, rp->ai_addr, rp->ai_addrlen) != -1)
            break;                  /* Success */

        close(dbg_status_socket);
    }

    if (rp == NULL) {               /* No address succeeded */
        fprintf(stderr, "Could not connect\n");
        return 3;
    }

    dbg_status_print("Connected to internal debugging port %s\n", DBG_DRVR_STATUS_PORT);

    freeaddrinfo(result);           /* No longer needed */

    return 0;
}

void dbg_status_cleanup()
{
#if defined(_WIN32) || defined(_WIN64)
    if (dbg_status_socket)
        closesocket(dbg_status_socket);
#else
    if (dbg_status_socket)
        close(dbg_status_socket);
#endif
}

#else
void dbg_status_print(const char* fmt, ...) {}
int dbg_status_init() { return 0; }
void dbg_status_cleanup() {}
#endif

long long get_sys_time()
{
    struct timeval te;
    gettimeofday(&te, NULL); // get current time
    long long milliseconds = te.tv_sec*1000LL + te.tv_usec/1000; // calculate milliseconds

    return milliseconds;
}

#if defined(_WIN32) || defined(_WIN64)
int gettimeofday(struct timeval* trans_pipeline, struct timezone* tzp)
{
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
    // until 00:00:00 January 1, 1970 
    static const uint64_t EPOCH = ((uint64_t)116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime(&system_time);
    SystemTimeToFileTime(&system_time, &file_time);
    time = ((uint64_t)file_time.dwLowDateTime);
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    trans_pipeline->tv_sec = (long)((time - EPOCH) / 10000000L);
    trans_pipeline->tv_usec = (long)(system_time.wMilliseconds * 1000);
    return 0;
}
#endif
