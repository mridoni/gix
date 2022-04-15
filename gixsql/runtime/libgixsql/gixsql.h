/*
* Copyright (C) 2021 Marco Ridoni
* Copyright (C) 2013 Tokyo System House Co.,Ltd.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 3,
* or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; see the file COPYING.LIB.  If
* not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
* Boston, MA 02110-1301 USA
*/

#ifndef GIXSQL_H
#define GIXSQL_H

#include "sqlca.h"

#define GIXSQL_CONN_CONNECT_OK 0
#define GIXSQL_CONN_FAIL_CONNECT (-1)

#define GIXSQL_NO_CONNECTION (-1)

/*
#define SQLERRMC_LEN	70
#define SQLSTATE_LEN	5

struct sqlca_t
{
char		sqlcaid[8];
int		sqlabc;
int		sqlcode; // error code
struct
{
short		sqlerrml;
char		sqlerrmc[SQLERRMC_LEN];
}			sqlerrm; // error message
char		sqlerrp[8];
int		sqlerrd[6];
char		sqlwarn[8];
char		sqlstate[5]; //err status
} sqlca;
*/

static struct sqlca_t sqlca_init =
{
	// char	sqlcaid[8];
	{
		'S', 'Q', 'L', 'C', 'A', ' ', ' ', ' '
	},
	
	// int		sqlabc;
	sizeof(struct sqlca_t),

	// int		sqlcode;
	0,

	{
		0,
		{
			' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
			' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
			' ',' ',' ',' ',' ',' ',' ',' ',' ',' '
		}
	},

	// char		sqlerrp[8];
	{
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '
	},

	// int			sqlerrd[6];
	{
		0, 0, 0, 0, 0, 0
	},

	// char		sqlwarn[8];
	{
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '
	},

	// char		sqlstate[5];
	{
		'0', '0', '0', '0', '0'
	}
};



#define GIXSQL_TYPE_PD_POSITIVE 'C'
#define GIXSQL_TYPE_PD_NEGATIVE 'D'

#define GIXSQL_OK 0
#define GIXSQL_NORECORD 1
#define GIXSQL_TIMEOUT 2

#define RESULT_SUCCESS	0
#define RESULT_FAILED	1

//#define GIXSQL_USE_DEFAULT_CONNECTION -999

// autocommit
#define GIXSQL_AUTOCOMMIT_OFF false
#define GIXSQL_AUTOCOMMIT_ON  true
#define GIXSQL_AUTOCOMMIT_DEFAULT GIXSQL_AUTOCOMMIT_OFF

#define GIXSQL_CLIENT_ENCODING_DEFAULT ""

#if defined(_WIN32) || defined(_WIN64)
#define LIBGIXSQL_API __declspec(dllexport)   
#else  
#define LIBGIXSQL_API
#endif  

extern "C" {
	//LIBGIXSQL_API int GIXSQLConnect(struct sqlca_t* st, char* user, int userlen, char* passwd, int passwdlen, char* name, int namelen);
	LIBGIXSQL_API int GIXSQLConnect(struct sqlca_t *st, void *d_data_source, int data_source_tl, void *d_connection_id, int connection_id_tl,
										void *d_username, int username_tl, void *d_password, int password_tl);

	LIBGIXSQL_API int GIXSQLConnectReset(struct sqlca_t *, void *d_connection_id, int connection_id_tl);
	LIBGIXSQL_API int GIXSQLDisconnect(struct sqlca_t *, void *d_connection_id, int connection_id_tl);

	LIBGIXSQL_API int GIXSQLExec(struct sqlca_t *, void *d_connection_id, int connection_id_tl, char *);
	LIBGIXSQL_API int GIXSQLExecParams(struct sqlca_t *, void *d_connection_id, int connection_id_tl, char *, int);
	LIBGIXSQL_API int GIXSQLExecSelectIntoOne(struct sqlca_t *, void *d_connection_id, int connection_id_tl, char *, int, int);
	LIBGIXSQL_API int GIXSQLExecImmediate(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, void *d_query, int query_tl);

	LIBGIXSQL_API int GIXSQLCursorDeclare(struct sqlca_t *, void *d_connection_id, int connection_id_tl, char *, int, char *);
	LIBGIXSQL_API int GIXSQLCursorDeclareParams(struct sqlca_t* st, void *d_connection_id, int connection_id_tl, char* cname, int with_hold, char* _query, int nParams);
	LIBGIXSQL_API int GIXSQLCursorOpen(struct sqlca_t *, char *);
	LIBGIXSQL_API int GIXSQLCursorFetchOne(struct sqlca_t *, char *);
	LIBGIXSQL_API int GIXSQLCursorClose(struct sqlca_t *, char *);

	LIBGIXSQL_API int GIXSQLPrepareStatement(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, char *stmt_name, void *d_statement_src, int statement_src_tl);
	LIBGIXSQL_API int GIXSQLExecPrepared(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, char *stmt_name, int nParams);

	LIBGIXSQL_API int GIXSQLStartSQL(void);
	LIBGIXSQL_API int GIXSQLSetSQLParams(int type, int length, int scale, uint32_t flags, void* addr);
	LIBGIXSQL_API int GIXSQLSetResultParams(int type, int length, int scale, uint32_t flags, void* addr);
	LIBGIXSQL_API int GIXSQLEndSQL(void);

}

#endif
