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

#ifndef __SQLCA_H__
#define __SQLCA_H__

#define SQLERRMC_LEN	70
#define SQLSTATE_LEN	5

struct sqlca_t
{
	char	sqlcaid[8];
	int		sqlabc;
	int		sqlcode; // error code
	struct
	{
		short		sqlerrml;
		char		sqlerrmc[SQLERRMC_LEN];
	}			sqlerrm; // error message
	char		sqlerrp[8];
	int			sqlerrd[6];
	char		sqlwarn[8];
	char		sqlstate[5]; //err status
};

#endif