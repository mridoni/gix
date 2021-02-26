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