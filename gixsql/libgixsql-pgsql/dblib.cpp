
#if defined(_WIN32) || defined(_WIN64)
#define LIBGIXSQL_API __declspec(dllexport)   
#else  
#define LIBGIXSQL_API
#endif  

#include <string>
#include <vector>

#include "DbInterfacePGSQL.h"

using namespace std;

extern "C" {

	LIBGIXSQL_API IDbInterface *get_dblib()
	{
		IDbInterface *dbi = new DbInterfacePGSQL();
		return dbi;
	}

}