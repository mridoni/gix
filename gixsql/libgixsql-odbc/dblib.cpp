
#if defined(_WIN32) || defined(_WIN64)
#define LIBGIXSQL_API __declspec(dllexport)   
#else  
#define LIBGIXSQL_API
#endif  

#include <string>
#include <vector>

#include "DbInterfaceODBC.h"

using namespace std;

DbInterfaceODBC dbi;

extern "C" {

	LIBGIXSQL_API IDbInterface *get_dblib()
	{
		//IDbInterface *dbi = new DbInterfaceODBC();
		//return dbi;
		return &dbi;
	}

}