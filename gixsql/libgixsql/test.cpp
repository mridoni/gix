#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gixsql.h"

static void adjust_sign_display(char*cobol_num);

int main(int argv, char *args[])
{
	int rc = 0;

	const char *SQ0001 = "SELECT EMP_NO, EMP_NAME, EMP_SALARY FROM EMP ORDER BY EMP_NO";
	const char *SQ0002 = "SELECT COUNT( * ) FROM EMP";
	const char *SQ0003 = "SELECT EMP_NO, EMP_NAME, EMP_SALARY FROM EMP WHERE EMP_NO = $1 ORDER BY EMP_NO";

	char *usr = "postgres";
	char *pwd = "postgres";
	char *host = "localhost:5432/testdb";

	//char *usr = "postgres";
	//char *pwd = "postgres";
	//char *host = "testdb";

	char EMPQ[5];
	char EMP_CNT[5];
	char EMP_NO[5];
	char EMP_NAME[21];
	char EMP_SALARY[5];	// signed

	memset(EMPQ, 0, sizeof(EMPQ));
	memset(EMP_CNT, 0, sizeof(EMP_CNT));
	memset(EMP_NO, 0, sizeof(EMP_NO));
	memset(EMP_NAME, 0, sizeof(EMP_NAME));
	memset(EMP_SALARY, 0, sizeof(EMP_SALARY));

	struct sqlca_t *_sqlca = (struct sqlca_t *) malloc(sizeof(struct sqlca_t));

	rc = GIXSQLStartSQL();
	rc = GIXSQLCursorDeclare(_sqlca, "FETCHTBL_C1", SQ0001);
	rc = GIXSQLEndSQL();

	rc = GIXSQLConnect(_sqlca, usr, strlen(usr), pwd, strlen(pwd), host, strlen(host));

	rc = GIXSQLStartSQL();
	rc = GIXSQLExec(_sqlca, "BEGIN TRANSACTION");
	rc = GIXSQLEndSQL();

	rc = GIXSQLStartSQL();
	rc = GIXSQLSetResultParams(1, 4, 0, (char *) &EMP_CNT);
	rc = GIXSQLExecSelectIntoOne(_sqlca, SQ0002, 0, 1);
	rc = GIXSQLEndSQL();

	printf("--> %s\n", EMP_CNT);

	rc = GIXSQLCursorOpen(_sqlca, "FETCHTBL_C1");

	rc = GIXSQLStartSQL();
	rc = GIXSQLSetResultParams(3, 4, 0, &EMP_NO);
	rc = GIXSQLSetResultParams(16, 20, 0, &EMP_NAME);
	rc = GIXSQLSetResultParams(3, 4, 0, &EMP_SALARY);
	rc = GIXSQLCursorFetchOne(_sqlca, "FETCHTBL_C1");
	rc = GIXSQLEndSQL();

	printf("---- -------------------- ------\n");
	printf("NO   NAME                 SALARY\n");
	printf("---- -------------------- ------\n");

	int n = 0;
	do {
		//adjust_sign_display(&EMP_SALARY);
		printf("%04s %20s %s\n", EMP_NO, EMP_NAME, EMP_SALARY);

		GIXSQLStartSQL();
		GIXSQLSetResultParams(3, 4, 0, &EMP_NO);
		GIXSQLSetResultParams(16, 20, 0, &EMP_NAME);
		GIXSQLSetResultParams(3, 4, 0, &EMP_SALARY);
		rc = GIXSQLCursorFetchOne(_sqlca, "FETCHTBL_C1");
		GIXSQLEndSQL();
		//if (n++ == 12) {
		//	printf("%d\n", n);
		//}
	} while (!_sqlca->sqlcode);

	printf("After loop -> %d : %s\n", _sqlca->sqlcode, _sqlca->sqlstate);

	GIXSQLStartSQL();
	GIXSQLCursorClose(_sqlca, "FETCHTBL_C1");
	GIXSQLEndSQL();
	printf("After cursor close -> %d : %s\n", _sqlca->sqlcode, _sqlca->sqlstate);


	// TEST CURSOR WITH PARAMS
	strcpy(EMPQ, "0002");

	GIXSQLStartSQL();
	GIXSQLSetSQLParams(1, 4, 0, (char *) &EMPQ);
	GIXSQLCursorDeclareParams(_sqlca, "FETCHTBL_C2", SQ0003, 1);
	GIXSQLEndSQL();

	rc = GIXSQLCursorOpen(_sqlca, "FETCHTBL_C2");

	GIXSQLStartSQL();
	GIXSQLSetResultParams(3, 4, 0, &EMP_NO);
	GIXSQLSetResultParams(16, 20, 0, &EMP_NAME);
	GIXSQLSetResultParams(3, 4, 0, &EMP_SALARY);
	rc = GIXSQLCursorFetchOne(_sqlca, "FETCHTBL_C2");
	GIXSQLEndSQL();

	printf("FETCHTBL_C2\n");
	printf("%04s %20s %s\n", EMP_NO, EMP_NAME, EMP_SALARY);

	GIXSQLStartSQL();
	GIXSQLCursorClose(_sqlca, "FETCHTBL_C2");
	GIXSQLEndSQL();
	printf("After cursor close -> %d : %s\n", _sqlca->sqlcode, _sqlca->sqlstate);

	GIXSQLStartSQL();
	GIXSQLExec(_sqlca, "COMMIT");
	GIXSQLEndSQL();
	printf("After commit -> %d : %s\n", _sqlca->sqlcode, _sqlca->sqlstate);

	GIXSQLDisconnect(_sqlca);
	printf("After disconnect -> %d : %s\n", _sqlca->sqlcode, _sqlca->sqlstate);

	return 0;
}

static void adjust_sign_display(char*cobol_num)
{
	if (!strlen(cobol_num))
		return;

	char c = cobol_num[strlen(cobol_num) - 1];
	if (c & 64) {
		cobol_num[strlen(cobol_num) - 1] = c - 64;
		cobol_num[0] = '-';
	}
	else
		cobol_num[0] = '0';

}
