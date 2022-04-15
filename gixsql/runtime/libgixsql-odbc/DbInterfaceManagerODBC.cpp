/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
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

#include "DbInterfaceODBC.h"

// For schema functions

#include <algorithm>

struct DataBinding
{
	SQLSMALLINT TargetType;
	SQLPOINTER TargetValuePtr;
	SQLINTEGER BufferLength;
	SQLLEN StrLen_or_Ind;
};


int is_sql_any_success(SQLRETURN rc)
{
	return (rc == SQL_SUCCESS || rc == SQL_SUCCESS_WITH_INFO);
}

#define ERR_SRC_ENV		1
#define ERR_SRC_CONN	2
#define ERR_SRC_STMT	3

#define NUMCOLS 5
#define BUFFERSIZE 1024
#define STR_LEN 128 + 1
#define REM_LEN 254 + 1
#define TAB_LEN SQL_MAX_TABLE_NAME_LEN + 1
#define COL_LEN SQL_MAX_COLUMN_NAME_LEN + 1


#define EXIT_ON_ERR(rc) 	if (!is_sql_any_success(rc)) {\
								retrieve_odbc_error(ERR_SRC_STMT); \
								SQLFreeStmt(cur_stmt_handle, SQL_CLOSE); \
								return false; \
							}

#define EXIT_ON_ERR_OR_NO_DATA(rc) 	if (rc != SQL_SUCCESS && rc != SQL_SUCCESS_WITH_INFO && rc != SQL_NO_DATA) {\
								retrieve_odbc_error(ERR_SRC_STMT); \
								SQLFreeStmt(cur_stmt_handle, SQL_CLOSE); \
								return false; \
							}

bool DbInterfaceODBC::getSchemas(vector<SchemaInfo*>& res)
{
	struct DataBinding* schemaResult = (struct DataBinding*)
		malloc(NUMCOLS * sizeof(struct DataBinding));

	for (int i = 0; i < NUMCOLS; i++) {
		schemaResult[i].TargetType = SQL_C_CHAR;
		schemaResult[i].BufferLength = (BUFFERSIZE + 1);
		schemaResult[i].TargetValuePtr =
			malloc(sizeof(unsigned char) * schemaResult[i].BufferLength);
	}

	SQLRETURN retcode = SQLAllocHandle(SQL_HANDLE_STMT, conn_handle, &cur_stmt_handle);
	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_CONN);
		logger->log_debug(__FILE__, __func__, "FATAL ERROR: Can't allocate SQL Handle for the ODBC statement");
		logger->log_error("FATAL ERROR: Can't allocate SQL Handle for the ODBC statement");
		return DBERR_CONNECTION_FAILED;
	}

	for (int i = 0; i < NUMCOLS; i++) {
		retcode = SQLBindCol(cur_stmt_handle, (SQLUSMALLINT)i + 1,
			schemaResult[i].TargetType,
			schemaResult[i].TargetValuePtr,
			schemaResult[i].BufferLength,
			&(schemaResult[i].StrLen_or_Ind));

		EXIT_ON_ERR(retcode);
	}

	retcode = SQLTables(cur_stmt_handle, (SQLCHAR*)"", SQL_NTS,
		(SQLCHAR*)SQL_ALL_SCHEMAS, SQL_NTS, (SQLCHAR*)"",
		SQL_NTS, (SQLCHAR*)"", SQL_NTS);

	EXIT_ON_ERR(retcode);

	for (retcode = SQLFetch(cur_stmt_handle); is_sql_any_success(retcode); retcode = SQLFetch(cur_stmt_handle)) {

		// index 0 - catalog e.q. msdb
		// index 1 - schema  e.g dbo
		// index 2 - table name e.g. TestTBL1
		// index 3 - type - e.g. TABLE

		if (schemaResult[1].StrLen_or_Ind != SQL_NULL_DATA) {
			logger->log_debug(__FILE__, __func__, "Schema (%s)\n", (char*)schemaResult[1].TargetValuePtr);
			SchemaInfo* s = new SchemaInfo();
			s->name = (char*)schemaResult[1].TargetValuePtr;
			res.push_back(s);
		}

	}

	EXIT_ON_ERR_OR_NO_DATA(retcode);

	SQLFreeStmt(cur_stmt_handle, SQL_CLOSE);
	return true;
}

bool DbInterfaceODBC::getTables(string schema, vector<TableInfo*>& res)
{
	struct DataBinding* tableResult = (struct DataBinding*)
		malloc(NUMCOLS * sizeof(struct DataBinding));

	for (int i = 0; i < NUMCOLS; i++) {
		tableResult[i].TargetType = SQL_C_CHAR;
		tableResult[i].BufferLength = (BUFFERSIZE + 1);
		tableResult[i].TargetValuePtr =
			malloc(sizeof(unsigned char) * tableResult[i].BufferLength);
	}

	SQLRETURN retcode = SQLAllocHandle(SQL_HANDLE_STMT, conn_handle, &cur_stmt_handle);
	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_CONN);
		logger->log_debug(__FILE__, __func__, "FATAL ERROR: Can't allocate SQL Handle for the ODBC statement");
		logger->log_error("FATAL ERROR: Can't allocate SQL Handle for the ODBC statement");
		return DBERR_CONNECTION_FAILED;
	}

	for (int i = 0; i < NUMCOLS; i++) {
		retcode = SQLBindCol(cur_stmt_handle, (SQLUSMALLINT)i + 1,
			tableResult[i].TargetType,
			tableResult[i].TargetValuePtr,
			tableResult[i].BufferLength,
			&(tableResult[i].StrLen_or_Ind));

		EXIT_ON_ERR(retcode);
	}

	retcode = SQLTables(cur_stmt_handle, (SQLCHAR*)"%",
		SQL_NTS, (SQLCHAR*)schema.c_str(),
		SQL_NTS, (SQLCHAR*)SQL_ALL_TABLE_TYPES,
		SQL_NTS, (SQLCHAR*)"'TABLE'", SQL_NTS);

	EXIT_ON_ERR(retcode);

	for (retcode = SQLFetch(cur_stmt_handle); is_sql_any_success(retcode); retcode = SQLFetch(cur_stmt_handle)) {

		// index 0 - catalog e.q. msdb
		// index 1 - schema  e.g dbo
		// index 2 - table name e.g. TestTBL1
		// index 3 - type - e.g. TABLE

		if (tableResult[2].StrLen_or_Ind != SQL_NULL_DATA) {
			logger->log_debug(__FILE__, __func__, "Table (%s) = %s\n", (char*)tableResult[1].TargetValuePtr, (char*)tableResult[2].TargetValuePtr);
			TableInfo* t = new TableInfo();
			t->schema_name = (char*)tableResult[1].TargetValuePtr;
			t->name = (char*)tableResult[2].TargetValuePtr;
			res.push_back(t);
		}

	}

	EXIT_ON_ERR_OR_NO_DATA(retcode);

	SQLFreeStmt(cur_stmt_handle, SQL_CLOSE);
	return true;
}

ColumnType decode_odbc_data_type(int odbc_dt)
{
	switch (odbc_dt) {
		case SQL_BIGINT: return ColumnType::Bigint;
		case SQL_BINARY: return ColumnType::Binary;
		case SQL_BIT: return ColumnType::Bit;
		case SQL_CHAR: return ColumnType::Char;
		case SQL_DECIMAL: return ColumnType::Decimal;
		case SQL_DOUBLE: return ColumnType::Double;
		case SQL_GUID: return ColumnType::Guid;
		case SQL_INTEGER: return ColumnType::Integer;
		case SQL_LONGVARCHAR: return ColumnType::LongVarChar;
		case SQL_LONGVARBINARY: return ColumnType::LongVarBinary;
		case SQL_NUMERIC: return ColumnType::Numeric;
		case SQL_REAL: return ColumnType::Real;
		case SQL_SMALLINT: return ColumnType::SmallInt;
		case SQL_TINYINT: return ColumnType::TinyInt;
		case SQL_TYPE_TIME: return ColumnType::Time;
		case SQL_TYPE_TIMESTAMP: return ColumnType::Timestamp;
		case SQL_VARBINARY: return ColumnType::VarBinary;
		case SQL_VARCHAR: return ColumnType::VarChar;
		case SQL_WCHAR: return ColumnType::VarChar;
		case SQL_WLONGVARCHAR: return ColumnType::VarChar;
		case SQL_WVARCHAR: return ColumnType::VarChar;
		default: return ColumnType::VarChar;
	}
}

bool DbInterfaceODBC::getColumns(string schema, string table, vector<ColumnInfo*>& columns)
{
	SQLCHAR strSchema[STR_LEN];
	SQLCHAR strCatalog[STR_LEN];
	SQLCHAR strColumnName[STR_LEN];
	SQLCHAR strTableName[STR_LEN];
	SQLCHAR strTypeName[STR_LEN];
	SQLCHAR strRemarks[REM_LEN];
	SQLCHAR strColumnDefault[STR_LEN];
	SQLCHAR strIsNullable[STR_LEN];

	SQLINTEGER ColumnSize;
	SQLINTEGER BufferLength;
	SQLINTEGER CharOctetLength;
	SQLINTEGER OrdinalPosition;

	SQLSMALLINT DataType;
	SQLSMALLINT DecimalDigits;
	SQLSMALLINT NumPrecRadix;
	SQLSMALLINT Nullable;
	SQLSMALLINT SQLDataType;
	SQLSMALLINT DatetimeSubtypeCode;

	SQLLEN lenCatalog;
	SQLLEN lenSchema;
	SQLLEN lenTableName;
	SQLLEN lenColumnName;
	SQLLEN lenDataType;
	SQLLEN lenTypeName;
	SQLLEN lenColumnSize;
	SQLLEN lenBufferLength;
	SQLLEN lenDecimalDigits;
	SQLLEN lenNumPrecRadix;
	SQLLEN lenNullable;
	SQLLEN lenRemarks;
	SQLLEN lenColumnDefault;
	SQLLEN lenSQLDataType;
	SQLLEN lenDatetimeSubtypeCode;
	SQLLEN lenCharOctetLength;
	SQLLEN lenOrdinalPosition;
	SQLLEN lenIsNullable;

	UCHAR strPkTable[TAB_LEN];
	UCHAR strPkCol[COL_LEN];

	SQLLEN lenPkTable, lenPkCol, lenKeySeq;
	SQLSMALLINT   iKeySeq;

	SQLRETURN retcode = SQLAllocHandle(SQL_HANDLE_STMT, conn_handle, &cur_stmt_handle);
	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_CONN);
		logger->log_debug(__FILE__, __func__, "FATAL ERROR: Can't allocate SQL Handle for the ODBC statement");
		logger->log_error("FATAL ERROR: Can't allocate SQL Handle for the ODBC statement");
		return DBERR_CONNECTION_FAILED;
	}

	// Primary key

	retcode = SQLBindCol(cur_stmt_handle, 3, SQL_C_CHAR, strPkTable,
		TAB_LEN, &lenPkTable);
	EXIT_ON_ERR(retcode);

	retcode = SQLBindCol(cur_stmt_handle, 4, SQL_C_CHAR, strPkCol,
		COL_LEN, &lenPkCol);
	EXIT_ON_ERR(retcode);

	retcode = SQLBindCol(cur_stmt_handle, 5, SQL_C_SSHORT, &iKeySeq,
		TAB_LEN, &lenKeySeq);
	EXIT_ON_ERR(retcode);

	//retcode = SQLBindCol(cur_stmt_handle, 7, SQL_C_CHAR, strFkTable,
	//	TAB_LEN, &lenFkTable);
	//EXIT_ON_ERR(retcode);

	//retcode = SQLBindCol(cur_stmt_handle, 8, SQL_C_CHAR, strFkCol,
	//	COL_LEN, &lenFkCol);
	//EXIT_ON_ERR(retcode);

	retcode = SQLPrimaryKeys(cur_stmt_handle,
		NULL, 0,             // Catalog name
		(SQLCHAR*)(schema.c_str()), SQL_NTS,   // Schema name
		(SQLCHAR*)(table.c_str()), SQL_NTS);   // Table name

	PkInfo* pk = new PkInfo();
	while ((retcode == SQL_SUCCESS) || (retcode == SQL_SUCCESS_WITH_INFO)) {
		retcode = SQLFetch(cur_stmt_handle);
		if (retcode == SQL_SUCCESS || retcode == SQL_SUCCESS_WITH_INFO)
			logger->log_debug(__FILE__, __func__, "Table: %s Column: %s Key Seq: %hd \n", strPkTable, strPkCol, iKeySeq);
		pk->columns.push_back(string((const char*)&strPkCol));
	}

	retcode = SQLFreeStmt(cur_stmt_handle, SQL_CLOSE);

	// Columns

	retcode = SQLColumns(cur_stmt_handle,
		NULL, 0,
		(SQLCHAR*)(schema.c_str()), schema.length(),
		(SQLCHAR*)(table.c_str()), table.length(),
		NULL, 0);

	EXIT_ON_ERR_OR_NO_DATA(retcode);

	// Bind columns in result set to buffers
	if (retcode == SQL_SUCCESS || retcode == SQL_SUCCESS_WITH_INFO) {
		SQLBindCol(cur_stmt_handle, 1, SQL_C_CHAR, strCatalog, STR_LEN, &lenCatalog);
		SQLBindCol(cur_stmt_handle, 2, SQL_C_CHAR, strSchema, STR_LEN, &lenSchema);
		SQLBindCol(cur_stmt_handle, 3, SQL_C_CHAR, strTableName, STR_LEN, &lenTableName);
		SQLBindCol(cur_stmt_handle, 4, SQL_C_CHAR, strColumnName, STR_LEN, &lenColumnName);
		SQLBindCol(cur_stmt_handle, 5, SQL_C_SSHORT, &DataType, 0, &lenDataType);
		SQLBindCol(cur_stmt_handle, 6, SQL_C_CHAR, strTypeName, STR_LEN, &lenTypeName);
		SQLBindCol(cur_stmt_handle, 7, SQL_C_SLONG, &ColumnSize, 0, &lenColumnSize);
		SQLBindCol(cur_stmt_handle, 8, SQL_C_SLONG, &BufferLength, 0, &lenBufferLength);
		SQLBindCol(cur_stmt_handle, 9, SQL_C_SSHORT, &DecimalDigits, 0, &lenDecimalDigits);
		SQLBindCol(cur_stmt_handle, 10, SQL_C_SSHORT, &NumPrecRadix, 0, &lenNumPrecRadix);
		SQLBindCol(cur_stmt_handle, 11, SQL_C_SSHORT, &Nullable, 0, &lenNullable);
		SQLBindCol(cur_stmt_handle, 12, SQL_C_CHAR, strRemarks, REM_LEN, &lenRemarks);
		SQLBindCol(cur_stmt_handle, 13, SQL_C_CHAR, strColumnDefault, STR_LEN, &lenColumnDefault);
		SQLBindCol(cur_stmt_handle, 14, SQL_C_SSHORT, &SQLDataType, 0, &lenSQLDataType);
		SQLBindCol(cur_stmt_handle, 15, SQL_C_SSHORT, &DatetimeSubtypeCode, 0, &lenDatetimeSubtypeCode);
		SQLBindCol(cur_stmt_handle, 16, SQL_C_SLONG, &CharOctetLength, 0, &lenCharOctetLength);
		SQLBindCol(cur_stmt_handle, 17, SQL_C_SLONG, &OrdinalPosition, 0, &lenOrdinalPosition);
		SQLBindCol(cur_stmt_handle, 18, SQL_C_CHAR, strIsNullable, STR_LEN, &lenIsNullable);

		// retrieve column data
		while (SQL_SUCCESS == retcode) {
			retcode = SQLFetch(cur_stmt_handle);

			if (retcode)
				break;

			// Display column name, size and type
			logger->log_debug(__FILE__, __func__, " Column Name : %s, ", strColumnName);
			logger->log_debug(__FILE__, __func__, "Column Size : %i, ", ColumnSize);
			logger->log_debug(__FILE__, __func__, "Data Type   : %i\n", SQLDataType);

			ColumnInfo* c = new ColumnInfo();
			c->name = string((const char*)&strColumnName);
			c->is_nullable = Nullable;
			c->length = ColumnSize;
			c->base = NumPrecRadix;
			c->decimal_digits = DecimalDigits;
			c->type = decode_odbc_data_type(DataType);
			c->native_type = string((const char*)&strTypeName);

			vector<string>::iterator it = std::find_if(pk->columns.begin(), pk->columns.end(), [c](const string &s) { return s == c->name; });
			c->is_pk_column = (it != pk->columns.end());



			columns.push_back(c);
		}

		if (retcode > 0 && retcode != SQL_NO_DATA)
			columns.clear();

		EXIT_ON_ERR_OR_NO_DATA(retcode);

		SQLFreeStmt(cur_stmt_handle, SQL_CLOSE);
		return true;
	}
}

bool DbInterfaceODBC::getIndexes(string schema, string tabl, vector<IndexInfo*>& idxs)
{
	return false;
}
