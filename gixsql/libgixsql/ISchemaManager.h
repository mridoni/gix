#pragma once

#include <string>
#include <vector>



enum class ColumnType
{
	Bigint,
	Binary,
	Bit,
	Char,
	Decimal,
	Double,
	Guid,
	Integer,
	LongVarChar,
	LongVarBinary,
	Numeric,
	Real,
	SmallInt,
	TinyInt,
	Time,
	Timestamp,
	VarBinary,
	VarChar
};

/*
	For numeric data types, base is either 10 or 2. If it is 10, the values in COLUMN_SIZE and DECIMAL_DIGITS give the number of decimal digits allowed for the column. 
	For example, a DECIMAL(12,5) column would return a NUM_PREC_RADIX of 10, a COLUMN_SIZE of 12, and a DECIMAL_DIGITS of 5; 
	a FLOAT column could return a NUM_PREC_RADIX of 10, a COLUMN_SIZE of 15, and a DECIMAL_DIGITS of NULL.

	If it is 2, the values in COLUMN_SIZE and DECIMAL_DIGITS give the number of bits allowed in the column. 
	For example, a FLOAT column could return a RADIX of 2, a COLUMN_SIZE of 53, and a DECIMAL_DIGITS of NULL.

	NULL is returned for data types where NUM_PREC_RADIX is not applicable.
*/
class ColumnInfo
{
public:
	std::string name;
	ColumnType  type;
	std::string native_type;
	int length;	// See above
	int base;	// See above
	int decimal_digits;
	bool is_nullable;
	bool is_pk_column;

	bool isNumeric() { 
		return type == ColumnType::Bigint ||
			type == ColumnType::Decimal ||
			type == ColumnType::Double ||
			type == ColumnType::Integer ||
			type == ColumnType::Numeric ||
			type == ColumnType::SmallInt ||
			type == ColumnType::TinyInt ||
			type == ColumnType::Real;
	};
};

class PkInfo
{
public:
	std::string name;
	std::vector<std::string> columns;
};

class IndexInfo
{
public:
	std::string name;
	std::vector<std::string> columns;
	bool is_unique;
};

class TableInfo
{
public:
	std::string name;
	std::string schema_name;
	//PkInfo primary_key;
};

class SchemaInfo
{
public:
	std::string name;
	//PkInfo primary_key;
};
