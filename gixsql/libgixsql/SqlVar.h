#pragma once

#define COBOL_TYPE_UNSIGNED_NUMBER		1         
#define COBOL_TYPE_SIGNED_NUMBER_TC		3        // (trailing combined)
#define COBOL_TYPE_SIGNED_NUMBER_LS		4        // (leading separate)
#define COBOL_TYPE_UNSIGNED_NUMBER_PD	8
#define COBOL_TYPE_SIGNED_NUMBER_PD		9     
#define COBOL_TYPE_ALPHANUMERIC			16
#define COBOL_TYPE_JAPANESE				24
#define COBOL_TYPE_MIN					0 
#define COBOL_TYPE_MAX					29 

#define COBOL_TYPE_UNSIGNED_BINARY		22
#define COBOL_TYPE_SIGNED_BINARY		23


class SqlVar
{
	friend class SqlVarList;

public:
	SqlVar();
	~SqlVar();

	SqlVar *copy();

	char *getRealData();

	void setType(int);
	void setLength(int);
	void setPower(int);
	void setAddr(void *);
	void setFlags(uint32_t);
	void setRealData(char *);
	void createRealData();

	void* getAddr();

	void createCobolData(char * retstr);

	void createCobolDataLowValue();

	int getType();
	int getLength();

private:
	int type; // set OCDB_TYPE_*
	int length; // size
	int power; // power
	void *addr; // address of variable
	char *realdata; // realdata

	// Level 49 support
	bool is_variable_length;
};

