#include ../gix.cf

gixsql		:= gixsql
libgixsql	:= libgixsql
libgixsql-mysql	:= libgixsql-mysql 
libgixsql-odbc	:= libgixsql-odbc
libgixsql-pgsql	:= libgixsql-pgsql
libgixsql-test	:= libgixsql-test

libraries	:= libgixsql libgixsql-mysql libgixsql-odbc libgixsql-pgsql

.PHONY: all clean $(gixsql) $(libraries)

all: gixsql $(libraries)

$(gixsql) $(libraries):
	$(MAKE) --directory=$@

$(gixsql): $(libraries)

clean install :
	for dir in gixsql $(libraries); do \
		$(MAKE) -C $$dir -f Makefile $@; \
		done
