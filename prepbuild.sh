#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# If /E is specified on the command line we take the following variables from the current environment
if [ "$1" != "/E" ] ; then

	# Check these variables and in case adjust them depending on your environment
	HOST_PLATFORM=x64
	QTDIR=/usr/lib
	# Check these variables (end)

	# These indicates the current version of Gix-IDE
	GIXIDEMAJ=1
	GIXIDEMIN=1
	GIXIDEREL=0dev1
	GIX_REVISION=1922
	# These indicates the current version of Gix-IDE (end)

	# These indicates the current version of GixSQL included in Gix-IDE
	GIXSQLMAJ=1
	GIXSQLMIN=0
	GIXSQLREL=18b
	# These indicates the current version of GixSQL included in Gix-IDE (end)

fi

echo "#define VERSION \"$GIXSQLMAJ.$GIXSQLMIN.$GIXSQLREL\"" > $SCRIPT_DIR/gixsql/config.h

cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/gixsql.pro ${SCRIPT_DIR}/gixsql/
cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/gixpp.pro ${SCRIPT_DIR}/gixsql/gixpp/
cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/libcpputils.pro ${SCRIPT_DIR}/gixsql/libcpputils/
cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixpp.pro ${SCRIPT_DIR}/gixsql/libgixpp/

cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql/
cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-mysql.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-mysql/
cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-odbc.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-odbc/
cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-oracle.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-oracle/
cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-pgsql.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-pgsql/
cp -v ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-sqlite.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-sqlite/
