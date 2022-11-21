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
	GIX_REVISION=1923
	# These indicates the current version of Gix-IDE (end)

	# These indicates the current version of GixSQL included in Gix-IDE
	GIXSQLMAJ=1
	GIXSQLMIN=0
	GIXSQLREL=18b
	# These indicates the current version of GixSQL included in Gix-IDE (end)

fi
echo "Configuring version ($GIXIDEMAJ.$GIXIDEMIN.$GIXIDEREL-$GIX_REVISION) in header file"
rm -f $SCRIPT_DIR/gix-ide/GixVersion.h
cp $SCRIPT_DIR/gix-ide/GixVersion.h.tpl $SCRIPT_DIR/gix-ide/GixVersion.h
sed -i "s/{GIXIDEMAJ}/$GIXIDEMAJ/g" $SCRIPT_DIR/gix-ide/GixVersion.h
sed -i "s/{GIXIDEMIN}/$GIXIDEMIN/g" $SCRIPT_DIR/gix-ide/GixVersion.h
sed -i "s/{GIXIDEREL}/$GIXIDEREL/g" $SCRIPT_DIR/gix-ide/GixVersion.h
sed -i "s/{GIXIDEBLD}/$GIX_REVISION/g" $SCRIPT_DIR/gix-ide/GixVersion.h

echo "Configuring version ($GIXSQLMAJ.$GIXSQLMIN.$GIXSQLREL) in header file for GixSQL"
echo "#define VERSION \"$GIXSQLMAJ.$GIXSQLMIN.$GIXSQLREL\"" > $SCRIPT_DIR/gixsql/config.h

echo "Configuring GixSQL for QMake"
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/gixsql.pro ${SCRIPT_DIR}/gixsql/
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/gixpp.pro ${SCRIPT_DIR}/gixsql/gixpp/
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/libcpputils.pro ${SCRIPT_DIR}/gixsql/libcpputils/
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixpp.pro ${SCRIPT_DIR}/gixsql/libgixpp/

cp ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql/
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-mysql.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-mysql/
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-odbc.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-odbc/
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-oracle.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-oracle/
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-pgsql.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-pgsql/
cp ${SCRIPT_DIR}/build-tools/gixsql-pro/libgixsql-sqlite.pro ${SCRIPT_DIR}/gixsql/runtime/libgixsql-sqlite/
