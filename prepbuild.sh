#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

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
