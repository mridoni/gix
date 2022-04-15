#!/bin/bash

BUILD_NAME=gixsql-$GIXIDEMAJ.$GIXIDEMIN.$GIXIDEREL-$GIX_REVISION
BUILD_DIR=$WORKSPACE/$BUILD_NAME
mkdir $BUILD_DIR
if [ "$?" != "0" ] ; then echo "Cannot create build directory" ; exit 1 ; fi

mv libcpputils libgixpp gixpp copy libgixsql libgixsql-mysql libgixsql-odbc libgixsql-pgsql $BUILD_DIR
if [ "$?" != "0" ] ; then echo "Cannot setup build directory" ; exit 1 ; fi

cd $BUILD_DIR
if [ "$?" != "0" ] ; then echo "Cannot switch to build directory" ; exit 1 ; fi

cp -fravp $WORKSPACE/deploy/installers/linux/gixsql-autoconf/* .
if [ "$?" != "0" ] ; then echo "Cannot copy build scripts" ; exit 1 ; fi

autoreconf --install --force
if [ "$?" != "0" ] ; then echo "Cannot reconfigure into build directory" ; exit 1 ; fi

cp $WORKSPACE/build-tools/grammar-tools/FlexLexer.h ./libgixpp/
if [ "$?" != "0" ] ; then echo "Cannot copy lexer support files" ; exit 1 ; fi

mkdir ./test
if [ "$?" != "0" ] ; then echo "Cannot create package directory for test files" ; exit 1 ; fi

cp -fravp $WORKSPACE/deploy/installers/linux/gixsql-test/* ./test
if [ "$?" != "0" ] ; then echo "Cannot copy test files" ; exit 1 ; fi

# Pre-build gix_esql_parser.cc gix_esql_parser.hh gix_esql_scanner.cc location.hh
pushd $BUILD_DIR/libgixpp > /dev/null

bison -Wno-yacc -Wall -o gix_esql_parser.cc --defines=gix_esql_parser.hh gix_esql_parser.yy
if [ "$?" != "0" ] ; then echo "Cannot build parser" ; exit 1 ; fi

flex -c++ -v --debug -o "gix_esql_scanner.cc" gix_esql_scanner.ll
if [ "$?" != "0" ] ; then echo "Cannot build scanner" ; exit 1 ; fi

if [ ! -f "gix_esql_parser.cc" ] ; then echo "File not found: gix_esql_parser.cc" ; exit 1 ; fi
if [ ! -f "gix_esql_parser.hh" ] ; then echo "File not found: gix_esql_parser.hh" ; exit 1 ; fi
if [ ! -f "gix_esql_scanner.cc" ] ; then echo "File not found: gix_esql_scanner.cc" ; exit 1 ; fi
if [ ! -f "location.hh" ] ; then echo "File not found: location.hh" ; exit 1 ; fi

popd > /dev/null

rm -fr $(find . -name ".svn")
rm -fr $(find . -name "x64")
rm -fr $(find . -name "x86")
rm -fr $(find . -name "Makefile.linux")
rm -fr $(find . -name "Makefile.macOS")
rm -fr $(find . -name "Makefile.mingw-w64")
rm -fr $(find . -name "*.sln")
rm -fr $(find . -name "*.vcxproj")
rm -fr $(find . -name "*.filters")
rm -fr $(find . -name "*.user")
rm -fr $(find . -name "*.pro")
rm -fr $(find . -name "*.pri")

cd $WORKSPACE

PKGFILE=$BUILD_NAME.tar.gz

tar czvf $PKGFILE $BUILD_NAME
if [ "$?" != "0" ] ; then echo "Cannot build tarball" ; exit 1 ; fi

gpg --pinentry-mode loopback --batch --quiet --yes -b --passphrase-file $HOME/m.ridoni.keypwd -o $PKGFILE.sig $PKGFILE
if [ "$?" -ne "0" ] ; then
	echo "Error while signing package $PKGFILE"
    exit 1
fi

mv $PKGFILE $PKGFILE.sig $DOWNLOAD_DIR
if [ "$?" -ne "0" ] ; then
	echo "Error while moving archive $PKGFILE"
    exit 1
fi

exit 0