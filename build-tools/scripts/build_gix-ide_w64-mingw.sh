#!/bin/bash

#WORKSPACE=/tmp/gix_build_$(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 8 ; echo '')
#if [ ! -d "$WORKSPACE" ] ; then mkdir $WORKSPACE ; fi

#cd $WORKSPACE
#svn co file:///opt/svn/public/gix-ide/trunk .

#rm .svn -fr
#------
export CONFIG=Release

export MXE_HOME=/opt/mxe
export MXE_BIN=$MXE_HOME/usr/bin
export MXE_LIBS=$MXE_HOME/usr/x86_64-w64-mingw32.shared/bin
export QT5_BIN=$MXE_HOME/usr/x86_64-w64-mingw32.shared/qt5/bin
export QT5_PLUGINS=$MXE_HOME/usr/x86_64-w64-mingw32.shared/qt5/plugins
export QIF_BASE_DIR=$WORKSPACE/deploy/installers/mingw-w64
export BUILD_DIR=$WORKSPACE/x64/$CONFIG
export FINAL_OUTPUT_DIR=/var/www/gix-ide/download
export DIST_BASE_DIR=$WORKSPACE/build_tmp
export DIST_VERSION=1.0.0

if [ "$?" != "0" ] ; then
	echo "Cannot checkout from file:///opt/svn/public/gix-ide/trunk to $WORKSPACE/gix-ide"
	exit 1
fi

export PATH=$PATH:$MXE_BIN

mkdir -p ${BUILD_DIR}
# scintilla
cd $WORKSPACE/libs/scintilla/src
unzip scintilla432.zip
if [ "$?" != "0" ] ; then  echo "Cannot expand Scintilla" ; exit 1 ;fi

cd scintilla/qt/ScintillaEdit
python WidgetGen.py
if [ "$?" != "0" ] ; then  echo "Cannot generate Scintilla Qt headers" ; exit 1 ;fi

x86_64-w64-mingw32.shared-qmake-qt5
if [ "$?" != "0" ] ; then  echo "Cannot run QMake for Scintilla" ; exit 1 ;fi

make -j 4MXE_TARGETS=x86_64-w64-mingw32.shared MXE_PLUGIN_DIRS=plugins/gcc8
if [ "$?" != "0" ] ; then  echo "Cannot build Scintilla" ; exit 1 ;fi

cd $WORKSPACE/libs/scintilla/bin

# gixsql components
cd $WORKSPACE/gixsql

DESTDIR=${BUILD_DIR} make -j4 -f Makefile.mingw-w64
if [ "$?" != "0" ] ; then  echo "Cannot build GixSQL components" ; exit 1 ;fi

# gix-common
cd $WORKSPACE/gix-common

x86_64-w64-mingw32.shared-qmake-qt5
if [ "$?" != "0" ] ; then  echo "Cannot run QMake for gix-common" ; exit 1 ;fi

make -j 4 MXE_TARGETS=x86_64-w64-mingw32.shared MXE_PLUGIN_DIRS=plugins/gcc8
if [ "$?" != "0" ] ; then  echo "Cannot build gix-common" ; exit 1 ;fi

# gix-ide
cd $WORKSPACE/gix-ide

x86_64-w64-mingw32.shared-qmake-qt5
if [ "$?" != "0" ] ; then  echo "Cannot run QMake for gix-ide" ; exit 1 ;fi

make -j 4 MXE_TARGETS=x86_64-w64-mingw32.shared MXE_PLUGIN_DIRS=plugins/gcc8
if [ "$?" != "0" ] ; then  echo "Cannot build gix-ide" ; exit 1 ;fi

# build dist archive

export DIST_LEAF=gix-ide-$DIST_VERSION
export DIST_DIR=$DIST_BASE_DIR/$DIST_LEAF

if [ -d "$DIST_DIR" ] ; then rm -fr $DIST_DIR ; fi

cd $WORKSPACE
mkdir -p $DIST_DIR/bin
mkdir -p $DIST_DIR/compilers/defs
mkdir -p $DIST_DIR/examples

cp $BUILD_DIR/gix-common.dll $DIST_DIR/bin
cp $BUILD_DIR/libgixsql.dll $DIST_DIR/bin
cp $BUILD_DIR/libgixsql-odbc.dll $DIST_DIR/bin
cp $BUILD_DIR/libgixsql-mysql.dll $DIST_DIR/bin
cp $BUILD_DIR/libgixsql-pgsql.dll $DIST_DIR/bin
cp $BUILD_DIR/gix-ide.exe $DIST_DIR/bin
cp $WORKSPACE/libs/scintilla/src/scintilla/bin/ScintillaEdit4.dll $DIST_DIR/bin

for DLL in \
	libcrypto-1_1-x64.dll \
	libfreetype-6.dll \
	libgcc_s_seh-1.dll \
	libglib-2.0-0.dll \
	libharfbuzz-0.dll \
	libiconv-2.dll \
	libintl-8.dll \
	libpcre-1.dll \
	libpcre2-16-0.dll \
	libpng16-16.dll \
	libssl-1_1-x64.dll \
	libstdc++-6.dll \
	libwinpthread-1.dll \
	zlib1.dll \
	libbz2.dll \
	libzstd.dll ; do

	cp $MXE_LIBS/$DLL $DIST_DIR/bin
done

for DLL in \
	Qt5Core.dll \
	Qt5Gui.dll \
	Qt5Network.dll \
	Qt5Widgets.dll \
	Qt5Xml.dll ; do

	cp $QT5_BIN/$DLL $DIST_DIR/bin
done

for PLUGIN_DIR in \
	bearer \
	iconengines \
	imageformats \
	platforms \
	styles \
	translations ; do

	cp -fr $QT5_PLUGINS/$PLUGIN_DIR $DIST_DIR/bin
done

cp -fr $WORKSPACE/deploy/compiler-pkgs/windows/defs/* $DIST_DIR/compilers/defs
if [ "$?" != "0" ] ; then  echo "Cannot locate compiler definitions" ; exit 1 ;fi

cp -fr $WORKSPACE/deploy/compiler-pkgs/windows/compilers/* $DIST_DIR/compilers/
if [ "$?" != "0" ] ; then  echo "Cannot locate compiler binaries" ; exit 1 ;fi

# portable version
cd $DIST_BASE_DIR > /dev/nul 2>&1
zip -r $DIST_LEAF.zip $DIST_LEAF
mv $DIST_LEAF.zip $FINAL_OUTPUT_DIR

# installer

cd $DIST_DIR
7z a -r $QIF_BASE_DIR/packages/info.mediumgray.gix-ide/data/gix-ide-$DIST_VERSION.7z *

cd $QIF_BASE_DIR
x86_64-pc-linux-gnu-binarycreator -t "$MXE_HOME/usr/x86_64-w64-mingw32.static/qt5/bin/installerbase.exe" -c config/config.xml -p packages $FINAL_OUTPUT_DIR/gix-ide-$DIST_VERSION-installer.exe 


#-----
#rm $WORKSPACE -fr
