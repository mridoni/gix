#!/bin/bash

WORKSPACE=/tmp/gix_build_$(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 8 ; echo '')
if [ ! -d "$WORKSPACE" ] ; then mkdir $WORKSPACE ; fi

cd $WORKSPACE
rm .svn -fr
#------
export CONFIG=Release

export OSXCROSS_VERSION=1.2
export OSXCROSS_OSX_VERSION_MIN=10.9
export OSXCROSS_TARGET=darwin19
export OSXCROSS_BASE_DIR=/opt/osxcross/build/..
export OSXCROSS_SDK=/opt/osxcross/target/bin/../SDK/MacOSX10.15.sdk
export OSXCROSS_SDK_DIR=/opt/osxcross/target/bin/../SDK/MacOSX10.15.sdk/..
export OSXCROSS_SDK_VERSION=10.15
export OSXCROSS_TARBALL_DIR=/opt/osxcross/build/../tarballs
export OSXCROSS_PATCH_DIR=/opt/osxcross/build/../patches
export OSXCROSS_TARGET_DIR=/opt/osxcross/target/bin/..
export OSXCROSS_DIR_SDK_TOOLS=/opt/osxcross/target/bin/../SDK/MacOSX10.15.sdk/../tools
export OSXCROSS_BUILD_DIR=/opt/osxcross/build
export OSXCROSS_CCTOOLS_PATH=/opt/osxcross/target/bin
export OSXCROSS_LIBLTO_PATH=
export OSXCROSS_LINKER_VERSION=512.4

export QT5_BASE=/opt/osxcross/Qt
export QT5_VERSION=5.14.2
export QT5_BIN=$QT5_BASE/$QT_VERSION/clang_64/bin
export QT5_LIB=$QT5_BASE/$QT5_VERSION/clang_64/lib
export QT5_PLUGINS=$QT5_BASE/$QT5_VERSION/clang_64/plugins
export QIF_BASE_DIR=

export BUILD_DIR=$WORKSPACE/x64/$CONFIG
export FINAL_OUTPUT_DIR=/var/www/gix-ide/download
export DIST_BASE_DIR=$WORKSPACE/build_tmp
export DIST_VERSION=1.0.0

svn co file:///opt/svn/public/gix-ide/trunk .
if [ "$?" != "0" ] ; then
	echo "Cannot checkout from file:///opt/svn/public/gix-ide/trunk to $WORKSPACE/gix-ide"
	exit 1
fi

export PATH=$PATH:$OSXCROSS_CCTOOLS_PATH:$QT5_BIN

mkdir -p ${BUILD_DIR}

# scintilla
cd $WORKSPACE/libs/scintilla/src
unzip scintilla432.zip
if [ "$?" != "0" ] ; then  echo "Cannot expand Scintilla" ; exit 1 ;fi

cp Makefile.macOS scintilla/qt/ScintillaEdit

cd scintilla/qt/ScintillaEdit
python WidgetGen.py
if [ "$?" != "0" ] ; then  echo "Cannot generate Scintilla Qt headers" ; exit 1 ;fi

make -j 4 -f Makefile.macOS
if [ "$?" != "0" ] ; then  echo "Cannot build Scintilla" ; exit 1 ;fi

# gixsql components
cd $WORKSPACE/gixsql

DESTDIR=${BUILD_DIR} make -j4 -f Makefile.macOS
if [ "$?" != "0" ] ; then  echo "Cannot build GixSQL components" ; exit 1 ;fi

# gix-common
cd $WORKSPACE/gix-common

DESTDIR=${BUILD_DIR} make -j 4 -f Makefile.macOS
if [ "$?" != "0" ] ; then  echo "Cannot build gix-common" ; exit 1 ;fi

# gix-ide
cd $WORKSPACE/gix-ide

DESTDIR=${BUILD_DIR} make -j 4 -f Makefile.macOS
if [ "$?" != "0" ] ; then  echo "Cannot build gix-ide" ; exit 1 ;fi

# build dist archive
if [ -d "$DIST_DIR" ] ; then rm -fr $DIST_DIR ; fi

export DIST_LEAF=gix-ide-$DIST_VERSION
export DIST_DIR=$DIST_BASE_DIR/$DIST_LEAF

mkdir -p $DIST_DIR
cd $DIST_DIR

mkdir -p gix-ide.app

mkdir -p gix-ide.app/Contents

mkdir -p gix-ide.app/Contents/MacOS
cp ${BUILD_DIR}/gix-ide gix-ide.app/Contents/MacOS

mkdir -p gix-ide.app/Contents/Resources
touch gix-ide.app/Contents/Resources/empty.lproj

mkdir -p gix-ide.app/Contents/Frameworks

cp ${BUILD_DIR}/libgix-common.dylib gix-ide.app/Contents/Frameworks
x86_64-apple-darwin19-install_name_tool -change libgix-common.dylib "@executable_path/../Frameworks/libgix-common.dylib" ./gix-ide.app/Contents/MacOS/gix-ide

cp ${BUILD_DIR}/libgixsql.dylib gix-ide.app/Contents/Frameworks
x86_64-apple-darwin19-install_name_tool -change libgixsql.dylib "@executable_path/../Frameworks/libgixsql.dylib" ./gix-ide.app/Contents/MacOS/gix-ide

cp ${BUILD_DIR}/libgixsql-odbc.dylib gix-ide.app/Contents/Frameworks
x86_64-apple-darwin19-install_name_tool -change libgixsql-odbc.dylib "@executable_path/../Frameworks/libgixsql-odbc.dylib" ./gix-ide.app/Contents/MacOS/gix-ide

cp ${BUILD_DIR}/libgixsql-mysql.dylib gix-ide.app/Contents/Frameworks
x86_64-apple-darwin19-install_name_tool -change libgixsql-mysql.dylib "@executable_path/../Frameworks/libgixsql-mysql.dylib" ./gix-ide.app/Contents/MacOS/gix-ide

cp ${BUILD_DIR}/libgixsql-pgsql.dylib gix-ide.app/Contents/Frameworks
x86_64-apple-darwin19-install_name_tool -change libgixsql-pgsql.dylib "@executable_path/../Frameworks/libgixsql-pgsql.dylib" ./gix-ide.app/Contents/MacOS/gix-ide

cp $WORKSPACE/libs/scintilla/src/scintilla/qt/ScintillaEdit/libScintillaEdit.dylib gix-ide.app/Contents/Frameworks
x86_64-apple-darwin19-install_name_tool -change libScintillaEdit.dylib "@executable_path/../Frameworks/libScintillaEdit.dylib" ./gix-ide.app/Contents/MacOS/gix-ide

for QTFW in QtGui QtWidgets QtNetwork QtCore QtXml QtXmlPatterns QtPrintSupport QtDBus QtSvg; do
	mkdir -p gix-ide.app/Contents/Frameworks/$QTFW.framework
	mkdir -p gix-ide.app/Contents/Frameworks/$QTFW.framework/Versions/5
	mkdir -p gix-ide.app/Contents/Frameworks/$QTFW.framework/Versions/5/Resources
	
	cp -fravp $QT5_LIB/$QTFW.framework/Versions/5/$QTFW gix-ide.app/Contents/Frameworks/$QTFW.framework/Versions/5/
	cp -fravp $QT5_LIB/$QTFW.framework/Versions/5/Resources/* gix-ide.app/Contents/Frameworks/$QTFW.framework/Versions/5/Resources/
	
	ln -s gix-ide.app/Contents/Frameworks/$QTFW.framework/Versions/5 gix-ide.app/Contents/Frameworks/$QTFW.framework/Versions/Current
	
	ln -s gix-ide.app/Contents/Frameworks/$QTFW.framework/Versions/Current/$QTFW gix-ide.app/Contents/Frameworks/$QTFW.framework/$QTFW
	ln -s gix-ide.app/Contents/Frameworks/$QTFW.framework/Versions/Current/Resources gix-ide.app/Contents/Frameworks/$QTFW.framework/Resources
done	

mkdir -p gix-ide.app/Contents/PlugIns
mkdir -p gix-ide.app/Contents/PlugIns/platforms
mkdir -p gix-ide.app/Contents/PlugIns/printsupport
mkdir -p gix-ide.app/Contents/PlugIns/styles
mkdir -p gix-ide.app/Contents/PlugIns/bearer
mkdir -p gix-ide.app/Contents/PlugIns/iconengines
mkdir -p gix-ide.app/Contents/PlugIns/imageformats

cp $QT5_PLUGINS/platforms/libqcocoa.dylib gix-ide.app/Contents/PlugIns/platforms

cp $QT5_PLUGINS/printsupport/libcocoaprintersupport.dylib gix-ide.app/Contents/PlugIns/printsupport

cp $QT5_PLUGINS/styles/libqmacstyle.dylib gix-ide.app/Contents/PlugIns/styles

cp $QT5_PLUGINS/bearer/libqgenericbearer.dylib gix-ide.app/Contents/PlugIns/bearer

cp $QT5_PLUGINS/iconengines/libqsvgicon.dylib gix-ide.app/Contents/PlugIns/iconengines

cp $QT5_PLUGINS/imageformats/libqgif.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqwbmp.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqwebp.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqico.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqmacheif.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqjpeg.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqtiff.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqsvg.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqicns.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqtga.dylib gix-ide.app/Contents/PlugIns/imageformats
cp $QT5_PLUGINS/imageformats/libqmacjp2.dylib gix-ide.app/Contents/PlugIns/imageformats

cat << EOF > gix-ide.app/Resources/qt.conf
[Paths]
Plugins = PlugIns
Imports = Resources/qml
Qml2Imports = Resources/qml
EOF

cat << EOF > gix-ide.app/Contents/Info.plist
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleExecutable</key>
	<string>gix-ide</string>
	<key>CFBundleIconFile</key>
	<string></string>
	<key>CFBundleIdentifier</key>
	<string>info.mediumgray.gix-ide</string>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
	<key>CFBundleSignature</key>
	<string>????</string>
	<key>LSMinimumSystemVersion</key>
	<string>10.13</string>
	<key>NOTE</key>
	<string></string>
	<key>NSPrincipalClass</key>
	<string>NSApplication</string>
	<key>NSSupportsAutomaticGraphicsSwitching</key>
	<true/>
</dict>
</plist>
EOF

# adding: gix-ide.app/Contents/PkgInfo
echo -e "APPL???" > gix-ide.app/Contents/PkgInfo

# DMG version
cd $DIST_DIR
genisoimage -D -V "Gix-IDE $DIST_VERSION" -no-pad -r -apple -o $FINAL_OUTPUT_DIR/$DIST_LEAF-macOS.dmg .

# zip version
cd $DIST_BASE_DIR
zip -r $DIST_LEAF-macOS.zip $DIST_LEAF
mv $DIST_LEAF-macOS.zip $FINAL_OUTPUT_DIR/

# installer

# cd $DIST_DIR
# 7z a -r $QIF_BASE_DIR/packages/info.mediumgray.gix-ide/data/gix-ide-$DIST_VERSION.7z *

# cd $QIF_BASE_DIR
# x86_64-pc-linux-gnu-binarycreator -t "$MXE_HOME/usr/x86_64-w64-mingw32.static/qt5/bin/installerbase.exe" -c config/config.xml -p packages $FINAL_OUTPUT_DIR/gix-ide-$DIST_VERSION-installer.exe 


#-----
#rm $WORKSPACE -fr
