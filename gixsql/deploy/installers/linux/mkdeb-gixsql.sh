#!/bin/bash

echo "************ Packaging GixSQL $GIXSQLMAJ.$GIXSQLMIN.$GIXSQLREL-$GIX_REVISION"

# No leading "/"
export INSTALL_PREFIX=opt/gixsql

cd $WORKSPACE_LINUX

export WORKSPACE=$WORKSPACE_LINUX

export PKGDEBDIR=$WORKSPACE/pkg
export PKGNAME=gixsql-linux-x64-$GIXSQLMAJ.$GIXSQLMIN.$GIXSQLREL-$GIX_REVISION
export PKGFILE=$PKGNAME.deb

mkdir -p $PKGDEBDIR/${INSTALL_PREFIX}/share/gixsql/examples && \
	mkdir -p $PKGDEBDIR/${INSTALL_PREFIX}/share/gixsql/doc && \
	mkdir -p $PKGDEBDIR/DEBIAN
if [ "$?" != "0" ] ; then echo "Cannot create package directory" ; exit 1 ; fi

# ********************************

BUILD_NAME=gixsql-$GIXSQLMAJ.$GIXSQLMIN.$GIXSQLREL-$GIX_REVISION
BUILD_DIR=$WORKSPACE/$BUILD_NAME

#mkdir $BUILD_DIR
#if [ "$?" != "0" ] ; then echo "Cannot create build directory" ; exit 1 ; fi
#
#mv libcpputils libgixutils libgixpp gixpp copy runtime/libgixsql runtime/libgixsql-mysql runtime/libgixsql-odbc runtime/libgixsql-pgsql $BUILD_DIR
#if [ "$?" != "0" ] ; then echo "Cannot setup build directory" ; exit 1 ; fi
#
#cd $BUILD_DIR
#if [ "$?" != "0" ] ; then echo "Cannot switch to build directory" ; exit 1 ; fi
#
#cp -fravp $WORKSPACE/deploy/installers/linux/gixsql-autoconf/* .
#if [ "$?" != "0" ] ; then echo "Cannot copy build scripts" ; exit 1 ; fi
#
#autoreconf --install --force
#if [ "$?" != "0" ] ; then echo "Cannot reconfigure into build directory" ; exit 1 ; fi
#
#cp $WORKSPACE/build-tools/grammar-tools/FlexLexer.h ./libgixpp/
#if [ "$?" != "0" ] ; then echo "Cannot copy lexer support files" ; exit 1 ; fi

#rm -fr $(find . -name ".svn")
#rm -fr $(find . -name "x64")
#rm -fr $(find . -name "x86")
#rm -fr $(find . -name "Makefile.linux")
#rm -fr $(find . -name "Makefile.macOS")
#rm -fr $(find . -name "Makefile.mingw-w64")
#rm -fr $(find . -name "*.sln")
#rm -fr $(find . -name "*.vcxproj")
#rm -fr $(find . -name "*.filters")
#rm -fr $(find . -name "*.user")
#rm -fr $(find . -name "*.pro")
#rm -fr $(find . -name "*.pri")

autoreconf --install --force
if [ "$?" != "0" ] ; then echo "Cannot preconfigure the build environment" ; exit 1 ; fi

./configure --prefix=$PKGDEBDIR/${INSTALL_PREFIX}
if [ "$?" != "0" ] ; then echo "Cannot configure the build environment" ; exit 1 ; fi

make
if [ "$?" != "0" ] ; then echo "Build failed" ; exit 1 ; fi

make install
if [ "$?" != "0" ] ; then echo "Cannot install build" ; exit 1 ; fi

# ********************************

cp -frv $WORKSPACE/gixsql-tests-nunit/data/*.cbl $WORKSPACE/gixsql-tests-nunit/data/*.cpy $WORKSPACE/gixsql-tests-nunit/data/*.sql $PKGDEBDIR/${INSTALL_PREFIX}/share/gixsql/examples
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL examples"; exit 1
fi  

cp -frv $WORKSPACE/README $PKGDEBDIR/${INSTALL_PREFIX}/share/gixsql/doc/
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL docs"; exit 1
fi  

cd $WORKSPACE

# debian-binary
echo "2.0" > $PKGDEBDIR/DEBIAN/debian-binary

# control.tar.gz
cat $WORKSPACE/deploy/installers/linux/control-gixsql.tpl | \
	sed "s/#GIXSQLMAJ#/$GIXSQLMAJ/g" | \
	sed "s/#GIXSQLMIN#/$GIXSQLMIN/g" | \
	sed "s/#GIXSQLREL#/$GIXSQLREL/g" | \
	sed "s/#GIX_REVISION#/$GIX_REVISION/g" > $PKGDEBDIR/DEBIAN/control
	
cat <<EOF > $PKGDEBDIR/DEBIAN/postinst
#!/bin/sh

exit 0
EOF

cd $WORKSPACE
find $PKGDEBDIR -type d | xargs chmod 755 
chmod 755 $PKGDEBDIR/DEBIAN/postinst

dpkg-deb --build $PKGDEBDIR $PKGFILE
if [ "$?" -ne "0" ] ; then
	echo "Error while building .deb package"
    exit 1
fi


gpg --pinentry-mode loopback --batch --quiet --yes -b --passphrase-file $HOME/m.ridoni.keypwd -o $PKGFILE.sig $PKGFILE
if [ "$?" -ne "0" ] ; then
	echo "Error while signing package"
    exit 1
fi

mv $PKGFILE $PKGFILE.sig $DOWNLOAD_DIR
if [ "$?" -ne "0" ] ; then
	echo "Error while moving archive"
    exit 1
fi

echo "Done, package is $PKGFILE, signature is $PKGFILE.sig"

exit 0
