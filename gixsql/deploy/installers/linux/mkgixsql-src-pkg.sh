#!/bin/bash


PKGFILE=gixsql-$GIXSQLMAJ.$GIXSQLMIN.$GIXSQLREL-$GIX_REVISION.tar.gz

autoreconf --install --force
if [ "$?" != "0" ] ; then echo "Cannot preconfigure the build environment" ; exit 1 ; fi

./configure --prefix=$PKGDEBDIR/${INSTALL_PREFIX}
if [ "$?" != "0" ] ; then echo "Cannot configure the build environment" ; exit 1 ; fi

make distcheck
if [ "$?" != "0" ] ; then echo "Build failed" ; exit 1 ; fi

BUILD_PKG_NAME=$(find . -name "gixsql-*.tar.gz")
if [ "$BUILD_PKG_NAME" == "" ] ; then echo "Cannot find the build package" ; exit 1 ; fi

mv $BUILD_PKG_NAME $PKGFILE
if [ "$?" != "0" ] ; then echo "Cannot rename the build package" ; exit 1 ; fi

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