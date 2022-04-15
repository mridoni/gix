#!/bin/bash

INSTALL_QT_RUNTIME=$1

echo "************ Packaging Gix-IDE $GIXIDEMAJ.$GIXIDEMIN.$GIXIDEREL-$GIX_REVISION"

cd $WORKSPACE_LINUX

export WORKSPACE=WORKSPACE_LINUX

# export HOST_PLATFORM=x64
# export QT_LINUX_DIR=/opt/Qt/5.14.2/gcc_64
# export DOWNLOAD_DIR=/mnt/c/inetpub/gix-download

export DISTDIR=gix-ide-linux-x64-$GIXIDEMAJ.$GIXIDEMIN.$GIXIDEREL-$GIX_REVISION
export PKGFILE=$DISTDIR.tar.gz

mkdir -p $DISTDIR && mkdir -p $DISTDIR/bin && mkdir -p $DISTDIR/lib/x64
if [ "$?" -ne "0" ] ; then
	echo "Error while creating package structure"
    exit 1
fi  

cp -frv $(find x64/Release -type f) $DISTDIR/bin
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE binaries (files)"
    exit 1
fi  

cp -frv $(find x64/Release -type l) $DISTDIR/bin
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE binaries (symbolic links)"
    exit 1
fi  

cp -frv $(find x64/Release -type f -name "libgixsql*") $DISTDIR/lib/x64
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL libraries (files)"
    exit 1
fi  

cp -frv $(find x64/Release -type l -name "libgixsql*") $DISTDIR/lib/x64
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL libraries (links)"
    exit 1
fi  

if [ "$INSTALL_QT_RUNTIME" == "NO_QT" ] ; then
	echo "** Using system-provided Qt 5.14.2 runtime"
else
	echo "** Copying Qt 5.14.2 runtime"
	
	for QT_SLIB in \
		libQt5Core \
		libQt5Gui \
		libQt5Network \
		libQt5Widgets \
		libQt5Xml \
		libQt5XmlPatterns ; do
	
		cp -frv $QT_LINUX_DIR/${QT_SLIB}.* $DISTDIR/bin
		if [ "$?" -ne "0" ] ; then
			echo "Error while packaging Qt binaries"
			exit 1
		fi    
	done
	
	for PLUGIN_DIR in \
		bearer \
		iconengines \
		imageformats \
		platforms \
		styles \
		translations ; do
	
		cp -frv $QT_LINUX_DIR/qt5/plugins $DISTDIR/bin
		if [ "$?" -ne "0" ] ; then
			echo "Error while packaging Qt plugins"
			exit 1
		fi       
	done
fi

tar -czvf $PKGFILE $DISTDIR
if [ "$?" -ne "0" ] ; then
	echo "Error while building archive"
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


