#!/bin/bash

# we expect the following environment varaiables to be correctly set:
# WORKSPACE DIST HOST_PLATFORM GIXIDEMAJ GIXIDEMIN GIXIDEREL GIX_REVISION

echo "************ Packaging Gix-IDE $GIXIDEMAJ.$GIXIDEMIN.$GIXIDEREL-$GIX_REVISION"
echo "WORKSPACE    : $WORKSPACE"
echo "HOST_PLATFORM: $HOST_PLATFORM"
echo "DIST         : $DIST"
echo "GIXIDEMAJ    : $GIXIDEMAJ"
echo "GIXIDEMIN    : $GIXIDEMIN"
echo "GIXIDEREL    : $GIXIDEREL"
echo "GIX_REVISION : $GIX_REVISION"

if [ "$WORKSPACE" == "" ] || [ "$HOST_PLATFORM" == "" ] || [ "$DIST" == "" ] || [ "$GIXIDEMAJ" == "" ] || [ "$GIXIDEMIN" == "" ] || [ "$GIXIDEREL" == "" ] || [ "$GIX_REVISION" == "" ] ; then
	echo "ERROR: environment not set"
	exit 1
fi

if [ ! -f "$WORKSPACE/deploy/installers/linux/control-${DIST}.tpl" ] ; then
	echo "ERROR: invalid distribution id (DIST: $DIST)"
	exit 1
fi

export PKGDEBDIR=$WORKSPACE/pkg
export PKGNAME=gix-ide-${DIST}-${HOST_PLATFORM}-$GIXIDEMAJ.$GIXIDEMIN.$GIXIDEREL-$GIX_REVISION
export PKGFILE=$PKGNAME.deb

rm -fr $PKGDEBDIR

mkdir -p $PKGDEBDIR && \
mkdir -p $PKGDEBDIR/usr/bin && \
mkdir -p $PKGDEBDIR/usr/lib/gix-ide && \
mkdir -p $PKGDEBDIR/usr/lib/gixsql/lib/${HOST_PLATFORM}/gcc && \
mkdir -p $PKGDEBDIR/usr/share/gixsql/copy && \
mkdir -p $PKGDEBDIR/usr/share/gix-ide/examples && \
mkdir -p $PKGDEBDIR/usr/share/gixsql/examples && \
mkdir -p $PKGDEBDIR/usr/share/doc/gix-ide && \
mkdir -p $PKGDEBDIR/usr/share/doc/gixsql && \
mkdir -p $PKGDEBDIR/usr/share/applications && \
mkdir -p $PKGDEBDIR/usr/share/icons/hicolor/32x32/apps && \
mkdir -p $PKGDEBDIR/usr/share/icons/hicolor/64x64/apps && \
mkdir -p $PKGDEBDIR/usr/share/icons/hicolor/128x128/apps && \
mkdir -p $PKGDEBDIR/usr/share/icons/hicolor/256x256/apps && \
mkdir -p $PKGDEBDIR/DEBIAN

if [ "$?" -ne "0" ] ; then
	echo "Error while creating package structure"
    exit 1
fi

cp -frv $(find ${HOST_PLATFORM}/Release -type f) $PKGDEBDIR/usr/bin
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE binaries (files)"
    exit 1
fi

#cp -frv $(find ${HOST_PLATFORM}/Release -type l) $PKGDEBDIR/usr/bin
#if [ "$?" -ne "0" ] ; then
#	echo "Error while packaging Gix-IDE binaries (symbolic links)"
#    exit 1
#fi

cp -frv $(find ${HOST_PLATFORM}/Release -type f -name "lib*") $PKGDEBDIR/usr/lib/gix-ide
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE/GixSQL local libraries (files)"
    exit 1
fi  

cp -frv $(find ${HOST_PLATFORM}/Release -type l -name "lib*") $PKGDEBDIR/usr/lib/gix-ide
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE/GixSQL local libraries (links)"
    exit 1
fi

cp -frv $(find ${HOST_PLATFORM}/Release -type f -name "libgixsql*") $PKGDEBDIR/usr/lib/gixsql/lib/${HOST_PLATFORM}/gcc
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL runtime libraries (files)"
    exit 1
fi

cp -frv $(find ${HOST_PLATFORM}/Release -type l -name "libgixsql*") $PKGDEBDIR/usr/lib/gixsql/lib/${HOST_PLATFORM}/gcc
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL runtime ibraries (links)"
    exit 1
fi

cp -fv gixsql/copy/SQLCA.cpy $PKGDEBDIR/usr/share/gixsql/copy
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL (COPY files)"
    exit 1
fi

cp -frv $WORKSPACE/deploy/examples/* $PKGDEBDIR/usr/share/gix-ide/examples
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE examples"
    exit 1
fi

cp -frv $WORKSPACE/deploy/examples/* $PKGDEBDIR/usr/share/gixsql/examples
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL examples"
    exit 1
fi

cp -frv $WORKSPACE/doc/* $PKGDEBDIR/usr/share/doc/gix-ide
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE examples"
    exit 1
fi

cp -frv $WORKSPACE/doc/* $PKGDEBDIR/usr/share/doc/gixsql
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL examples"
    exit 1
fi

patchelf --set-rpath /usr/lib/gix-ide:$ORIGIN $PKGDEBDIR/usr/bin/gix-ide
if [ "$?" -ne "0" ] ; then
	echo "Error while patching gix-ide"
    exit 1
fi

patchelf --set-rpath /usr/lib/gix-ide:$ORIGIN $PKGDEBDIR/usr/bin/gix-http
if [ "$?" -ne "0" ] ; then
	echo "Error while patching gix-http"
    exit 1
fi

patchelf --set-rpath /usr/lib/gix-ide:$ORIGIN $PKGDEBDIR/usr/bin/gix-debugger
if [ "$?" -ne "0" ] ; then
	echo "Error while patching gix-debugger"
    exit 1
fi

patchelf --set-rpath /usr/lib/gix-ide:$ORIGIN $PKGDEBDIR/usr/bin/gixpp
if [ "$?" -ne "0" ] ; then
	echo "Error while patching gixpp"
    exit 1
fi

#patchelf --set-rpath /usr/lib/gix-ide:$ORIGIN $PKGDEBDIR/usr/bin/gixdbgr
#if [ "$?" -ne "0" ] ; then
#	echo "Error while packaging GixSQL examples"
#    exit 1
#fi

# md5sums
cd $PKGDEBDIR
find $PKGDEBDIR -type f -exec md5sum '{}' \; > ./DEBIAN/md5sums


cd $WORKSPACE

# debian-binary
echo "2.0" > $PKGDEBDIR/DEBIAN/debian-binary

# control.tar.gz
cat $WORKSPACE/deploy/installers/linux/control-${DIST}.tpl | \
	sed "s/#GIXIDEMAJ#/$GIXIDEMAJ/g" | \
	sed "s/#GIXIDEMIN#/$GIXIDEMIN/g" | \
	sed "s/#GIXIDEREL#/$GIXIDEREL/g" | \
	sed "s/#GIX_REVISION#/$GIX_REVISION/g" > $PKGDEBDIR/DEBIAN/control

# postinst file
cat <<EOF > $PKGDEBDIR/DEBIAN/postinst
#!/bin/bash

if [ -f "/home/\$SUDO_USER/.config/MediumGray/gix-ide.conf" ] ; then
	exit 0
fi;
	
mkdir -p /home/\$SUDO_USER/.config/MediumGray
touch /home/\$SUDO_USER/.config/MediumGray/gix-ide.conf
echo "[General]" >> /home/\$SUDO_USER/.config/MediumGray/gix-ide.conf
echo "screen_resolution=72" >> /home/\$SUDO_USER/.config/MediumGray/gix-ide.conf

chown -R \$SUDO_USER:\$SUDO_USER /home/\$SUDO_USER/.config/MediumGray/
chmod -R 775 /home/\$SUDO_USER/.config/MediumGray/
chmod 664 /home/\$SUDO_USER/.config/MediumGray/gix-ide.conf
EOF

cd $WORKSPACE
find $PKGDEBDIR -type d | xargs chmod 755 
chmod 755 $PKGDEBDIR/DEBIAN/postinst
# postinst file (end)

# .desktop file
cp -v $WORKSPACE/deploy/installers/linux/gix-ide.desktop $PKGDEBDIR/usr/share/applications/

# build icons
convert $WORKSPACE/misc/main_icon.png -resize 32x32 $PKGDEBDIR/usr/share/icons/hicolor/32x32/apps/gix-ide.png
convert $WORKSPACE/misc/main_icon.png -resize 64x64 $PKGDEBDIR/usr/share/icons/hicolor/64x64/apps/gix-ide.png
convert $WORKSPACE/misc/main_icon.png -resize 128x128 $PKGDEBDIR/usr/share/icons/hicolor/128x128/apps/gix-ide.png
convert $WORKSPACE/misc/main_icon.png -resize 256x256 $PKGDEBDIR/usr/share/icons/hicolor/256x256/apps/gix-ide.png

# build .deb package
dpkg-deb --build $PKGDEBDIR tmp.deb
if [ "$?" -ne "0" ] ; then
	echo "Error while building .deb package"
    exit 1
fi

mv -f tmp.deb $PKGFILE

#gpg --pinentry-mode loopback --batch --quiet --yes -b --passphrase-file $HOME/m.ridoni.keypwd -o $PKGFILE.sig $PKGFILE
#if [ "$?" -ne "0" ] ; then
#	echo "Error while signing package"
#    exit 1
#fi

#mv $PKGFILE $PKGFILE.sig $DOWNLOAD_DIR
#if [ "$?" -ne "0" ] ; then
#	echo "Error while moving archive"
#    exit 1
#fi

#echo "Done, package is $PKGFILE, signature is $PKGFILE.sig"
echo "Done, package is $PKGFILE"


