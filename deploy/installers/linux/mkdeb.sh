#!/bin/bash

echo "************ Packaging Gix-IDE $GIXIDEMAJ.$GIXIDEMIN.$GIXIDEREL-$GIX_REVISION"

cd $WORKSPACE_LINUX

export WORKSPACE=$WORKSPACE_LINUX

export PKGDEBDIR=$WORKSPACE/pkg
export PKGNAME=gix-ide-linux-x64-$GIXIDEMAJ.$GIXIDEMIN.$GIXIDEREL-$GIX_REVISION
export PKGFILE=$PKGNAME.deb

rm -fr $PKGDEBDIR

mkdir -p $PKGDEBDIR && \
mkdir -p $PKGDEBDIR/opt/gix-ide/bin && \
mkdir -p $PKGDEBDIR/opt/gix-ide/lib/copy && \
mkdir -p $PKGDEBDIR/opt/gix-ide/lib/x64/gcc && \
mkdir -p $PKGDEBDIR/opt/gix-examples && \
mkdir -p $PKGDEBDIR/opt/gix-doc && \
mkdir -p $PKGDEBDIR/DEBIAN

if [ "$?" -ne "0" ] ; then
	echo "Error while creating package structure"
    exit 1
fi  

cp -frv $(find x64/Release -type f) $PKGDEBDIR/opt/gix-ide/bin 
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE binaries (files)"
    exit 1
fi  

cp -frv $(find x64/Release -type l) $PKGDEBDIR/opt/gix-ide/bin 
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE binaries (symbolic links)"
    exit 1
fi  

cp -frv $(find gixsql/Release -type f -name "libgixsql*") $PKGDEBDIR/opt/gix-ide/bin
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL libraries (files)"
    exit 1
fi  

cp -frv $(find gixsql/Release -type l -name "libgixsql*") $PKGDEBDIR/opt/gix-ide/bin
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL libraries (links)"
    exit 1
fi  

cp -frv $(find gixsql/Release -type f -name "libgixsql*") $PKGDEBDIR/opt/gix-ide/lib/x64/gcc
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL runtime libraries (files)"
    exit 1
fi  

cp -frv $(find gixsql/Release -type l -name "libgixsql*") $PKGDEBDIR/opt/gix-ide/lib/x64/gcc
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL runtime ibraries (links)"
    exit 1
fi  

cp -fv gixsql/copy/SQLCA.cpy $PKGDEBDIR/opt/gix-ide/lib/copy
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging GixSQL (COPY files)"
    exit 1
fi  

cp -frv $WORKSPACE/deploy/examples/* $PKGDEBDIR/opt/gix-examples
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDE examples"
    exit 1
fi  

cp -frv $WORKSPACE/doc/* $PKGDEBDIR/opt/gix-doc
if [ "$?" -ne "0" ] ; then
	echo "Error while packaging Gix-IDe examples"
    exit 1
fi  

# md5sums
cd $PKGDEBDIR
find ./opt -type f -exec md5sum '{}' \; > ./DEBIAN/md5sums


cd $WORKSPACE

# debian-binary
echo "2.0" > $PKGDEBDIR/DEBIAN/debian-binary

# control.tar.gz
cat $WORKSPACE/deploy/installers/linux/control.tpl | \
	sed "s/#GIXIDEMAJ#/$GIXIDEMAJ/g" | \
	sed "s/#GIXIDEMIN#/$GIXIDEMIN/g" | \
	sed "s/#GIXIDEREL#/$GIXIDEREL/g" | \
	sed "s/#GIX_REVISION#/$GIX_REVISION/g" > $PKGDEBDIR/DEBIAN/control
	
cat <<EOF > $PKGDEBDIR/DEBIAN/postinst
#!/bin/bash

mkdir -p /home/\$SUDO_USER/Documents/gix/examples
mv /opt/gix-examples/* /home/\$SUDO_USER/Documents/gix/examples
rm -fr /opt/gix-examples

mkdir -p /home/\$SUDO_USER/Documents/gix/doc
mv /opt/gix-doc/* /home/\$SUDO_USER/Documents/gix/doc
rm -fr /opt/gix-doc
chown -R \$SUDO_USER:\$SUDO_USER /home/\$SUDO_USER/Documents/gix/
chmod -R 775 /home/\$SUDO_USER/Documents/gix/
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


