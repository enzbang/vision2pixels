#!/bin/sh

if [ "$1" = "" -o "$2" = "" ]; then
	echo old version must be provided;
	echo usage: prepare-install old_ver new_ver;
	exit 1;
fi

OLD=$1
NEW=$2

mkdir .backups/$OLD
cp -pr gwiad lib .backups/$OLD/

tar xfz $HOME/v2p-linux-i686-v$NEW.tgz
