#! /bin/sh

GNATLS=`which gnatls`
COMPILER=`dirname $GNATLS`/..
TARGET=v2p-externals/bin
LIBMAGIC=/usr/lib/libMagick.so*

mkdir -p $TARGET
cp $COMPILER/lib/xmlada/libxmlada* $TARGET

if [ -z "$LIBMAGIC" ]; then
   echo libMagick shared library not found. aborting.
   exit 1;
else
   cp $LIBMAGIC $TARGET
fi

tar cfz v2p-externals.tgz $TARGET
rm -fr $TARGET
echo $TARGET build with success.
