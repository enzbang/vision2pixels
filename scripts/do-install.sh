#!/bin/sh

DIST=$(pwd)/dist.tgz

function ask_gwiad_root () {
    echo -n "Where is Gwiad Root ? "
    read GWIAD_ROOT
}


if [ -z $GWIAD_ROOT ]
then
    ask_gwiad_root;
else
    echo -n "Accept Gwiad Root $GWIAD_ROOT ? [Y/n] "
    read choice

    if [[ "$choice" = "n" || "$choice" = "N" ]]
    then
        ask_gwiad_root;
    fi
fi

if [ -z $GWIAD_ROOT ]
then
    echo "GWIAD_ROOT is empty ! abort"
    exit 1
fi

echo "Installing plugin in $GWIAD_ROOT"

cd $GWIAD_ROOT
tar --extract --verbose --backup --file $DIST

echo
echo "Done ! You should run argwiadctl restart (or reload)"

