#!/bin/sh

DIST=$(pwd)/dist.tgz

function ask_gwiad_root () {
    echo -n "Where is Gwiad Root ? "
    read ARGWIAD_ROOT
}

if [ -z $ARGWIAD_ROOT ]
then
    ask_gwiad_root;
else
    echo -n "Accept Gwiad Root $ARGWIAD_ROOT ? [Y/n] "
    read choice

    if [[ "$choice" = "n" || "$choice" = "N" ]]
    then
        ask_gwiad_root;
    fi
fi

if [ -z $ARGWIAD_ROOT ]
then
    echo "ARGWIAD_ROOT is empty ! abort"
    exit 1
fi

echo "Installing plugin in $ARGWIAD_ROOT"

cd $ARGWIAD_ROOT
tar --extract --verbose --file $DIST

#  If aws.ini exists, remove current settings

if [ -f "aws.ini" ];
then
    grep -iv max_connection aws.ini > aws.ini-tmp
    rm aws.ini
    mv aws.ini-tmp aws.ini
fi

echo "max_connection 30" >> aws.ini

#  3 months for the session lifetime
echo "session_lifetime 7776000" >> aws.ini

echo
echo "Done ! You should run argwiadctl restart (or reload)"
