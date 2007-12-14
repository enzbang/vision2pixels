#!/bin/sh

DB=$1

if [ -z $DB ]
then
    echo "usage: $0 database"
    exit 1
fi

LOGIN=
PASSWORD=
EMAIL=
ADMIN=

while [ -z $LOGIN ]
do
    echo -n "login : "
    read LOGIN
done

while [ -z $PASSWORD ]
do
    echo -n "password : "
    read PASSWORD
done

while [ -z $EMAIL ]
do
    echo -n "email : "
    read EMAIL
done

while [[ "$ADMIN" != "Y"
        && "$ADMIN" != "y"
        && "$ADMIN" != "n"
        && "$ADMIN" != "N" ]]
do
    echo -n "is admin ? [y/n] "
    read ADMIN
done


echo "insert into user (login, password, email, admin) \
values ('$LOGIN', '$PASSWORD', '$EMAIL', '$ADMIN');" | sqlite3 $DB

