#!/bin/sh

if [ "$1" = "" ]; then
    echo $0 login
    exit 1
fi

cp v2p.db "v2p.db.$(date)"

# remove e-mail and password

echo "update user set password=hex(randomblob(16)), email='nobody@nowhere.com' where login='"$1"';" | sqlite3 v2p.db

echo "update user_page set content='Compte désactivé', content_html='Compte désactivé' where user_login='"$1"';" | sqlite3 v2p.db
