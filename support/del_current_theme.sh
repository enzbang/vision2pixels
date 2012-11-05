#!/bin/sh

cp v2p.db "v2p.db.$(date)"

#  remove all photos in the thème

echo "delete from themes_photos where theme_id=(select id from themes where stage=0);" | sqlite3 v2p.db

#  remove the theme itself

echo "delete from themes where stage=0;" | sqlite3 v2p.db
