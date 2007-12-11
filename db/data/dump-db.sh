#!/bin/sh

DATABASE_PATH=$1

if [[ -z $DATABASE_PATH
            || "$DATABASE_PATH" == "-h"
            || "$DATABASE_PATH" == "-help"
            || "$DATABASE_PATH" == "--help" ]]
then
    echo "usage: $(basename $0) database-path"
    echo 'To restore : zcat database-timestamp.dump.gz | sqlite3 new_database.db'
else
    DB_NAME=$(basename $DATABASE_PATH .db)
    DATE=$(date +"%F_%H%M%S")
    echo '.dump' | sqlite3 $DATABASE_PATH | gzip -c > ${DB_NAME}_$DATE.dump.gz
fi