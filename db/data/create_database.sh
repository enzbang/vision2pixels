#!/bin/bash

DATABASE_NAME="v2p.db"

SCHEMA_DB="schema-sqlite.sql"
INITIAL_DATA="initial-data.sql"
SQLITE=sqlite3

BUILD=1

if [ -f files.md5 ]; then
    md5sum --status --check files.md5
    BUILD=$?
fi;

if [ $BUILD == 1 -o ! -f $DATABASE_NAME ]; then
    rm -f ${DATABASE_NAME}

    ${SQLITE} ${DATABASE_NAME} < ${SCHEMA_DB}
    ${SQLITE} ${DATABASE_NAME} < ${INITIAL_DATA}
    md5sum *.sql > files.md5
fi;
