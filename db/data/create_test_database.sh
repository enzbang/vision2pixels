#!/bin/bash

DATABASE_NAME="testing.db"

SCHEMA_DB="schema-sqlite.sql"
INITIAL_DATA="initial-test-data.sql"
VOTE_PONDERATED="vote_ponderated.sql"
DO_VOTE="vote.sql"

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
    ${SQLITE} ${DATABASE_NAME} < ${VOTE_PONDERATED}
    ${SQLITE} ${DATABASE_NAME} < ${DO_VOTE}
    md5sum *.sql > files.md5
fi;
