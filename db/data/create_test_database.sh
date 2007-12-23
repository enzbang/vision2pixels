#!/bin/bash

DATABASE_NAME="testing.db"

SCHEMA_DB="schema-sqlite.sql"
INITIAL_DATA="initial-test-data.sql"
VOTE_PONDERATED="vote_ponderated.sql"
DO_VOTE="vote.sql"

SQLITE=sqlite3

rm -f ${DATABASE_NAME}

${SQLITE} ${DATABASE_NAME} < ${SCHEMA_DB}
${SQLITE} ${DATABASE_NAME} < ${INITIAL_DATA}
${SQLITE} ${DATABASE_NAME} < ${VOTE_PONDERATED}
${SQLITE} ${DATABASE_NAME} < ${DO_VOTE}