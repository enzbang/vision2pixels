#!/bin/bash

DATABASE_NAME="testing.db"

SCHEMA_DB="schema-sqlite.sql"
INITIAL_DATA="initial-data.sql"

SQLITE=sqlite3

rm -f ${DATABASE_NAME}

${SQLITE} ${DATABASE_NAME} < ${SCHEMA_DB}
${SQLITE} ${DATABASE_NAME} < ${INITIAL_DATA}
