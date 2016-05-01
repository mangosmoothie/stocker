#!/bin/bash
set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
    drop database if exists stocker;
    drop role if exists stocker;
    CREATE USER stocker createdb createuser password 'welcome1';
    CREATE database stocker owner stocker;
EOSQL

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" -a -f /sql/init.sql -d stocker
