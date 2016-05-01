FROM postgres

ENV POSTGRES_PASSWORD=welcome1 \
    DB_PORT=5432

ADD initdb.sh /docker-entrypoint-initdb.d/initdb.sh
ADD init.sql /sql/init.sql

EXPOSE $DB_PORT

