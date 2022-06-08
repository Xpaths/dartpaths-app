#!/bin/bash
set -e

# restore database if not yet available
export PGPASSWORD=$POSTGRESQL_PASSWORD

echo "Restoring database $QSARTOOLBOX_DB_NAME"
psql -v --username postgres <<-EOSQL
      CREATE DATABASE $QSARTOOLBOX_DB_NAME;
      GRANT ALL PRIVILEGES ON DATABASE $QSARTOOLBOX_DB_NAME TO postgres;
      CREATE ROLE qsartoolbox;
      GRANT ALL PRIVILEGES ON DATABASE $QSARTOOLBOX_DB_NAME TO qsartoolbox;
EOSQL
pg_restore -p 5432 -U postgres --dbname $QSARTOOLBOX_DB_NAME --no-owner --role qsartoolbox /tmp/data_raw/$QSARTOOLBOX_DUMP_BASENAME || echo "Restored $QSARTOOLBOX_DB_NAME"
