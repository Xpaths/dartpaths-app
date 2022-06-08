#!/bin/bash
QSARTOOLBOX_VERSION=22

THIS_SCRIPT=$(readlink -f "${BASH_SOURCE[0]}")
THIS_WORKING_DIR=$(dirname "$THIS_SCRIPT")
export QSARTOOLBOX_INIT_SH=$THIS_WORKING_DIR/init_qsardb.sh

# storage and log dirs for postgres
export QSARTOOLBOX_PGSQL_DIR=/opt/qsartoolboxdb/qsartoolbox_$QSARTOOLBOX_VERSION
export QSARTOOLBOX_LOGDIR=/opt/qsartoolboxdb/logs

# directory with .backup file(s)
# in this example we assume it is in ETL/data_manual_download/qsartoolbox
export BACKUPS_DIR=$THIS_WORKING_DIR/../../data_manual_download/qsartoolbox

# script assumes that backup files are already extracted from zip files
# original zip files are stored in https://cloud.openanalytics.eu/f/20208
case $QSARTOOLBOX_VERSION in
  20)
    export QSARTOOLBOX_DB_NAME=toolboxv20
    export QSARTOOLBOX_DUMP=$BACKUPS_DIR/Toolboxv4v20.backup
    #unzip -p $BACKUPS_DIR/QSARToolbox43Feb2019.R4.zip QSARToolbox43Feb2019/Database/Toolboxv4v20.backup > $QSARTOOLBOX_DUMP
    ;;

  21)
    export QSARTOOLBOX_DB_NAME=Toolboxv21R1
    export QSARTOOLBOX_DUMP=$BACKUPS_DIR/Toolboxv4v21.backup
    #unzip -p $BACKUPS_DIR/QSARToolbox4.4.1April2020.R4.zip QSARToolbox4.4.1April2020/Database/Toolboxv4v21.backup > $QSARTOOLBOX_DUMP
    ;;

  22)
    export QSARTOOLBOX_DB_NAME=toolboxv22
    export QSARTOOLBOX_DUMP=$BACKUPS_DIR/Toolboxv4v22.backup
    #unzip -p $BACKUPS_DIR/QSARToolbox45March2022.R5.zip QSARToolbox45March2022/Database/Toolboxv4v22.backup > $QSARTOOLBOX_DUMP
    ;;

esac 
export QSARTOOLBOX_DUMP_BASENAME=$(basename $QSARTOOLBOX_DUMP)

mkdir -p $QSARTOOLBOX_PGSQL_DIR
chmod a+rwx $QSARTOOLBOX_PGSQL_DIR
mkdir -p $QSARTOOLBOX_LOGDIR
chmod a+rwx $QSARTOOLBOX_LOGDIR

cd $THIS_WORKING_DIR
docker-compose up
