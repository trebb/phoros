#! /bin/bash

export PHOROS_HOST=localhost
export PHOROS_DATABASE=phoros_test
export PHOROS_AUX_HOST=localhost
export PHOROS_AUX_DATABASE=phoros_test
export PHOROS_COMMON_ROOT=`pwd`/testsuite/
export PHOROS_LOG_DIR=preparation-test.log/

./phoros --version --verbose=libraries:1

./phoros --nuke-all-tables 

./phoros --create-sys-tables

./testsuite/calib-storage.sh

./phoros --create-acquisition-project=cottbus-acq-1 
./phoros --create-acquisition-project=cottbus-acq-2 

./phoros --create-presentation-project=cottbus-presentation 

./phoros --create-user=pub --user-password=pp6 --user-full-name="John Q. Public" --user-role=write --presentation-project=cottbus-presentation 
./phoros --create-user=uu6 --user-password=pp6 --user-full-name="Ulf6 Uuu6" --user-role=admin --presentation-project=cottbus-presentation

# ./phoros --store-user-points=cottbus-presentation --json-file=user-points-cottbus-presentation.json

# ./phoros --create-aux-view=cottbus-presentation \
#     --aux-table= \
#     --coordinates-column=the_geom \
#     --numeric-column=nk_station \
#     --numeric-column= \
#     --numeric-column= \
#     --text-column= \
#     --text-column=vnk \
#     --text-column=nnk \
#     --text-column=strasse \
#     $pg_aux_credentials

./phoros --create-image-attribute=cottbus-presentation --tag="east" --sql-clause="heading BETWEEN 0 AND 180"
./phoros --create-image-attribute=cottbus-presentation --tag="south" --sql-clause="heading BETWEEN 90 AND 270"
./phoros --create-image-attribute=cottbus-presentation --tag="west" --sql-clause="heading BETWEEN 180 AND 360"
./phoros --create-image-attribute=cottbus-presentation --tag="north" --sql-clause="heading BETWEEN 270 AND 360 OR heading BETWEEN 0 AND 90"

./phoros --store-images-and-points=cottbus-acq-1 --directory=`pwd`/testsuite/cottbus_0696/
./phoros --add-to-presentation-project=cottbus-presentation --acquisition-project=cottbus-acq-1 

./phoros --store-images-and-points=cottbus-acq-2 --directory=`pwd`/testsuite/cottbus_0756/
./phoros --add-to-presentation-project=cottbus-presentation --acquisition-project=cottbus-acq-2 


./phoros --server --verbose=no-daemon:1 --verbose=render-footprints:1 --images=7 --http-port=8080 --login-intro="Please use Firefox or one of its relatives." --login-intro="It is recommendable to set your browser cache size to about 2000 MB." --login-intro="Also make sure that you have a decent Internet connection."
