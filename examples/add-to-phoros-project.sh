#! /bin/bash

shopt -s extglob                # make !() available

export PHOROS_HOST=my_db_server
export PHOROS_DATABASE=phoros_workspace
export PHOROS_USER=some_db_user
export PHOROS_PASSWORD=wonttell

export PHOROS_AUX_HOST=my_db_server
export PHOROS_AUX_DATABASE=phoros_aux
export PHOROS_AUX_USER=some_db_user
export PHOROS_AUX_PASSWORD=wonttell

export PHOROS_COMMON_ROOT=/path/to/all/projects/
export PHOROS_LOG_DIR=preparation.log/

cd ../                          # where the phoros binary lives

for i in /path/to/all/projects/one_project/!(already_done_01|already_done_02|already_done_03);
do
    ./phoros --store-images-and-points=this_acquisition_project \
             --directory=$i/

    ./phoros --add-to-presentation-project=my_presentation_project      \
             --acquisition-project=this_acquisition_project
done

./phoros --insert-footprints=this_acquisition_project
