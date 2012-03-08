#! /bin/bash

shopt -s extglob                # make !() available

pg_credentials='--host=my_db_server --database=phoros_workspace --user=some_db_user --password=wonttell'

cd ../                          # where the phoros binary lives

for i in /path/to/all/projects/one_project/!(already_done_01|already_done_02|already_done_03);
do
    ./phoros --store-images-and-points=this_acquisition_project \
             --directory=$i/                                    \
             --common-root=/path/to/all/projects/               \
             $pg_credentials

    ./phoros --add-to-presentation-project=my_presentation_project      \
             --acquisition-project=this_acquisition_project             \
             $pg_credentials
done

./phoros --insert-footprints=this_acquisition_project $pg_credentials
