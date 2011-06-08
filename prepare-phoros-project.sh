#! /bin/bash

pg_credentials='--host=my_db_server --database=phoros_workspace --user=some_db_user --password=wonttell'
pg_aux_credentials='--host=my_db_server --aux-database=phoros_aux --user=some_db_user --password=wonttell'

cd phoros

./phoros --get-user-points=my_presentation_project --json-file=backed-up-user-points.json $pg_credentials

./phoros --nuke-all-tables $pg_credentials
./phoros --create-sys-tables $pg_credentials

./phoros --create-presentation-project=my_presentation_project $pg_credentials

./phoros --create-user=ahr --user-password=ahr --user-full-name="Andreas Heine" --user-role=read --presentation-project=my_presentation_project $pg_credentials
./phoros --create-user=tl --user-password=tl --user-full-name="Thomas Lehmann" --user-role=write --presentation-project=my_presentation_project $pg_credentials
./phoros --create-user=bb --user-password=bb --user-full-name="Bert Burgemeister" --user-role=admin --presentation-project=my_presentation_project $pg_credentials

./phoros --create-acquisition-project=this_acquisition_project $pg_credentials

./phoros --store-user-points=my_presentation_project --json-file=backed-up-user-points.json $pg_credentials

./phoros --redefine-trigger-function=my_presentation_project --plpgsql-body=trigger-example.sql $pg_credentials

./phoros --create-aux-view=my_presentation_project --aux-table=some_foreign_table --coordinates-column=the_geom --numeric-column=nk-station --text-column=vnk --text-column=nnk --text-column=strasse $pg_aux_credentials

../calibration-storage.sh my_db_server phoros_workspace some_db_user 'wonttell'

for i in /path/to/all/projects/one_project/*;
do
    ./phoros --store-images-and-points=this_acquisition_project --directory=$i/ --common-root=/path/to/all/projects/ $pg_credentials
done

./phoros --add-to-presentation-project=my_presentation_project --acquisition-project=this_acquisition_project $pg_credentials
