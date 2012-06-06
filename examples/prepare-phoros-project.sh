#! /bin/bash

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

./phoros --get-user-points=my_presentation_project      \
         --json-file=backed-up-user-points.json

./phoros --nuke-all-tables

./phoros --create-sys-tables

./phoros --create-acquisition-project=this_acquisition_project

./examples/calibration-storage.sh

./phoros --create-presentation-project=my_presentation_project

./phoros --create-user=ahr                              \
         --user-password="ahr"                          \
         --user-full-name="Andreas Heine"               \
         --user-role=read                               \
         --presentation-project=my_presentation_project

./phoros --create-user=tl                               \
         --user-password="tl"                           \
         --user-full-name="Thomas Lehmann"              \
         --user-role=write                              \
         --presentation-project=my_presentation_project

./phoros --create-user=bb                               \
         --user-password="bb"                           \
         --user-full-name="Bert Burgemeister"           \
         --user-role=admin                              \
         --presentation-project=my_presentation_project

./phoros --store-user-points=my_presentation_project    \
         --json-file=backed-up-user-points.json

./phoros --redefine-trigger-function=my_presentation_project    \
         --plpgsql-body=trigger-example.sql

./phoros --create-aux-view=my_presentation_project      \
         --aux-table=some_foreign_table                 \
         --coordinates-column=the_geom                  \
         --numeric-column=nk_station                    \
         --text-column=vnk                              \
         --text-column=nnk                              \
         --text-column=strasse

./phoros --create-image-attribute=my_presentation_project                       \
         --tag="front cams only"                                                \
         --sql-clause="recorded_device_id = '21' OR recorded_device_id = '22'"

./phoros --create-image-attribute=my_presentation_project                                               \
         --tag="within 10 minutes"                                                                      \
         --sql-clause="trigger_time BETWEEN (first_trigger_time - 300) AND (first_trigger_time + 300)"

for i in /path/to/all/projects/one_project/*;
do
    ./phoros --store-images-and-points=this_acquisition_project \
             --directory=$i/

    ./phoros --add-to-presentation-project=my_presentation_project      \
             --acquisition-project=this_acquisition_project
done

./phoros --insert-footprints=this_acquisition_project