#! /bin/bash

pg_credentials='--host=my_db_server --database=phoros_workspace --user=some_db_user --password=wonttell'
pg_aux_credentials='--host=my_db_server --aux-database=phoros_aux --user=some_db_user --password=wonttell'

cd phoros

./phoros --server --common-root=/path/to/all/projects/                                          \
         $pg_credentials $pg_aux_credentials                                                    \
         --login-intro="<b>Our Phoros Project</b>"                                              \
         --login-intro="Please use Firefox or one of its relatives."                            \
         --login-intro="It is recommendable to set your browser cache size to about 2000 MB."   \
         --login-intro="etc."
