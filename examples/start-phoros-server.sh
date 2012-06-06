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
export PHOROS_LOG_DIR=server.log/

cd ../                          # where the phoros binary lives

./phoros --server --common-root=/path/to/all/projects/                                          \
         --aux-numeric-label=Station                                                            \
         --aux-text-label=VNK                                                                   \
         --aux-text-label=NNK                                                                   \
         --aux-text-label=Name                                                                  \
         --login-intro="<b>Our Phoros Project</b>"                                              \
         --login-intro="Please use Firefox or one of its relatives."                            \
         --login-intro="It is recommendable to set your browser cache size to about 2000 MB."   \
         --login-intro="etc."
