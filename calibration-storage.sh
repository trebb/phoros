#! /bin/bash

## Usage: ./calib-storage.sh host database user password

CAMERA_HARDWARE_ID=`./phoros --store-camera-hardware --host=$1 --database=$2 --user=$3 --password=$4    \
    --sensor-width-pix=1700                                                                             \
    --sensor-height-pix=1500                                                                            \
    --pix-size=0.007397                                                                                 \
    --channels=3                                                                                        \
    --pix-depth=255                                                                                     \
    --color-raiser=1.2,1,1                                                                              \
    --bayer-pattern=#00ff00,#ff0000                                                                     \
    --serial-number='bogus-pulnix-front-left'                                                           \
    --description='JAI TMC4200 (Position: front left)'` 

LENS_ID=`./phoros --store-lens --host=$1 --database=$2 --user=$3 --password=$4  \
    --c=10                                                                      \
    --serial-number='bogus-skr-cinegon-10-front-left'                           \
    --description='Schneider Kreuznach SKR KMP CINEGON 10/1,9-M62,0'`

GENERIC_DEVICE_ID=`./phoros --store-generic-device --host=$1 --database=$2 --user=$3 --password=$4      \
    --camera-hardware-id=$CAMERA_HARDWARE_ID                                                            \
    --lens-id=$LENS_ID`

DEVICE_STAGE_OF_LIFE_ID=`./phoros --store-device-stage-of-life --host=$1 --database=$2 --user=$3 --password=$4  \
    --recorded-device-id=21                                                                                     \
    --event-number=1                                                                                            \
    --generic-device-id=$GENERIC_DEVICE_ID                                                                      \
    --vehicle-name='UNO'                                                                                        \
    --casing-name='front left'                                                                                  \
    --computer-name=''                                                                                          \
    --computer-interface-name=''                                                                                \
    --mounting-date=2009-04-01`

./phoros --store-device-stage-of-life-end --host=$1 --database=$2 --user=$3 --password=$4       \
    --device-stage-of-life-id=$DEVICE_STAGE_OF_LIFE_ID                                          \
    --unmounting-date=2010-01-01

./phoros --store-camera-calibration --host=$1 --database=$2 --user=$3 --password=$4     \
    --device-stage-of-life-id=$DEVICE_STAGE_OF_LIFE_ID                                  \
    --date=2009-05-25                                                                   \
    --person=Scheller                                                                   \
    --main-description ''                                                               \
    --debug=no                                                                          \
    --usable=yes                                                                        \
    --photogrammetry-version=0                                                          \
    --mounting-angle=0                                                                  \
    --inner-orientation-description='calibration field'                                 \
    --c=-10.18083                                                                       \
    --xh=-0.08149                                                                       \
    --yh=0.19289                                                                        \
    --a1=-9.13833e-004                                                                  \
    --a2=1.04652e-005                                                                   \
    --a3=0                                                                              \
    --b1=-5.35737e-005                                                                  \
    --b2=9.49128e-005                                                                   \
    --c1=-1.33724e-004                                                                  \
    --c2=2.11705e-004                                                                   \
    --r0=4.715882                                                                       \
    --outer-orientation-description=''                                                  \
    --dx=0                                                                              \
    --dy=0                                                                              \
    --dz=0                                                                              \
    --omega=0                                                                           \
    --phi=0                                                                             \
    --kappa=0                                                                           \
    --boresight-description=''                                                          \
    --b-dx=-1.458119959                                                                 \
    --b-dy=1.72479135                                                                   \
    --b-dz=1.912406042                                                                  \
    --b-ddx=0                                                                           \
    --b-ddy=0                                                                           \
    --b-ddz=0                                                                           \
    --b-rotx=-1.390264206                                                               \
    --b-roty=0.0138192676                                                               \
    --b-rotz=0.1022317722                                                               \
    --b-drotx=0                                                                         \
    --b-droty=0                                                                         \
    --b-drotz=0                                                                         \
    --nx=-0.00278                                                                       \
    --ny=0.02638                                                                        \
    --nz=-0.99965                                                                       \
    --d=0.20855

######################################################################

CAMERA_HARDWARE_ID=`./phoros --store-camera-hardware --host=$1 --database=$2 --user=$3 --password=$4    \
    --sensor-width-pix=1700                                                                             \
    --sensor-height-pix=1500                                                                            \
    --pix-size=0.0074                                                                                   \
    --channels=3                                                                                        \
    --pix-depth=255                                                                                     \
    --color-raiser=1.2,1,1                                                                              \
    --bayer-pattern=#00ff00,#ff0000                                                                     \
    --serial-number='bogus-pulnix-front-left'                                                           \
    --description='JAI TMC4200 (Position: front left)'`

LENS_ID=`./phoros --store-lens --host=$1 --database=$2 --user=$3 --password=$4  \
    --c=10                                                                      \
    --serial-number='bogus-skr-cinegon-10-front-left'                           \
    --description='Schneider Kreuznach SKR KMP CINEGON 10/1,9-M62,0'`

GENERIC_DEVICE_ID=`./phoros --store-generic-device --host=$1 --database=$2 --user=$3 --password=$4      \
    --camera-hardware-id=$CAMERA_HARDWARE_ID                                                            \
    --lens-id=$LENS_ID`

DEVICE_STAGE_OF_LIFE_ID=`./phoros --store-device-stage-of-life --host=$1 --database=$2 --user=$3 --password=$4  \
    --recorded-device-id=21                                                                                     \
    --event-number=1                                                                                            \
    --generic-device-id=$GENERIC_DEVICE_ID                                                                      \
    --vehicle-name='UNO'                                                                                        \
    --casing-name='front left'                                                                                  \
    --computer-name=''                                                                                          \
    --computer-interface-name=''                                                                                \
    --mounting-date=2010-01-01`

#./phoros --store-device-stage-of-life-end --host=$1 --database=$2 --user=$3 --password=$4 \
#    --device-stage-of-life-id=$DEVICE_STAGE_OF_LIFE_ID                                    \
#    --unmounting-date=1900-01-01

./phoros --store-camera-calibration --host=$1 --database=$2 --user=$3 --password=$4     \
    --device-stage-of-life-id=$DEVICE_STAGE_OF_LIFE_ID                                  \
    --date=2010-05-07                                                                   \
    --person=Scheller                                                                   \
    --main-description ''                                                               \
    --debug=no                                                                          \
    --usable=yes                                                                        \
    --photogrammetry-version=0                                                          \
    --mounting-angle=0                                                                  \
    --inner-orientation-description='calibration field, software Aicon'                 \
    --c=-10.1902                                                                        \
    --xh=-0.0630953                                                                     \
    --yh=0.186176                                                                       \
    --a1=-0.000915845                                                                   \
    --a2=1.07966e-005                                                                   \
    --a3=0                                                                              \
    --b1=2.14923e-005                                                                   \
    --b2=4.47203e-005                                                                   \
    --c1=-0.000554607                                                                   \
    --c2=0.000409916                                                                    \
    --r0=4.715882                                                                       \
    --outer-orientation-description='calibration field, software Aicon'                 \
    --dx=0                                                                              \
    --dy=0                                                                              \
    --dz=0                                                                              \
    --omega=0                                                                           \
    --phi=0                                                                             \
    --kappa=0                                                                           \
    --boresight-description='RWS'                                                       \
    --b-dx=-1.344490161                                                                 \
    --b-dy=1.53705857                                                                   \
    --b-dz=1.536669766                                                                  \
    --b-ddx=0                                                                           \
    --b-ddy=0                                                                           \
    --b-ddz=0                                                                           \
    --b-rotx=1.450069961                                                                \
    --b-roty=-0.1025556399                                                              \
    --b-rotz=-0.00653031875                                                             \
    --b-drotx=0                                                                         \
    --b-droty=0                                                                         \
    --b-drotz=0                                                                         \
    --nx=-0.00278                                                                       \
    --ny=0.02638                                                                        \
    --nz=-0.99965                                      \
    --d=0.20855
