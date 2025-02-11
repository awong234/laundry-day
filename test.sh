#!/bin/bash

# Run through all the work and make sure no errors appear. Currently no support
# for multiple R versions, although it is possible to do on windows. `rig` is an
# option too.

# Right now using R 4.4.2

function report {
    if [ $? -ne 0 ]; then
        echo "Error in $(pwd)"
    else
        R --version > rversion.txt 2>&1
    fi
}

pushd datasets/202403-cuyahoga-county-voter-registration-locations
Rscript sample_registration_locations.R > log.txt 2>&1
report
popd

pushd datasets/202404-tsa-passenger-volume
Rscript clean_tsa.R > log.txt 2>&1
report
popd

pushd datasets/202405-fuel-economy
Rscript sample.R > log.txt 2>&1
report
popd

pushd datasets/202406-nexrad
Rscript nexrad.R > log.txt 2>&1
report
popd

pushd datasets/202410-hurrdat2
Rscript fetch_hurricane_data.R > log.txt 2>&1
report
popd

# Needs to be manually inspected because it is a shiny app
pushd datasets/202502-netflix-engagement
Rscript app.R > log.txt 2>&1
report
popd
