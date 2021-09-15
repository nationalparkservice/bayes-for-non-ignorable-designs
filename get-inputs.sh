#!/usr/bin/env bash

rm -r tmp/file-share
wget -P tmp https://irma.nps.gov/DataStore/DownloadFile/663063
unzip tmp/663063 -d tmp/file-share
rm tmp/663063

# # Find all input files in the project root and move them to test/ directory
# find * \
#   \( -name "00-input" -o -name ".00-input" -o -name "spsurvey-input-data.csv" \) \
#   -not -path "tmp/*" -not -path "output/*" \
#   -exec bash -c '\
#     for d; do
#       mkdir -p "test/$(dirname $d)"
#       mv "$d" "test/$(dirname $d)"
#     done' _ {} +
# rm -r test

# # Move all input files into their respective example directories
# find tmp \
#   \( -name "00-input" -o -name ".00-input" -o -name "spsurvey-input-data.csv" \) \
#   -exec bash -c ‘\
#     for d; do
#       cp -r $d $(grep -o "example.*" <<< $d)
#     done’ _ {} +
