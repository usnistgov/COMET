#!/bin/sh

set -e

if [[ "$PWD" != "/home/ubuntu" ]]; then
  echo "Need to run script from home directory, not project folder!"
  exit
fi

rm comet-master.zip
rm -r COMET-master

wget https://github.com/usnistgov/COMET/archive/refs/heads/master.zip -O comet-master.zip
unzip comet-master.zip

sudo docker build -t comet ./COMET-master
sudo docker rm -f comet
sudo docker image prune -f
sudo docker run -d -p 8000:3838 --name=comet comet