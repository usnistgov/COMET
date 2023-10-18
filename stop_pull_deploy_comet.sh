#!/bin/sh

set -e

if [[ "$PWD" != "/home/ubuntu" ]]; then
  echo "Need to run script from home directory, not project folder!"
  exit
fi

rm master.zip
rm -r COMET-master

wget https://github.com/usnistgov/COMET/archive/refs/heads/master.zip
unzip master.zip

sudo docker build -t comet ./COMET-master
sudo docker rm -f $(sudo docker ps -a -q)
sudo docker image prune -f
sudo docker run -d -p 8080:3838 comet