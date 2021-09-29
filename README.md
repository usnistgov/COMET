# COMET: Counting Method Evaluation Tool
R Shiny application for analyzing dilution series count data

The easiest way to run and use COMET is by first installing Docker (https://docs.docker.com/get-docker/).

Once Docker is installed, download the COMET repository (at github.com/usnistgov/COMET, click 'Code' and then 'Download ZIP'), and navigate your terminal to the main directory of the project (the same level as Dockerfile). Then, run the following command to build the image:
```
docker build -t comet .
```
To run the container, run the following command:
```
docker run -d -p 3838:3838 --name my_container comet
```
(-d for 'detached', -p specifies the port mapping, '--name' gives a name to the running container, and 'comet' tells docker which image to build the container from.) Then the app should be visible at localhost:3838 (accessed via your web browser).

To stop and remove the running container, run the following:
```
docker rm -f my_container
```

Alternatively, you can run, stop, and remove the container using the Docker Desktop application.
