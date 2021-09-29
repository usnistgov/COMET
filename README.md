# COMET: Counting Method Evaluation Tool
R Shiny application for analyzing dilution series data.

The easiest way to run and use COMET is by first installing Docker (https://docs.docker.com/get-docker/).

Once Docker is installed, download the COMET repository, and navigate your terminal to the main directory of the project (the same level as Dockerfile). Then, run the following command to build the image:
```
docker build -t comet .
```
To run the container, run the following command:
```
docker run -d -p 3838:3838 comet
```
Then the app should be visible at localhost:3838 (accessed via your web browser).
