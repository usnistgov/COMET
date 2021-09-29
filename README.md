# COMET
R Shiny application for analyzing dilution series cell count data.

The easiest way to run the application is by first installing Docker (https://docs.docker.com/get-docker/).

Once Docker is installed, download the COMET repository, and navigate your terminal to the main directory of the project (the same level as Dockerfile). Then, run the following command to build the image:
```
docker build -t comet .
```
To run the container, run the following command:
```
docker run -d p 3838:3838 comet
```
Then the app should be visible at localhost:3838 (accessed via your web browser).
