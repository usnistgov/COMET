# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.1.0

## update system libraries and install wget
RUN apt-get update

# copy necessary files
COPY ./ /

# install packages
RUN Rscript -e 'install.packages("shinythemes")'
RUN Rscript -e 'install.packages("tidyr")'
RUN Rscript -e 'install.packages("plyr")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("DT")'
RUN Rscript -e 'install.packages("readxl")'
RUN Rscript -e 'install.packages("readr")'

# expose port
EXPOSE 3838

CMD R -e 'shiny::runApp(port = 3838, host = "0.0.0.0")'
# app should run from 'inherited' RUN command from rocker/shiny