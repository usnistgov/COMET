# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.1.1

## update system libraries and install wget
RUN apt-get update -y

# install packages
RUN R -e "install.packages(c('ggplot2','dplyr','tidyr'))"
RUN R -e "install.packages('shinythemes')"
RUN R -e "install.packages('plyr')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('readxl')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('shinycssloaders')"

# copy necessary files
COPY . /srv/shiny-server/

WORKDIR /srv/shiny-server
RUN rm -r 01_hello 02_text 03_reactivity 04_mpg 05_sliders 06_tabsets 07_widgets 08_html 09_upload 10_download 11_timer
RUN chmod -R 755 /srv/shiny-server
RUN chgrp -R shiny /srv/shiny-server

# expose port
EXPOSE 3838