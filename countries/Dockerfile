FROM rocker/r-ver:4.0.2

# File Author / Maintainer
MAINTAINER Luca Cozzuto <lucacozzuto@gmail.com>

RUN apt-get update; apt-get upgrade -y
RUN Rscript -e "install.packages(c('dplyr', 'stringr', 'EnvStats', 'spatialEco', 'zoo'), repos='http://cran.us.r-project.org')"

# Clean cache
RUN apt-get clean
RUN set -x; rm -rf /var/lib/apt/lists/*
