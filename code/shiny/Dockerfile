FROM rocker/shiny-verse:4.0.3

LABEL authors="Luca Cozzuto <lucacozzuto@gmail.com>, Toni Hermoso Pulido <toni.hermoso@crg.eu>"

RUN apt-get update; apt-get upgrade -y
RUN apt-get install -y patchelf libudunits2-dev libgdal-dev gdal-bin libproj-dev proj-data proj-bin libgeos-dev
COPY deps.R /tmp

RUN Rscript /tmp/deps.R

RUN mkdir -p /srv/shiny-server/covid-cfr-estimator/

COPY server.R /srv/shiny-server/covid-cfr-estimator/
COPY ui.R /srv/shiny-server/covid-cfr-estimator/
COPY functions.R /srv/shiny-server/covid-cfr-estimator/

RUN sudo chown -R shiny:shiny /srv/shiny-server

# Clean cache
RUN apt-get clean
RUN set -x; rm -rf /var/lib/apt/lists/*

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/covid-cfr-estimator/', host = '0.0.0.0', port = 3838)"]
#CMD ["/usr/bin/shiny-server"]
