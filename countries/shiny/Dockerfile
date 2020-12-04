FROM rocker/shiny-verse:4.0.3

LABEL authors="Luca Cozzuto <lucacozzuto@gmail.com>, Toni Hermoso Pulido <toni.hermoso@crg.eu>"

RUN apt-get update; apt-get upgrade -y
COPY deps.R /tmp

RUN Rscript /tmp/deps.R

RUN mkdir -p /srv/shiny-server/covid/

COPY server.R /srv/shiny-server/covid/
COPY ui.R /srv/shiny-server/covid/

RUN sudo chown -R shiny:shiny /srv/shiny-server

# Clean cache
RUN apt-get clean
RUN set -x; rm -rf /var/lib/apt/lists/*

CMD ["/usr/bin/shiny-server"]
