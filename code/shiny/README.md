# Deployment

## Shiny server

    rsconnect::deployApp('./')

## Docker

    docker build -t covid-cfr-estimator -f Dockerfile ../
    
    docker run -d --volume /home/admin/shiny/covid-cfr-estimator:/srv/shiny-server/covid-cfr-estimator --name my-covid-cfr-estimator -p 3838:3838 covid-cfr-estimator
