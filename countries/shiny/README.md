# Deployment

## Shiny server

    rsconnect::deployApp('./')

## Docker

    docker build -t covid-cfr-estimator .
    
    docker run --name my-covid-cfr-estimator -p 3838:3838 covid-cfr-estimator


