# base image
FROM rocker/shiny:4.1.0 AS base

## remove example apps
RUN rm -rf /srv/shiny-server/*

## install system libraries
RUN apt-get update && apt-get install -y \
  software-properties-common

## install R package dependencies
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable && \
    apt-get update && apt-get install -y \
      libcurl4-gnutls-dev \
      libssl-dev \
      libudunits2-dev \
      libudunits2-dev \
      libgdal-dev \
      libgeos-dev \
      libproj-dev \
      coinor-libcbc-dev \
      coinor-libclp-dev \
      coinor-libsymphony-dev \
      coinor-libcgl-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

## install R packages
RUN mkdir /renv
COPY renv.lock /renv/renv.lock
RUN cd /renv && \
    Rscript -e 'install.packages(c("renv", "remotes"))' && \
    Rscript -e 'renv::restore()'

## install app
RUN mkdir /app
COPY inst /app/inst
COPY man /app/man
COPY R /app/R
COPY .Rbuildignore /app
COPY DESCRIPTION /app
COPY NAMESPACE /app
RUN cd /app && \
    Rscript -e 'remotes::install_local(upgrade = "never")'

## prepare data cap-bc project
COPY Makefile /app
RUN cd /app && make cap-bc

# set command
CMD ["/bin/bash"]

# shiny image
FROM base AS shiny

## set user
USER shiny

## select port
EXPOSE 3838

## configure shiny
## store environmental variables
ENV R_SHINY_PORT=3838
ENV R_SHINY_HOST=0.0.0.0
RUN env | grep R_SHINY_PORT > /home/shiny/.Renviron && \
    env | grep R_SHINY_HOST >> /home/shiny/.Renviron

## set working directory
RUN mkdir /home/shiny/app
COPY app.R /home/shiny/app/app.R
WORKDIR /home/shiny/app

## run app
CMD ["/usr/local/bin/Rscript", "/home/shiny/app/app.R"]
