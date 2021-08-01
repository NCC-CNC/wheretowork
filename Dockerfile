# source image
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

## set working directory
RUN touch restart.txt && \
    chmod 777 restart.txt

# update shiny server configuration
# RUN cp  /etc/shiny-server/shiny-server.conf /tmp/shiny-server.conf && \
#    sed -i 's/run_as shiny;/run_as shiny;preserve_logs true;/g' \
#      /tmp/shiny-server.conf && \
#    cp  /tmp/shiny-server.conf /etc/shiny-server/shiny-server.conf && \
#    cat /etc/shiny-server/shiny-server.conf

# set command
CMD ["/bin/bash"]

# shinyapps deployment image
## source image
FROM base AS shinyapps

# install latest version of rsconnect is installed
ADD https://cran.r-project.org/web/packages/rsconnect/index.html /tmp/rsconnect.html
RUN Rscript -e "remotes::install_cran('rsconnect', force = TRUE)"

# copy app file for deployment
COPY app.R /app

# set working directory
WORKDIR /app

## run app
CMD Rscript -e "rsconnect::setAccountInfo(name=Sys.getenv('SHINYAPPS_USER'), token=Sys.getenv('SHINYAPPS_TOKEN'),secret=Sys.getenv('SHINYAPPS_SECRET'));rsconnect::deployApp('.', appName=Sys.getenv('SHINYAPPS_APPNAME'))"

# main image
FROM base AS main

## set user
USER shiny

## select port
EXPOSE 3838

## copy app file for shiny server
COPY --chown=shiny:shiny app.R /srv/shiny-server

## store environmental variables
ENV R_CONFIG_ACTIVE=production
RUN env | grep R_CONFIG_ACTIVE > /home/shiny/.Renviron

## set working directory
WORKDIR /tmp

## run app
CMD ["/usr/bin/shiny-server"]
