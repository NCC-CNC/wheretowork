# source image
FROM rocker/shiny:4.1.0

# remove example apps
RUN rm -rf /srv/shiny-server/*

# install system libraries
RUN apt-get update && apt-get install -y \
  software-properties-common

# install R package dependencies
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
      libharfbuzz-dev \
      libfribidi-dev

# install R packages needed to setup renv
RUN Rscript -e 'install.packages(c("renv", "remotes"))'

# copy the app
COPY --chown=shiny:shiny . /srv/shiny-server

# set user
USER shiny

# install R packages using renv
RUN cd /srv/shiny-server && Rscript -e 'renv::restore()'
RUN cd /srv/shiny-server && Rscript -e 'remotes::install_local(upgrade = "never")'

# select port
EXPOSE 3838

# run app
CMD ["/usr/bin/shiny-server"]
