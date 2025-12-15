FROM rocker/shiny:latest

# https://specs.opencontainers.org/image-spec/annotations/
LABEL \
    org.opencontainers.image.authors="Benjamin Hlina <benjamin.hlina@gmail.com>" \
    org.opencontainers.image.vendor="NAAED-App" \
    org.opencontainers.image.version="0.0.0.9999" \
    org.opencontainers.image.source="https://github.com/benjaminhlina/NAAED-App" \
    org.opencontainers.image.licenses="MIT"

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    cmake \
    g++ \
    gdal-bin \
    git \
    gfortran \
    libabsl-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libgeos-dev \
    libgdal-dev \
    libicu-dev \
    libjpeg-dev \ 
    libmysqlclient-dev \
    libpng-dev \
    libpq-dev \
    libproj-dev \
    libsqlite3-dev \
    libssl-dev \
    libtiff5-dev \
    libudunits2-dev \
    libxml2-dev \
    libxt-dev \
    make \
    pandoc \
    proj-bin \
    proj-data \
    && rm -rf /var/lib/apt/lists/*

# Install pak for quicker install 
RUN R -e "install.packages('pak', repos = 'https://cran.rstudio.com/')"

# Install packages
RUN R -e "pak::pkg_install(c(\ 
    'cli', 'DBI', 'dplyr', 'dbplyr', 'DT','ggplot2','ggtext', 'here', \
    'plotly', 'pryr', 'readr', 'readxl', 'RPostgres','RPostgreSQL', \ 
    'shiny', 'shinydashboard','shinyjs','shinymanager', \
    'stringr', 'writexl'\ 
    ))"
    
# Install geospatial packages separately (they're larger/slower)
RUN  R -e "pak::pkg_install(c('leaflet', 'mapview', 'sf'))"

# Copy app files
COPY app.R /srv/shiny-server/
COPY www /srv/shiny-server/www
COPY data /srv/shiny-server/data
COPY modules /srv/shiny-server/modules

RUN chown -R shiny:shiny /srv/shiny-server && \
    chmod -R 755 /srv/shiny-server

# Expose port
EXPOSE 3838

# Run app
CMD ["/usr/bin/shiny-server"]