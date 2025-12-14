FROM rocker/shiny:latest

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
    'DBI', 'dplyr','DT','ggplot2','ggtext', 'here', \
    'plotly', 'readr','RPostgres','RPostgreSQL', \ 
    'shiny', 'shinydashboard','shinyjs','shinymanager'\ 
    ))"
    
# Install geospatial packages separately (they're larger/slower)
RUN  R -e "pak::pkg_install(c('leaflet', 'mapview', 'sf'))"

# Copy app files
COPY app.R /srv/shiny-server/
COPY www /srv/shiny-server/www
COPY data /srv/shiny-server/data
COPY modules /srv/shiny-server/modules

# Expose port
EXPOSE 3838

# Run app
CMD ["/usr/bin/shiny-server"]