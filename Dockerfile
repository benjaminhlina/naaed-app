FROM rocker/r2u:latest

# https://specs.opencontainers.org/image-spec/annotations/
LABEL \
    org.opencontainers.image.authors="Benjamin Hlina <benjamin.hlina@gmail.com>" \
    org.opencontainers.image.vendor="naaed-app" \
    org.opencontainers.image.version="0.0.0.9999" \
    org.opencontainers.image.source="https://github.com/benjaminhlina/naaed-app" \
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
    r-cran-cli \
    r-cran-dbi \
    r-cran-dplyr \
    r-cran-dbplyr \
    r-cran-dt \
    r-cran-ggplot2 \
    r-cran-ggtext \
    r-cran-here \
    r-cran-plotly \
    r-cran-readr \
    r-cran-readxl \
    r-cran-rpostgres \
    r-cran-shiny \
    r-cran-shinydashboard \
    r-cran-shinyjs \
    r-cran-stringr \
    r-cran-leaflet \
    r-cran-sf \
    && rm -rf /var/lib/apt/lists/*

# Install pak for remaining packages
RUN R -e "install.packages('pak', repos='https://cran.rstudio.com/')"

# Install remaining packages not available via apt using pak
RUN R -e "pak::pkg_install(c('pryr', 'shinymanager', 'writexl', 'mapview'))"

# remove shiny-server template apps --- 
RUN rm -rf /srv/shiny-server/*
# Copy app files
COPY app.R /srv/shiny-server/
COPY www /srv/shiny-server/www
COPY data /srv/shiny-server/data
COPY modules /srv/shiny-server/modules
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf


RUN chown -R shiny:shiny /srv/shiny-server && \
    chmod -R 755 /srv/shiny-server

# Expose port

COPY shiny_entry.sh /usr/local/bin/shiny_entry.sh
RUN chmod +x /usr/local/bin/shiny_entry.sh

EXPOSE 3838
CMD ["/usr/local/bin/shiny_entry.sh"]