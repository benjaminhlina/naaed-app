FROM rocker/shiny:latest

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
    && rm -rf /var/lib/apt/lists/*

# ---- Set working directory ----
WORKDIR /srv/shiny-server

# ---- ops for got to install renv ---- 
RUN R -e "install.packages('renv', repos = 'https://cran.rstudio.com')"
# ---- Copy renv files ----
COPY renv.lock renv.lock
COPY renv/ renv/

# ---- Restore R packages ----
ENV RENV_PATHS_CACHE=/renv/cache
RUN R -e "renv::restore()"


# ---- remove shiny-server template apps --- 
RUN rm -rf /srv/shiny-server/*

# Copy app files
COPY app.R ./NAAED-App/
COPY www ./NAAED-App/www
COPY data ./NAAED-App/data
COPY modules ./NAAED-App/modules
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# ---- change file ownership and rew -----
RUN chown -R shiny:shiny /srv/shiny-server && \
    chmod -R 755 /srv/shiny-server


# --- copy shiny_entry and change rew ---- 
COPY shiny_entry.sh /usr/local/bin/shiny_entry.sh
RUN chmod 755 /usr/local/bin/shiny_entry.sh
# Expose port
EXPOSE 3838
CMD ["/usr/local/bin/shiny_entry.sh"]