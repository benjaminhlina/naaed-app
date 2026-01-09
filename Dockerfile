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
    libfreetype6-dev \
    libfribidi-dev \
    libgeos-dev \
    libgdal-dev \
    libharfbuzz-dev \
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
    pkg-config \
    proj-bin \
    proj-data \
    && rm -rf /var/lib/apt/lists/*


# # ---- remove shiny-server template apps --- 
RUN rm -rf /srv/shiny-server/*

# ---- ops for got to install renv ---- 
RUN R -e "install.packages(c('renv', 'pak'), repos = 'https://cran.rstudio.com')"

# # ---- Set working directory ----
WORKDIR /srv/shiny-server/NAAED-App/
# ---- Copy renv files ----
COPY renv.lock renv.lock
COPY renv/ renv/

# ---- Restore R packages ----
ENV RENV_PATHS_CACHE=/renv/cache
ENV RENV_CONFIG_PAK_ENABLED=TRUE
ENV RENV_CONFIG_REPOS_OVERRIDE=https://cloud.r-project.org
RUN R -e "options(renv.verbose = TRUE); renv::restore(prompt = FALSE)"

# Copy app files
COPY app.R app.R
COPY www/ www/
COPY data/ data/
COPY modules/ modules/
# copy shiny-server config file 
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# ---- change file ownership and rew -----
RUN chown -R shiny:shiny /srv/shiny-server && \
    chmod -R 755 /srv/shiny-server


# --- copy shiny_entry and change rew ---- 
COPY shiny_entry.sh /usr/local/bin/shiny_entry.sh
RUN chmod 755 /usr/local/bin/shiny_entry.sh
# Expose port
USER shiny
EXPOSE 3838
CMD ["/usr/local/bin/shiny_entry.sh"]