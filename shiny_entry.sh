#!/bin/sh
set -e

# Write env vars to shiny user's Renviron
cat <<EOF > /home/shiny/.Renviron
POSTGRES_HOST=${POSTGRES_HOST}
POSTGRES_USER=${POSTGRES_USER}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
POSTGRES_DB=${POSTGRES_DB}
SHINY_USER=${SHINY_USER}
SHINY_PASSWORD=${SHINY_PASSWORD}
EOF

chown shiny:shiny /home/shiny/.Renviron
chmod 600 /home/shiny/.Renviron

# Start Shiny Server
exec shiny-server