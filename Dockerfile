FROM rocker/shiny:latest
RUN apt-get update && apt-get install -y libpoppler-cpp-dev

RUN R -e "install.packages(c('devtools', 'bcrypt', 'rio', 'here', 'tidyverse', 'flexdashboard', 'plotly', 'powerjoin', 'lubridate', 'DT', 'dplyr', 'stringr', 'readxl', 'shinydashboard', 'pacman', 'openxlsx', 'shinyjs'))"

RUN rm -rf /srv/shiny-server/*

# Copie de l'application Shiny
COPY . /srv/shiny-server/

# (optionnel mais recommandé) droits pour shiny
RUN chown -R shiny:shiny /srv/shiny-server