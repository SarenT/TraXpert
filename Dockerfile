# Use an official R runtime as a parent image
FROM rocker/shiny:latest

# Install necessary packages
RUN apt-get update && apt-get install -y \
    git libpq-dev libudunits2-dev

# Clone the Shiny app code from GitHub
# RUN rm -r /srv/shiny-server/
RUN git clone https://github.com/SarenT/TraXpert /srv/shiny-server/traxpert

COPY --chown=shiny:shiny ./shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY --chown=shiny:shiny ./ /srv/shiny-server/traxpert

RUN install2.r --error \
    --deps TRUE \ 
    xml2\
    ggplot2\
    magrittr\
    dplyr\
    ggpubr\
    RColorBrewer\
    udunits2\
    sticky\
    shiny\
    sortable\
    colourpicker\
    DT\
    svglite\
    readxl\
    colorspace\
    smoother\
    shinyBS\
    car\
    latex2exp\
    shinyjs\
    htmlwidgets\
    shinyWidgets\
    dplyr\
    tidyr\
    purrr\
    rlang\
    viridisLite\
    e1071\
    circular\
    stringr\
    ggiraphExtra\
    && rm -rf /tmp/downloaded_packages

# Start the Shiny app
# CMD ["/usr/bin/shiny-server"]
