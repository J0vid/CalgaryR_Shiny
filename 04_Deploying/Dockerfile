# get shiny serves plus tidyverse packages image
FROM rocker/shiny

#system libraries of general use
RUN apt-get update && apt-get install -y \
    libssl-dev \
    build-essential \
    gfortran \
    libjpeg-dev \
    xorg \
    libx11-dev \
    libglu1-mesa-dev \
    libfreetype6-dev \
    libcurl4-openssl-dev \
    libxml2-dev 

# install R packages required
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rmarkdown', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"


# select port
EXPOSE 3838

# allow permission
RUN chown -R shiny:shiny /srv/shiny-server
