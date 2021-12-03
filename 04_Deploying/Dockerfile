# get shiny serves plus tidyverse packages image
FROM rocker/shiny

#system libraries of general use
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev

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
