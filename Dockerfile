# FROM rocker/shiny-verse:latest

# # Download and install libraries
# RUN R -e "install.packages('remotes')"
# RUN R -e "remotes::install_github('daqana/dqshiny')"
# RUN Rscript -e "install.packages('BiocManager')"
# RUN Rscript -e "BiocManager::install('limma')"
# RUN R -e "devtools::install_github('singha53/omicsBioAnalytics@master')" 

# # copy app to image
# COPY inst/app /app

# EXPOSE 3838

# CMD ["R", "-e", "options(shiny.port = 3838, shiny.host = '0.0.0.0'); shiny::runApp('/app')"] 

FROM rocker/shiny:3.6.3

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcairo2-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadbclient-dev \
  libpq-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libssh2-1-dev \
  unixodbc-dev \
  && rm -rf /tmp/downloaded_packages

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('remotes')"
#RUN R -e "remotes::install_github('daqana/dqshiny')"
#RUN Rscript -e "install.packages(c('BiocManager', 'aws.s3', 'canvasXpress', 'caret', 'dplyr', 'DT', 'ellipse', 'enrichR', 'ggrepel', 'glmnet', 'googleVis', 'gridExtra', 'gvlma', 'jsonlite', 'lattice', 'magrittr', 'pheatmap', 'plotly', 'pROC','RColorBrewer', 'sortable', 'shinyjs', 'shinydashboard', 'shinyBS', 'tidyr', 'UpSetR', 'visNetwork'))"
#RUN Rscript -e "BiocManager::install('limma')"
RUN R -e "remotes::install_github('singha53/omicsBioAnalytics@udacity')"

# copy the app to the image
COPY omicsBioAnalytics.Rproj /srv/shiny-server/
COPY inst/app/ /srv/shiny-server/
COPY R /srv/shiny-server/R
COPY data /srv/shiny-server/data

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]

