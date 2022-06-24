FROM rocker/shiny-verse:latest

# copy the app directory into the image
COPY *.Rproj /srv/shiny-server/
COPY ./shiny-app/* /srv/shiny-server/
COPY modules /srv/shiny-server/modules
COPY www /srv/shiny-server/www


# select port
EXPOSE 8282
 

# install R packages required 
# Change the packages list to suit your needs

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e 'remotes::install_github("daattali/shinycssloaders")'
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('GGally', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packa   ges('dplyr', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('inline', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', repos='http:/cran.rstudio.com/')"
#RUN R -e "install.packages('caret', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('randomForest', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('earth', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('devtools', repos='http:/cran.rstudio.com/')"
RUN R -e 'devtools::install_github("hadley/emo")'
RUN R -e "install.packages('gfonts', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('xgboost', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('scales', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('RSQLite', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('data.table', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('gganimate', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('gifski', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('ranger', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('MASS', repos='http:/cran.rstudio.com/')"
RUN R -e "install.packages('moments', repos='http:/cran.rstudio.com/')"


# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# run app
CMD ["/usr/bin/shiny-server.sh"]