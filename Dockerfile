FROM rocker/tidyverse:4.4.1 

# copy files
COPY build /usr/local/workdir/build

# install packages
RUN Rscript /usr/local/workdir/build/01_install_packages.R

# remove unnecessary files
RUN rm -rf /usr/local/workdir/build

# 8880 is exposed by CF
EXPOSE 8880

WORKDIR /usr/local/workdir/shiny

