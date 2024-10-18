FROM rocker/tidyverse:4.4.1 
# set working directory

# copy files
COPY build /usr/local/workdir/build

# install packages
RUN Rscript /usr/local/workdir/build/01_install_packages.R

# remove unnecessary files
RUN rm -rf /usr/local/workdir/build

# volume

# add the shiny apps scripts, which is persistent
EXPOSE 8880
COPY shiny /usr/local/workdir/shiny
WORKDIR /usr/local/workdir/shiny
CMD ["Rscript", "run.R"]

# docker build -t shiny-apps . ;docker run -ti --rm -p 13838:13838 shiny-apps
# docker run -ti --rm shiny-apps bash
# docker run -ti --rm -p 13838:13838 shiny-apps