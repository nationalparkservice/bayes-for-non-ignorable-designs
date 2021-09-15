FROM rocker/geospatial:4.0.0-ubuntu18.04

# Copy Latin Modern font files to fonts directory and refresh fonts cache.
COPY Latin-Modern-Roman-fontfacekit.zip /tmp
RUN unzip tmp/Latin-Modern-Roman-fontfacekit.zip -d /usr/share/fonts && \
    fc-cache -f -v && \
    rm tmp/Latin-Modern-Roman-fontfacekit.zip

# Install JAGS.
RUN apt-get update -y && \
    apt-get install -y jags

# Graphics and other required packages.
RUN apt-get install tree && \
    Rscript -e "update.packages(ask = FALSE)" && \
    install2.r --error \
        abind \
        spsurvey \
        hrbrthemes \
        ggthemes \
        ggridges \
        cowplot \
        HDInterval \
        rjags \
        coda

# Copies the code to the filesystem path `/` of the container
COPY . .

# Code file to execute when the docker container starts up
ENTRYPOINT ["/src/wrap-r.sh"]
