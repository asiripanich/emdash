FROM rocker/r-ver:4.1.0
RUN apt-get update && apt-get install -y default-jre-headless gdal-bin git-core imagemagick libcairo2-dev libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libpng-dev libsasl2-dev libssh2-1-dev libssl-dev libudunits2-dev libv8-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_github("asiripanich/DTedit")'
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
ENV MONGO_URL=${MONGO_URL:-"mongodb://host.docker.internal:27017"}
ENV ANON_LOCATIONS=${ANON_LOCATIONS:-FALSE}
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');emdash::run_app(mongo_url = '$MONGO_URL', anon_locations=$ANON_LOCATIONS)"
