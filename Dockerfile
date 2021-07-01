FROM rocker/r-ver:4.0.2
RUN apt-get update && apt-get install -y default-jre-headless gdal-bin git-core imagemagick libcairo2-dev libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libpng-dev libsasl2-dev libssh2-1-dev libssl-dev libudunits2-dev libv8-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("DTedit",upgrade="never",version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.12.8")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "1.5")'
RUN Rscript -e 'remotes::install_version("checkmate",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("mapview",upgrade="never", version = "2.7.8")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.0")'
RUN Rscript -e 'remotes::install_version("mongolite",upgrade="never", version = "2.2.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.14")'
RUN Rscript -e 'remotes::install_version("flexdashboard",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("esquisse",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.7.8")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("janitor",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.2.1")'
RUN Rscript -e 'remotes::install_version("randomcoloR",upgrade="never", version = "1.1.0.1")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-5")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80

ENV MONGO_URL=${MONGO_URL:-"mongodb://host.docker.internal:27017"}
ENV ANON_LOCATIONS=${ANON_LOCATIONS:-FALSE}
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');emdash::run_app(mongo_url = '$MONGO_URL', anon_locations=$ANON_LOCATIONS)"
