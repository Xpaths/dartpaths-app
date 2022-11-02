FROM openanalytics/r-shiny-rdkit:4.1.2-2022.03.1
MAINTAINER Marvin Steijaert "marvin.steijaert@openanalytics.eu"

# System libraries (incl. system requirements for R packages)
RUN apt-get update && apt-get install --no-install-recommends -y \
    libgit2-dev \
    libssl-dev \
    zlib1g-dev \
    python \
    pandoc \
    pandoc-citeproc \
    libharfbuzz-dev \
    make \
    libxml2-dev \
    libfribidi-dev \
    libicu-dev \
    libcurl4-openssl-dev \
    libtiff-dev \
    libpng-dev \
    git-core \
    && rm -rf /var/lib/apt/lists/*

RUN pip3 install --no-cache-dir urllib3 wget scipy pebble

RUN R -e "cat(\"local(options(repos = c(CRAN = 'https://cloud.r-project.org')))\n\", file = R.home('etc/Rprofile.site'), append = TRUE)"

# install dependencies
RUN R -q -e "install.packages('remotes')" && \
    R -q -e "remotes::install_version('BH', version = '1.78.0-0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('MASS', version = '7.3-58.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('R.methodsS3', version = '1.8.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('R6', version = '2.5.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('RColorBrewer', version = '1.1-3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('Rcpp', version = '1.0.9', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('XML', version = '3.99-0.11', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('assertthat', version = '0.2.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('base64enc', version = '0.1-3', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('brew', version = '1.0-8', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('brio', version = '1.1.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('cli', version = '3.4.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('clipr', version = '0.8.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('colorspace', version = '2.0-3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('commonmark', version = '1.8.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('cpp11', version = '0.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('crayon', version = '1.5.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('curl', version = '4.3.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('data.table', version = '1.14.2', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('digest', version = '0.6.29', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('evaluate', version = '0.17', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('fansi', version = '1.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('farver', version = '2.1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('fastmap', version = '1.1.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('fs', version = '1.5.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('generics', version = '0.1.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('gitcreds', version = '0.1.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('glue', version = '1.6.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('gtable', version = '0.3.1', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('ini', version = '0.3.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('isoband', version = '0.2.6', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('jsonlite', version = '1.8.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('labeling', version = '0.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('lattice', version = '0.20-45', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('lazyeval', version = '0.2.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('magrittr', version = '2.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('mime', version = '0.12', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pkgconfig', version = '2.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('png', version = '0.1-7', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('praise', version = '1.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('prettyunits', version = '1.1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ps', version = '1.7.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rappdirs', version = '0.3.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('remotes', version = '2.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rlang', version = '1.0.6', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rprojroot', version = '2.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rstudioapi', version = '0.14', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('sourcetools', version = '0.1.7', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('stringi', version = '1.7.8', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('sys', version = '3.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('utf8', version = '1.2.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('viridisLite', version = '0.4.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('whisker', version = '0.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('withr', version = '2.5.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('xfun', version = '0.33', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('xml2', version = '1.3.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('xtable', version = '1.8-4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('yaml', version = '2.3.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('zip', version = '2.2.1', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('Matrix', version = '1.5-1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('R.oo', version = '1.25.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('RcppTOML', version = '0.1.7', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('anytime', version = '0.3.9', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('askpass', version = '1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('cachem', version = '1.0.6', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('desc', version = '1.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('diffobj', version = '0.3.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ellipsis', version = '0.3.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('here', version = '1.0.1', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('highr', version = '0.9', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('htmltools', version = '0.5.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('later', version = '1.3.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('lifecycle', version = '1.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('munsell', version = '0.5.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('nlme', version = '3.1-159', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('openxlsx', version = '4.2.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pander', version = '0.6.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('processx', version = '3.7.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('purrr', version = '0.3.5', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('rex', version = '1.2.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rlist', version = '0.4.6.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rversions', version = '2.1.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('sessioninfo', version = '1.2.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('stringr', version = '1.4.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('systemfonts', version = '1.0.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tinytex', version = '0.42', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('urlchecker', version = '1.0.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('vctrs', version = '0.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('R.utils', version = '2.12.0', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('callr', version = '3.7.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('crosstalk', version = '1.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('fontawesome', version = '0.3.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('htmlwidgets', version = '1.5.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('jquerylib', version = '0.1.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('knitr', version = '1.40', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('memoise', version = '2.0.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('mgcv', version = '1.8-40', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('openssl', version = '2.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pillar', version = '1.8.1', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('pkgload', version = '1.3.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('promises', version = '1.2.0.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('reticulate', version = '1.26', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('sass', version = '0.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('scales', version = '1.2.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('textshaping', version = '0.3.6', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tidyselect', version = '1.1.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('xopen', version = '1.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('DT', version = '0.25', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('bslib', version = '0.4.0', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('credentials', version = '1.3.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('downlit', version = '0.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('httpuv', version = '1.6.6', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('httr', version = '1.4.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pkgbuild', version = '1.3.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('profvis', version = '0.3.7', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ragg', version = '1.2.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('roxygen2', version = '7.2.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tibble', version = '3.1.8', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('covr', version = '3.6.1', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('dplyr', version = '1.0.10', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('gert', version = '1.9.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ggplot2', version = '3.3.6', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('gh', version = '1.3.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rcmdcheck', version = '1.4.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rematch2', version = '2.1.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rmarkdown', version = '2.17', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('shiny', version = '1.7.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('miniUI', version = '0.1.1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pkgdown', version = '2.0.6', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('shiny.router', version = '0.2.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('shinyWidgets', version = '0.7.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('shinycssloaders', version = '1.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('shinyjs', version = '2.1.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tidyr', version = '1.2.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('usethis', version = '2.1.6', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('waldo', version = '0.4.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('plotly', version = '4.10.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('testthat', version = '3.1.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('devtools', version = '2.4.4', upgrade = FALSE)"

# install local package(s)
RUN mkdir -p /tmp
WORKDIR /tmp
COPY dartTools /tmp/dartTools
RUN R -q -e "Rcpp::compileAttributes('dartTools'); roxygen2::roxygenize('dartTools'); install.packages(pkgbuild::build('dartTools'), repos = NULL, dependencies = FALSE)"


# Settings for dartpaths shiny app
ENV DARTPATHS_PYTHON=/usr/bin/python3
ENV DARTPATHS_DUMPPATH=/opt/dartpaths/dbdump
ENV DARTPATHS_SMILESCACHE=/opt/dartpaths/smilesCache.txt
RUN mkdir '/opt/dartpaths'
COPY ETL/data_app/smilesCache.txt /opt/dartpaths/smilesCache.txt
COPY ETL/data_app/txt_database_dump /opt/dartpaths/dbdump
CMD ["R", "-e dartTools::runDart()"]
