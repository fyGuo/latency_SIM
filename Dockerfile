FROM rocker/r-ver:4.5.3

# System libraries required by the R packages below
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libgit2-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libfontconfig1-dev \
    liblapack-dev \
    libblas-dev \
    libsodium-dev \
    cmake \
    pandoc \
    git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# rocker sets ${CRAN} to the correct P3M binary URL for this image's Ubuntu version.
# Re-register it in Rprofile so all R -e calls in subsequent layers pick it up.
RUN echo "options(repos = c(CRAN = '${CRAN}'))" \
    >> /usr/local/lib/R/etc/Rprofile.site

# Install pak (fast parallel package installer)
RUN R -e "install.packages('pak')"

# Install all CRAN packages (split into batches to isolate failures)
RUN R -e "pak::pak(c( \
  'abind','askpass','backports','base64enc','bit','bit64','bitops','blob', \
  'brew','brio','broom','bslib','cachem','callr','car','carData', \
  'caTools','cellranger','checkmate','cli','clipr','collections','colorspace', \
  'commonmark','conflicted','corrplot','cowplot','cpp11','crayon','credentials' \
))"

RUN R -e "pak::pak(c( \
  'curl','cvAUC','data.table','DBI','dbplyr','Deriv','desc','devtools', \
  'diffobj','digest','doBy','downlit','dplyr','dtplyr','ellipsis','evaluate', \
  'fansi','farver','fastmap','fontawesome','forcats','foreach','forecast', \
  'Formula','fracdiff','fs','furrr','future' \
))"

RUN R -e "pak::pak(c( \
  'gam','gargle','geex','generics','gert','ggplot2','ggpubr','ggrepel', \
  'ggsci','ggsignif','gh','gitcreds','globals','glue','gmm','googledrive', \
  'googlesheets4','gplots','gridExtra','gtable','gtools','haven','highr', \
  'Hmisc','hms','htmlTable','htmltools','htmlwidgets','httpuv','httr','httr2' \
))"

RUN R -e "pak::pak(c( \
  'ids','ini','isoband','iterators','jquerylib','jsonlite','knitr','labeling', \
  'languageserver','later','lifecycle','lintr','listenv','lme4','lmtest', \
  'lubridate','magrittr','MatrixModels','memoise','microbenchmark','mime', \
  'miniUI','minqa','modelr','nloptr','nnls','numDeriv','openssl','otel' \
))"

RUN R -e "pak::pak(c( \
  'pak','parallelly','pbkrtest','pillar','pkgbuild','pkgconfig','pkgdown', \
  'pkgload','polynom','praise','prettyunits','processx','profvis','progress', \
  'promises','ps','purrr','quantreg','R.cache','R.methodsS3','R.oo','R.utils', \
  'R6','ragg','ranger','rappdirs','rbibutils','rcmdcheck','RColorBrewer' \
))"

RUN R -e "pak::pak(c( \
  'Rcpp','RcppArmadillo','RcppEigen','Rdpack','readr','readxl','reformulas', \
  'rematch','rematch2','remotes','reprex','rex','rlang','rmarkdown','ROCR', \
  'rootSolve','roxygen2','rprojroot','rstatix','rstudioapi','rversions','rvest' \
))"

RUN R -e "pak::pak(c( \
  'S7','sandwich','sass','scales','selectr','sessioninfo','shiny','sourcetools', \
  'SparseM','stringi','stringr','styler','SuperLearner','sys','systemfonts', \
  'testthat','textshaping','tibble','tictoc','tidyr','tidyselect','tidyverse', \
  'timechange','timeDate','tinytex','tzdb','urca','urlchecker','usethis', \
  'utf8','uuid','vctrs','viridisLite','vroom','waldo','whisker','withr', \
  'xfun','xgboost','xml2','xmlparsedata','xopen','xtable','yaml','zip','zoo' \
))"

# Install LatencyA from GitHub
RUN R -e "pak::pak('fyGuo/LatencyA')"

# SSH server so future::makeClusterPSOCK can start R workers remotely
RUN apt-get update && apt-get install -y --no-install-recommends openssh-server \
    && apt-get clean && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /var/run/sshd /root/.ssh \
    && chmod 700 /root/.ssh \
    && sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config \
    && sed -i 's/PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config \
    && echo "PasswordAuthentication no" >> /etc/ssh/sshd_config \
    && echo "PubkeyAuthentication yes"  >> /etc/ssh/sshd_config

WORKDIR /simulation

EXPOSE 22
CMD ["sh", "-c", "mkdir -p /run/sshd && /usr/sbin/sshd -D"]
