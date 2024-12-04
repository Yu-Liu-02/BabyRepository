FROM rocker/verse:latest

RUN apt-get update && apt-get install -y \
    software-properties-common \
    git \
    libx11-6 \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "install.packages(c('dplyr','tidyverse','reshape2','ggplot2','RColorBrewer','xgboost','pROC','e1071'), repos = 'https://cloud.r-project.org/', dependencies = TRUE, quiet = TRUE)"


RUN apt-get update -y && apt-get install -y \
    texlive-base \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    lmodern \
    && apt-get clean
# RUN tlmgr init-usertree
# RUN tlmgr update --self
# RUN tlmgr install pdftexcmd

WORKDIR /Result

Run R -e "install.packages('tibble',repos = 'https://cloud.r-project.org/', dependencies = TRUE, quiet = TRUE)"

CMD ["tail", "-f", "/dev/null"]
