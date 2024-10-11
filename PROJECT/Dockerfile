FROM rocker/verse:latest
RUN apt update && apt install -y \
    && rm -rf /var/lib/apt/lists/*
RUN apt update && apt install git

RUN yes | unminimize && \
    apt update && \
    apt install -y man-db manpages && \
    rm -rf /var/lib/apt/lists/*


RUN git --version
RUN apt-get update && apt-get install -y python3 python3-pip
RUN R -e "install.packages(c('ggplot2', 'rmarkdown', 'tidyverse'))"

CMD ["tail", "-f", "/dev/null"]
EXPOSE 8787
