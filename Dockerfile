FROM rocker/rstudio:latest

RUN yes | unminimize && \
    apt update && \
    apt install -y man-db manpages && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 8787