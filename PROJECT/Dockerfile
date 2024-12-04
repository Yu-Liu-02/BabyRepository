FROM rocker/verse:latest

RUN apt-get update && apt-get install -y \
    software-properties-common \
    git \
    libx11-6 \
    && rm -rf /var/lib/apt/lists/*

RUN add-apt-repository ppa:deadsnakes/ppa && \
    apt-get update && \
    apt-get install -y python3.12 python3.12-distutils python3.12-venv && \
    rm -rf /var/lib/apt/lists/*

RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.12 1

RUN python3 --version

RUN apt-get install -y python3-pip

RUN R -e "install.packages(c('ggnewscale', 'tidyverse', 'corrplot', 'pROC', 'factoextra', 'foreach', 'ggplot2'), repos = 'https://cloud.r-project.org/', dependencies = TRUE, quiet = TRUE)"

RUN add-apt-repository ppa:kelleyk/emacs && \
    apt-get update && \
    apt-get install -y emacs && \
    rm -rf /var/lib/apt/lists/*

RUN emacs --version

RUN git --version

CMD ["tail", "-f", "/dev/null"]
