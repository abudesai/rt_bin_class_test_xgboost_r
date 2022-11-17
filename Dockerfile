FROM rocker/tidyverse:4


# Install dependencies
RUN apt-get update --allow-releaseinfo-change && apt-get install -y \
    liblapack-dev \
    libpq-dev \
    build-essential libssl-dev libxml2-dev libcurl4-gnutls-dev  



COPY ./requirements.txt .

RUN R -e "install.packages('xgboost', dependencies=T)"
RUN R -e "install.packages('rjson', dependencies=T)"
RUN R -e "install.packages('caTools', dependencies=T)"
RUN R -e "install.packages('imputeTS', dependencies=T)"
RUN R -e "install.packages('tictoc', dependencies=T)"
RUN R -e "install.packages('pROC', dependencies=T)"
RUN R -e "install.packages('plumber', dependencies=T)"


# RUN R -e \
#     "install.packages( \
#         readLines('requirements.txt'), \
#         repos = 'http://cran.us.r-project.org' \
#     )"




COPY app ./opt/app
WORKDIR /opt/app


ENV PATH="/opt/app:${PATH}"

RUN chmod +x train &&\
    chmod +x test &&\
    chmod +x serve 

