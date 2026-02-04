# Official R base image
FROM r-base:latest

# Install system deps needed for plumber and other packages
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
 && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('plumber','jsonlite','TAM'), repos='https://cloud.r-project.org')"

# Copy app
WORKDIR /app
COPY . /app

# Expose default Render HTTP port
EXPOSE 8080

# Start API using plumber.R
CMD ["R", "-e", "source('plumber.R')"]
