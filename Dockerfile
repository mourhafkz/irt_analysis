# Use a Debian-based R image
FROM rstudio/r-base:4.3.1

# Install system dependencies for TAM, jsonlite, plumber
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('plumber','jsonlite','TAM'), repos='https://cloud.r-project.org')"

# Set working directory
WORKDIR /app
COPY . /app

# Expose the port
EXPOSE 8080

# Start the API
CMD ["R", "-e", "source('plumber.R')"]
