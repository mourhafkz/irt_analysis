# Use a Rocker image with basic R tools
FROM rocker/r-ver:4.3.1

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('plumber','jsonlite','TAM'), repos='https://cloud.r-project.org')"

# Set working directory
WORKDIR /app
COPY . /app

# Expose Render port
EXPOSE 8080

# Start API using plumber.R
CMD ["R", "-e", "source('plumber.R')"]
