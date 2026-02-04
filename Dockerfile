# Use official R image
FROM r-base:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('plumber','jsonlite','TAM'), repos='https://cloud.r-project.org')"

# Set working directory
WORKDIR /app
COPY . /app

# Expose the port Render will use
EXPOSE 8080

# Start the API
CMD ["R", "-e", "source('plumber.R')"]