# Use official R Shiny image with R 4.4.3
FROM rocker/shiny:4.4.3

# Install system dependencies needed by R packages
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libxt6 \
    libfontconfig1 \
    tzdata \
    pandoc \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install R packages your app needs
RUN R -e "install.packages(c('shiny', 'ggplot2', 'rmarkdown', 'knitr'), repos='https://cloud.r-project.org')"

# Set working directory inside container
WORKDIR /srv/shiny-server

# Copy app files into the container
COPY . .

# Expose port 3591 inside container
EXPOSE 3591

# Run the Shiny app on port 3591 and all network interfaces
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3591)"]
