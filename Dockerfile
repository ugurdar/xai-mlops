FROM rocker/r-ver:4.4.1

# System dependencies for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev \
    libharfbuzz-dev libfribidi-dev \
    libpng-dev libjpeg-dev libtiff-dev zlib1g-dev cmake \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Layer 1: renv restore (cached until renv.lock changes)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
RUN Rscript -e "renv::restore(prompt = FALSE)"

# Layer 2: data + scripts + master runner
COPY data/ data/
COPY scripts/ scripts/
COPY reproduce.R reproduce.R

RUN mkdir -p results/tables results/plots

ENTRYPOINT ["Rscript", "reproduce.R"]
