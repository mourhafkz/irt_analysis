library(plumber)

# Load your API script
r <- plumb("api.R")

# Use the PORT from Render (or default to 8080 locally)
port <- as.numeric(Sys.getenv("PORT", 8080))

# Run the API
r$run(host = "0.0.0.0", port = port)