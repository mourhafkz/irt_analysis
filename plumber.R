install.packages(c("plumber", "jsonlite", "TAM"))

library(plumber)

# Load your API
r <- plumb("api.R")

# Use Render-provided PORT
port <- as.numeric(Sys.getenv("PORT", 8080))

# Start the server
r$run(host="0.0.0.0", port=port)
