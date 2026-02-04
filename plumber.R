install.packages(c("plumber", "jsonlite", "TAM"))



library(plumber)

# Load your API
r <- plumb("api.R")

# Use Render PORT
port <- as.numeric(Sys.getenv("PORT", 8080))

# Start API
r$run(host="0.0.0.0", port=port)