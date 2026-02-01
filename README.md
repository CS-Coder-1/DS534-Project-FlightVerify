## Quick Start
```r
# Install
devtools::install_github("yourusername/flightverify")

# Load package
library(flightverify)

# Get current aircraft states
states <- get_states(auth = FALSE)
head(states)

# Get recent arrivals at Vancouver airport
end_time <- Sys.time()
begin_time <- end_time - (6 * 3600)  # Last 6 hours
arrivals <- get_airport_arrivals("CYVR", begin_time, end_time, auth = FALSE)

# Get Canadian airports reference
airports <- get_canadian_airports()
```

## Authentication

OpenSky Network requires OAuth2 authentication for higher rate limits.

### Setup

1. Create account at https://opensky-network.org/
2. Generate API credentials in account settings
3. Save credentials in `.Renviron`:
```
   OPENSKY_CLIENT_ID=your_client_id
   OPENSKY_CLIENT_SECRET=your_client_secret
```

### Usage
```r
library(flightverify)

# Authenticate with OAuth2
opensky_auth(
  client_id = Sys.getenv("OPENSKY_CLIENT_ID"),
  client_secret = Sys.getenv("OPENSKY_CLIENT_SECRET")
)

# Or use anonymous access (limited rate)
opensky_auth_anonymous()
```