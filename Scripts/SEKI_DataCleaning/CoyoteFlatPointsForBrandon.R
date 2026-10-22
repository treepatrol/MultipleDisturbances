library(sf)

# 1. Read your clean database export CSV
df <- read.csv("/Users/jennifercribbs/Documents/SEKI_beetles/Fieldwork/2026/CoyoteFlat_PICO3aPIAL0.csv", stringsAsFactors = FALSE)

# 2. Convert UTM (Zone 11N / EPSG 26911) -> Lat/Lon (WGS84 / EPSG 4326)
sf_obj <- st_as_sf(df, coords = c("Easting", "Northing"), crs = 26911)
sf_latlon <- st_transform(sf_obj, crs = 4326)

# 3. Explicitly set the display label column
sf_latlon$Name <- df$Name

# 4. Write directly to KML format (overwrites existing)
st_write(sf_latlon, "Plot31_Trees.kml", driver = "KML", delete_dsn = TRUE)

cat("\nSUCCESS! 'Plot31_Trees.kml' was generated.\n")
