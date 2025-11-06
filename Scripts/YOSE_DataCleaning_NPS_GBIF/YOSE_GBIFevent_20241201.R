# GBIF: Event Tab
# Authors: Jenny Cribbs
# Date: 09 September 2024

# Overall Input: Read in csv files for plot-level data, fire severity data, and PRISM data
# Overall Output: Clean csv with a row for each plot and columns matching GBIF columns, so the output can be pasted into NPS template Event tab

# read in clean plot data
plotData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/PlotLevelData.csv")

# bring in PRISM data
prismData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/PRISMdata_YOSEonlyApril29_24.csv")

# bring in merged ground and MTBS fire severity data
fireData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/fireCompare_22052024.csv")

# bring in estimated effort
effortData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/estimatedEffort.csv") %>% 
  rename(plotID = "YPE.Plot.ID", effort = "Estimated.Effort..in.hrs.",
         startDateTime = "Start.Date.time",
         endDateTime = "End.Date.time") %>% 
  select(plotID, effort, startDateTime, endDateTime)

# join prism and fire data then join to plot data
prismFire <- left_join(fireData, prismData, by = c("plotID" = "PlotID"))
data <- left_join(plotData, prismFire)

# add estimated effort data
combinedData <- left_join(data, effortData)

# rename and add columns to match template
eventData <- combinedData %>% rename(eventID = plotID, 
                                     samplingProtocol = plot_type,
                                     samplingEffort = effort,
                                     verbatimLatitudeStart = plot_beg_UTM_N,
                                     verbatimLongitudeStart = plot_beg_UTM_E,
                                     verbatimLatitudeStop = plot_end_UTM_N,
                                     verbatimLongitudeStop = plot_end_UTM_E,
                                     eventRemarks = plot_notes,
                                     azimuth = plot_azimuth) %>% 
  mutate(parentEventID = "", 
         sampleSizeValue = trans_length * width_pila, 
         sampleSizeUnit = "square meters",
         waterFeatureName = "",
         countryCode = "US",
         stateProvince = "California",
         county = "Mariposa", # Mt. Raymond and plot south of fish camp are in Madera and several in Tuolumne
         habitatDescription = "mixed-conifer forest",
         verbatimCoordinateSystem = "UTM",
         verbatimSRS = "EPSG:26911", # 26910 for zone 10 plots or 4326 for lat long for all
         verbatimDatum = "NAD 83",# WGS84 for lat/long
         coordinateUncertaintyinMeters = "", # maybe add GPS field uncertainty with metadata notes 
         coordinatePrecision = "", # this may be a feature of the GPS units
         publicDisplay = "yes",
         dataAccess = "unrestricted",
         fieldNumber = "",
         fieldNotes = "",
         verbatimElevation = (plot_elevation_ft * 3.28084), # convert to meters
         verbatimDepth = "",
         waterFeatureSubstrate = "",
         locationRemarks = "",
         footprintWKT = "",
         footprintSRS = "", 
         verbatimLatitude = verbatimLatitudeStart,
         verbatimLongitude = verbatimLongitudeStart)

eventExcelData <- eventData %>% select (eventID, parentEventID, samplingProtocol, sampleSizeValue, sampleSizeUnit, samplingEffort, startDateTime, endDateTime, waterFeatureName, countryCode, stateProvince, county, habitatDescription, verbatimLatitude, verbatimLongitude, verbatimLatitudeStart, verbatimLongitudeStart, verbatimLatitudeStop, verbatimLongitudeStop, verbatimCoordinateSystem, verbatimSRS, verbatimDatum, coordinateUncertaintyinMeters, coordinatePrecision, publicDisplay, dataAccess, fieldNumber, fieldNotes, eventRemarks, verbatimElevation, verbatimDepth, waterFeatureSubstrate, locationRemarks, footprintWKT, footprintSRS, slope, aspect, azimuth)

write.csv(eventExcelData, "eventExcelTab.csv")

# Spatial data wrangling to fix up for December deadline
# For now, changing 2 zone 10 plots by hand in Excel sheet (YPE32, YPE50) to zone 10
# Changed plots 10 and 43 to Madera county by hand
# Changed Tuolume County plots based on eyeballing county line: 3 plots near Smith Mountain (199-201, 208-209, 202-206), 3 near Hetch Hetchy (1281-1301, 0941, 881-931), Cathedral Creek, Mount Hooper, 2 near Lake Eleanor (247-248, 137-138), 266-264 (plot44?), 0091-0092, 01751-01761 (return of the saplings?), 0093-0094, 0095-0096, 01871-01861, North Crane Creek, and Tuolume Grove, bald mountain YPE 5, Aspen Valley Area (0255-0254, 01311-01321, Yosemite Wilderness, need to be changed to Tuolume county
