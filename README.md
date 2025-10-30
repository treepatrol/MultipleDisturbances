# Multiple Disturbances and Tree Resilience in the Southern Sierra Nevada
Scripts in this folder bring in data from the project Transcribed Data folder on Google Drive, clean the data, and run analyses. 
Scripts are designed to be run in order, but can also be used as building blocks for a different work flow: scripts with a 0 bring in user defined functions into the global environment that can be used in scripts later in the series. The gbif script series cleans and wrangles the data to be compatible with NPS and GBIF standards. The understory script series reads in the raw data from the Google Drive and applies a function to transform the data from a series of lists to a data frame and converts the data format from wide to long format. The third part of the understory script species cleans up the raw data by recoding unknown species, and taking care of misspellings and other inconsistencies. Part 4 calculates species richness for understory species. Part 5 conducts a preliminary EDA with the understory richness data. SEKI scripts clean data for import into a relational database locally running PostgreSQL using PGAdmin. 

## Data Directory

```

Data/           Data sets (located on Google Drive)

└── rawData/        Downloaded, unprocessed (raw) data

└── cleanData/      Cleaned data


Output/           CSV files for NPS Species Inventory Excel File


Scripts/              R source code for cleaning data

└── Exploratory/        Scripts that are shared, but in progress



README.md

```
