# Starter script for Shawn

HEpointframe <- read.csv("data/QHI_biodiversity/Herschel_ITEXdata_1999-2018_updated.csv")
levels(HEpointframe$SPP) # Checking all spelling is correct
unique(HEpointframe$YEAR) # Check years

HEcover <- read.csv("data/QHI_biodiversity/QHI_cover_1999_2018_ITEX_updated.csv")
levels(HEcover$name) # Checking all spelling is correct
unique(HEcover$year) # Check years

# Integrate 2019 data

# Check plot order using cover of different species - especially in KO

# Confirm species IDs and spellings are consistent

# Calculate biodiversity metrics
