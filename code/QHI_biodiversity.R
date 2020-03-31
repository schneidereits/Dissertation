# Starter script for Shawn

QHI_pointframe <- read.csv("data/QHI_biodiversity/Herschel_ITEXdata_1999-2018_updated.csv")
head(QHI_pointframe)
levels(QHI_pointframe$SPP) # Checking all spelling is correct
unique(QHI_pointframe$YEAR) # Check years

QHI_cover <- read.csv("data/QHI_biodiversity/QHI_cover_1999_2018_ITEX_updated.csv")
head(QHI_cover)
levels(QHI_cover$name) # Checking all spelling is correct
unique(QHI_cover$year) # Check years

# Integrate 2019 data

# Check plot order using cover of different species - especially in KO

# Confirm species IDs and spellings are consistent

# Calculate biodiversity metrics
