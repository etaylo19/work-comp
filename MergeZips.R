# May 16, 2017
# ELT
# Start with text files CountyFIPS,fips, and zcta_county
# This creates the file TNzip which will be needed to define which ZCs are specific to TN
#           and possibly aggregate industries by TN county
# The final few lines create injury tables by TN zip code only

setwd("/Users/edwardtaylorUTK/Desktop/2015WCfiles")


# Load packages
load_pkg <- c('dplyr', 'readr', 'data.table')      

for (i in 1:length(load_pkg)) {
  
  if (!require(load_pkg[i], character.only=T)){
    install.packages(load_pkg[i], 
                     repos="https://cran.rstudio.com/",
                     quiet=TRUE)
    require(load_pkg[i], character.only=T) #same as library(package name)
  }
}

# fips and CountyFIPS previously created from publically available (? US Census) data

# 
# fips <- read_csv("~/Desktop/2015WCfiles/fips.txt",
#                  col_names = FALSE)
# colnames(fips) <- c("Zip", 'StateFIPS', 'CountyFIPS' )
# 
# 
# 
# CountyFIPS <- read_csv("~/Desktop/2015WCfiles/CountyFIPS.txt",
#                        col_names = FALSE    )
# colnames(CountyFIPS) <- c('State', 'StateFIPS', 'CountyFIPS', 'County')
# 
# 
# # Drop extraneous column
# CountyFIPS <- CountyFIPS[,1:4]
# 
# 
# STcountyFIPS <- merge(CountyFIPS, fips, by=c("StateFIPS","CountyFIPS")) # NA's match

# Filtered StateCounty file to get only TN counties; exported to csv 
  # then manually added East, Middle, West to counties; stored as TNzip.csv 

# TNzip <- STcountyFIPS[which(STcountyFIPS$StateFIPS=='47'),]

# Read stored file w/ EAST, MIDDLE, WEST divisions & county designations

TNzip <- read_csv(paste0("./TNzipJohn.csv" ),
      col_types = cols(CountyFIPS = col_character(),
                       StateFIPS = col_character(), 
                       Zip = col_character(),
                      `Original County` = col_skip()) )


# We know that a single zip code can appear in two counties; e.g. 38305 is both Madison and Carroll counties
# This can be an issue; find the number of duplicates in the Zip column of TNzip


DupCounties <- sum(duplicated(TNzip$Zip)) # there are 358 duplicates; this is more than expected; 
# Therefore, you can't assign count injury sites by county
# In fact, there are cases when the same zip code occurs in both EAST and MIDDLE; e.g. Sequatchie County	37365	EAST
        # and Grundy County	37365	MIDDLE; therefore even counting injury sites by EAST, MIDDLE, WEST will have some error
# We will attempt to 'assign' a zip code to a single county by visually determining the predominate county with a ZC

# Remove duplicates
TNzip <- TNzip[!duplicated(TNzip$Zip),]
