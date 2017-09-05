
# U.S. Department of Labor
# Bureau of Labor Statistics 
# Quarterly Census of Employment and Wages
# June 2016
#  
# QCEW Open Data Access RScript Example
#
# R Language 3.1.0 
#
# The three functions defined in this file are called at the bottom 
# of this script. So, if you run this code, you'll get some results.
#
# Submit questions at: http://www.bls.gov/cgi-bin/forms/cew?/cew/home.htm 
#

# ******************************************************************************************
# September 2016 - This script can be used to determine Tennessee employment by two-digit
# through six-digit NAICS industry codes as well as Tennessee employment by QCEW Establishment
# size classes.
# ******************************************************************************************


# ******************************************************************************************
# June 2017 - This script is not working properly. Using the qcewGetAreaData function
# returns an error message -     Error in file(file, "rt") : cannot open the connection
# Added curl package to make it work
# ******************************************************************************************

load_pkg <- c('curl')      

for (i in 1:length(load_pkg)) {
  
  if (!require(load_pkg[i], character.only=T)){
    install.packages(load_pkg[i], 
                     repos="https://cran.rstudio.com/",
                     quiet=TRUE)
    require(load_pkg[i], character.only=T) #same as library(package name)
  }
}





# qcewGetAreaData : This function takes a year, quarter, and area argument and
# returns an array containing the associated area data. use 'a' for annual
# averages. 
# For all area codes and titles see:
# http://data.bls.gov/cew/doc/titles/area/area_titles.htm

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(curl(url)) ######, header = TRUE,  sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# ******************************************************************************************



# ******************************************************************************************
# qcewGetIndustryData : This function takes a year, quarter, and industry code
# and returns an array containing the associated industry data. Use 'a' for 
# annual averages. Some industry codes contain hyphens. The CSV files use
# underscores instead of hyphens. So 31-33 becomes 31_33. 
# For all industry codes and titles see:
# http://data.bls.gov/cew/doc/titles/industry/industry_titles.htm

qcewGetIndustryData <- function (year, qtr, industry) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/industry/INDUSTRY.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("INDUSTRY", industry, url, ignore.case=FALSE)
  read.csv(curl(url)) ######, header = TRUE,  sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# ******************************************************************************************




# ******************************************************************************************
# qcewGetSizeData : This function takes a year and establishment size class code
# and returns an array containing the associated size data. Size data
# is only available for the first quarter of each year.
# For all establishment size classes and titles see:
# http://data.bls.gov/cew/doc/titles/size/size_titles.htm

qcewGetSizeData <- function ( year, size) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/1/size/SIZE.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("SIZE", size, url, ignore.case=FALSE)
  read.csv(curl(url)) ######, header = TRUE,  sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# ******************************************************************************************



# EXAMPLES ----------------------------------------------------

# Get 2014 and 2015 Tennessee Data using a loop

TennesseeData2014 <- qcewGetAreaData("2014", "A", "47000")
TennesseeData2015 <- qcewGetAreaData("2015", "A", "47000")

# Use dplyr::bind_rows to join the years

TennesseeData <- dplyr::bind_rows(TennesseeData2014, TennesseeData2015)

rm(TennesseeData2014, TennesseeData2015)

# Construction <- qcewGetIndustryData("2015", "1", "23")
# SizeData <- qcewGetSizeData("2015","6")


 # Prints first line of data
# TennesseeData[1, ]
# Construction[1, ]
# SizeData[1, ]

###################################################################################################
# We start with all state (TN) data
# Then get only private ownership observations
privateTN <- TennesseeData[which(TennesseeData$own_code == 5),]

# Next, create a variable for the number of characters in the NAICS industry code
privateTN$digits <- nchar(as.character(privateTN$industry_code))

# Next, trim the NAICS industry code to two digits and name the varible twodigit
privateTN$twodigit <-  as.factor(strtrim(privateTN$industry_code,2))

# Keep only the six digit industries
QCEWprivateTNsix <- privateTN[which(privateTN$digits==6),]
# Keep only the six digit industries in construction
constQCEWprivateTNsix <- QCEWprivateTNsix[which(QCEWprivateTNsix$twodigit=='23'),]

# Keep only the five digit industries
QCEWprivateTNfive <- privateTN[which(privateTN$digits==5),]
# Keep only the five digit industries in construction
constQCEWprivateTNfive <- QCEWprivateTNfive[which(QCEWprivateTNfive$twodigit=='23'),]

# Keep only the four digit industries
QCEWprivateTNfour <- privateTN[which(privateTN$digits==4),]
# Keep only the four digit industries in construction
constQCEWprivateTNfour <- QCEWprivateTNfour[which(QCEWprivateTNfour$twodigit=='23'),]

# Keep only the three digit industries
QCEWprivateTNthree <- privateTN[which(privateTN$digits==3),]
 # Keep only the three digit industries in construction
constQCEWprivateTNthree <- QCEWprivateTNthree[which(QCEWprivateTNthree$twodigit=='23'),]

# Keep only the two digit industries
QCEWprivateTNtwo <- privateTN[which(privateTN$digits==2),]
# Keep only the two digit industries in construction
constQCEWprivateTNtwo <- QCEWprivateTNtwo[which(QCEWprivateTNtwo$twodigit=='23'),]

# Make a table with only the 4 digit industries and annual industry employment numbers
QCEW_TNprivateFourTable <- QCEWprivateTNfour[,c(3,10)]

# Make an employment table and extract to a csv
write.table(QCEW_TNprivateFourTable, file= "/Users/edwardtaylorUTK/Desktop/2016 Work Comp/TNprivateFourTable.csv", sep=",", row.names=F)
##################   IMPORTANT: NOTE THAT THE FIRST 11 ROWS OF THE TABLE ARE INDUSTRY SUPERSECTORS ###################

################################################################################################################
# This section uses qcewGetSizeData to construct a dataframe of TN private industry - all size classes
################################################################################################################
# QCEW establishment size classes 1 through 9

codes <- c('1','2','3','4','5','6','7','8','9')
 
tnSizeData <- data.frame()
# Use a loop to get extract every size category
for (i in 1:length(codes)){
  SizeData <- qcewGetSizeData("2015", i)
  SizeData <- SizeData[SizeData$area_fips=="47000",]
  tnSizeData <- rbind(tnSizeData, SizeData)
}
# Get private establishments only (own_code==5) in the first quarter
Private_tnSizeData <- tnSizeData[tnSizeData$own_code==5 & tnSizeData$qtr==1,]
