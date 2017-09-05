# ELT 
# May 30, 2017
# Rev1 (May 31) improved function's capability; changed data frame names
# Rev1 (June 13) reworked QCEWprivateTNtwo to include 31-33, 44-45, 48-49
# Rev2 (July 7) changed line 82 of the firm_emp_func to eliminate an error in the compilation of UI employment
#     as an example Naics 4811 shows only 9 EEs for 2015; NAICS 481112 has only 9 but 481111 has many more

##### NOTES ####
# This is a script to estimate annual employemnt numbers for firms by 2 digit, 3 digit, & 4 digit NAICS codes as well as QCEW firm sizes
# There is a function called     firm_emp_func
# It does all the work. The output of the funcion is a list

# To output employment table for a data set:
#  use this syntax             Name_emp <- firm_emp_func(Name)[[1, 2, 3, 4, 5, or 6]]
# 1 through 6 represent elements of the list
# where 1 gives employment by CIRPC_GUID (i.e. at the firm level)
# where 2 gives the sum of employment for this data set (i.e. the sum of all employment for firms with claims)
        # this will be for all years; if a single year is desired; change the filter in line 60
# where 3 gives employment by 4 digit Naics - Naics4_emp
# where 4 gives employment by 3 digit Naics - Naics3_emp
# where 5 gives employment by 2 digit Naics - Naics2_emp and 
# where 6 gives employment by industry size category - firm_sz_emp

##### END NOTES ####


# First, set the working directory to the folder where the data exist
setwd("/Users/edwardtaylorUTK/Desktop/2015WCfiles")

# To empty the current environment in R Studio,
#   uncomment and execute the next line

#          rm(list=ls())

#### LOAD DESIRED PACKAGES ####

load_pkg <- c('dplyr', 'data.table', 'curl')      

for (i in 1:length(load_pkg)) {
  
  if (!require(load_pkg[i], character.only=T)){
    install.packages(load_pkg[i], 
                     repos="https://cran.rstudio.com/",
                     quiet=TRUE)
    require(load_pkg[i], character.only=T) #same as library(package name)
  }
}


#### END LOAD DESIRED PACKAGES ####


##### REMEMBER THAT WE WILL BE WORKING LARGELY WITH PRIVATE, SINGLE ESTABLISHMENT FIRMS ###################

# Use dplyr inside a function to get employment estimates BY FIRM
 firm_emp_func <- function(df){                    # beginning of firm_emp_func
# First get the average quarterly employment
      
      table_emp <-      df %>% 
                        group_by(inj_yr, CIRPC_GUID) %>%
                        arrange(CIRPC_GUID) %>%
                        select(inj_yr, CIRPC_GUID, EMPL1, EMPL2, EMPL3, QCEW_NAICS_CODE, Naics2, Naics3, Naics4, firm_size_cat) %>% 
                        mutate (avg_qtr_emp = (EMPL1 + EMPL2 + EMPL3)/3) %>%
                        filter(inj_yr == '2014' | inj_yr =='2015')
      
      # Then keep only the rows having a distinct value of EMPL1 (i.e. there will be 1 to 4 distinct rows per year)
      
      distinctEMPL1 <-  distinct(table_emp, EMPL1, .keep_all=T)
      
      # Take the average of the distinct rows to get an estimate of average annual employment by firm
      
      annual_emp_firm <-  distinctEMPL1 %>%
        group_by(inj_yr, CIRPC_GUID) %>%
        select(inj_yr, CIRPC_GUID, EMPL1, avg_qtr_emp, QCEW_NAICS_CODE, Naics2, Naics3, Naics4, firm_size_cat) %>% 
        group_by(inj_yr, CIRPC_GUID, QCEW_NAICS_CODE) %>%             # This may create some duplicates if a firm has multiple QCEW Naics codes
        summarise( firm_avg_annual_emp = round(mean(avg_qtr_emp)))    # e.g. CIRPC_GUID = '99EC9BF3-C2C6-41D4-8BA3-AB0763F09A3B'
      
      # Remove duplicated firms; some firms may have changed size enough to be in two or more firm size categories (see note above)
      
      
      ######################## COMMENT OUT THE NEXT LINE --- IT WORKS WITH A SINGLE YEAR ---- BUT REMOVES GOOD INFORMATION WITH MULTIPLE YEARS ###################
      # annual_emp_firm <- annual_emp_firm[!duplicated(annual_emp_firm$CIRPC_GUID) , ]
      
      emp_estimate_firm <- sum(annual_emp_firm$firm_avg_annual_emp)   # List item [[1]]
      
      # Now we have employment tables with the year, unique firm identifier, 6 digit Naics and avg. annual employment
      # It is necessary to add information to these tables (i.e. Naics2, Naics3, Naics4, firm_size_cat) 
      # Rather than use a join on CIRPC_GUID (which could present problems with same firms in different years),
      # Create the variables instead 
      
    
                  annual_emp_firm$Naics4 =  substring(annual_emp_firm$QCEW_NAICS_CODE,1,4)
                  annual_emp_firm$Naics3 =  substring(annual_emp_firm$QCEW_NAICS_CODE,1,3)
                  annual_emp_firm$Naics2 =  substring(annual_emp_firm$QCEW_NAICS_CODE,1,2)
      
      # Need to create levels for firm size based on QCEW
      # Groups are (< 5, 5-9, 10-19, 20-49, 50-99, 100-249, 250-499, 500-999, 1000+)
      # Create variable firm_size_category
      
      annual_emp_firm$firm_size_cat <- cut(annual_emp_firm$firm_avg_annual_emp, c(0,5,10,20,50,100,250,500,1000,1000000), 
                                      labels=c('0-4', '5-9', '10-19', '20-49', '50-99', '100-249', '250-499', '500-999', '1000+'), right=F)
      # annual_emp_firm is a table which  is list item [[1]]
      
      
      
      # Now get the employment numbers by 4 digit NAICS; list item [[3]]
      Naics4_emp <- annual_emp_firm %>%
                      group_by(inj_yr, Naics4) %>%
                      select(inj_yr, Naics4, firm_avg_annual_emp) %>%
                      summarise(ind4_emp = sum(firm_avg_annual_emp))
      
      # Now get the employment numbers by 3 digit NAICS; list item [[4]]
      Naics3_emp <- annual_emp_firm %>%
        group_by(inj_yr, Naics3) %>%
        select(inj_yr, Naics3, firm_avg_annual_emp) %>%
        summarise(ind3_emp = sum(firm_avg_annual_emp))
      
      # Now get the employment numbers by 2 digit NAICS; list item [[5]]
      Naics2_emp <- annual_emp_firm %>%
        group_by(inj_yr, Naics2) %>%
        select(inj_yr, Naics2, firm_avg_annual_emp) %>%
        summarise(ind2_emp = sum(firm_avg_annual_emp))
      
      # Now get the employment numbers by industry size; list item [[6]]
      firm_sz_emp <- annual_emp_firm %>%
        group_by(inj_yr, firm_size_cat) %>%
        select(inj_yr, firm_size_cat, firm_avg_annual_emp) %>%
        summarise(sz_cat_emp = sum(firm_avg_annual_emp))
                  
                 
      emp_list <- list(annual_emp_firm, paste('The employment estimate is', emp_estimate_firm), Naics4_emp, Naics3_emp, 
                       Naics2_emp, firm_sz_emp)

}       # End of firm_emp_func

# Employ the function; the employment estimates will only be valid ONLY for Matched data because unmatched data has no 
        # associated employment numbers

# To get the total employment (for all years), print the second item in the emp_list
# Examples
 
print(firm_emp_func(M_data)[[2]])    # Matches Employment- includes all private, public, & all establishments 

print(firm_emp_func(MP_ND_data))[[2]]    # Private Employment- includes all private, public, & all establishments w/o duplicates

print(firm_emp_func(MP_SE_ND_data))[[2]]    # Matched Private Single Establishment Employment- 
                                                                  # includes all private single establishments w/o duplicates



# Comment out the lines below until needed

# # Here is an example of outputting a firm employment table
# Output_table_name  <- firm_emp_func(TABLE_NAME)[[1]] # To get the table itself, get the first item in the emp_list



# This next section will require the QCEW private employment data for all years under consideration (i.e. 2014 and 2015)

.qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(curl(url)) ######, header = TRUE,  sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}


.qcewGetSizeData <- function ( year, size) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/1/size/SIZE.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("SIZE", size, url, ignore.case=FALSE)
  read.csv(curl(url)) ######, header = TRUE,  sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# Get 2014 and 2015 Tennessee Data for all industries
TennesseeData2014 <- .qcewGetAreaData("2014", "A", "47000")
TennesseeData2015 <- .qcewGetAreaData("2015", "A", "47000")

# Use dplyr::bind_rows to join the years
TennesseeDataArea <- dplyr::bind_rows(TennesseeData2014, TennesseeData2015)[, c(1:3, 6:7, 9:10)]
# Remove unneeded files
rm(TennesseeData2014, TennesseeData2015)




########################################################################################
# Now get the QCEW Size Data for year 2015
# Initialize the data frame
QCEWSizeData <- data.frame()

sz_class <- c('1', '2', '3', '4', '5', '6', '7', '8', '9') #  for all size classes

for (i in 1:length(sz_class)){
                a <- .qcewGetSizeData("2015", sz_class[i] )
                a <- dplyr::filter(a, area_fips=='47000' & qtr=='1' & industry_code=='10')   # 10 is all industries
                QCEWSizeData <- rbind(QCEWSizeData, a)
                on.exit(close(url))
}


# Groups are (< 5, 5-9, 10-19, 20-49, 50-99, 100-249, 250-499, 500-999, 1000+)
# Create variable firm_size_cat

# Append labels to QCEWSizeData
QCEWSizeData$firm_size_cat <- c('0-4', '5-9', '10-19', '20-49', '50-99', '100-249', '250-499', '500-999', '1000+')
# Keep columns 5 size_code, 6 year, 10 month1_emplvl (month1 of qtr 1 is arbitrary), and 43 firm_sz_cat
QCEWSizeData <- QCEWSizeData[ , c(5,6,10,43)]  # now we have QCEW employment by size category
########################################################################################

# Coalesce the data to get QCEW 2, 3, and 4 digit NAICS industry employment numbers

# Then get only private ownership observations
privateTN <- TennesseeDataArea[which(TennesseeDataArea$own_code == 5),]

# Next, create a variable for the number of characters in the NAICS industry code
privateTN$digits <- nchar(as.character(privateTN$industry_code))

# Keep only the six digit industries
QCEWprivateTNsix <- privateTN[which(privateTN$digits==6),]


# Keep only the five digit industries
QCEWprivateTNfive <- privateTN[which(privateTN$digits==5),]


# Keep only the four digit industries
QCEWprivateTNfour <- privateTN[which(privateTN$digits==4),]


# Keep only the three digit industries
QCEWprivateTNthree <- privateTN[which(privateTN$digits==3),]

# For QCEWprivateTNtwo to follow convention, need to combine 31, 32, & 33 to 31-33,  44 & 45 to 44-45, and 48 & 49 to 48-49
# Keep only the two digit industries
QCEWprivateTNtwo <- privateTN[which(privateTN$digits==2) , ]
              a31.33 <-   privateTN[which(privateTN$industry_code=='31-33') , ]
              a44.45 <-   privateTN[which(privateTN$industry_code=='44-45') , ]                                    
              a48.49 <-   privateTN[which(privateTN$industry_code=='48-49') , ]  
QCEWprivateTNtwo <- rbind(QCEWprivateTNtwo, a31.33, a44.45, a48.49)



