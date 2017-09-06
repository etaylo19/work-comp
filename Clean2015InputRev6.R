# ELT April 24, 2017

      ##### NOTES ####
# This is a script to read Tennessee Bureau of Workers' Compensation data from txt tab-delimited fileS supplied by Boyd CBER
#   and process them.  ****These files must be present in the working directory.*****
# Note that cols are M-mandatory, C-conditional, or O-optional according to the Tennessee FROI Element Requirement Table
# First, it reads FROI data into a Basetable then filters it to get private sector, single establishment, non-duplicate,
#     matched (u/ UI - FEIN number) observations.
# Rev 2 (5/9/2017) uses more functions, makes better use of dplyr, & loads packages more efficiently
# Rev 3 (5/10/2017) adds automatic capability to check the data quality/validity (i.e. Ck_data)
# Rev 4 (5/15/2017) supplements Ck_data to include more columns; there are 67 columns when including UI
# Rev 5 (5/26/2017) changed the order of variable creation & misc.  
# Rev 6 (5/31/2017) removed the InjTable_func made obsolete by MakeTables2015Rev3.R script
# Rev 6 (6/2/2017) revised the data names in accordance with the following:
      # Basetable - INITIALLY is      downloaded FROI and matched UI data 
      # NOTE: Basetable morphs as additional variables are created 
      # Basetable_sr - downloaded SROI data
      # Basetable_inj - downloaded injury narratives
      # All - result of joined (morphed) Basetable & Basetable_sr & Basetable
      #   N - non ,  M - matched , D - duplicate , SE - single establishment , P - private , fr - FROI
      # Added the .InputBT2014_func because the 2014 & 2015 input data are formatted differently.

      
# Quality function produces a very large list of eight items; these ultimately become eight column vectors
      # in the table Ck_data (cols 3-10) along with the DNs (col 1), descriptions (col 2) , and uniq_values (col 11)
      
      ##### END NOTES ####

# First, set the working directory to the folder where the original data exist
setwd("/Users/edwardtaylorUTK/Desktop/2015WCfiles")

# To empty the current environment in R Studio,
#   uncomment and execute the next line

#          rm(list=ls())

      #### LOAD DESIRED PACKAGES ####
      
load_pkg <- c('dplyr', 'readr', 'data.table')      

for (i in 1:length(load_pkg)) {
  
      if (!require(load_pkg[i], character.only=T)){
        install.packages(load_pkg[i], 
                         repos="https://cran.rstudio.com/",
                         quiet=TRUE)
        require(load_pkg[i], character.only=T) #same as library(package name)
      }
}


      #### END LOAD DESIRED PACKAGES ####

      #### READ DATA ####
      
# Read in the data from a txt file
# The file, NAME.txt, is supplied by Boyd CBER
# This is 2015 (date of injury) data on TN BWC claims

# Read the input data for the different years with a function

# Below is the modified .InputBT_func to accommodate new date formats "%Y%m%d" except for 
#   TransformedDOB = col_date(format = "%Y-%m-%d")
.InputBT2014_func <- function(InputFile){
  read_delim(paste0("./",InputFile), 
             "\t", escape_double = FALSE, col_types = cols(CLM_TRAN_NBR = col_character(), 
                                                           DTE_DSBL_BEG = col_date(format = "%Y%m%d"), 
                                                           DTE_EMPR_INFO = col_date(format = "%Y%m%d"), 
                                                           DTE_OF_DTH = col_date(format = "%Y%m%d"), 
                                                           DTE_OF_INJR = col_date(format = "%Y%m%d"), 
                                                           DTE_RPT_TO_CLM_ADMN = col_date(format = "%Y%m%d"), 
                                                           EMPLMT_CLS_CDE = col_character(), 
                                                           EMPLM_STA_CDE = col_character(), 
                                                           EMPL_PSTL_CDE = col_character(), 
                                                           EMPR_PSTL_CDE = col_character(), 
                                                           FROI_TRAN_NBR = col_character(), 
                                                           HIRE_DTE = col_date(format = "%Y%m%d"), 
                                                           INIT_TMNT = col_character(), LAST_DTE_WRKD = col_date(format = "%Y%m%d"), 
                                                           MAIL_ZIP4 = col_character(), MEEI = col_character(), 
                                                           MTC = col_character(), MTC_DTE = col_date(format = "%Y%m%d"), 
                                                           NAICS_CDE = col_character(),
                                                           OWNER = col_character(), PHONE = col_character(), 
                                                           PHYSICAL_ZIP = col_character(), PHYSICAL_ZIP4 = col_character(),
                                                           PSTL_INJR_SITE = col_character(), 
                                                           QCEW_NAICS_CODE = col_character(), 
                                                           QCEW_YEARQ = col_character(), RTRN_WRK_DTE = col_date(format = "%Y%m%d"), 
                                                           SRCE = col_character(), TAX_ZIP4 = col_character(),
                                                           TME_OF_INJR = col_character(), 
                                                           TransformedDOB = col_date(format = "%Y-%m-%d"), 
                                                           WGE = col_integer()), trim_ws = TRUE)
}

# This is the 2014 annual file
Basetable2014 <- .InputBT2014_func("2014 utk stats Employer Data.txt")

# Use the '.' prefix to make the function invisible in the environment
# Now input the 2015 data

.InputBT_func <- function(InputFile){
  read_delim(paste0("./",InputFile), 
                             "\t", escape_double = FALSE, col_types = cols(CLM_TRAN_NBR = col_character(), 
                                                                           DTE_DSBL_BEG = col_date(format = "%Y-%m-%d"), 
                                                                           DTE_EMPR_INFO = col_date(format = "%Y-%m-%d"), 
                                                                           DTE_OF_DTH = col_date(format = "%Y-%m-%d"), 
                                                                           DTE_OF_INJR = col_date(format = "%Y-%m-%d"), 
                                                                           DTE_RPT_TO_CLM_ADMN = col_date(format = "%Y-%m-%d"), 
                                                                           EMPLMT_CLS_CDE = col_character(), 
                                                                           EMPLM_STA_CDE = col_character(), 
                                                                           EMPL_PSTL_CDE = col_character(), 
                                                                           EMPR_PSTL_CDE = col_character(), 
                                                                           FROI_TRAN_NBR = col_character(), 
                                                                           HIRE_DTE = col_date(format = "%Y-%m-%d"), 
                                                                           INIT_TMNT = col_character(), LAST_DTE_WRKD = col_date(format = "%Y-%m-%d"), 
                                                                           MAIL_ZIP4 = col_character(), MEEI = col_character(), 
                                                                           MTC = col_character(), MTC_DTE = col_date(format = "%Y-%m-%d"), 
                                                                           NAICS_CDE = col_character(),
                                                                           OWNER = col_character(), PHONE = col_character(), 
                                                                           PHYSICAL_ZIP = col_character(), PHYSICAL_ZIP4 = col_character(),
                                                                           PSTL_INJR_SITE = col_character(), 
                                                                           QCEW_NAICS_CODE = col_character(), 
                                                                           QCEW_YEARQ = col_character(), RTRN_WRK_DTE = col_date(format = "%Y-%m-%d"), 
                                                                           SRCE = col_character(), TAX_ZIP4 = col_character(),
                                                                           TME_OF_INJR = col_character(), 
                                                                           TransformedDOB = col_date(format = "%Y-%m-%d"), 
                                                                           WGE = col_integer()), trim_ws = TRUE)
}

# This is the 2015 annual file
Basetable2015 <- .InputBT_func("2015 utk stats Employer Data2017-04-24.txt")

# Use dplyr::bind_rows() to append annual files
Basetable <- bind_rows(Basetable2015, Basetable2014)

# Basetable includes duplicates, non-matches, and all types of establishments and ownerships

      #### END READ Basetable DATA ####

# If the following variables are not already in the sole remaining file, add them below

# Use the dplyr package to add variables  IF NOT ALREADY IN THE 'Basetable'
Basetable <- mutate(Basetable, tenure_yrs = as.numeric((DTE_OF_INJR - HIRE_DTE)/365),
                     age_yrs = as.numeric((DTE_OF_INJR - TransformedDOB)/365),
                     inj_yr = format ( as.Date( DTE_OF_INJR, format="%Y-%m-%d") ,"%Y" ) ) 
                       
#### CREATE ADDITIONAL VARIABLES SECTION ####

# Need to create levels for firm size based on QCEW
# Groups are (< 5, 5-9, 10-19, 20-49, 50-99, 100-249, 250-499, 500-999, 1000+)
# Create variable firm_size_category

Basetable$firm_size_cat <- cut(Basetable$EMPL2, c(0,5,10,20,50,100,250,500,1000,1000000), 
                                labels=c('0-4', '5-9', '10-19', '20-49', '50-99', '100-249', '250-499', '500-999', '1000+'), right=F)

# Create 4-digit NAICS code
Basetable$Naics4 <- substring(Basetable$QCEW_NAICS_CODE,1,4)

# Create 3-digit NAICS code
Basetable$Naics3 <- substring(Basetable$QCEW_NAICS_CODE,1,3)

# Create 2-digit NAICS code
Basetable$Naics2 <- substring(Basetable$QCEW_NAICS_CODE,1,2)

# Use the dplyr package to add variables
Basetable <- mutate(Basetable, tenure_yrs = as.numeric((DTE_OF_INJR - HIRE_DTE)/365),
                     age_yrs = as.numeric((DTE_OF_INJR - TransformedDOB)/365),
                     inj_yr = format(as.Date(DTE_OF_INJR, format="%Y-%m-%d"),"%Y"))

##### END ADDITIONAL VARIABLES SECTION ####

# Create Basetable with non-duplicate observations only

# Find the row indices for duplicated rows 
index_dup <- which(duplicated(Basetable[,2]))
BasetableND <- Basetable[-index_dup,]

# Make M_data (i.e. matched data)

NM_data <- Basetable[which(Basetable$Match =='No Match') , ]   # 22,455 observations are not matches
M_data <- Basetable[-which(Basetable$Match =='No Match') , ]    # 79,618 observations are matches 




      ##### CREATE FILTERED TABLES #########

# Find duplicated values of CLM_TRAN_NBR
# A logical vector is obtained by using the expression; which(duplicated(M_data[,2]))


# Find the row indices for duplicated rows 
index_dup <- which(duplicated(M_data[,2]))

# Get a table of duplicate rows
MD_data <- M_data[index_dup, ]

# Get a table of non-duplicate rows
M_ND_data  <- M_data[-index_dup, ]

# Sort the table on the first column
MD_data <- MD_data[order(MD_data$CLM_TRAN_NBR) ,]

# Count the number of duplicates; the maximum result for 2015 is nine (9)
# This is FROI_TRAN_NBR 4664010; evidently a restaurant; sub-unit representing several establishments
# Below is a function named .count.dups

.count.dups <- function(DF){   # function starts here
  
  DT <- data.table(DF)
  DT[,.N, by = names(DT)]
}         # End of function

DupFreq <- .count.dups(M_data[, c(1:2)])

# Sort in ascending order using data.table package on the data.table object

DupFreq <- setorder(DupFreq,-N)

# Sort non-duplicates by N in descending order
# See the table; there are as many as 9 duplicates 

M_ND_data <- M_ND_data[order(M_ND_data$FROI_TRAN_NBR) , ]  # These are non-duplicate matches

# Filter for Private sector only; these are    *matched*       observations
MP_ND_data <- M_ND_data[which(M_ND_data$OWNER_DESC=='Private Business') , ]

# Filter for Single Establishment, Private Sector only; these are     *matched*      observations
MP_SE_ND_data <- MP_ND_data[which(MP_ND_data$MEEI_DESC=='Single Establishment Unit') , ]



# Note Single Establishment, Private Sector table is    IDENTICAL TO     Matched Single Establishment, 
# Private Sector table; 37,838 rows


      ##### END CREATE FILTERED TABLES #######





      #### NOW READ SOME SROI AND INJURY DESCRIPTION DATA ####

# This is the SROI data file and the injury description file

# Read in the data from a txt file
# The file's name is supplied by Boyd CBER
# This is 2015 (date of injury) data on TN BWC SROI claims

# Use another masked function to read the SROI files for all years
.InputBT_sr_func <- function(InputFile){
        read_delim(paste0("./", InputFile), "\t", escape_double = FALSE, col_types = cols(CLM_TRAN_NBR = col_character(), 
                                                                                       SROI_TRAN_NBR = col_character()), trim_ws = TRUE)
}

Basetable_sr  <-.InputBT_sr_func('2015 SROI utk.txt') 

# Use dplyr::bind_rows() to append annual files
# Basetable_sr <- bind_rows(Basetable_sr2015, Basetable2014_sr, ...)

# Basetable_sr includes non-matches, and all types of establishments and ownerships


# Now read the injury description data that was furnished in a separate file
# The file's name is supplied by Boyd CBER
# This is 2015 (date of injury)  narrative data on TN BWC FROI claims


# Use another masked function to read the SROI files for all years
.InputBT_desc_func <- function(InputFile){
              read_delim( paste0("./", InputFile), "\t", escape_double = FALSE, 
                          col_types = cols(CLM_TRAN_NBR = col_character(), FROI_TRAN_NBR = col_character()),
                          trim_ws = TRUE)
}


Basetable_desc <- .InputBT_desc_func("INJZ_OCCUR_RSN.txt")
      #### END READ MORE DATA ####



      #### JOIN  TABLES ####

# First join

# Use left_join to match all rows & cols in MP_SE_ND_data (x) w/ Basetable_sr (y)
# The result will be a Matcbed_Private_SingEst_NonDup_FROI_SROI Table (i.e. MP_SE_ND_fr_sr_data)

MP_SE_ND_fr_sr_data <- left_join(MP_SE_ND_data, Basetable_sr, by= c('CLM_TRAN_NBR' = 'CLM_TRAN_NBR'))

# Sort the table on the transaction number
MP_SE_ND_fr_sr_data <- MP_SE_ND_fr_sr_data[order(MP_SE_ND_fr_sr_data$CLM_TRAN_NBR) ,]



# Second join

# Use a left-join to match all rows & cols in MP_SE_ND_fr_sr_data (x) w/ Basetable_desc (y)
# The result will be the final joined table that includes all 2015 matched, private, single establishment,
#       non-duplicated FROI, SROI, & UI info, with the injury descritiption info  (i.e. All_data)
# There will be many missing injury descriptions -- 31,155 not all of them unique claims; 27,442 matched
#       in the MP_SE_ND_fr_sr_data

# The table below is a compilation of FROI, SROI, and injury narratives
All_data <- left_join(MP_SE_ND_fr_sr_data, Basetable_desc, by= c('CLM_TRAN_NBR' = 'CLM_TRAN_NBR'))

# Sort the table on the transaction number
All_data <- All_data[order(All_data$CLM_TRAN_NBR) ,]


      ##### END TABLE CREATION ##########






      #### DEFINE A Quality_Func FOR DATA MISSINGNESS AND VALIDITY ####

####   It may be desirable to limit the data observations to a single year of injury ####

Quality_func <- function(table){     # function begins here

               #### MISSING DATA SECTION ####
        # Determine how much data is missing for each column
        # Note that some data is C-conditional or O-optional; the remainder is M-mandatory
        
          missing_sum <- apply(table,2,function(x) sum(is.na(x)))
        
        
        # The following lines will give you the average, minumum, and maximum number of characters in each column
        # It doesn't behave well for numerical columns (i.e. integer values)
        
        avg_len <- apply (apply(table ,2, nchar , allowNA=T), 2, mean, na.rm=T)
        
        min_len <- apply (apply(table ,2, nchar , allowNA=T) , 2, min, na.rm=T)
        
        max_len <- apply (apply(table ,2, nchar , allowNA=T) , 2, max, na.rm=T)
        
        # The following lines will give you the minimum and maximum values of numerical values
        
        min_value <- apply(table ,2, min, na.rm=T )
        
        max_value <- apply(table ,2, max, na.rm=T )
        
        # Make a table
        
        Len_table <- cbind(avg_len, min_len, max_len)
        
        # If min. or max. length is out of range, investigate manually
        
              #### END MISSING DATA SECTION ####
        
        ############## Perform some data validity checks here #################
        ###### What are the critical data elements?
        
        # Identify number of unique values for all columns of table
        unique_sum <- apply(table,2,function(x) length(unique(x)))

        # Identify unique values of all columns of table; this is a list; unique_x will be a list
            # within a list; ultimately moved unique_x outside the function so comment out
                  # unique_x <- apply(table, 2,function(x) unique(x))  # unique_x is a list
        
        Everything <- list(missing_sum, unique_sum, avg_len,  min_len, max_len, Len_table, 
                           min_value, max_value)
        
        return(Everything)   # Everything is a list with eight (8) elements; omit list unique_x

}     ### End of Quality_func



      ##### END OF QUALITY FUNCTION ##########

              








          ##### MAKE SOME REPORT CALCULATIONS ##########



# The number of 2014 and 2015 FROI observations

 
M_ND_to_BTpct <- round(nrow(M_ND_data)*100/nrow(BasetableND),1) 
M_to_BTpct <-    round(nrow(M_ND_data)*100/nrow(BasetableND),1) 


            ##### END REPORT CALCULATIONS ##########



######## MAKE A CONSTRUCTION TABLE ###############

# Extract construction observations
# Get ALL NonDuplicated construction claims; includes all types of establishments and gov't workers
ConstTable <- (M_ND_data[which(grepl("^23",M_ND_data$QCEW_NAICS_CODE)),] )

# Add some variables using dplyr
ConstTable <- mutate(ConstTable, tenure_yrs = as.numeric((DTE_OF_INJR - HIRE_DTE)/365),
                       age_yrs = as.numeric((DTE_OF_INJR - TransformedDOB)/365), 
                        inj_yr = format(as.Date(DTE_OF_INJR, format="%Y-%m-%d"),"%Y"))

# Output construction claims; includes all types of estblishments and gov't workers
# For privacy concerns, eliminate columns that identify below the state level
# For simplicity & privacy keep only needed columns

# Index columns to keep
keep <- c(2,6,10,11,13:15,17,19:21,24:30,32:45,50,58,60:74)

write.csv( ConstTable[ , keep] , paste0('./Const', Sys.Date(),".csv"))


######## END CONSTRUCTION TABLE SECTION ###############

    ######### APPLY THE QUALITY FUNCTION ########################

# As an example, Use the function Quality on the the table in  the next line
TABLE_NAME <- 'Basetable'  # Use a character value for naming the file 

num_cols <- ncol(Basetable)

Quality <- Quality_func(Basetable)   # Quality is a list of eight ; list(missing_sum, unique_sum, avg_len,  
                                        #  min_len, max_len, Len_table, min_value, max_value)

# Compute some vectors to use in compiling a quality check table
    
    # First column vector of identifiers
    DN <- c('DN0001', NA, 'DN0002','DN0003',	'DN0021', 'DN0022','DN0023', 'DN0024',	'DN0025','DN0031'
            ,'DN0032', 'DN0033','DN0034','DN0035','DN0036','DN0037','NCCI Table 284 Desc',
            'DN0035'	, 'DN0036',  'DN0039'	, 'DN0040'	, 'DN0041','DN0050','DN0052',	'DN0053',	'DN0054',
            'DN0055','DN0056',	'DN0057',	'DN0058',	 'DN0059','DN0060',	'DN0061',	'DN0062',	 'DN0063','DN0064',	
            'DN0065','DN0066','DN0067', 'DN0068', rep('From UI Matching',26), 'Masked ER Identifier',
            rep(' ',(num_cols -   67   ) ))
    
    # This is a column vector of the names matching the labels in column vector above 
    b <- names(Basetable[1:num_cols])
    
    # This is a column vector of mandatory, conditional, or optional categories
    requirement  <- c('M', 'M', rep('M',6), 'O', 'M', 'O', 'M','M', NA, NA, 'M', NA, 'M', 'M', rep('O',3),
                      rep('M',3), rep(c('O','C'),2) , rep('O',4) , rep('C',3), rep('O',4) , rep('  ', (num_cols-40)))
    
    # This is a column vector of the number of missing values
    num_missing <- Quality[[1]][1:num_cols]
    
    # This is a column vector of the number of unique values
    num_uniq <- Quality[[2]][1:num_cols]
    
    # This is a column vector showing the average # of characters
    avg_len <- format(Quality[[3]][1:num_cols], digits=3, nsmall=1)
    
    # This is a column vector showing the minimum # of characters
    min_len <- format(Quality[[4]][1:num_cols], digits=3, nsmall=1)
    
    # This is a column vector showing the maximum # of characters 
    max_len <- format(Quality[[5]][1:num_cols], digits=3, nsmall=1)
    
    # This is a column vector showing the minimum value 
    min_value <- format(Quality[[7]][1:num_cols], digits=3, nsmall=1)
    
    # This is a column vector showing the maximum value
    max_value <- format(Quality[[8]][1:num_cols], digits=3, nsmall=1)
    
    ###### unique_x TAKES A LOT OF MEMORY ########
    
    # This is a column vector of all unique values in EVERY COLUMN
    unique_x <- apply(Basetable, 2, function(x) unique(x))  # unique_x is a list equal in length
                                                                  # to the number of columns


###### unique_x TAKES A LOT OF MEMORY #######

# This statement uses the list named unique_x and produces a column vector of unique values of 
        # each variable when <= 30 or a note ('> 30 unique values')

uniq_values <- c()
for (i in 1:length(num_uniq)){ 
  uniq_values[i] <- ifelse(num_uniq[i] <= 30 , paste0(unique_x[[i]][1:(num_uniq[i])],sep=", ",collapse="") ,
                           paste0('> 30 unique values'))

  ##### END OF COMPUTE VECTORS SECTION #######  
  
  
}           # End of for uniq_values loop




Ck_data <- as.data.frame(cbind(DN, b, requirement, num_missing, num_uniq, avg_len, min_len, max_len, uniq_values,
                               min_value, max_value), row.names=F)

names(Ck_data) <- c('DN', 'Column Name', 'Mand(M) Cond(C) Opt(O) for MTC00', 'Number Missing','Number Unique',
                    'Avg Len (char)', 'Min Len (char)', 'Max Len (char)', 'Unique Values', 'Min Value', 'Max Value')

write.csv( Ck_data, paste0('./Ck_data_', TABLE_NAME, Sys.Date(),".csv"))

####################### End of validity and missingness checks ##############################

####################### Get Public Data for R Shiny download ##############################

#### JOIN  TABLES TO MAKE PUBLIC DATA SET####

# First join

# Use a left-join to match all rows & cols in MP_ND_data (x) w/ Basetable_desc
# The result will be a joined table that includes all 2014-15 matched, private, 
#       non-duplicated FROI, & UI info, with the injury description info  
# There will be many missing injury descriptions -- 
#       in the MP_ND_desc_data

# The table below is a compilation of FROI, SROI, and injury narratives
MP_ND_desc_data <- left_join(MP_ND_data, Basetable_desc, by= c('CLM_TRAN_NBR' = 'CLM_TRAN_NBR'))

# Sort the table on the transaction number
Public_data <- MP_ND_desc_data[order(MP_ND_desc_data$CLM_TRAN_NBR) ,]

# Compile a public file to post on the internet for download
Public_data <- Public_data[, c(70, 74, 43, 14, 16, 76)]

# Rename the columns
names(Public_data) <- c('InjYr', 'Naics2', 'Naics6', 'BodyPartCode', 'CauseCode', 'Narrative')



# Narrative has non-std characters in row 109,504 ;  e.g.  '  EMPLOYEE_WAS �  '

# Convert them using iconv()
Public_data$Narrative <- iconv(Public_data$Narrative, "latin1", "ASCII", sub="")

# make narrative lower case;
Public_data$Narrative <- tolower(Public_data$Narrative)
Public_data$BodyPartCode <- as.character(Public_data$BodyPartCode)

Public_data2014 <- filter(Public_data, InjYr=='2014')
Public_data2015 <- filter(Public_data, InjYr=='2015')

#############################################################################
#############################################################################
                                                                          ###
## Some of the narratives contain PII (i.e. EE names); delete them         ##
                                                                          ###
#############################################################################
#############################################################################
