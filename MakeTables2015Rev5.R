# May 15, 2017
# ELT
# This is a file to produce some tables
# The tables are similar to those for California found at http://www.dir.ca.gov/dwc/wcis/WCIS_Reports.html
# The code will make use of the dplyr package for grouping and aggregating
# Rev1 & 2 (May 23) fixes the problems with producing multi-year tables using dplyr inside a function
# Rev3 (May 26) added tableZip_func; filtered for 2014-2015 obs.; (June2) added table9
# Rev4 (June 8) revised tables to delete Pct column and add a second Claims column
#     the two claims fields will be named MPS.Claims and MP.Claims
#     rate names are changed to MPS.Rate (from UI.rate) and MP.Rates (from QCEW.Rates); added Table2x_Rate_func
#     (June 12-13) added NAICS description to form table10x_final ; reworked table10a to get 31-33, 44-45, and 48-49
#     June 15 added tables 10aa, 10bb, and 10cc w/ 90% CI and BLS rates
#     June 28 added rates to table 12
#     July 7 added table 12a

# Set  ********** TABLE_NAME (~ line 42) ************ to the desired subset of data with which you wish to work

##### END OF NOTES  #####


# First, set the working directory to the folder where the data exist
setwd("/Users/edwardtaylorUTK/Desktop/2015WCfiles")


#### LOAD DESIRED PACKAGES ####

load_pkg <- c('dplyr', 'readr', 'data.table', 'tables', 'lazyeval', 'dummies', 'ggplot2')      

for (i in 1:length(load_pkg)) {
  
  if (!require(load_pkg[i], character.only=T)){
    install.packages(load_pkg[i], 
                     repos="https://cran.rstudio.com/",
                     quiet=TRUE)
    require(load_pkg[i], character.only=T) #same as library(package name)
  }
}

######################################################
# Primary File to use for making tables is TABLE_NAME; this will change depending on the analysis
######################################################

# This will be the table used in Tables 1-7, & 9 where NO rates are calculated
TABLE_NAME <- BasetableND
# The table below will be used to calculate MPS.Claims and MPS.rate (matched, private, single-estab); tables 10-12
MPS_TABLE <- MP_SE_ND_data
# The table below will be used to calculate MP.Claims and MP.rate (matched, private, single/multi); tables 10-12
MP_TABLE <- MP_ND_data

# Keep only 2014 and 2015 values of 'inj_yr'

TABLE_NAME <- dplyr::filter(TABLE_NAME, inj_yr == '2015' | inj_yr == '2014')
MPS_TABLE <-  dplyr::filter(MPS_TABLE,  inj_yr == '2015' | inj_yr == '2014')
MP_TABLE <-   dplyr::filter(MP_TABLE,   inj_yr == '2015' | inj_yr == '2014')


# Start with a table generated from the Clean2015InputRev6.R file

# Remove unneeded objects from environment except those below; keep table listed in TABLE_NAME & TNzip

rm(list= ls()[!(ls() %in% c('TABLE_NAME', 'MPS_TABLE', 'MP_TABLE', 'TNzip', 'All_data',  'Basetable', 'Basetable_sr',
                            'Quality_func' , 'InjTable_func' , 'firm_emp_func',
                            'QCEWprivateTNfour', 'QCEWprivateTNthree', 'QCEWprivateTNtwo', 'QCEWSizeData'))])


   

############### LOAD SOME FUNCTIONS TO CREATE AND FORMAT TABLES   ###############

# This is a function using a dataframe and two columns for arguments

########### For claimant privacy, do not accept less than 5 claims for a unit ################

table2x_func <- function(df, field1, field2) {
    df %>% 
    group_by_(field1, field2) %>%     # Notice the use of 'group_by_' rather than 'group_by' for NSE
    select_(field1, field2) %>%       # Notice the use of 'select_' rather than 'select' for NSE
    summarise(claims = n()) %>%       # NSE - Non-standard evaluation
    filter(claims > 0) %>%
    mutate (Pct.=round(claims * 100/sum(claims), digits=2))
}

# A table with only three columns
table2xRate_func <- function(df, field1, field2) {
  df %>% 
    group_by_(field1, field2) %>%     # Notice the use of 'group_by_' rather than 'group_by' for NSE
    select_(field1, field2) %>%       # Notice the use of 'select_' rather than 'select' for NSE
    summarise(claims = n()) %>% 
    filter(claims > 0) 
}
########### For claimant privacy, do not accept less than 10? or 20? claims for a ZC ################
############ If less than 10; the table is too long to print ########

tableZip10x_func <- function(df, field1, field2) {
  df %>% 
    group_by_(field1, field2) %>% 
    select_(field1, field2) %>% 
    summarise(claims = n()) %>% 
    filter(claims >= 10) %>%          # set 10 claims as minimum
    mutate (Pct.=round(claims * 100/sum(claims), digits=2))
}

tableZip20x_func <- function(df, field1, field2) {
  df %>% 
    group_by_(field1, field2) %>% 
    select_(field1, field2) %>% 
    summarise(claims = n()) %>% 
    filter(claims >= 20) %>%          # set 20 claims as minimum
    mutate (Pct.=round(claims * 100/sum(claims), digits=2))
}


# This function (creates a table) using a dataframe and three other fields for arguments

table3x_func <- function(df, field1, field2, field3) {
    df %>% 
    group_by_(field1, field2, field3) %>% 
    select_(field1, field2, field3) %>% 
    summarise(claims = n()) %>% 
    group_by_(field1) %>% 
    select( inj_yr, body_part, IA_BODY_PART_DESC,claims) %>%
    mutate (Pct.=round(claims * 100/sum(claims), digits=2))
}


# This is a function (creates a table) using a dataframe and four other fields for arguments

table4x_func <- function(df, field1, field2, field3, field4) {
    df %>% 
    group_by_(field1, field2) %>% 
    select_(field1, field2, field3, field4) %>% 
      summarise(claims = n()) %>% 
      mutate (Pct.=round(claims * 100/sum(claims), digits=2))
}


# This is a function (creates a wide table) using a dataframe and three other fields for arguments

wide_table3x_func <- function(df, field1, field2, field3) {   
  df %>%
  group_by_(field1, field2, field3) %>%
  select_(field1, field2, field3  ) %>%
    summarise(claims=n())   %>%
    group_by_(field1)     %>%
    mutate(Pct.= round((claims * 100)/sum(claims), digits=2))
}



############### END LOAD FUNCTION SECTION ###############






# The table numbering corresponds to a list of tables suggested by M.Jones on 4/17/2017*
# *roughly the same as used by CA on their WC website

        ###### BEGIN TABLE 1 ##########

# Table 1. Number of Claims by Part of Body and Year of Injury
# Table 1a. Claims by Part of Body (Detailed) and Year of Injury

# Create a function called Body_func to create a variable base on INJR_BODY_PART_ID
# Now add a variable called body_part

.Body_func <- function(x){

      if(x >= 10 & x <= 19){
        TABLE_NAME$body_part = 'HEAD'
        }else if(x >= 20 & x <= 26){   
        TABLE_NAME$body_part = 'NECK'  
        }else if(x >= 40 & x <= 49){   
        TABLE_NAME$body_part = 'TRUNK'
        }else if(x >= 60 & x <= 63){   
        TABLE_NAME$body_part = 'TRUNK'
        }else if(x >= 50 & x <= 58){   
        TABLE_NAME$body_part = 'LOWER EXTREMITIES' 
        }else if(x >= 30 & x <= 39){   
        TABLE_NAME$body_part = 'UPPER EXTREMITIES' 
        }else if(x >= 64 & x <= 99){   
        TABLE_NAME$body_part = 'MULTIPLE BODY PARTS' 
        }else {TABLE_NAME$body_part = 'OTHER' 
        }   #### End if statement
}           #### End function

# Use the function Body_func in an sapply statement
TABLE_NAME$body_part <- sapply(TABLE_NAME$INJR_BODY_PART_ID, .Body_func)

# Change the IA_BODY_PART_DESC variable to lower case
TABLE_NAME$IA_BODY_PART_DESC <- tolower(TABLE_NAME$IA_BODY_PART_DESC)

# Use the table?x_func found above to create table1 and table1a

table1   <- table4x_func(TABLE_NAME, 'inj_yr', 'body_part' ,'IA_BODY_PART_DESC','INJR_BODY_PART_ID')

table1a  <- table3x_func(TABLE_NAME, 'inj_yr', 'body_part' ,'IA_BODY_PART_DESC')

###### END TABLE 1 AND 1A ##########


################################################################################################################
# Create data for a histogram of ratios; there will be matched private claims and total claim counts
# Do the same for all matched private claims to find ratio of matched private (IA_BODY_PART_DESC)
#       to total claims (IA_BODY_PART_DESC) and compare to ratio of all private matched to total (i.e. for 2015; 66738 / 98061 = 0.681)
# Use data for year 2015


temp <- dplyr::filter(TABLE_NAME, OWNER=='5' & inj_yr=='2015')

# Use the table3x_func to get table of IA_BODY_PART_DESC for MP_TABLE 2015 only
MPtable1a  <- table3x_func(temp, 'inj_yr', 'body_part' ,'IA_BODY_PART_DESC')

table1a_merge <- merge(table1a, MPtable1a, by.x= c('inj_yr', 'body_part', 'IA_BODY_PART_DESC'), 
                       by.y= c('inj_yr', 'body_part', 'IA_BODY_PART_DESC'), suffixes=c('.x', '.y'), all.x=TRUE)

# Filter for 2015 observations
table1a_merge2015 <- dplyr::filter(table1a_merge, inj_yr=='2015')

# Prepare vector for histogram
BodyPartRatio <- table1a_merge2015[6] / table1a_merge2015[4]

# Find the standard deviation
a <- sd(as.vector(BodyPartRatio$claims.y))
b <- qt(c(.05, .95), df=55)   # 55 degrees of freedom (n=56)

# Find the interval lower limit
limits <- mean(as.vector(BodyPartRatio$claims.y)) - (a * b)  # vector of upper & lower limits



ggplot(data=table1a_merge2015, aes(x=BodyPartRatio)) + geom_histogram(binwidth=0.025) +
  labs( x='Ratio', y='Count') + 
  ggtitle('Histogram for Ratios of 2015 Matched Private to Total FROI Claims*', subtitle = '*Using Body Part Description Categories (n=56), Distribution with Mean = 0.693 & Std. Dev. = 0.081')

# End of histogram section          
############################################################################################



        ###### BEGIN TABLE 2 and 2a ##########

# Table 2. Claims by Cause of Injury and Year of Injury
# Table 2a. Claims by Cause of Injury (Detailed) and Year of Injury
# Import the file named 'Causes of Injury_Codes.csv'


Causes_of_Injury_Codes <- read_csv("./Causes of Injury Codes.csv",
                                   col_names = FALSE, col_types = cols(X1 = col_character()))
names(Causes_of_Injury_Codes) <- c('SRCE', 'Minor_Head', "Major_Head")

# Find the index to yield the correct major and minor headings for cause of injury
Index <- match(TABLE_NAME$SRCE, Causes_of_Injury_Codes$SRCE)

# Create variables using the csv file Cause of Injury Codes
TABLE_NAME$inj_cause_detailed <- Causes_of_Injury_Codes$Minor_Head[Index]

TABLE_NAME$inj_cause <- Causes_of_Injury_Codes$Major_Head[Index]

table2   <- table2x_func(TABLE_NAME, 'inj_yr', 'inj_cause')

table2a  <- wide_table3x_func(TABLE_NAME, 'inj_yr', 'inj_cause', 'inj_cause_detailed')



        ###### END TABLE 2 and 2a  ##########
    









        ###### BEGIN TABLE 3 & 3a ##########

# Table 3. Number of Claims by Nature of Injury and Year of Injury
# Table 3a. Number of Claims by Nature of Injury and Year of Injury (Detailed)

# Now add a variable called Inj_nature

.Inj_func <- function(x){
  
  if(x >= 1 & x <=59){
    TABLE_NAME$Inj_nature = 'SPECIFIC INJURY'
  }else if(x >= 60 & x <= 80){   
    TABLE_NAME$Inj_nature = 'OCCUPATIONAL DISEASE OR CUMULATIVE INJURY'  
  }else if (x >= 81 & x <= 91){
    TABLE_NAME$Inj_nature = 'MULTIPLE INJURIES'   
  }else {TABLE_NAME$Inj_nature = 'OTHER' 
  }   #### End if statement
}           #### End function

# Call the function Inj_func in an sapply statement
TABLE_NAME$Inj_nature <- sapply(TABLE_NAME$NATR_OF_INJR_ID, .Inj_func)


table3   <- table2x_func(TABLE_NAME, 'inj_yr', 'Inj_nature')

# Make NATR_OF_INJR_DESC variable lower case
TABLE_NAME$NATR_OF_INJR_DESC <- tolower(TABLE_NAME$NATR_OF_INJR_DESC)

table3a  <- wide_table3x_func(TABLE_NAME, 'inj_yr', 'Inj_nature',  'NATR_OF_INJR_DESC')



        ###### END TABLE 3 AND 3A ##########







        ###### START TABLE 4 ##########

# Table 4. Claims by Insurer Type and Market Share 

# Make self_ins variable
TABLE_NAME$self_ins <- ifelse(TABLE_NAME$SELF_INSD_IND == 'Y', 'Self-Insured', 'Insured')
  
table4   <- table2x_func(TABLE_NAME, 'inj_yr', 'self_ins')

      ###### END TABLE 4 ##########







      ###### START TABLE 5 ##########

# Table 5. Claims by Age and Year of Injury

# Make age_cat variable


TABLE_NAME$age_cat <- cut(TABLE_NAME$age_yrs, breaks = c(14, 16, 20, 25, 35, 45, 55,  65, 84, 120),
                          labels=c('14-15', '16-19', '20-24', '25-34', '35-44', '45-54', '55-64', '65-84' , '85+'), right=F)

table5   <- table2x_func(TABLE_NAME,  'inj_yr',  'age_cat' )
       
      ###### END TABLE 5 ##########






        ###### START TABLE 6 ##########

# Table 6. Claims by Gender and Year of Injury

.Gender_func <- function(x){
  if(x =='M'){
    TABLE_NAME$gender = 'Male'
  }else if(x =='F'){   
    TABLE_NAME$gender = 'Female'
  }else {TABLE_NAME$gender = 'Unknown' 
  }   #### End if statement
}           #### End function



# Make gender variable
TABLE_NAME$gender <- sapply(TABLE_NAME$GNDR_CDE , .Gender_func)

table6   <- table2x_func(TABLE_NAME, 'inj_yr', 'gender')


        ###### END TABLE 6 ##########

# Create one additional variable to be used in the injuries by county table





# Need table TNzip created by MergeZips.R; run MergeZips.R script prior to this one 

# Make ER_fiveZC variable
TABLE_NAME$ER_fiveZC <- substr(TABLE_NAME$EMPR_PSTL_CDE, 1, 5)
# Make EE_fiveZC variable
TABLE_NAME$EE_fiveZC <- substr(TABLE_NAME$EMPL_PSTL_CDE, 1, 5)
# Make Inj_fiveZC variable
TABLE_NAME$Inj_fiveZC <- substr(TABLE_NAME$PSTL_INJR_SITE, 1, 5)


# Use a dplyr left join on TABLE_NAME and TNzip tables; common columns are Inj_fiveZC & Zip

TABLE_NAME <- left_join(TABLE_NAME, TNzip, by = c('Inj_fiveZC' = 'Zip') )  

        ###### START TABLE 7 & 7a ##########


# Table 7. Claims by County of Injury Site (TN only) and Year of Injury
table7   <- table2x_func(TABLE_NAME,  'inj_yr',  'Counties' )


# Table 7a. Claims by Geographic Region of Injury Site and Year of Injury
table7a   <- table2x_func(TABLE_NAME,  'inj_yr',  'Division' )

      ####### BIG PROBLEM #########


      # These comments from MergeZips.R
# We know that a single zip code can appear in two counties; e.g. 38305 is both Madison and Carroll counties
# This can be an issue; find the number of duplicates in the Zip column of TNzip

# Using this code in MergeZips.R
        ## DupCounties <- sum(duplicated(TNzip$Zip))            # there are 358 duplicates; this is more than expected
# Therefore, you can't assign count injury sites by county

# In fact, there are cases when the same zip code occurs in both EAST and MIDDLE; e.g. Sequatchie County	37365	EAST
# and Grundy County	37365	MIDDLE; therefore even counting injury sites by EAST, MIDDLE, WEST will have some error



        ###### END TABLE 7 & 7a ##########








      ###### START TABLE 7b and TNtable7b    ##########

# Table 7b. Claims by Postal Code of Injury Site and Year of Injury
# TNtable 7b. Claims by Postal Code of Injury Site and Year of Injury (Tennessee zip codes only)




table7b  <- tableZip20x_func(TABLE_NAME, 'inj_yr', 'Inj_fiveZC')

############# See Bottom of page for TNtable ###########################

        ###### END TABLE 7b  ##########




        ###### START TABLE 7c and TNtable7c ##########

# Table 7c. Claims by Postal Code of Employer and Year of Injury
# TNtable 7c. Claims by Postal Code of Employer and Year of Injury (Tennessee zip codes only)



table7c  <- tableZip20x_func(TABLE_NAME, 'inj_yr', 'ER_fiveZC')

    ############# See Bottom of page for TNtable 

        ###### END TABLE 7c  ##########










        ###### START TABLE 7d and TNtable7d ##########

# Table 7d. Claims by Postal Code of Employee and Year of Injury
# TNtable 7d. Claims by Postal Code of Employee and Year of Injury (Tennessee zip codes only)



table7d  <- tableZip20x_func(TABLE_NAME, 'inj_yr', 'EE_fiveZC')  

# Move the lines below to the Merge.Zips.R file since the TNzip table therein is required


      ############# See Bottom of page for TNtable #########################

        ###### END TABLE 7d  ##########



# There is no table 8





            ###### BEGIN TABLE 9 ##########

# Total FROI, SROI, and denials


# Using dplyr, find the number of SROI CLM_TYP claims by year in Basetable_sr

# Remove duplicates if present
Basetable_sr <- Basetable_sr[!duplicated(Basetable_sr$SROI_TRAN_NBR) , ]

# Create inj_yr variable for Basetable_sr
Basetable_sr <- mutate(Basetable_sr, 
                      inj_yr = format(as.Date(DTE_OF_INJR, format="%Y-%m-%d"),"%Y"))


# Using dplyr, find the last (i.e. latest entry) row for each unique claim number in Basetable_sr

LastSROI_TRN <- Basetable_sr %>%
  group_by(inj_yr, CLM_TRAN_NBR) %>%
  slice(which.max(DTE_OF_INJR) )

# Make temp df
temp <- LastSROI_TRN %>%
  group_by(inj_yr) 

##############################################################################################
# There was no SROI data for 2014 and that for 2015 was not helpful; comment out lines below
##############################################################################################


# Count the medical and indemnity claims in the data where:

# These lines are commented out. Probably won't be needed.   

# # Medical Only = ( M + B )
# countM_B <- length(which(LatestSROI_TRN$CLM_TYP=='B'  | LatestSROI_TRN$CLM_TYP=='M'))
# 
# # Indemnity    = ( I + L )
# countI_L <- length(which(LatestSROI_TRN$CLM_TYP=='I'  | LatestSROI_TRN$CLM_TYP=='L'))
# 
# # Other        = ( N + T )
# countN_T <- length(which(LatestSROI_TRN$CLM_TYP=='N'  | LatestSROI_TRN$CLM_TYP=='T'))

# Use the dummies package to create dummy variables for all CLM_TYP (i.e. B, I, L, M, N, & T); T is rare

# Comment these lines out
# temp_dummy <- as.data.frame(dummies::dummy(temp$CLM_TYP, verbose=F))
# 
# temp_merge <- dplyr::bind_cols(temp, temp_dummy)
# names(temp_merge) <- c( "CLM_TRAN_NBR" , "DTE_OF_INJR", "SROI_TRAN_NBR" ,  "MTC"  ,                  
#                           "MTC_DTE"  ,   "CLM_STA" , "CLM_TYP" ,  "inj_yr",              
#                           "B",  "I",  "L",  "M", "N",  "Missing")   # T did not appear in 2015 data
# 
# table9_sr <- temp_merge %>%
#           group_by(inj_yr) %>%
#           summarise(
#             countB = sum(B),
#             countI = sum(I),
#             countL = sum(L),
#             countM = sum(M),
#             countN = sum(N),
#             countNA = sum(Missing)) %>%
#           group_by(inj_yr) %>%
#           transmute(Medical.Only = (countB + countM),
#                     Indemnity =    (countI + countL),
#                     Other =        (countNA + countN)  )
# 
               


# Produce a table showing the type of claims

table9 <- TABLE_NAME %>%
           group_by(inj_yr, Match, OWNER_DESC, MEEI_DESC) %>%
            count(inj_yr, Match, OWNER_DESC, MEEI_DESC)
           
table9$Match[is.na(table9$Match)] <- 'Match'



                ###### END TABLE 9 ##########

  

      ###### START TABLES 10a, 10b, and 10c  ##########

# 10a Two digit zip code claims
# 10b Three digit zip code claims
# 10c Four digit zip code claims

# Table 10a Claims by Two Digit NAICS Code
table10a_temp   <- table2xRate_func(MPS_TABLE, 'inj_yr', 'Naics2')   # This is the UI data; MP_SE_ND_data
names(table10a_temp) <-   c('Year', 'Naics2' ,    'MPS.Claims')
temp_table <- table2xRate_func(MP_TABLE,  'inj_yr', 'Naics2')   # This is the QCEW data; MP_ND_data
names(temp_table) <- c('Year', 'Naics2' ,    'MP.Claims')
# Merge the two
table10a <- merge(table10a_temp, temp_table, all.x=T)

# Table 10b Claims by Three Digit NAICS Code
table10b_temp <- table2xRate_func(MPS_TABLE, 'inj_yr', 'Naics3')
names(table10b_temp) <-   c('Year', 'Naics3' ,    'MPS.Claims')
temp_table <- table2xRate_func(MP_TABLE,  'inj_yr', 'Naics3')   # This is the QCEW data; MP_ND_data
names(temp_table) <- c('Year', 'Naics3' ,    'MP.Claims')
# Merge the two
table10b <- merge(table10b_temp, temp_table, all.x=T)

# Table 10c Claims by Four Digit NAICS Code
table10c_temp <- table2xRate_func(MPS_TABLE, 'inj_yr', 'Naics4')
names(table10c_temp) <-   c('Year', 'Naics4' ,    'MPS.Claims')
temp_table <- table2xRate_func(MP_TABLE,  'inj_yr', 'Naics4')   # This is the QCEW data; MP_ND_data
names(temp_table) <- c('Year', 'Naics4' ,    'MP.Claims')
# Merge the two
table10c <- merge(table10c_temp, temp_table, all.x=T)

# To add employment numbers, use the function firm_emp_func (run Est_EmploymentRev2.R first) to generate tables 
#  and then merge tables at bottom of this page 


# Output from function firm_emp_func
table10a_emp <- firm_emp_func(MPS_TABLE)[[5]] # Number of employees by 2 digit industry # This is the UI data; MP_SE_ND_data

table10b_emp <- firm_emp_func(MPS_TABLE)[[4]] # Number of employees by 3 digit industry

table10c_emp <- firm_emp_func(MPS_TABLE)[[3]] # Number of employees by 4 digit industry

      ###### END TABLES 10a, 10b, and 10c  ##########







        ###### START TABLE 11 ##########

# Table 11. Average Employee Tenure (with current Employer) for Claims by 4 digit NAICS Code

# Use the tenure_yrs variable created earlier

table11 <-    MP_TABLE %>%                          # This is the QCEW data; MP_ND_data
  group_by(inj_yr, Naics4) %>%
  select( inj_yr, tenure_yrs, Naics4 ) %>%
  summarise(claims = n(),       
            avg_tenure= round(mean(tenure_yrs, na.rm=T) , 2))  %>%
  mutate(  Pct.=round(claims * 100 /sum(claims) , digits=2) )
 
names(table11)  <- c('Year', 'Naics4' , 'MP.Claims', 'Avg.Tenure', 'Pct.MP.Claims')
                    
                  
        ###### END TABLE 11 ##########








      ###### START TABLE 12 ##########

# Table 12. Claims by Size of Employer

# Need to create levels for firm size based on QCEW
# Groups are (< 5, 5-9, 10-19, 20-49, 50-99, 100-249, 250-499, 500-999, 1000+)
# Create variable firm_size_category; consistent w/ Clean2015InputRev6 file ***************ARBITRARILY PICKED EMPL2 **************

# Table 12 Claims per firm size category

table12_temp <- table2xRate_func(MPS_TABLE, 'inj_yr', 'firm_size_cat')          # This is the UI data; MPS_TABLE
names(table12_temp)  <- c('Year', 'Firm.Sz.Cat','MPS.Claims')   

temp_table <- table2xRate_func(MP_TABLE,  'inj_yr', 'firm_size_cat')   # This is the QCEW data; MP_ND_data
names(temp_table) <- c('Year', 'Firm.Sz.Cat' ,    'MP.Claims')
# Merge the two
table12 <- merge(table12_temp, temp_table, all.x=T)

table12_emp <- firm_emp_func(MPS_TABLE)[[6]] # # Number of employees by firm size category





###### END TABLE 12 ##########




# Change column names to be uniform and avoid underscore '_' to be compatible w/ Latex


names(table1)   <- c('Year', 'Body.Part', 'Claims',  'Pct')
names(table1a)  <- c('Year', 'Body.Part', 'Body.Part.Desc',     'Claims', 'Pct')
names(table2)   <- c('Year', 'Inj.Cause', 'Claims',  'Pct')
names(table2a)  <- c('Year', 'Inj.Cause', 'Inj.Cause.Detailed', 'Claims', 'Pct')
names(table3)   <- c('Year', 'Inj.Nature','Claims',  'Pct')
names(table3a)  <- c('Year', 'Inj.Nature', 'Inj.Nature.Desc',   'Claims', 'Pct')
names(table4)   <- c('Year', 'Self.Ins',  'Claims',  'Pct')
names(table5)   <- c('Year', 'Age.Cat' ,  'Claims',  'Pct')
names(table6)   <- c('Year',  'Gender',   'Claims',  'Pct')
names(table7)   <- c('Year',  'County',   'Claims',  'Pct')
names(table7a)   <- c('Year',  'Grand.Division' ,   'Claims',  'Pct')
# Reserved for table7a

names(table7b)  <- c('Year', 'Inj.FiveZC' ,'Claims', 'Pct')
names(table7c)  <- c('Year', 'ER.FiveZC' , 'Claims', 'Pct')
names(table7d)  <- c('Year', 'EE.FiveZC' , 'Claims', 'Pct')


names(table9)     <- c('Year', 'Match.Status', 'Owner.Desc' , 'Establishment.Desc',   'Claims')

# names(table10a) <- c('Year', 'Naics2' ,    'MPS.Claims')
# names(table10b) <- c('Year', 'Naics3' ,    'MPS.Claims')
# names(table10c) <- c('Year', 'Naics4' ,    'MPS.Claims')
# names(table11)  <- c('Year', 'Naics4' ,    'MPS.Claims', 'Avg.Tenure', 'Pct.MPS.Claims')
# names(table12)  <- c('Year', 'Firm.Sz.Cat','MPS.Claims')   


##################################################################################







#####################################################################################
# Create a merge function for table10a table10b, table10c, & table12 to their corresponding '_emp' tables
# The resultant merged tables have computed injury rates for the classifications
# These rates are valid only for matched PRIVATE SINGLE ESTABLISHMENT (i.e. MPS) firms

table10a_UI <- merge(table10a, table10a_emp, by.x= c('Year', 'Naics2'), by.y= c('inj_yr', 'Naics2' ), all.x=T)
table10a_UI$MPS.Rate <- round((table10a_UI$MPS.Claims * 100 / table10a_UI$ind2_emp ), digits=1)
names(table10a_UI) <- c("Year"  ,"Naics2",    "MPS.Claims",  "MP.Claims", "UI.Empl", "MPS.Rate")

table10b_UI <- merge(table10b, table10b_emp, by.x= c('Year', 'Naics3'), by.y= c('inj_yr', 'Naics3' ), all.x=T)
table10b_UI$MPS.Rate <- round((table10b_UI$MPS.Claims * 100 / table10b_UI$ind3_emp ), digits=1)
names(table10b_UI) <- c("Year"  ,"Naics3",    "MPS.Claims",  "MP.Claims", "UI.Empl", "MPS.Rate")

table10c_UI <- merge(table10c, table10c_emp, by.x= c('Year', 'Naics4'), by.y= c('inj_yr', 'Naics4' ), all.x=T)
table10c_UI$MPS.Rate <- round((table10c_UI$MPS.Claims * 100 / table10c_UI$ind4_emp ), digits=1)
names(table10c_UI) <- c("Year"  ,"Naics4",    "MPS.Claims",  "MP.Claims", "UI.Empl", "MPS.Rate")

table12_UI <- merge(table12, table12_emp, by.x= c('Year', 'Firm.Sz.Cat'), by.y= c('inj_yr', 'firm_size_cat' ), all.x=T)
# Sort the rows
table12_UI <- table12_UI[order(table12_UI$Year, table12$Firm.Sz.Cat), ]
table12_UI$MPS.Rate <- round((table12_UI$MPS.Claims * 100 / table12_emp$sz_cat_emp ), digits=1)
names(table12_UI) <- c("Year"  ,"Firm.Sz.Cat",    "MPS.Claims",  "MP.Claims", "UI.Empl", "MPS.Rate")


# Now merge the UI tables with QCEW employment tables (from Est_EmploymentRev2.R script) for 2014 and 2015

# NAICS two digit
table10a_merged <- merge(table10a_UI, QCEWprivateTNtwo, by.x=c('Year', 'Naics2'), by.y=c('year', 'industry_code'), all.x=TRUE, all.y=TRUE )
# Keep selected columns
table10a_merged <- table10a_merged[,c(1:6,11)]
# Rename the columns
names(table10a_merged) <- c("Year"  ,"Naics2",    "MPS.Claims",  "MP.Claims", "UI.Empl", "MPS.Rate", "QCEW.Empl")

#################################################################################################################
# The next section is unique to table10a_merged; create a loop to apply the code to each year of the data separately 
#################################################################################################################
loop.year <- c('2014', '2015')
table10a_temp <- data.frame()         # initialize data frame

for (i in 1:length(loop.year)){       # beginning of loop
  
          
        # this is a temp table  
        work.table <- filter(table10a_merged, Year== loop.year[i])
        # For Naics 2 digit to follow convention, need to combine 31, 32, & 33 to 31-33,  44 & 45 to 44-45, and 48 & 49 to 48-49
        # Combine MPS.Claims
        work.table[which(work.table$Naics2=='31-33'), 3 ] <- sum(work.table[which(work.table$Naics2=='31'), 3 ] ,
                                                                           work.table[which(work.table$Naics2=='32'), 3 ] ,
                                                                           work.table[which(work.table$Naics2=='33'), 3 ])
        
        
        work.table[which(work.table$Naics2=='44-45'), 3 ] <- sum(work.table[which(work.table$Naics2=='44'), 3 ] ,
                                                                           work.table[which(work.table$Naics2=='45'), 3 ])
        
        work.table[which(work.table$Naics2=='48-49'), 3 ] <- sum(work.table[which(work.table$Naics2=='48'), 3 ] ,
                                                                           work.table[which(work.table$Naics2=='49'), 3 ])
        
        
        # Combine MP.Claims
        work.table[which(work.table$Naics2=='31-33'), 4 ] <- sum(work.table[which(work.table$Naics2=='31'), 4 ] ,
                                                                           work.table[which(work.table$Naics2=='32'), 4 ] ,
                                                                           work.table[which(work.table$Naics2=='33'), 4 ])
        
        
        work.table[which(work.table$Naics2=='44-45'), 4 ] <- sum(work.table[which(work.table$Naics2=='44'), 4 ] ,
                                                                           work.table[which(work.table$Naics2=='45'), 4 ])
        
        work.table[which(work.table$Naics2=='48-49'), 4 ] <- sum(work.table[which(work.table$Naics2=='48'), 4 ] ,
                                                                           work.table[which(work.table$Naics2=='49'), 4 ])
        
        # Combine UI.Empl
        work.table[which(work.table$Naics2=='31-33'), 5 ] <- sum(work.table[which(work.table$Naics2=='31'), 5 ] ,
                                                                           work.table[which(work.table$Naics2=='32'), 5 ] ,
                                                                           work.table[which(work.table$Naics2=='33'), 5 ])
        
        
        work.table[which(work.table$Naics2=='44-45'), 5 ] <- sum(work.table[which(work.table$Naics2=='44'), 5 ] ,
                                                                           work.table[which(work.table$Naics2=='45'), 5 ])
        
        work.table[which(work.table$Naics2=='48-49'), 5 ] <- sum(work.table[which(work.table$Naics2=='48'), 5 ] ,
                                                                           work.table[which(work.table$Naics2=='49'), 5 ])
        
        # Calculate MPS.Rate
        work.table[which(work.table$Naics2=='31-33'), 6 ] <- format((work.table[which(work.table$Naics2=='31-33'), 3 ] * 100 /
                                                                           work.table[which(work.table$Naics2=='31-33'), 5 ]) , digits=2, nsmall=1)
        
        
        work.table[which(work.table$Naics2=='44-45'), 6 ] <- format(work.table[which(work.table$Naics2=='44-45'), 3 ] * 100 /
                                                                           work.table[which(work.table$Naics2=='44-45'), 5 ] , digits=2, nsmall=1)
        
        work.table[which(work.table$Naics2=='48-49'), 6 ] <- format(work.table[which(work.table$Naics2=='48-49'), 3 ] * 100 /
                                                                           work.table[which(work.table$Naics2=='48-49'), 5 ] , digits=2, nsmall=1)
        
        
        # Calculate another rate based on matched private sector claims (numerator) & QCEW empl. (denominator)
        work.table$MP.Rate <- round(work.table$MP.Claims * 100/work.table$QCEW.Empl , digits=1)
        # Rearrange the column order
        work.table <- work.table[, c(1:5, 7, 6, 8)]
        
        # Remove extraneous rows for Naics2=='10', 31', '32', '33', '44', '45', '48', & '49'
        work.table <- work.table[- which(work.table$Naics2=='10')  , ]
        work.table <- work.table[- which(work.table$Naics2=='31')  , ]
        work.table <- work.table[- which(work.table$Naics2=='32')  , ]
        work.table <- work.table[- which(work.table$Naics2=='33')  , ]
        work.table <- work.table[- which(work.table$Naics2=='44')  , ]
        work.table <- work.table[- which(work.table$Naics2=='45')  , ]
        work.table <- work.table[- which(work.table$Naics2=='48')  , ]
        work.table <- work.table[- which(work.table$Naics2=='49')  , ]
        
        table10a_temp <- rbind(table10a_temp , work.table )
}                                                         # End of loop

# Reassign the result of the loop to table10a_merged
table10a_merged <- table10a_temp
#################################################################################################################
#################################################################################################################

# Repeat merge the UI tables with QCEW employment tables (from Est_EmploymentRev2.R script) for 2014 and 2015 for three digit
table10b_merged <- merge(table10b_UI, QCEWprivateTNthree, by.x=c('Year', 'Naics3'), by.y=c('year', 'industry_code'), all.x=TRUE ) 
table10b_merged <- table10b_merged[,c(1:6,11)]
names(table10b_merged) <- c("Year"  ,"Naics3",    "MPS.Claims",  "MP.Claims", "UI.Empl", "MPS.Rate", "QCEW.Empl")
table10b_merged$MP.Rate <- round(table10b_merged$MP.Claims * 100/table10b_merged$QCEW.Empl , digits=1)
table10b_merged <- table10b_merged[, c(1:5, 7, 6, 8)]

# Repeat for # NAICS four digit
table10c_merged <- merge(table10c_UI, QCEWprivateTNfour , by.x=c('Year', 'Naics4'), by.y=c('year', 'industry_code'), all.x=TRUE ) 
table10c_merged <- table10c_merged[,c(1:6,11)]
names(table10c_merged) <- c("Year"  ,"Naics4",    "MPS.Claims",  "MP.Claims", "UI.Empl", "MPS.Rate", "QCEW.Empl")
table10c_merged$MP.Rate <- round(table10c_merged$MP.Claims * 100/table10c_merged$QCEW.Empl , digits=1)
table10c_merged <- table10c_merged[, c(1:5, 7, 6, 8)]

# Repeat for size categories
table12_merged <- merge(table12_UI, QCEWSizeData , by.x=c('Year', 'Firm.Sz.Cat'), by.y=c('year', 'firm_size_cat'), all.x=TRUE ) 

# Change column order for printing
names(table12_merged) <- c("Year"  ,"Firm.Sz.Cat",    "MPS.Claims",  "MP.Claims", "UI.Empl", "MPS.Rate", "QCEW.Size", "QCEW.Empl")
table12_merged$MP.Rate <- round(table12_merged$MP.Claims * 100/table12_merged$QCEW.Empl , digits=1)
#  table12_merged <- table12_merged[, c(1:6, 8, 7, 9)]
# Reduce the size of the table to omit MP.Rate and MP.Claims; b/c size classes
#       rates are invalid for all but single estab. enterprises 
table12_merged <- table12_merged[, c(1:3, 5, 6)]


##################################################################################
# Read in the csv file containing NAICS industry descriptions
NAICS2012 <- read.csv('./2012.NAICS.csv' , sep=',')
# Remove the T suffix with/without a space after the Tß
NAICS2012$Desc. <- gsub("T$", "", NAICS2012$Desc., perl=T)
NAICS2012$Desc. <- gsub("T $", " ", NAICS2012$Desc., perl=T)
##################################################################################

# Need one last merge to add the industry description text

table10a_final <- merge(table10a_merged, NAICS2012, by.x=('Naics2'), by.y=('NAICS'), all.x=TRUE)
table10a_final <- table10a_final[c(1, 9, 2:8)]
# Desc. is a factor; change to character
table10a_final$Desc. <- as.character(table10a_final$Desc.)
# MPS.Rate is character; change to number
table10a_final$MPS.Rate <- as.numeric(table10a_final$MPS.Rate)


table10b_final <- merge(table10b_merged, NAICS2012, by.x=('Naics3'), by.y=('NAICS'), all.x=TRUE)
table10b_final <- table10b_final[c(1, 9, 2:8)]
table10b_final$Desc. <- as.character(table10b_final$Desc.)

table10c_final <- merge(table10c_merged, NAICS2012, by.x=('Naics4'), by.y=('NAICS'), all.x=TRUE)
table10c_final <- table10c_final[c(1, 9, 2:8)]
table10c_final$Desc. <- as.character(table10c_final$Desc.)

###################################################################################
# 2 Digit Naics Prediction Intervals for 2015
###################################################################################
# 10aa Prediction Intervals for 10a
# Create some additional tables to include prediction intervals for tables 10a, 10b, and 10c
#       and FTE adjustments for 2015 rates
# First, create new tables keeping only the necessary columns
table10aa <- table10a_final[, c(1:3,5,9)]  # Keep only the columns with rates
table10aa <- filter(table10aa, Year=='2015')          ## Filter for 2015


# Import the table having the 2015 BLS industry rates
FTEtable2 <- read.csv('./FTE2015TN2.csv', sep=',')  # 2 digit NAICS FTE adj. factors

# Add the FTE.Adj to table10aa using the match function
temp <- match(table10aa$Naics2, FTEtable2$naicsp)   # get an index for the NAICS matches
table10aa$ACS.FTEadj <- round((FTEtable2$tnFTEadjFactor[temp]), 2) # apply the index to FTEtable2 to add column FTEadj
table10aa$MP.Adj <- round((table10aa$MP.Rate/table10aa$ACS.FTEadj), 1)  # compute MP.Adj

# Compute the prediction intervals for TP.Adj (total rate) based on the table1a distribution (see approx. line 200 above)
# The vector 'limits' is a vector of the 90% prediction interval of the 2015 MP to TP claims (mean= 0.693)
# Therefore, 1/limits is 2015 vector of multipliers; c(1.21, 1.80)
table10aa$TP.Adj.L90 <- round((table10aa$MP.Adj * 1.21), 1)
table10aa$TP.Adj.U90 <- round((table10aa$MP.Adj * 1.80), 1)

# Add the BLS rates to the table
#Import BLS 2 and 3 digit rates
BLStable <- read.csv('./BLSrates.csv', sep=',')

# Add the BLS.Rate to table10aa using the match function
temp <- match(table10aa$Naics2, BLStable$Naics)   # get an index for the NAICS matches
table10aa$BLS.Rate <- round((BLStable$BLS2015[temp]), 2) # apply the index to FTEtable2 to add column FTEadj

###################################################################################
# 3 Digit Naics Prediction Intervals for 2015
###################################################################################
# 10bb Prediction Intervals for 10b
# Create some additional tables to include confidence intervals for table 10b
#       and FTE adjustments for 2015 rates
# First, create new tables keeping only the necessary columns
table10bb <- table10b_final[, c(1:3,5,9)]
table10bb <- filter(table10bb, Year=='2015')      ## Filter for 2015
 # Keep only the columns with rates

# Import the table having the 2015 BLS industry rates
FTEtable3 <- read.csv('./FTE2015TN3.csv', sep=',')  # 3 digit NAICS FTE adj. factors

# Add the FTE.Adj to table10bb using the match function
temp <- match(table10bb$Naics3, FTEtable3$naicsp)   # get an index for the NAICS matches
table10bb$ACS.FTEadj <- round((FTEtable3$tnFTEadjFactor[temp]), 2) # apply the index to FTEtable3 to add column FTEadj
table10bb$MP.Adj <- round((table10bb$MP.Rate/table10bb$ACS.FTEadj), 1)  # compute MP.Adj

# Compute the prediction interval for TP.Adj (total rate) based on the table1a distribution (see approx. line 200 above)
table10bb$TP.Adj.L90 <- round((table10bb$MP.Adj * 1.21), 1)
table10bb$TP.Adj.U90 <- round((table10bb$MP.Adj * 1.80), 1)


# Add the BLS.Rate to table10bb using the match function
temp <- match(table10bb$Naics3, BLStable$Naics)   # get an index for the NAICS matches
table10bb$BLS.Rate <- round((BLStable$BLS2015[temp]), 2) # apply the index to FTEtable2 to add column FTEadj

###################################################################################
# 4 Digit Naics Prediction Intervals for 2015
###################################################################################
# 10cc Prediction Intervals for 10c
# Create some additional tables to include prediction intervals for table 10c
#       and FTE adjustments for 2015 rates
# First, create new tables keeping only the necessary columns
table10cc <- table10c_final[, c(1:3,5,9)]
table10cc <- filter(table10cc, Year=='2015')    ## Filter for 2015
# Keep only the columns with rates

# Import the csv file listing the 2015 BLS industry rates
FTEtable4 <- read.csv('./FTE2015TN4.csv', sep=',')  # 4 digit NAICS FTE adj. factors

# Add the FTE.Adj to table10cc using the match function
temp <- match(table10cc$Naics4, FTEtable4$naicsp)   # get an index for the NAICS matches
table10cc$ACS.FTEadj <- round((FTEtable4$tnFTEadjFactor[temp]), 2) # apply the index to FTEtable4 to add column FTEadj
table10cc$MP.Adj <- round((table10cc$MP.Rate/table10cc$ACS.FTEadj), 1)  # compute MP.Adj

# Compute the prediction interval for TP.Adj (total rate) based on the table1a histogram/distribution (see approx. line 200 above)
table10cc$TP.Adj.L90 <- round((table10cc$MP.Adj * 1.21), 1)
table10cc$TP.Adj.U90 <- round((table10cc$MP.Adj * 1.80), 1)


# Add the BLS.Rate to table10cc using the match function
temp <- match(table10cc$Naics4, BLStable$Naics)   # get an index for the NAICS matches
table10cc$BLS.Rate <- round((BLStable$BLS2015[temp]), 2) # apply the index to FTEtable4 to add column FTEadj
###################################################################################




###################################################################################
# Size Tables - Table 12 series
###################################################################################

# Create some additional tables to include prediction intervals for table 12
#       and FTE adjustments for 2015 rates
# First, create new tables keeping only the necessary columns
table12_final <- table12_merged[, c(1:2,4,8)]
table12_final <- filter(table12_final, Year=='2015')   ## Filter for 2015
# Keep only the columns with rates



# Compute the prediction interval for TP.Adj (total rate) based on the table1a distribution (see approx. line 200 above)
table12_final$TP.Rate.L90 <- round((table12_final$MP.Rate * 1.21), 1)
table12_final$TP.Rate.U90 <- round((table12_final$MP.Rate * 1.80), 1)
###################################################################################
# Make some priority rank tables from Tables 10aa, 10bb, and 10cc
table10aa_rank <- table10aa
table10bb_rank <- table10bb
table10cc_rank <- table10cc

# Filter table10xx_rank to remove infinite rates; Inf rate probably occurs only w/ few obs. in category
table10aa_rank <- filter(table10aa_rank, table10aa_rank$MP.Rate!='Inf' & !is.na(table10aa$Desc.))
table10bb_rank <- filter(table10bb_rank, table10bb_rank$MP.Rate!='Inf' & !is.na(table10bb$Desc.))
table10cc_rank <- filter(table10cc_rank, table10cc_rank$MP.Rate!='Inf' & !is.na(table10cc$Desc.))

# Add a column for rank of MP.Rate 
# Negate x so that highest rates and claims have lowest ranks
table10aa_rank$MP.Rank <- rank(-table10aa_rank$MP.Rate, na.last=T)
table10bb_rank$MP.Rank <- rank(-table10bb_rank$MP.Rate, na.last=T)
table10cc_rank$MP.Rank <- rank(-table10cc_rank$MP.Rate, na.last=T)

# Add a column for rank of total private claims named CLM.Rank
table10aa_rank$CLM.Rank <- rank(-table10aa_rank$MP.Claims, na.last=T)
table10bb_rank$CLM.Rank <- rank(-table10bb_rank$MP.Claims, na.last=T)
table10cc_rank$CLM.Rank <- rank(-table10cc_rank$MP.Claims, na.last=T)

# Avg claim rank & rate rank to equal composite score
table10aa_rank$Comp.Score <- (table10aa_rank$CLM.Rank + table10aa_rank$MP.Rank)/2
table10bb_rank$Comp.Score <- (table10bb_rank$CLM.Rank + table10bb_rank$MP.Rank)/2
table10cc_rank$Comp.Score <- (table10cc_rank$CLM.Rank + table10cc_rank$MP.Rank)/2



# Order the tables; sort in order of increasing composite score (i.e. lower score = lower rank & higher priority)
table10aa_rank <- table10aa_rank[order(table10aa_rank$Comp.Score, decreasing=F), ]
table10bb_rank <- table10bb_rank[order(table10bb_rank$Comp.Score, decreasing=F), ]
table10cc_rank <- table10cc_rank[order(table10cc_rank$Comp.Score, decreasing=F), ]



# Make Rank.Ord column; not used here
# table10aa_rank$Rank.Ord <- c(1:nrow(table10aa_rank))
# table10bb_rank$Rank.Ord <- c(1:nrow(table10bb_rank))
# table10cc_rank$Rank.Ord <- c(1:nrow(table10cc_rank))

# Drop unneeded columns
table10aa_rank <- table10aa_rank[, c(13, 1,2,4,5,11:12) ]
table10bb_rank <- table10bb_rank[, c(13, 1,2,4,5,11:12) ]
table10cc_rank <- table10cc_rank[, c(13, 1,2,4,5,11:12) ]

row.names(table10aa_rank) <- c(1:nrow(table10aa_rank))
row.names(table10bb_rank) <- c(1:nrow(table10bb_rank))
row.names(table10cc_rank) <- c(1:nrow(table10cc_rank))



#############################################################################################
# # This next section can be used to write those files beginning with 'table' in the R Studio
# environment into a temporary directory named 'Temp'
# Conversion problem: errors are present in the .csv files; '5-9' is read as May 5;
        # '10-19' is read as October 19
        # it will also read unwanted files beginning with 'table'

   for (i in ls()[  grep('^table'   ,ls())      ]){
 
   write.csv(get(i), paste0("../Temp/", i, "_" , Sys.Date(), ".csv") , sep=',' )
}
#############################################################################################
