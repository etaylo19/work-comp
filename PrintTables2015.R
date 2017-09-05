# May 15, 2017
# ELT
# This is a file to produce Latex code for printing tables; it's a cut and paste (into Latex editor) process 
# It uses the tables package and the tables produced with MakeTables2015Rev3.R file
# Zip Code tables 7b, 7c, and 7d are too long ---Do not print
# Table 8 is reserved for future use
# The desired Table 9 requires additional data elements DN85 and DN95 from TN BWC; will modify here 
# Tables 10a_merged, 10b_merged, and 10c_merged are too wide for 2014 and 2015 data
#       Use 2015 data only for these tables
# The goal is for the latex code to be used in a knitr document with booktabs
# Latex document must load packages longtable, booktabs, & knitr
# June 2, 2017 added tables 7, 11, and 12
# June 12; added printLongNineX_func
# June 15; added printLongTenX_func 
# June 28; modified printing for table 12

##### END OF NOTES  #####


# First, set the working directory to the folder where the data exist
setwd("/Users/edwardtaylorUTK/Desktop/2015WCfiles")


#### LOAD DESIRED PACKAGES ####

load_pkg <- c('dplyr', 'readr', 'data.table', 'tables' , 'knitr')      

for (i in 1:length(load_pkg)) {
  
  if (!require(load_pkg[i], character.only=T)){
    install.packages(load_pkg[i], 
                     repos="https://cran.rstudio.com/",
                     quiet=TRUE)
    require(load_pkg[i], character.only=T) #same as library(package name)
  }
}



        ################# Start Code Here  ######################

# This is sample code for table 1

# latex( tabular( Heading(Body.Part) *  factor(Body.Part) + Hline(1:3) + 1  ~                   # the rows are the body parts
#                     Heading(Year) * factor(Year) * ( (Claims * Heading() * sum) + (Pct. * Heading() * sum)  )  *
#                     DropEmpty(empty='.') , data=table1 ) )                                    # the columns are Claims & Pct

                                                                                                # Claims & Pct are grouped by Year


# This is sample code for table1a; it prints in the 'long' format hence the 'options' statement

# latex( tabular( Heading(Body.Part) *  factor(Body.Part) * Heading(Body.Part.Desc) * factor(Body.Part.Desc) + Hline(1:2) + 1  ~
#                   Heading(Year) * factor(Year) * ( (Claims * Heading() * sum) + (Pct. * Heading() * sum)  )  *
#                   DropEmpty(empty='.'),   data=table1a)  ,
#                     options = list(tabular="longtable",
#                       toprule="\\caption{This table crosses page boundaries.}\\\\
#                       \\toprule", midrule="\\midrule\\\\[-1\\normalbaselineskip]\\endhead\\hline\\endfoot") ) 



# Produce sample code for Table 2a
########### ADJUST COLUMN WIDTHS USING SOMETHING LIKE THIS {p{2.5}p{2.5}cccc}   ##########################

# latex( tabular( Heading(Inj.Cause) *  factor(Inj.Cause) * Heading(Inj.Cause.Detailed) * factor(Inj.Cause.Detailed) + Hline(1:2) + 1  ~  
#                   Heading(Year) * factor(Year) * ( (Claims * Heading() * sum) + (Pct. * Heading() * sum)  )  *
#                   DropEmpty(empty='.')  *  DropEmpty(empty='.') , data=table2a ),
#        options = list(tabular="longtable",
#                       toprule="\\caption{This table crosses page boundaries.}\\\\
#                       \\toprule", midrule="\\midrule\\\\[-1\\normalbaselineskip]\\endhead\\hline\\endfoot") )


# Many tables to be produced will have common formats; use a function to produce the latex code
# Write a function to produce Latex code for a table with four fields (e.g. table1)
# It may be simpler to use the 'longtable' format for all printing; i.e. substitute printLongFourX for printFourX

printFourX_func <- function(field1, field2, field3, field4,  dt ){
  latex( tabular( Heading(field1, character.only=T)  *  factor(dt[[field1]]) +  Hline(1:4) + 1  ~  
                  Heading(field2, character.only=T) *  factor(dt[[field2]]) * 
                 (Heading(field3, character.only=T) * (dt[[field3]] * Heading()  * sum * Heading() * complete.cases(dt))   +
                  Heading(field4, character.only=T) * (dt[[field4]] * Heading()  * sum * Heading() * complete.cases(dt)) ) *
                  DropEmpty(empty='.') , data=dt ) ,
                  options = list(tabular="tabular", 
                        toprule="\\caption{ table.}\\\\ \\hline
                        \\toprule" ,  midrule="\\midrule\\\\ [-1\\normalbaselineskip]\\hline\\endhead\\hline\\endfoot") )
}


# Now, use the code above and supplement it for a 'long' printFourX table that spans pages

printLongFourX_func <- function(field1, field2, field3, field4,  dt ){
  latex( tabular( Heading(field1, character.only=T) * factor(dt[[field1]])+  Hline(1:5) + 1  ~  
                  Heading(field2, character.only=T) * factor(dt[[field2]]) * 
                 (Heading(field3, character.only=T) *  (dt[[field3]] * Heading()  * sum * Heading() * complete.cases(dt))    +
                  Heading(field4, character.only=T) *  (dt[[field4]] * Heading()  * sum * Heading() * complete.cases(dt)) ) *
                  DropEmpty(empty='.')   , data=dt ) ,
                  options = list(tabular="longtable", 
                  toprule="\\caption{This is a long table.}\\\\ \\hline
                  \\toprule" ,  midrule="\\midrule\\\\ [-1\\normalbaselineskip]\\hline\\endhead\\hline\\endfoot") )
}




# Write a function to produce Latex code for a table with five fields (e.g. table1a)

printLongFiveX_func <- function(field1, field2, field3, field4, field5, dt ){
  latex( tabular(   Heading(field1, character.only=T) *  factor(dt[[field1]]) +  Hline(1:7) +  1  ~  
                    Heading(field2, character.only=T) *  factor(dt[[field2]]) *
                   (Heading(field3, character.only=T) * (dt[[field3]] * Heading()  * sum * Heading() * complete.cases(dt))   +
                    Heading(field4, character.only=T) * (dt[[field4]] * Heading()  * identity * Heading() * complete.cases(dt))   +  
                    Heading(field5, character.only=T) * (dt[[field5]] * Heading()  * sum * Heading() * complete.cases(dt)) ) *
                    DropEmpty(empty='.')   , data=dt ) ,
                    options = list(tabular="longtable", 
                    toprule="\\caption{This is a long table.}\\\\ \\hline
                    \\toprule" ,  midrule="\\midrule\\\\ [-1\\normalbaselineskip]\\hline\\endhead\\hline\\endfoot") )
}

# This one is different from above; note placement of opening parens
M_printLongFiveX_func <- function(field1, field2, field3, field4, field5, dt ){
  latex( tabular(   Heading(field1, character.only=T) *  factor(dt[[field1]]) *
                    Heading(field2, character.only=T) *  factor(dt[[field2]]) +  Hline(1:5) +  1  ~  
                    Heading(field3, character.only=T) *  factor(dt[[field3]]) *
                   (Heading(field4, character.only=T) *  (dt[[field4]] * Heading()  * sum * Heading() * complete.cases(dt))  +  
                    Heading(field5, character.only=T) *  (dt[[field5]] * Heading()  * sum * Heading() * complete.cases(dt)) )  *
                      DropEmpty(empty='.')   , data=dt ) ,
         options = list(tabular="longtable", 
                        toprule="\\caption{This is a long table.}\\\\ \\hline
                        \\toprule" ,  midrule="\\midrule\\\\ [-1\\normalbaselineskip]\\hline\\endhead\\hline\\endfoot") ) 
}




# Table 1, use a function to generate Latex code for table1
# Switch to Longtable form  if desired for easier printing
printLongFourX_func('Body.Part', 'Year', 'Claims', 'Pct', table1)



# Tqble 1a, use a function to generate Latex code for table1a
M_printLongFiveX_func('Body.Part', 'Body.Part.Desc' , 'Year', 'Claims', 'Pct', table1a)



# Produce code for Table 2

# Use the function to generate Latex code for table2
# Switch to Longtable if desired for easier printing
printLongFourX_func('Inj.Cause', 'Year', 'Claims', 'Pct', table2)


# Use the function to generate Latex code for table2a
M_printLongFiveX_func('Inj.Cause', 'Inj.Cause.Detailed' , 'Year', 'Claims', 'Pct', table2a)



# Produce code for Table 3

# Use the function to generate Latex code for table3
# Switch to Longtable if desired for easier printing
printLongFourX_func('Inj.Nature', 'Year', 'Claims', 'Pct', table3)


# Produce code for Table 3a

# Use the function to generate Latex code for table3a
M_printLongFiveX_func('Inj.Nature', 'Inj.Nature.Desc' , 'Year', 'Claims', 'Pct', table3a)

# Produce code for Table 4

# Use the function to generate Latex code for table4
# Switch to Longtable if desired for easier printing
printLongFourX_func('Self.Ins', 'Year', 'Claims', 'Pct', table4)

# Produce code for Table 5

# Use the function to generate Latex code for table5
# Switch to Longtable if desired for easier printing
printLongFourX_func('Age.Cat', 'Year', 'Claims', 'Pct', table5)


# Produce code for Table 6

# Use the function to generate Latex code for table6
# Switch to Longtable if desired for easier printing
printLongFourX_func('Gender', 'Year', 'Claims', 'Pct', table6)


# Produce code for Table 7

# Use the function to generate Latex code for table7
printLongFourX_func('County', 'Year', 'Claims', 'Pct', table7)

# Produce code for Table 7a

# Use the function to generate Latex code for table7a
printLongFourX_func('Grand.Division', 'Year', 'Claims', 'Pct', table7a)

# Produce code for Table 7b; this is a VERY long table

# Use the function to generate Latex code for table7b
# printLongFourX_func('Inj.FiveZC', 'Year', 'Claims', 'Pct', table7b)

# Produce code for TNtable 7b; this is a VERY long table

# Use the function to generate Latex code for TNtable7b
# printLongFourX_func('Inj.FiveZC', 'Year', 'Claims', 'Pct', TNtable7b)




# Produce code for Table 7c; this is a VERY long table

# Use the function to generate Latex code for table7c
# printLongFourX_func('ER.FiveZC', 'Year', 'Claims', 'Pct', table7c)

# Produce code for TNtable 7c; this is a VERY long table

# Use the function to generate Latex code for TNtable7c
# printLongFourX_func('ER.FiveZC', 'Year', 'Claims', 'Pct', TNtable7c)


# Produce code for Table 7d; this is a very, very long table

# Use the function to generate Latex code for table7d

# This is an error generated by the code below
# FUNCTION FAILED; ERROR
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
# Error during wrapup: evaluation nested too deeply: infinite recursion / options(expressions=)?


# printLongFourX_func('EE.FiveZC', 'Year', 'Claims', 'Pct', table7d)  

# Produce code for TNtable 7d; this is a long table
# printLongFourX_func('EE.FiveZC', 'Year', 'Claims', 'Pct', TNtable7d)



# Table 9 printing

# Use the function to generate Latex code for table9
# Need a new function; note placement of opening paren



.M1_printLongFiveX_func <- function(field1, field2, field3, field4, field5, dt ){
  latex( tabular(     Heading(field1, character.only=T) *  factor(dt[[field1]]) *
                      Heading(field2, character.only=T) *  factor(dt[[field2]]) * 
                      Heading(field4, character.only=T) *  factor(dt[[field4]]) +  Hline(1:5) +  1  ~  
                      Heading(field3, character.only=T) *  factor(dt[[field3]]) *
                      (Heading(field5, character.only=T) *  (dt[[field5]] * Heading()  * sum * Heading() * complete.cases(dt)) )  *
                      DropEmpty(empty='.') , data=dt ) ,
         options = list(tabular="longtable", 
                        toprule="\\caption{This is a long table.}\\\\ \\hline
                        \\toprule" ,  midrule="\\midrule\\\\ [-1\\normalbaselineskip]\\hline\\endhead\\hline\\endfoot") ) 
}

.M1_printLongFiveX_func('Match.Status', 'Owner.Desc' , 'Year', 'Establishment.Desc', 'Claims',  table9)

# Note: It may be desirable to print injury rates for Tables10a_merged, etc. If so,
#   this will require another function---- printLongEightX_func 
# These are column names of table to be printed
# "Year"       "Naics2"     "MPS.Claims" "MP.Claims"  "UI.Empl"    "QCEW.Empl"  "MPS.Rate"   "MP.Rate"  

printLongEightX_func <- function(field1, field2, field3, field4, field5, field6, field7, field8, dt ){
  latex( tabular(     Heading(field1, character.only=T) *  factor(dt[[field1]])  +  Hline(1:7) +  1  ~  
                      Heading(field2, character.only=T) * factor(dt[[field2]]) *
                     (Heading(field3, character.only=T) * (dt[[field3]] * Heading()  * sum      * Heading() )   + 
                      Heading(field4, character.only=T) * (dt[[field4]] * Heading()  * sum      * Heading() )   +
                      Heading(field5, character.only=T) * (dt[[field5]] * Heading()  * sum      * Heading() )   +
                      Heading(field6, character.only=T) * (dt[[field6]] * Heading()  * sum      * Heading() * complete.cases(dt))   + 
                      Heading(field7, character.only=T) * (dt[[field7]] * Heading()  * identity * Heading() )   +
                      Heading(field8, character.only=T) * (dt[[field8]] * Heading()  * identity * Heading() ) ) *
                      DropEmpty(empty='.') , data=dt ) ,
         options = list(tabular="longtable", 
                        toprule="\\caption{This is a long table.}\\\\ \\hline
                        \\toprule" ,  midrule="\\midrule\\\\ [-1\\normalbaselineskip]\\hline\\endhead\\hline\\endfoot") )
}

# Note: It may be desirable to print injury rates for Tables10a_final, etc. If so,
#   this will require another function---- printLongNineX_func 
# These are column names of table to be printed
# "Year"       "Naics2"     "MPS.Claims" "MP.Claims"  "UI.Empl"    "QCEW.Empl"  "MPS.Rate"   "MP.Rate"  

.printLongNineX_func <- function(field1, field2, field3, field4, field5, field6, field7, field8, field9, dt ){
  latex( tabular(     Heading(field1, character.only=T) *  factor(dt[[field1]])  +  Hline(1:8) +  1  ~  
                           Heading(field2, character.only=T) * factor(dt[[field2]]) *
                          (Heading(field3, character.only=T) * (dt[[field3]] * Heading()  * identity * Heading() )   +
                           Heading(field4, character.only=T) * (dt[[field4]] * Heading()  * sum      * Heading() )   + 
                           Heading(field5, character.only=T) * (dt[[field5]] * Heading()  * sum      * Heading() )   +
                           Heading(field6, character.only=T) * (dt[[field6]] * Heading()  * sum      * Heading() )   +
                           Heading(field7, character.only=T) * (dt[[field7]] * Heading()  * sum      * Heading() * complete.cases(dt))   + 
                           Heading(field8, character.only=T) * (dt[[field8]] * Heading()  * identity * Heading() )   +
                           Heading(field9, character.only=T) * (dt[[field9]] * Heading()  * identity * Heading() ) ) *
                        DropEmpty(empty='.') , data=dt ) ,
         options = list(tabular="longtable", 
                        toprule="\\caption{This is a long table.}\\\\ \\hline
                        \\toprule" ,  midrule="\\midrule\\\\ [-1\\normalbaselineskip]\\hline\\endhead\\hline\\endfoot") )
}
 

# We need this function to print tables 10aa, 10bb, & 10cc  w/ BLS rates for comparison 
.printLongTenX_func <- function(field1, field2, field3, field4, field5, field6, field7, field8, field9,  field10, dt ){
  latex( tabular(     Heading(field1, character.only=T) *  factor(dt[[field1]])  +  Hline(1:9) +  1  ~  
                        Heading(field2, character.only=T) * factor(dt[[field2]]) *
                        (Heading(field3, character.only=T) * (dt[[field3]] * Heading()  * identity * Heading() )   +
                           Heading(field4, character.only=T) * (dt[[field4]] * Heading()  * sum      * Heading() )   + 
                           Heading(field5, character.only=T) * (dt[[field5]] * Heading()  * identity * Heading() )   +
                           Heading(field6, character.only=T) * (dt[[field6]] * Heading()  * identity * Heading() )   +
                           Heading(field7, character.only=T) * (dt[[field7]] * Heading()  * identity * Heading() )   + 
                           Heading(field8, character.only=T) * (dt[[field8]] * Heading()  * identity * Heading() )   +
                           Heading(field9, character.only=T) * (dt[[field9]] * Heading()  * identity * Heading() )   +
                           Heading(field10, character.only=T) * (dt[[field10]] *Heading() * identity * Heading() ))  *
                        DropEmpty(empty='.') , data=dt ) ,
         options = list(tabular="longtable", 
                        toprule="\\caption{This is a long table.}\\\\ \\hline
                        \\toprule" ,  midrule="\\midrule\\\\ [-1\\normalbaselineskip]\\hline\\endhead\\hline\\endfoot") )
}


# Produce code for Table 10a and 10a_merged (w/rates):  merged tables appear too wide for printing -- use only 2015 data to print merged tables

# Use the function to generate Latex code for table10a
# Switch to Longtable if desired for easier printing
###### printLongFourX_func('Naics2', 'Year', 'Claims', 'Pct', table10a)
# 
# # Instead print table10a w/ rates added (i.e. table10a_merged w/ 2015 data only)
# printLongEightX_func('Naics2',   'Year',         'MPS.Claims', 'MP.Claims',  'UI.Empl' , 
#                      'QCEW.Empl', 'MPS.Rate', 'MP.Rate', 
#                      table10a_merged[which(table10a_merged$Year=='2015') , ])  

###################################################################################
# Instead print table10a w/ description added (i.e. table10a_final w/ 2015 data only)
.printLongNineX_func('Naics2', 'Year', 'Desc.', 'MPS.Claims', 'MP.Claims',  'UI.Empl' , 
                     'QCEW.Empl', 'MPS.Rate', 'MP.Rate', 
                     table10a_final[which(table10a_final$Year=='2015') , ]) 
###################################################################################

###################################################################################
# Instead print table10aa w/ BLS rate for comparison
.printLongTenX_func('Naics2', 'Year', 'Desc.', 'MP.Claims', 'MP.Rate', 'ACS.FTEadj',
                     'MP.Adj', 'TP.Adj.L90', 'TP.Adj.U90', 'BLS.Rate',
                     table10aa[which(table10aa$Year=='2015') , ]) 
###################################################################################
# Produce code for Table 10b

# Use the function to generate Latex code for table10b and 10b_merged (w/rates)
#######  printLongFourX_func('Naics3', 'Year', 'Claims', 'Pct', table10b)

# printLongEightX_func('Naics3', 'Year', 'MPS.Claims', 'MP.Claims',  'UI.Empl' , 
#                      'QCEW.Empl', 'MPS.Rate', 'MP.Rate', 
#                      table10b_merged[which(table10b_merged$Year=='2015') , ]) 

###################################################################################
# Instead print table10b_final w/ description added (i.e. table10b_final w/ 2015 data only)
.printLongNineX_func('Naics3', 'Year', 'Desc.', 'MPS.Claims', 'MP.Claims',  'UI.Empl' , 
                    'QCEW.Empl', 'MPS.Rate', 'MP.Rate', 
                    table10b_final[which(table10b_final$Year=='2015') , ]) 
###################################################################################







###################################################################################
# Instead print table10bb w/ BLS rates for comparison
.printLongTenX_func('Naics3', 'Year', 'Desc.', 'MP.Claims', 'MP.Rate', 'ACS.FTEadj',
                    'MP.Adj', 'TP.Adj.L90', 'TP.Adj.U90', 'BLS.Rate',
                    table10bb[which(table10bb$Year=='2015') , ]) 
###################################################################################



# Produce code for Table 10c

# Use the function to generate Latex code for table10c and 10c_merged (w/rates)
######  printLongFourX_func('Naics4', 'Year', 'Claims', 'Pct', table10c)

printLongEightX_func('Naics4', 'Year', 'MPS.Claims', 'MP.Claims',  'UI.Empl' , 
                     'QCEW.Empl', 'MPS.Rate', 'MP.Rate',  
                     table10c_merged[which(table10c_merged$Year=='2015') , ])  

###################################################################################
# Instead print table10c w/ description added (i.e. table10c_final w/ 2015 data only)
.printLongNineX_func('Naics4', 'Year', 'Desc.', 'MPS.Claims', 'MP.Claims',  'UI.Empl' , 
                     'QCEW.Empl', 'MPS.Rate', 'MP.Rate', 
                     table10c_final[which(table10c_final$Year=='2015') , ]) 
###################################################################################

###################################################################################
# Instead print table10cc
.printLongTenX_func('Naics4', 'Year', 'Desc.', 'MP.Claims', 'MP.Rate', 'ACS.FTEadj',
                    'MP.Adj', 'TP.Adj.L90', 'TP.Adj.U90', 'BLS.Rate',
                    table10cc[which(table10cc$Year=='2015') , ]) 
###################################################################################

###################################################################################
# Produce code for Table 12_merged (use single estab. only )

# Use the function to generate Latex code for table12_merged (w/rates)
#######  printLongEightX_func('Firm.Sz.Cat', 'Year', 'MPS.Claims', 'MP.Claims',  'UI.Empl' , 'QCEW.Empl', 'MPS.Rate', 'MP.Rate', 'table12_merged') 

printLongEightX_func('Firm.Sz.Cat', 'Year', 'MPS.Claims', 'MP.Claims',  'UI.Empl' , 
                     'QCEW.Empl', 'MPS.Rate', 'MP.Rate', 
                     table12_merged[which(table12_merged$Year=='2015') , ]) 

###################################################################################
# Now we need to print tables showing the industry rank
# Table 10aa_rank, 10bb_rank, 10cc_rank
# Do not use a print function but print the following tables using latex(as.tabular(table10xx_rank))

###################################################################################
# Print table10aa_rank 
latex(as.tabular(table10aa_rank))
###################################################################################

###################################################################################
# Print table10bb_rank 
latex(as.tabular(table10bb_rank))
###################################################################################

###################################################################################
# Print table10cc_rank 
latex(as.tabular(table10cc_rank))
###################################################################################

# Produce code for Table 11; this is different than M_printLongFiveX_func above
printLongFiveX_func('Naics4', 'Year', 'MP.Claims', 'Avg.Tenure', 'Pct.MP.Claims',  table11)

###################################################################################
# Produce code for Table 12
# Switch to Longtable if desired for easier printing
printLongFourX_func('Firm.Sz.Cat', 'Year', 'MPS.Claims', 'MP.Claims',  table12)


# Alternate printing format for table 12
# # Use the function to generate Latex code for table12
# # Switch to Longtable if desired for easier printing
# printLongFourX_func('Firm.Sz.Cat', 'Year', 'Claims', 'Pct', table12)


###################################################################################
# Produce code for Table 12_final
# Change Firm_Sz_Cat to character (from factor)

table12_final$Firm.Sz.Cat <- lapply(table12_final$Firm.Sz.Cat, as.character)
# Filter for 2015 only
table12_final <- dplyr::filter(table12_final, Year=='2015')
latex(as.tabular(table12_final))



# Produce code for table12a
# Change Firm_Sz_Cat to character (from factor)

table12a$Firm.Sz.Cat <- lapply(table12a$Firm.Sz.Cat, as.character)
# Filter for 2015 only
table12a <- dplyr::filter(table12a, Year=='2015')
latex(as.tabular(table12a))   # manually edit to remove year column

####### End table 12 & 12a #########################################################