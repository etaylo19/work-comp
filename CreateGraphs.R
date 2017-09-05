# June 29, 2017
# ELT
# Create some graphics for 2017 report using 2015 data
# This creates a plot of Naics 2digit industry PIs and their BLS rates

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

# Filter data to get only 2015 observations
table10aa <- dplyr::filter(table10aa, Year=='2015')

# Add a set of labels
a <- c('NAICS 11 Ag., Frstry...','NAICS 21 Mining, Quarrying, …' ,'NAICS 22 Utilities',
       'NAICS 23 Construction','NAICS 31-33 Manufacturing','NAICS 42 Whlsale Trade',
       'NAICS 44-45 Retail Trade','NAICS 48-49 Trans. and Wrhsing.','NAICS 51 Information'
       ,'NAICS 52 Finance and Insurance','NAICS 53 Real Estate and Rent. and Leas.',
       'NAICS 54 Prof., Scientific, and Tech. Svcs.','NAICS 55 Mgmt. of Companies and Ent.'
       ,'NAICS 56 Admin. Spt. Wste Mgmt..', 'NAICS 61 Educational Services', 
       'NAICS 62 Health Care and Soc. Assist.',
       'NAICS 71 Arts, Entertainment, and Recreation','NAICS 72 Accom. and Food Svcs',
       'NAICS 81 Othr Svcs..', NA )

ggplot(table10aa, aes(a, BLS.Rate, TP.Adj.L90)) +
  theme(axis.text.x=element_text(angle=40,hjust=1)) +
  geom_point() +
  geom_linerange(data=table10aa, mapping=aes(x=a, ymin=TP.Adj.L90, 
                                             ymax=TP.Adj.U90), width=0.2, size=1, color="blue") +
  ggtitle("2015 Comparison of BLS Private Industry Rates and TP.Adj.L90/U90 Rate Estimates (90% Prob. Bounding Estimate)") +
  labs(x="NAICS Two-digit Industry",y="Rate per 100 FTE") 





# Filter data to get only 2015 observations
table10bb <- dplyr::filter(table10bb, Year=='2015')

ggplot(table10bb, aes(Naics3, BLS.Rate, TP.Adj.L90)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  geom_point() +
  geom_linerange(data=table10bb, mapping=aes(x=Naics3, ymin=TP.Adj.L90, 
                      ymax=TP.Adj.U90), width=0.2, size=1, color="blue") +
  ggtitle("2015 Comparison of BLS Private Industry Rates and TP.Adj.L90/U90 Rate Estimates (90% Prob. Bounding Estimate)") +
  labs(x="NAICS Three-digit Industry",y="Rate") 


