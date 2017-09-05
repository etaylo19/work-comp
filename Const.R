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

BT <- Const2015_2017_05_11 <- read_csv("~/Desktop/Const2015_2017-05-12.csv", 
                                        col_types = cols(DTE_DSBL_BEG = col_date(format = "%Y-%m-%d"), 
                                                                   DTE_EMPR_INFO = col_date(format = "%Y-%m-%d"), 
                                                                   DTE_OF_INJR = col_date(format = "%Y-%m-%d"), 
                                                                   HIRE_DTE = col_date(format = "%Y-%m-%d"), 
                                                                   RTRN_WRK_DTE = col_date(format = "%Y-%m-%d"), 
                                                                   TransformedDOB = col_date(format = "%Y-%m-%d"), 
                                                                   tenure_yrs = col_double()))


# Using dplyr

Group <- group_by(BT,CIRPC_GUID)
Count <- summarise(Group,
                   count=n(),
                   avg_age = mean(age_yrs,na.rm=T),
                   avg_tenure = mean(tenure_yrs,na.rm=T),
                   avg_num_EEs = mean(EMPL1))

inj_rate <- Count$count/Count$avg_num_EEs

Count <- arrange(Count, desc(count))

EECount <- cbind(Count, inj_rate)

EECount <- EECount[which(EECount$avg_num_EEs > 500),]

write.csv(EECount, '~/Desktop/ConstSortByInjNum.csv')

library(ggplot2)
ggplot(EECount, aes(avg_tenure, inj_rate )) + 
  geom_point(aes(size = avg_num_EEs), alpha = 1/2) +
 geom_smooth() +
  labs(title='EECount$avg_num_EEs >500')
