# July 27, 2017 elt 
# This spreadsheet was created to answer a question by MJones about this topic (below).
# 
# During the report process, I (elt) noticed no instances where a unique firm
# identifier (i.e. CIRPC_GUID) was associated with more than one NAICS code.
# Upon receiving your email this morning, I decided to search the data. Here are
# my findings: I started with 131,547 matched, non-duplicated observations.
# After a series of filtering and matching operations, I did find 123 duplicated
# observations containing only three unique firm identifiers. Each of the three
# CIRPC_GUID values had two associated NAICS codes for firms reporting as
# multi-establishment.




# Took all matched observations (M_data) 156,697 observations and eliminated single estab. units
a <- M_ND_data[which(M_ND_data[66]!="Single Establishment Unit") , ] # leaving 60,342 obs.

# reduced to 5 columns
c <- a[, c(43, 44, 61, 67, 72)]

# sorted by CIRPC_GUID
d <- c[order(c$CIRPC_GUID), ] #60,342 rows by 5 columns

# found that there are 451 unique NAICS codes
length(unique(d$QCEW_NAICS_CODE)) #435 unique

# found that there are 2,213 unique CIRPC_GUID values
length(unique(d$CIRPC_GUID))   #2,213 unique

# reduced 'd' from 60,342 rows to a dataframe with 2,450 unique rows (unique base on QCEW_NAICS_CODE & CIRPC_GUID 
a1 <- unique(d)

# note there are 2,450 unique rows in 'd' but only 2,213 unique CIRPC_GUID values
# therefore some of the unique CIRPC_GUID values must have more than one associated QCEW_NAICS_CODE

sum(is.na(d$CIRPC_GUID)) #1,810 of 2,450 CIRPC_GUID values are NA; these are 'matched'; there should be zero that are NA

# sorted rows
a3 <- a1[order(a1$CIRPC_GUID),]    #2,450 rows

# identified duplicated VECTOR
a4 <- duplicated(a3[4]) # found the vector of dupicated values of CIRPC_GUID

a5 <- a3[a4,] # used the vector to find 237 rows and 5 columns w/ duplicate values of CIRPC_GUID; most are NA

# 123 rows x 74 columns; these 123 observations appeared in the M_ND_data
a6 <- filter(a, a$CIRPC_GUID=='5658F2FC-3860-4CF6-BF01-2912DA6B22F7' |  
               a$CIRPC_GUID==  '7A91C3C3-1598-4A32-9E30-D4C7DFEB0435'    |  a$CIRPC_GUID==  '337F5EFD-0378-4326-A434-EB1698ABA0E9' )

# reduce the rows
#a6 <- a6[, c(1,2 , 43, 44, 61, 67, 72)]

# 6 rows x 5 columns; ; these 6 observations appeared in the unique dataframe
a7 <- filter(a1,  a1$CIRPC_GUID=='5658F2FC-3860-4CF6-BF01-2912DA6B22F7' |
               a1$CIRPC_GUID==  '7A91C3C3-1598-4A32-9E30-D4C7DFEB0435'    |  a1$CIRPC_GUID==  '337F5EFD-0378-4326-A434-EB1698ABA0E9' )

a8 <- unique(a6)
