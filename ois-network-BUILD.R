# ************************************** #
# This file pulls the OIS data and builds bipartite networks from those data
# ************************************** #

# ************************************** #

# ----
# clear workspace and load libraries

rm( list = ls() )

library( here )    # for accessing local directory
library( dplyr )   # for working with the data
library( network ) # for building the networks
library( sna )     # for working with the network data
library( readxl )  # for reading excel files
 
# ----
# Download the arrest data from the portal

url <- paste(
  "https://www.phoenixopendata.com/dataset/754b1156-a48f-4d1b-b17a-53bd96d5b2be/resource/",
  "dd5431ca-21f3-48fd-8404-4c1c77cae593/download/ois_ois-officer-summary_oisofficer.csv",
  sep = ""
)

dat <- read.csv(
  url, 
  as.is = TRUE,
  header = TRUE
)


# ----
# check the variables
names( dat )


# ----
# check the counts of duplicate incident ids
table( table( dat$INC_RPT ) )


# ----
# construct the network objects

dat.ois <- dat %>% 
  select( INC_RPT, REFERENCE_ID, PO_GENDER, OIS_YEAR ) %>%  # keep the variables you need for the edgelist
  group_by( INC_RPT ) %>%                                   # group by incident report
  filter( OIS_YEAR >= 2019 ) %>%                            # select cases that are at 2019 or more
  filter( n() > 1 ) %>%                                     # only keep cases that involve more than 1 person arrested
  ungroup() %>%                                             # ungroup the data
  na.omit()                                                 # drop a case missing an officer id


# ----
# create the actor and event as characters

dat.ois <- dat.ois %>% 
  mutate( actor = paste0( "a.", dat.ois$REFERENCE_ID ) ) %>%  # change to character
  mutate( event = paste0( "p.", dat.ois$INC_RPT ) )           # change to character


# ----
# create the edgelist  

dat.edgelist <- dat.ois %>% 
  select( actor, event ) %>%  # take the actor and event 
  arrange( actor )            # sort by the actor


# ----
# check the data and create objects

# check duplicate entries
table( duplicated( dat.edgelist ) )

# check dimensions
dim( dat.edgelist )

# check number of officers
n.actors <- dim( unique( dat.edgelist[,1] ) )[1]
n.actors

# check number of incidents
n.events <- dim( unique( dat.edgelist[,2] ) )[1]
n.events


# ----
# construct the network

# build the edgelist
mat.edgelist <- cbind( 
  dat.edgelist$actor, 
  dat.edgelist$event 
  )


# create the network
netOIS <- as.network(                             
  mat.edgelist, 
  bipartite=n.actors, 
  matrix.type="edgelist"
)


# ----
# attach attributes

# create the attributes object
dat.attrs <- dat.ois %>% 
  select( actor, PO_GENDER ) %>% 
  group_by( actor ) %>%
  distinct() %>% 
  arrange( actor )


# check names match 
id.check <- cbind( 
  dat.attrs$actor, 
  network.vertex.names( netOIS )[1:n.actors] 
  )


# match?
table( id.check[,1] == id.check[,2] )


# merge the officer gender attribute to the data
gender <- dat.attrs$PO_GENDER
gender <- c( gender, rep( NA, n.events ) )


# assign the gender variable to the network
netOIS %v% "gender" <- gender


# ----
# plot the network

# set the colors
col.gender <- netOIS %v% "gender"
col.gender[ netOIS %v% "gender" == "Male" ] <- "lightblue"
col.gender[ netOIS %v% "gender" == "Female" ] <- "#fc03c2"
col.gender[ is.na( netOIS %v% "gender") ] <- "black"


# plot it
gplot( netOIS,
       gmode = "twomode",
       usearrows = FALSE,
       vertex.col = col.gender,
       edge.col = "grey60",
       main = "OIS Incidents Network\n (color reflects gender)"
)


# ************************************** #
# Build the network for the hand cleaned data file

# for this section I just recycle code from above
# a more efficient way to do this would be to write a function
# or a loop, but we will not do that right here


# get the file
dat2 <- read_xlsx( 
  here( "OIS-DATA-EDITED.xlsx" ), # call the local directory
  na = "NA"                       # set character for missing values
  )

# check the variables
names( dat2 )

# check the counts of duplicate incident ids
table( table( dat2$INC_RPT ) )


# ----
# construct the network objects

dat.ois2 <- dat2 %>% 
  select( INC_RPT, REFERENCE_ID, PO_GENDER ) %>%  # keep the variables you need for the edgelist
  group_by( INC_RPT ) %>%                         # group by incident report
  filter( n() > 1 ) %>%                           # only keep cases that involve more than 1 person arrested
  ungroup() 


# check missing ids
table( is.na( dat.ois2$REFERENCE_ID ) )

# drop 32 cases with missing ids
dat.ois2 <- dat.ois2 %>% 
  na.omit()


# check again
table( is.na( dat.ois2$REFERENCE_ID ) )


# ----
# create the actor and event as characters

dat.ois2 <- dat.ois2 %>% 
  mutate( actor = paste0( "a.", dat.ois2$REFERENCE_ID ) ) %>%  # change to character
  mutate( event = paste0( "p.", dat.ois2$INC_RPT ) )           # change to character


# ----
# create the edgelist  

dat.edgelist2 <- dat.ois2 %>% 
  select( actor, event ) %>%  # take the actor and event 
  arrange( actor )            # sort by the actor


# ----
# check the data and create objects

# check duplicate entries
table( duplicated( dat.edgelist2 ) )

# check dimensions
dim( dat.edgelist2 )

# check number of officers
n.actors2 <- dim( unique( dat.edgelist2[,1] ) )[1]
n.actors2

# check number of incidents
n.events2 <- dim( unique( dat.edgelist2[,2] ) )[1]
n.events2


# ----
# construct the network

# build the edgelist
mat.edgelist2 <- cbind( 
  dat.edgelist2$actor, 
  dat.edgelist2$event 
)


# create the network
netOIS2 <- as.network(                             
  mat.edgelist2, 
  bipartite=n.actors2, 
  matrix.type="edgelist"
)


# ----
# attach attributes

# create the attributes object
dat.attrs2 <- dat.ois2 %>% 
  select( actor, PO_GENDER ) %>% 
  group_by( actor ) %>%
  distinct() %>% 
  arrange( actor )


# check names match 
id.check2 <- cbind( 
  dat.attrs2$actor, 
  network.vertex.names( netOIS2 )[1:n.actors2] 
)


# match?
table( id.check2[,1] == id.check2[,2] )


# merge the officer gender attribute to the data
gender2 <- dat.attrs2$PO_GENDER
gender2 <- c( gender2, rep( NA, n.events2 ) )


# assign the gender variable to the network
netOIS2 %v% "gender" <- gender2


# ----
# plot the network

# set the colors
col.gender2 <- netOIS2 %v% "gender"
col.gender2[ netOIS2 %v% "gender" == "Male" ] <- "lightblue"
col.gender2[ netOIS2 %v% "gender" == "Female" ] <- "#fc03c2"
col.gender2[ is.na( netOIS2 %v% "gender") ] <- "black"


# plot it
gplot( netOIS2,
       gmode = "twomode",
       usearrows = FALSE,
       vertex.col = col.gender2,
       edge.col = "grey60",
       main = "OIS Incidents Network\n (color reflects gender)"
)


# ************************************** #
# Integrate the two networks










