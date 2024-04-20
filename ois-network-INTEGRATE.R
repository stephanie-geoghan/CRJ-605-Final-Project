# ************************************** #
# This file pulls the OIS networks and builds bipartite graphs
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
# download the ois data from the portal

url <- paste(
  "https://www.phoenixopendata.com/dataset/754b1156-a48f-4d1b-b17a-53bd96d5b2be/resource/",
  "dd5431ca-21f3-48fd-8404-4c1c77cae593/download/ois_ois-officer-summary_oisofficer.csv",
  sep = ""
)

dat1 <- read.csv(
  url, 
  as.is = TRUE,
  header = TRUE
)

# drop the date and officer count vars as they creates conflict for the merge
dat1 <- dat1 %>% 
  select( !OIS_DATE & !OFFICER_COUNT )

# ----
# grab the handcoded data

dat2 <- read_xlsx( 
  here( "OIS-DATA-EDITED.xlsx" ), # call the local directory
  na = "NA"                       # set character for missing values
)

# drop the date and officer count vars as they creates conflict for the merge
dat2 <- dat2 %>% 
  select( !OIS_DATE & !OFFICER_COUNT )

# ----
# integrate the files

dat <- bind_rows( dat1, dat2 )


# ************************************** #
# Build the network

# ----
# construct the network object
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



# ! I don't think this is working correctly; seems to be 
# cutting out to many nodes

