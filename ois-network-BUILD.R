# ************************************** #
# This file pulls the OIS data and builds bipartite networks from those data


# ----
# clear workspace and load libraries

rm( list = ls() )

library( here )    # for accessing local directory
library( dplyr )   # for working with the data
library( network ) # for building the networks
library( sna )     # for working with the network data


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

# this shows that there are multiple incidents that involve several officers


# ----
# construct the network objects

dat.ois <- dat %>% 
  select( INC_RPT, REFERENCE_ID, PO_GENDER ) %>%  # keep the variables you need for the edgelist
  group_by( INC_RPT ) %>%                         # group by incident report
  filter( n() > 1 )  %>%                          # only keep cases that involve more than 1 person arrested
  ungroup()
  
dat.ois <- dat.ois %>% 
  mutate( actor = paste0( "a.", dat.ois$REFERENCE_ID ) ) %>%  # change to character
  mutate( event = paste0( "p.", dat.ois$INC_RPT ) )           # change to character

  
dat.edgelist <- dat.ois %>% 
  select( actor, event ) %>% 
  arrange( actor, event )   


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
  as.character( dat.edgelist$actor ),                                    
  as.character( dat.edgelist$event )                                     
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
  filter( n() == 1 )  %>%
  arrange( actor )



!!!here: stuck with trying to figure out a duplicate it



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
col.gender[ netOIS %v% "gender" == "NA" ] <- "black"


# plot it
gplot( netOIS,
       gmode = "twomode",
       usearrows = FALSE,
       vertex.col = col.gender,
       edge.col = "grey60"
)



