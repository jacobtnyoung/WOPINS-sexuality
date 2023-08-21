# ================================================================== #
# WOPINS Sexuality Paper.
# ================================================================== #

# ----------
# Setup 

# Clear the workspace
rm( list = ls() )

# libraries
library( haven ) # for importing a stata file
library( here ) # for calling local directory
library( dplyr ) # for working with the data
library( network ) # for creating the network object
library( sna ) # for working with the network
library( ergmito ) # to build the pooled network


# ----------
# Load the attributes file

attr.dat <- as.data.frame( 
  read_dta( 
    here( "young_data2.dta" ) ) )

# remove the cases that did not take the survey
attr.dat <- attr.dat %>% 
  mutate( drop = ifelse( MentalPhysicalHealth == -99, 1, 0 ) ) %>% 
  filter( drop != 1 ) %>% 
  select( !drop )

# recode missing values on the protestant variable to be 0
attr.dat$protestantbinary[is.na( attr.dat$protestantbinary ) == TRUE] <- 0

# recode a missing value for the sex acceptance variable
attr.dat$sexacceptance_reverse[is.na( attr.dat$sexacceptance_reverse ) == TRUE] <- round( mean( attr.dat$sexacceptance_reverse, na.rm = TRUE ), 0 )

# create the attribute object for each unit
attr.dat.u2 <- attr.dat %>% filter( unit == 2 )
attr.dat.u3 <- attr.dat %>% filter( unit == 3 )


# ----------
# Load the networks

# friendship
u2.net <- as.network(
  as.matrix( read.csv( 
    here( "GA_WOPINS2surveytakersonly.csv" ), 
    as.is = TRUE, header = TRUE, row.names = 1, check.names = FALSE  ) ) )
u3.net <- as.network(
  as.matrix( read.csv( 
    here( "GA_WOPINS3surveytakersonly.csv" ), 
    as.is = TRUE, header = TRUE, row.names = 1, check.names = FALSE  ) ) )

# power/influence
u2.p.net <- as.network(
  as.matrix( read.csv( 
    here( "PI_WOPINS2surveytakersonly.csv" ), 
    as.is = TRUE, header = TRUE, row.names = 1, check.names = FALSE  ) ) )
u3.p.net <- as.network(
  as.matrix( read.csv( 
    here( "PI_WOPINS3surveytakersonly.csv" ), 
    as.is = TRUE, header = TRUE, row.names = 1, check.names = FALSE  ) ) )


# ----------
# Link the attributes

# create the names for the loop
names.loop <- names( attr.dat )
names.loop <- names.loop[-c( 1,2 )]

# loop through and add the attributes
for( i in 1: length( names.loop ) ){
  u2.net %v% names.loop[i] <- as.numeric( attr.dat.u2[, i + 2 ] ) 
}

for( i in 1: length( names.loop ) ){
  u3.net %v% names.loop[i] <- as.numeric( attr.dat.u3[, i + 2 ] ) 
}
for( i in 1: length( names.loop ) ){
  u2.p.net %v% names.loop[i] <- as.numeric( attr.dat.u2[, i + 2 ] ) 
}
for( i in 1: length( names.loop ) ){
  u3.p.net %v% names.loop[i] <- as.numeric( attr.dat.u3[, i + 2 ] ) 
}


# ----------
# create the logged age variable
u2.net %v% "lage"   <- log( u2.net %v% "age" )
u3.net %v% "lage"   <- log( u3.net %v% "age" )
u2.p.net %v% "lage" <- log( u2.p.net %v% "age" )
u3.p.net %v% "lage" <- log( u3.p.net %v% "age" )


# ----------
# build the block network

u2.u3.net.list <- list( u2.net, u3.net )
u2.u3.pooled.net <- blockdiagonalize( u2.u3.net.list, attrname = "block" )

u2.u3.p.net.list <- list( u2.p.net, u3.p.net )
u2.u3.p.pooled.net <- blockdiagonalize( u2.u3.p.net.list, attrname = "block" )


# ----------
# clean workspace
rm( list = ls()[! ls() %in% c( 
  "u2.net","u3.net","u2.u3.pooled.net",
  "u2.p.net","u3.p.net","u2.u3.p.pooled.net" 
  )])


# ================================================================== #
# END syntax file.
# ================================================================== #