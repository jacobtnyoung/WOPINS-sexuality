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
    here( "young_data1.dta" ) ) )

# remove the cases that did not take the survey
attr.dat <- attr.dat %>% 
  mutate( drop = ifelse( MentalPhysicalHealth == -99, 1, 0 ) ) %>% 
  filter( drop != 1 ) %>% 
  select( !drop )

# create the attribute object for each unit
attr.dat.u2 <- attr.dat %>% filter( unit == 2 )
attr.dat.u3 <- attr.dat %>% filter( unit == 3 )


# ----------
# Load the networks

u2.net <- as.network(
  as.matrix( read.csv( 
    here( "GA_WOPINS2surveytakersonly.csv" ), 
    as.is = TRUE, header = TRUE, row.names = 1, check.names = FALSE  ) ) )

u3.net <- as.network(
  as.matrix( read.csv( 
    here( "GA_WOPINS3surveytakersonly.csv" ), 
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


# ----------
# build the block network

u2.u3.net.list <- list( u2.net, u3.net )
u2.u3.pooled.net <- blockdiagonalize( u2.u3.net.list, attrname = "block" )


# ----------
# clean workspace
rm( list = ls()[! ls() %in% c( "u2.net","u3.net","u2.u3.pooled.net" )])


# ================================================================== #
# END syntax file.
# ================================================================== #