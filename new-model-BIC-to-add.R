
# Clear the workspace
rm( list = ls() )

# libraries
library( here ) # for calling local directory
library( dplyr ) # for working with the data
library( network ) # for creating the network object
library( sna ) # for working with the network
library( ergm ) # for erg models
library( pander ) # for tables


# run the script to create the networks with the attributes
source( here( "WOPINS-sexuality-BUILD.R" ) )


# ----
# Build the function for the separate networks
# this fuction takes four arguments:
# the network, a vector for the gwidegree and gwesp terms
# and a scalar of seconds to elapse before the model should terminate
eval.bic <- function( net, I, E, duration ){
  model.bics <- matrix( 0, nrow = length( I ), ncol = length( E ) )
  
  for ( i in 1:length( I ) ){
    for ( j in 1:length( E ) ){
      
      model.bics[i,j] <- 
        { setTimeLimit( duration )
          try(
          summary( ergm( net ~ edges
                         + mutual + twopath 
                         + gwidegree( decay = I[i], fixed = TRUE )
                         + gwesp( E[j], fixed = TRUE ) ) )$bic
          )
        }
    }
  }
  
  rownames( model.bics ) <- paste("I @", as.character( I ) )
  colnames( model.bics ) <- paste("E @", as.character( E ) )
  
  model.bics[grepl( "Error", model.bics ) == TRUE] <- NA
  
  model.bics <- matrix( as.numeric( model.bics ), ncol = ncol( model.bics ) )
  
  return( model.bics )
}


# Set the range of values to examine
I <- seq( 40, 50, by = 0.25 )
E <- seq( 40, 50, by = 0.25 )

I <- c(0,1,50)
E <- c(0,1,50)

# Set the seed to reproduce results.
set.seed( 12345 )

# Estimate the models
bic.u3   <- eval.bic( u3.net, I, E, duration = 5 )
bic.u3
