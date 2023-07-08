
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




!!!THIS JUST NEEDS TO BE CLEANED UP AND BUILT



# ----
# Build the functions.

# Set the seed to reproduce results.
set.seed( 12345 )

I <- cbind( 0 , 0.5 )
E <- cbind( 0 , 0.5 )

eval.bic <- function( net, I, E ){
  model.bics <- matrix( 0, nrow = length( I ), ncol = length( E ) )
  
  for ( i in 1:length( I ) ){
    for ( j in 1:length( E ) ){
      model.bics[i,j] <- 
        summary( ergm( net ~ edges
                     + mutual + twopath 
                     + gwidegree( decay = I[i], fixed = TRUE )
                     + gwesp( E[j], fixed = TRUE ) ) )$bic
    }
  }
  return( model.bics )
}

I.u2   <- eval.bic( u2.net, I, E )
I.u2
fit


plot( I.u2[1:nrow( I.u2 ),], I.u2[,1:ncol( I.u2 )], type="b", main="BIC for gwidegree (u2)", xlab = "", ylab = "")
min( I.u2 )


# look at the BICs
par( mfrow = c( 2, 2 ) )
plot( I, I.u2, type="b", main="BIC for gwidegree (u2)" )
plot( I, I.u3, type="b", main="BIC for gwidegree (u3)" )
plot( I, I.u2.p, type="b", main="BIC for gwidegree (u2p)" )
plot( I, I.u3.p, type="b", main="BIC for gwidegree(u3p)" )
par( mfrow = c( 1, 1 ) )

# find the minimums
min( I.u2 )
min( I.u3 )
min( I.u2.p )
min( I.u3.p )



