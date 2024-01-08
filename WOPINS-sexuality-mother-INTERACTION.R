# ================================================================== #
# WOPINS Sexuality Paper.
# ================================================================== #

# This code estimates the interaction term for the pseudo-mother term.

# ----------
# Setup 

# Clear the workspace
rm( list = ls() )

# libraries
library( here ) # for calling local directory
library( network ) # for creating the network object
library( sna ) # for working with the network
library( ergm ) # for erg models
library( pander ) # for tables
library( stargazer ) # for nice html tables


# ----
# load the estimated models
load( here( "WOPINS-sexuality-ERGM-RESULTS.RData" ) )


# build function with formula for ergm ----
net.p.formula <- function( net, f.net, gwideg.val, gwesp.val, seed.num ){
  net.terms <- 
    ( net ~ edges 
      
      # structural terms
      + mutual 
      + twopath
      + gwidegree( gwideg.val, fixed=TRUE )
      + gwesp( gwesp.val, fixed = TRUE )
      
      # friendship term
      + edgecov( f.net )
      
      # terms of interest
      + nodeifactor( "sex_minority" )
      + nodeofactor( "sex_minority" )
      + nodematch( "sex_minority" )
      + nodeicov( "sexacceptance_reverse" )
      + nodeocov( "sexacceptance_reverse" )
      + absdiff( "sexacceptance_reverse" )
      
      ### Test whether prison mom influences sending behavior
      #+ nodeofactor( "prismom" ) : nodeicov( "sexacceptance_reverse" )
      #+ nodeofactor( "prismom" ) : nodeifactor( "sex_minority" )
      
      ### Test whether prison mom influences receiving behavior
      #+ nodeifactor( "prismom" ) : nodeicov( "sexacceptance_reverse" )
      #+ nodeifactor( "prismom" ) : nodeifactor( "sex_minority" )
      
      #+ nodeicov( "MentalPhysicalHealth" )
      #+ nodeocov( "MentalPhysicalHealth" )
      #+ absdiff( "MentalPhysicalHealth" )
      #+ nodeicov( "healthchangefrom3mopre" )
      #+ nodeocov( "healthchangefrom3mopre" )
      #+ absdiff( "healthchangefrom3mopre" )
      #+ nodeicov( "lage" )
      #+ nodeocov( "lage" )
      #+ absdiff( "lage" )
      #+ nodeicov( "unitdays100" )
      #+ nodeocov( "unitdays100" )
      #+ absdiff( "unitdays100" )
      + nodeifactor( "prismom" )
      + nodeofactor( "prismom" )
      + nodematch( "prismom" )
      #+ nodeicov( "grade" )
      #+ nodeocov( "grade" )
      #+ absdiff( "grade" )
      #+ nodeifactor( "white" )
      #+ nodeofactor( "white" )
      #+ nodematch( "white" )
      #+ nodeifactor( "protestantbinary" )
      #+ nodeofactor( "protestantbinary" )
      #+ nodematch( "protestantbinary" )
    )
  
  # set options for model
  net.fit <- ergm( net.terms, 
                   control = control.ergm(
                     seed        = seed.num,
                     MCMLE.maxit = 1000 ) )
  return( net.fit )
  
}



# estimate the model ----
u2.p.model <- net.p.formula( u2.p.net, u2.net, gwideg.val = 1.25, gwesp.val = 0.75, seed.num = 6016016 )

summary( u2.p.model )


# ================================================================== #
# END syntax file.
# ================================================================== #
