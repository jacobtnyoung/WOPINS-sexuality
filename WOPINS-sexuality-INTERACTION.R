# ================================================================== #
# WOPINS Sexuality Paper.
# ================================================================== #

# This code estimates the interaction term for the friendship ERGMs.

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




# ----------
# ERGMs

# build function with formula for ergm
net.f.formula <- function( net, gwideg.val, gwesp.val, seed.num ){
  net.terms <- 
    ( net ~ edges 
      
      # structural terms
      + mutual 
      + twopath
      + gwidegree( gwideg.val, fixed=TRUE )
      + gwesp( gwesp.val, fixed = TRUE )
      
      # interaction: age sending by sexual minority receiving
      + nodeocov( "lage" ) + nodeifactor( "sex_minority" )
      + nodeocov( "lage" ) : nodeifactor( "sex_minority" )
      
      # other terms
      + nodeicov( "lage" )
      + nodeofactor( "sex_minority" )
      + nodematch( "sex_minority" )
      + absdiff( "lage" )
      
      # terms of interest
      #+ nodeicov( "sexacceptance_reverse" )
      #+ nodeocov( "sexacceptance_reverse" )
      #+ absdiff( "sexacceptance_reverse" )
      #+ nodeicov( "MentalPhysicalHealth" )
      #+ nodeocov( "MentalPhysicalHealth" )
      #+ absdiff( "MentalPhysicalHealth" )
      #+ nodeicov( "healthchangefrom3mopre" )
      #+ nodeocov( "healthchangefrom3mopre" )
      #+ absdiff( "healthchangefrom3mopre" )
      #+ nodeicov( "unitdays100" )
      #+ nodeocov( "unitdays100" )
      #+ absdiff( "unitdays100" )
      #+ nodeifactor( "prismom" )
      #+ nodeofactor( "prismom" )
      #+ nodematch( "prismom" )
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

## Models of Friendship

# estimate the model ----
u2.model <- net.f.formula( u2.net, gwideg.val = 1.50, gwesp.val = 0.75, seed.num = 601601 )
u3.model <- net.f.formula( u3.net, gwideg.val = 1.50, gwesp.val = 0.75, seed.num = 12345 )

# output the table ----
stargazer( u2.model, u3.model,
           title = "ERGMs for Friendship",
           dep.var.caption = "",
           dep.var.labels = "",
           column.labels = c( "Unit 2", "Unit 3" ),
           dep.var.labels.include = FALSE,
           coef= list( coef( u2.model ), coef( u3.model ) ),
           se= list( sqrt( diag( u2.model$covar ) ), sqrt( diag( u3.model$covar ) ) ),
           type = "text" )


# ================================================================== #
# END syntax file.
# ================================================================== #
