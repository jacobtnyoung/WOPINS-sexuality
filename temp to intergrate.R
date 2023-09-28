# ================================================================== #
# WOPINS Sexuality Paper.
# ================================================================== #

# This code estimates the ERGMs.

# ----------
# Setup 

# Clear the workspace
rm( list = ls() )

# libraries
library( here ) # for calling local directory
library( dplyr ) # for working with the data
library( network ) # for creating the network object
library( sna ) # for working with the network
library( ergm ) # for erg models
library( pander ) # for tables
library( stargazer ) # for nice html tables
library( ergMargins ) #devtools::install_github("sduxbury/ergMargins") 

# run the script to create the networks with the attributes
source( here( "WOPINS-sexuality-BUILD.R" ) )


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
      
      # terms of interest
      + nodeifactor( "sex_minority" )
      + nodeofactor( "sex_minority" )
      + nodematch( "sex_minority" )
      + nodeicov( "sexacceptance_reverse" )
      + nodeocov( "sexacceptance_reverse" )
      + absdiff( "sexacceptance_reverse" )
      + nodeicov( "MentalPhysicalHealth" )
      + nodeocov( "MentalPhysicalHealth" )
      + absdiff( "MentalPhysicalHealth" )
      + nodeicov( "healthchangefrom3mopre" )
      + nodeocov( "healthchangefrom3mopre" )
      + absdiff( "healthchangefrom3mopre" )
      + nodeicov( "lage" )
      + nodeocov( "lage" )
      + absdiff( "lage" )
      + nodeicov( "unitdays100" )
      + nodeocov( "unitdays100" )
      + absdiff( "unitdays100" )
      + nodeifactor( "prismom" )
      + nodeofactor( "prismom" )
      + nodematch( "prismom" )
      + nodeicov( "grade" )
      + nodeocov( "grade" )
      + absdiff( "grade" )
      + nodeifactor( "white" )
      + nodeofactor( "white" )
      + nodematch( "white" )
      + nodeifactor( "protestantbinary" )
      + nodeofactor( "protestantbinary" )
      + nodematch( "protestantbinary" )
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


## Models of Power/Influence

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
      + nodeicov( "MentalPhysicalHealth" )
      + nodeocov( "MentalPhysicalHealth" )
      + absdiff( "MentalPhysicalHealth" )
      + nodeicov( "healthchangefrom3mopre" )
      + nodeocov( "healthchangefrom3mopre" )
      + absdiff( "healthchangefrom3mopre" )
      + nodeicov( "lage" )
      + nodeocov( "lage" )
      + absdiff( "lage" )
      + nodeicov( "unitdays100" )
      + nodeocov( "unitdays100" )
      + absdiff( "unitdays100" )
      + nodeifactor( "prismom" )
      + nodeofactor( "prismom" )
      + nodematch( "prismom" )
      + nodeicov( "grade" )
      + nodeocov( "grade" )
      + absdiff( "grade" )
      + nodeifactor( "white" )
      + nodeofactor( "white" )
      + nodematch( "white" )
      + nodeifactor( "protestantbinary" )
      + nodeofactor( "protestantbinary" )
      + nodematch( "protestantbinary" )
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
u3.p.model <- net.p.formula( u3.p.net, u3.net, gwideg.val = 1.25, gwesp.val = 0.75, seed.num = 9191919 )

# output the table ----
stargazer( u2.p.model, u3.p.model,
           title = "ERGMs for Power/Influence",
           dep.var.caption = "",
           dep.var.labels = "",
           column.labels = c( "Unit 2", "Unit 3" ),
           dep.var.labels.include = FALSE,
           coef= list( coef( u2.p.model ), coef( u3.p.model ) ),
           se= list( sqrt( diag( u2.p.model$covar ) ), sqrt( diag( u3.p.model$covar ) ) ),
           type = "text" )

## Test Marginal Effect for Unit 2

# build the model formula without the mutual term
u2.p.model2 <- ergm( u2.p.net ~ edges 
      #+ offset( mutual )
      + twopath 
      + gwidegree( 1.25, fixed=TRUE ) 
      + gwesp( 0.75, fixed = TRUE )
      + edgecov( u2.net )
      + nodeifactor( "sex_minority" )
      + nodeofactor( "sex_minority" )
      + nodematch( "sex_minority" )
      + nodeicov( "sexacceptance_reverse" )
      + nodeocov( "sexacceptance_reverse" )
      + absdiff( "sexacceptance_reverse" )
      + nodeicov( "MentalPhysicalHealth" )
      + nodeocov( "MentalPhysicalHealth" )
      + absdiff( "MentalPhysicalHealth" )
      + nodeicov( "healthchangefrom3mopre" )
      + nodeocov( "healthchangefrom3mopre" )
      + absdiff( "healthchangefrom3mopre" )
      + nodeicov( "lage" )
      + nodeocov( "lage" )
      + absdiff( "lage" )
      + nodeicov( "unitdays100" )
      + nodeocov( "unitdays100" )
      + absdiff( "unitdays100" )
      + nodeifactor( "prismom" )
      + nodeofactor( "prismom" )
      + nodematch( "prismom" )
      + nodeicov( "grade" )
      + nodeocov( "grade" )
      + absdiff( "grade" )
      + nodeifactor( "white" )
      + nodeofactor( "white" )
      + nodematch( "white" )
      + nodeifactor( "protestantbinary" )
      + nodeofactor( "protestantbinary" )
      + nodematch( "protestantbinary" ) , 
      #offset.coef = -Inf,
      control = control.ergm( seed = 9191919, MCMLE.maxit = 1000 ) 
      )

# estimate the AMEs ----
accept.ame  <- ergm.AME( u2.p.model2, var1 = "nodeicov.sexacceptance_reverse" )
accept.ame


# ----
# clean up and save the estimates as an .RData object

# clean workspace
rm( list = ls()[! ls() %in% c( 
  "u2.net","u3.net","u2.u3.pooled.net",
  "u2.p.net","u3.p.net","u2.u3.p.pooled.net" 
)])

save.image( here( "WOPINS-sexuality-ERGM-RESULTS.RData" ) )


# ================================================================== #
# END syntax file.
# ================================================================== #