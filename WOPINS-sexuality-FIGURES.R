

need the plots of the networks as well




# ----
# GOFs for estimated models

set.seed( 92915 )

u2.p.model2.gof <- gof( u2.p.model2 )
u2.p.model2.gof
plot( u2.p.model2.gof )

u3.p.model.gof <- gof( u3.p.model )
u3.p.model.gof
plot( u3.p.model.gof )




# ============================================================================ #
# tables file: this file creates the ergms table.

# ----
# clear workspace and call libraries needed

rm( list = ls() )

library( here )      # for reading local file paths
library( sna )       # for working with network data
library( network )   # for working with network data
library( ergm )      # for the erg models
library( stargazer ) # for the tables

# ----
# load the estimated models
load( here( "pins-w1-race-ERGM-RESULTS.RData" ) )

