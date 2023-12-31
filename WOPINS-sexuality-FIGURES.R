# ================================================================== #
# WOPINS Sexuality Paper.
# ================================================================== #

# This file creates the plots for the manuscript.

# ----------
# Setup 

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
load( here( "WOPINS-sexuality-ERGM-RESULTS.RData" ) )


# ----
# GOFs for estimated models

set.seed( 92915 )

# plots for friendship models
u2.model.gof <- gof( u2.model )
u2.model.gof
plot( u2.model.gof )
u3.model.gof <- gof( u3.model )
u3.model.gof
plot( u3.model.gof )

# plots for power models
u2.p.model2.gof <- gof( u2.p.model2 )
u2.p.model2.gof
plot( u2.p.model2.gof )
u3.p.model.gof <- gof( u3.p.model )
u3.p.model.gof
plot( u3.p.model.gof )


# ----
# network plots

# set up the colors
u2.col <- ifelse( u2.net %v% "sex_minority" == 1, "red", "white" )
u3.col <- ifelse( u3.net %v% "sex_minority" == 1, "red", "white" )


# networks for friendship
op <- par( mai = c( 0.2,0.2,0.8,0.2 ), omi = c( 0.1,0.1,0.1,0.1 ) )
set.seed( 081922 )
gplot( u2.net,
       edge.col = "grey40",
       vertex.col = u2.col,
       displayisolates = FALSE
)

op <- par( mai = c( 0.2,0.2,0.8,0.2 ), omi = c( 0.1,0.1,0.1,0.1 ) )
set.seed( 081922 )
gplot( u3.net,
       edge.col = "grey40",
       vertex.col = u3.col,
       displayisolates = FALSE
)



# networks for power/influence
op <- par( mai = c( 0.2,0.2,0.8,0.2 ), omi = c( 0.1,0.1,0.1,0.1 ) )
set.seed( 081922 )
gplot( u2.p.net,
       edge.col = "grey40",
       vertex.col = u2.col,
       displayisolates = FALSE
)

op <- par( mai = c( 0.2,0.2,0.8,0.2 ), omi = c( 0.1,0.1,0.1,0.1 ) )
set.seed( 081922 )
gplot( u3.p.net,
       edge.col = "grey40",
       vertex.col = u3.col,
       displayisolates = FALSE
)


# all
op <- par( mai = c( 0.2,0.2,0.8,0.2 ), omi = c( 0.1,0.1,0.1,0.1 ), mfrow = c( 2,2 ) )
set.seed( 081922 )
gplot( u2.net,
       main = "Friendship Unit 2",
       edge.col = "grey40",
       vertex.col = u2.col,
       displayisolates = FALSE
)

gplot( u3.net,
       main = "Friendship Unit 3",
       edge.col = "grey40",
       vertex.col = u3.col,
       displayisolates = FALSE
)

gplot( u2.p.net,
       main = "Power/Influence Unit 2",
       edge.col = "grey40",
       vertex.col = u2.col,
       displayisolates = FALSE
)

gplot( u3.p.net,
       main = "Power/Influence Unit 3",
       edge.col = "grey40",
       vertex.col = u3.col,
       displayisolates = FALSE
)


# ----
# modularity for network plots

# assign membership and add 1 so function works
minority.u2 <- u2.net %v% "sex_minority" + 1 
minority.u3 <- u3.net %v% "sex_minority" + 1

library( intergraph ) # for creating an igraph object
i.u2.net <- asIgraph( u2.net )
i.u3.net <- asIgraph( u3.net )
i.u2.p.net <- asIgraph( u2.p.net )
i.u3.p.net <- asIgraph( u3.p.net )


library( igraph ) # for the modularity fucntion
modularity( i.u2.net, minority.u2  )
modularity( i.u3.net, minority.u3  )
modularity( i.u2.p.net, minority.u2  )
modularity( i.u3.p.net, minority.u3  )


# ================================================================== #
# END syntax file.
# ================================================================== #
