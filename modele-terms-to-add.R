pooled.formula <- 
  (  u2.u3.pooled.net ~ edges 
    
    # structural terms
    + mutual
    + twopath
    + gwidegree( 0.75, fixed=TRUE )
    + gwesp( 1.25, fixed = TRUE )
    
    # terms of interest
    + nodeifactor( "sex_minority" )
    + nodeofactor( "sex_minority" )
    + nodematch( "sex_minority" )
    + nodeicov( "oldheadscale" )
    + nodeocov( "oldheadscale" )
    + absdiff( "oldheadscale" )
    #+ nodeicov( "prismom" )
    #+ nodeocov( "prismom" )
    #+ nodematch( "prismom" )
    + nodeicov( "sexacceptance_reverse" )
    + nodeocov( "sexacceptance_reverse" )
    + absdiff( "sexacceptance_reverse" )
    
    #controls
    + nodeicov( "age" )
    + nodeocov( "age" )
    + nodeicov( "grade" )
    + nodeocov( "grade" )
    + nodeicov( "white" )
    + nodeocov( "white" )
    + nodeicov( "unitdays100" )
    + nodeocov( "unitdays100" )
    + nodeicov( "protestantbinary" )
    + nodeocov( "protestantbinary" )
    + nodeicov( "healthchangefrom3mopre" )
    + nodeocov( "healthchangefrom3mopre" )
    + nodeicov( "MentalPhysicalHealth" )
    + nodeocov( "MentalPhysicalHealth" )
    + nodeicov( "SocialIntegration" )
    + nodeocov( "SocialIntegration" )
    + nodeicov( "timealone" )
    + nodeocov( "timealone" )
    
    # Terms to test
    + S(
      #~ edges + mutual + twopath + gwidegree( 0.75, fixed=TRUE ) + gwesp( 1.25, fixed = TRUE )
      ~ nodeifactor( "sex_minority" ) + nodeofactor( "sex_minority" ) + nodematch( "sex_minority" )
      + nodeicov( "sexacceptance_reverse" ) + nodeocov( "sexacceptance_reverse" ) + absdiff( "sexacceptance_reverse" ),
      ~ ( block == 1 ) # this is to test it across the blocks
    )
    + S(
      #~ edges + mutual + twopath + gwidegree( 0.75, fixed=TRUE ) + gwesp( 1.25, fixed = TRUE )
      ~ nodeifactor( "sex_minority" ) + nodeofactor( "sex_minority" ) + nodematch( "sex_minority" )
      + nodeicov( "sexacceptance_reverse" ) + nodeocov( "sexacceptance_reverse" ) + absdiff( "sexacceptance_reverse" ),
      ~ ( oldheadscale >= 0.3932894 ) # this tests whether these effects differ for those at or above the mean of oldheadsscale
    )
)

pooled.fit <- ergm( 
  pooled.formula,
  constraints = ~ blocks( "block", levels2 = c( 2, 3 ) )  
)
summary( pooled.fit )
