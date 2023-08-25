
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
      #+ nodeifactor( "prismom" )
      #+ nodeofactor( "prismom" )
      #+ nodematch( "prismom" )
      + nodeicov( "sexacceptance_reverse" )
      + nodeocov( "sexacceptance_reverse" )
      + absdiff( "sexacceptance_reverse" )
      
      # controls
      + nodeicov( "lage" )
      + nodeocov( "lage" )
      + absdiff( "lage" )
      + nodeicov( "grade" )
      + nodeocov( "grade" )
      + absdiff( "grade" )
      + nodeifactor( "white" )
      + nodeofactor( "white" )
      + nodematch( "white" )
      + nodeicov( "unitdays100" )
      + nodeocov( "unitdays100" )
      + absdiff( "unitdays100" )
      + nodeifactor( "protestantbinary" )
      + nodeofactor( "protestantbinary" )
      + nodematch( "protestantbinary" )
      + nodeicov( "healthchangefrom3mopre" )
      + nodeocov( "healthchangefrom3mopre" )
      + absdiff( "healthchangefrom3mopre" )
      + nodeicov( "MentalPhysicalHealth" )
      + nodeocov( "MentalPhysicalHealth" )
      + absdiff( "MentalPhysicalHealth" )
    )
  
  net.fit <- ergm( net.terms, 
                   control = control.ergm(
                     seed        = seed.num,
                     MCMLE.maxit = 1000 ) )
  return( net.fit )
  
}


u2.p.model <- net.p.formula( u2.p.net, u2.net, gwideg.val = 1.25, gwesp.val = 0.75, seed.num = 6016016 )

summary( u2.p.model )

u3.p.model <- net.p.formula( u3.p.net, u3.net, gwideg.val = 1.25, gwesp.val = 0.75, seed.num = 6016016 )

summary( u3.p.model )

stargazer( u2.p.model, u3.p.model,
           title = "ERGMs for Power",
           dep.var.caption = "",
           dep.var.labels = "",
           column.labels = c( "Unit 2", "Unit 3" ),
           dep.var.labels.include = FALSE,
           coef= list( coef( u2.p.model ), coef( u3.p.model ) ),
           se= list( sqrt( diag( u2.p.model$covar ) ), sqrt( diag( u3.p.model$covar ) ) ),
           type = "text" )


cor( u2.p.net %v% "sexacceptance_reverse", u2.p.net %v% "age")
