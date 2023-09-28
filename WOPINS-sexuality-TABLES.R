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
load( here( "WOPINS-sexuality-ERGM-RESULTS.RData" ) )


# ----
# first table

stargazer( u2.model, u3.model,
           title = "ERGMs for Friendship",
           dep.var.caption = "",
           dep.var.labels = "",
           column.labels = c( "Good Behavior Unit", "General Population Unit" ),
           dep.var.labels.include = FALSE,
           coef= list( coef( u2.model ), coef( u3.model ) ),
           se= list( sqrt( diag( u2.model$covar ) ), sqrt( diag( u3.model$covar ) ) ),
           type = "html" ,
           out = here( "WOPINS-sexuality-friendship-table.doc" ),
           show.se = TRUE,
           single.row = FALSE,
           no.space=FALSE,
           df=FALSE,
           notes.align = "l",
           digits = 3,
           style="ajs",
           covariate.labels = c( 
             "Edges",
             "Mutual",
             "Twopath",
             "GW indegree (decay = 1.50)",
             "GWESP (alpha = 0.75)",
             "Sexual Minority-Indegree",
             "Sexual Minority-Outdegree",
             "Sexual Minority-Homophily",
             "Sexual Acceptance-Indegree",
             "Sexual Acceptance-Outdegree",
             "Sexual Acceptance-Homophily",
             "Mental Health Index-Indegree",
             "Mental Health Index-Outdegree",
             "Mental Health Index-Homophily", 
             "Health Change Since in Prison-Indegree",
             "Health Change Since in Prison-Outdegree",
             "Health Change Since in Prison-Homophily",
             "Age (log)-Indegree",
             "Age (log)-Outdegree",
             "Age (log)-Homophily",
             "Days on Unit-Indegree",
             "Days on Unit-Outdegree",
             "Days on Unit-Homophily",
             "Prison Mom-Indegree",
             "Prison Mom-Outdegree",
             "Prison Mom-Homophily",
             "Years in School-Indegree",
             "Years in School-Outdegree",
             "Years in School-Homophily",
             "White-Indegree",
             "White-Outdegree",
             "White-Homophily",
             "Protestant-Indegree",
             "Protestant-Outdegree",
             "Protestant-Homophily" )
           )


# ----
# second table

stargazer( u2.p.model, u3.p.model,
           title = "ERGMs for Power/Influence",
           dep.var.caption = "",
           dep.var.labels = "",
           column.labels = c( "Good Behavior Unit", "General Population Unit" ),
           dep.var.labels.include = FALSE,
           coef= list( coef( u2.p.model ), coef( u3.p.model ) ),
           se= list( sqrt( diag( u2.p.model$covar ) ), sqrt( diag( u3.p.model$covar ) ) ),
           type = "html" ,
           out = here( "WOPINS-sexuality-power-table.doc" ),
           show.se = TRUE,
           single.row = FALSE,
           no.space=FALSE,
           df=FALSE,
           notes.align = "l",
           digits = 3,
           style="ajs",
           covariate.labels = c( 
             "Edges",
             "Mutual",
             "Twopath",
             "GW indegree (decay = 1.50)",
             "GWESP (alpha = 0.75)",
             "Friendship network-edge covariate",
             "Sexual Minority-Indegree",
             "Sexual Minority-Outdegree",
             "Sexual Minority-Homophily",
             "Sexual Acceptance-Indegree",
             "Sexual Acceptance-Outdegree",
             "Sexual Acceptance-Homophily",
             "Mental Health Index-Indegree",
             "Mental Health Index-Outdegree",
             "Mental Health Index-Homophily", 
             "Health Change Since in Prison-Indegree",
             "Health Change Since in Prison-Outdegree",
             "Health Change Since in Prison-Homophily",
             "Age (log)-Indegree",
             "Age (log)-Outdegree",
             "Age (log)-Homophily",
             "Days on Unit-Indegree",
             "Days on Unit-Outdegree",
             "Days on Unit-Homophily",
             "Prison Mom-Indegree",
             "Prison Mom-Outdegree",
             "Prison Mom-Homophily",
             "Years in School-Indegree",
             "Years in School-Outdegree",
             "Years in School-Homophily",
             "White-Indegree",
             "White-Outdegree",
             "White-Homophily",
             "Protestant-Indegree",
             "Protestant-Outdegree",
             "Protestant-Homophily"  )
)

