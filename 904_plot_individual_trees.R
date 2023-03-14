
#
# Plot individual trees of the forest
#

#
# First, define 'model1' by running one of the following scripts:
#
# "160parm_Time_series_results_James.Rmd"
# "161parm_Time_series_tocton.Rmd"
# "162parm_Currentstatus.Rmd"
# "163parm_Time_series_tocton.Rmd"

# Get tree data using getTree() in randomforest package
df <- getTree(model1, 1)
df <- getTree(model1, 1, labelVar = TRUE)
head(df)

#
# Plot using reprtree package
# See https://stats.stackexchange.com/a/241684
#
devtools::install_github('araastat/reprtree')

# Check help pages
?reprtree:::plot.getTree
?tree::plot.tree
?tree::text.tree

# Test trees
reprtree:::plot.getTree(model1, digits = 1, k = 1)
reprtree:::plot.getTree(model1, digits = 1, k = 2)
reprtree:::plot.getTree(model1, digits = 1, k = 3)
reprtree:::plot.getTree(model1, digits = 1, k = 4)

# Try debug
# debugonce(reprtree:::plot.getTree)
# reprtree:::plot.getTree(model1, k = 1)

# Check functions
reprtree:::plot.getTree
tree:::text.tree

