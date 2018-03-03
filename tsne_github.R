install.packages("Rtsne")
library("Rtsne")

install.packages("dummies")
library(dummies)
require(rgl)

poor <- dtrain_a$poor
dtrain_a$poor <- NULL

dtrain_a.new <- dummy.data.frame(dtrain_a, sep = ".")
#############################################################

#exclude poor
dtrain_a.new$poor <- NULL

##########################################################


tsne <- Rtsne(dtrain_a.new[, -1], dims = 3, perplexity=400, verbose=TRUE, max_iter = 5000)
plot3d(tsne$Y)
## Plotting
plot(tsne$Y, main="tsne")


