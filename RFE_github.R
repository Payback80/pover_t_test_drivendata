library(caret)


dtrain_a$poor <- as.factor(dtrain_a$poor)
dtrain_a <- as.data.frame(dtrain_a)
poor <- dtrain_a$poor
poor <- as.data.frame(poor)

dtrain_a$poor <- NULL
dtrain_a <- bind_cols(dtrain_a, poor)
#eliminate characters columns
dtrain_a[sapply(dtrain_a, is.character)] <- list(NULL)

# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function with cross validation
control <- rfeControl(functions = rfFuncs,
                      method = "cv", number = 10,
                      allowParallel = TRUE,
                      verbose = TRUE)

# run the RFE algorithm  set sizes = c(1:229) of the numbers of your predictors
# columns 
results_1 <- rfe(x = dtrain_a[, !names(dtrain_a) %in% c("poor")], y = dtrain_a$poor, sizes = c(1:229), rfeControl = control)

# chosen features
predictors(results_1)
plot(results_1)

a <- dtrain_a[,  which(colnames(dtrain_a) %in% predictors(results_1))]
df.subset <- dtrain_a[, a]
saveRDS(a, "./data/RFE_a.rds")
#
#c <- fread("./data/RFE_a.csv", stringsAsFactors = FALSE, sep= "\n" )




