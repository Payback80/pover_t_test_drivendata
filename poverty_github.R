install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))


library(data.table)
library(caret)
library(h2o)
library(dplyr)
library(plyr)

#install.packages("Rtsne")
#library("Rtsne")

#install.packages("dummies")
#library(dummies)



train_a_hhold <- fread("./data/A_hhold_train.csv", stringsAsFactors = TRUE)
test_a_hhold <- fread("./data/A_hhold_test.csv", stringsAsFactors = TRUE)

combi_a = bind_rows(train_a_hhold, test_a_hhold)

train_ind_a <- fread("./data/A_indiv_train.csv", stringsAsFactors = TRUE)
test_ind_a <- fread("./data/A_indiv_test.csv", stringsAsFactors = TRUE)

combi_a_indiv <- bind_rows(train_ind_a, test_ind_a)

combi_a_indiv <- subset( combi_a_indiv, select = -c(iid, poor,country ) )
combi_a_indiv <- combi_a_indiv[!duplicated(combi_a_indiv$id), ]

# Join household and individual data by ID 
combi_a <- join(combi_a, combi_a_indiv, by="id", type="inner")

#preprocessing
# eliminate near zero variance features
nzv <- nearZeroVar(combi_a[, -c("country")], saveMetrics= TRUE)

drop.cols <- rownames(nzv)[nzv$nzv == TRUE]
combi_a <- as.data.frame(combi_a)
combi_a <- combi_a[,!names(combi_a) %in% drop.cols]
combi_a <- setDT(combi_a)

pp_hpc <- preProcess(combi_a[, -1], 
                     method = c("center", "scale", "YeoJohnson"))
pp_hpc
combi_a <- predict(pp_hpc, newdata = combi_a)
########################################################
##############################################################
########################################################
#crazy feature interactions##
# the data are anonymized so the features are based on the RFE random forest
#rank importance, not all combination to avoid feature explosion

################################ sum 
combi_a$sum1 <-   combi_a$nEsgxvAq   +    combi_a$OMtioXZZ
combi_a$sum2 <-   combi_a$nEsgxvAq   +    combi_a$YFMZwKrU
combi_a$sum3 <-   combi_a$nEsgxvAq   +    combi_a$TiwRslOh
combi_a$sum4 <-   combi_a$nEsgxvAq   +    combi_a$ukWqmeSS

combi_a$sum5 <-   combi_a$OMtioXZZ   +    combi_a$YFMZwKrU
combi_a$sum6 <-   combi_a$OMtioXZZ   +    combi_a$TiwRslOh
combi_a$sum7 <-   combi_a$OMtioXZZ   +    combi_a$ukWqmeSS

combi_a$sum8 <-   combi_a$YFMZwKrU   +    combi_a$TiwRslOh
combi_a$sum9 <-   combi_a$YFMZwKrU   +    combi_a$ukWqmeSS

################################diff 
combi_a$diff1 <-   combi_a$nEsgxvAq   -    combi_a$OMtioXZZ
combi_a$diff2 <-   combi_a$nEsgxvAq   -    combi_a$YFMZwKrU
combi_a$diff3 <-   combi_a$nEsgxvAq   -    combi_a$TiwRslOh
combi_a$diff4 <-   combi_a$nEsgxvAq   -    combi_a$ukWqmeSS

combi_a$diff5 <-   combi_a$OMtioXZZ   -    combi_a$YFMZwKrU
combi_a$diff6 <-   combi_a$OMtioXZZ   -    combi_a$TiwRslOh
combi_a$diff7 <-   combi_a$OMtioXZZ   -    combi_a$ukWqmeSS

combi_a$diff8 <-   combi_a$YFMZwKrU   -    combi_a$TiwRslOh
combi_a$diff9 <-   combi_a$YFMZwKrU   -    combi_a$ukWqmeSS

#################################mul

combi_a$mul1 <-   combi_a$nEsgxvAq   *    combi_a$OMtioXZZ
combi_a$mul2 <-   combi_a$nEsgxvAq   *    combi_a$YFMZwKrU
combi_a$mul3 <-   combi_a$nEsgxvAq   *    combi_a$TiwRslOh
combi_a$mul4 <-   combi_a$nEsgxvAq   *    combi_a$ukWqmeSS

combi_a$mul5 <-   combi_a$OMtioXZZ   *    combi_a$YFMZwKrU
combi_a$mul6 <-   combi_a$OMtioXZZ   *    combi_a$TiwRslOh
combi_a$mul7 <-   combi_a$OMtioXZZ   *    combi_a$ukWqmeSS

combi_a$mul8 <-   combi_a$YFMZwKrU   *    combi_a$TiwRslOh
combi_a$mul9 <-   combi_a$YFMZwKrU   *    combi_a$ukWqmeSS

#################################div

combi_a$div1 <-   combi_a$nEsgxvAq   /    combi_a$OMtioXZZ
combi_a$div2 <-   combi_a$nEsgxvAq   /    combi_a$YFMZwKrU
combi_a$div3 <-   combi_a$nEsgxvAq   /    combi_a$TiwRslOh
combi_a$div4 <-   combi_a$nEsgxvAq   /    combi_a$ukWqmeSS

combi_a$div5 <-   combi_a$OMtioXZZ   /    combi_a$YFMZwKrU
combi_a$div6 <-   combi_a$OMtioXZZ   /    combi_a$TiwRslOh
combi_a$div7 <-   combi_a$OMtioXZZ   /    combi_a$ukWqmeSS

combi_a$div8 <-   combi_a$YFMZwKrU   /    combi_a$TiwRslOh
combi_a$div9 <-   combi_a$YFMZwKrU   /    combi_a$ukWqmeSS

##################################mean
combi_a$mean1 <-   sapply(combi_a$nEsgxvAq   +    combi_a$OMtioXZZ , mean)
combi_a$mean2 <-   sapply(combi_a$nEsgxvAq   +    combi_a$YFMZwKrU , mean)
combi_a$mean3 <-   sapply(combi_a$nEsgxvAq   +    combi_a$TiwRslOh , mean)
combi_a$mean4 <-   sapply(combi_a$nEsgxvAq   +    combi_a$ukWqmeSS , mean)

combi_a$mean5 <-   sapply(combi_a$OMtioXZZ   +    combi_a$YFMZwKrU , mean)
combi_a$mean6 <-   sapply(combi_a$OMtioXZZ   +    combi_a$TiwRslOh , mean)
combi_a$mean7 <-   sapply(combi_a$OMtioXZZ   +    combi_a$ukWqmeSS , mean)

combi_a$mean8 <-   sapply(combi_a$YFMZwKrU   +    combi_a$TiwRslOh , mean)
combi_a$mean9 <-   sapply(combi_a$YFMZwKrU   +    combi_a$ukWqmeSS , mean)
#######################################################################


# save ID for later submission, replacing target from boolean 
# TRUE/FALSE to binary 0/1
ID   <- combi_a$id
ID   <- as.data.frame(ID)
country <- combi_a$country
country <- as.data.frame(country)
poor <- combi_a$poor
poor <- as.numeric(poor)
poor <- as.factor(poor)
poor <- as.data.frame(poor)
#################################


#eliminate target, useful if you want to use t-sne from scratch
#avoid data leakeage, don't use t-sne with your target variable into it
combi_a$poor <- NULL

###################################################################
#load feature selection 
RFE_a <- readRDS("./data/RFE_a.rds")
RFE_a <- RFE_a

combi_a <- as.data.frame(combi_a)
combi_a <- combi_a[, RFE_a]



#####################################################################
# create dummy variables. if you want to build tsne map from scratch use
# dummy variables 
#combi_a.new <- dummy.data.frame(combi_a , sep = ".")
#run t SNE on the whole set perplexity 600

#tsne_a <- Rtsne(combi_a.new, dims = 2, perplexity=600, verbose=TRUE, max_iter = 5000)
#plot(tsne_a$Y)
#ts <- tsne_a$Y
# saveRDS(ts, "./data/tsne_a.rds")
# load t- SNE rapresentation
tsne_a <- readRDS("./data/tsne_a.rds")
####################################################################
# let's mix our ingredients togheter again
tsne_a  <- as.data.frame(tsne_a)
combi_a <- bind_cols(ID, combi_a)
combi_a <- bind_cols(combi_a, country)
combi_a <- bind_cols(combi_a, tsne_a)
combi_a <- bind_cols(combi_a, poor)
##############################################################################
###############################################################################
#split again
#if you want to run as regression uncomment below
#combi_a$poor <- as.numeric(combi_a$poor)-1
dtrain_a =combi_a[1:8203 ,]
dtest_a = combi_a[8204:12244 ,]
###########################
#eliminate ID
dtrain_a$ID <- NULL
################################################################################
# eliminate character columns
dtrain_a <- as.data.frame(dtrain_a)
dtrain_a[sapply(dtrain_a, is.character)] <- list(NULL)
###########################################################
# Create a stratified random sample to create train and validation sets
# If you want to use a validation set for h20 automl function or run 
#a local CV on you machine rather than use all the trainset for scoring
#this could be useful 

#trainIndex   <- createDataPartition(dtrain_a$poor , p=0.85, list=FALSE, times=1)
#dtrain_a.train        <- dtrain_a[ trainIndex, ]
#dtrain_a.test         <- dtrain_a[-trainIndex, ]
############################
dtrain_a$id <- NULL

# Identify predictors and response
y <- "poor"
x <- setdiff(names(dtrain_a), y)

#feed the h2o cloud
h2o.init(nthreads = -1,  max_mem_size = "16g")
train <- as.h2o(dtrain_a)

dtest_a.test <- as.h2o(dtest_a)
###*******************

#automl function, use it if you want to check up new ideas instead of 
#searching a new hyperparameters space
#aml_a <- h2o.automl(x = x, y = y,
#                    training_frame = dtrain_a.train,
#                    validation_frame = dtrain_a.test,
#                    nfolds = 10,
#                    seed = 1234,
#                    max_runtime_secs = 1200
#                    )
###################################################
###################################################
###################################################
# stacked ensemble country A starts here
#######################################################
##############################################

nfolds <- 10

glm1 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #    validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.99,
                  lambda = 0.002499,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm2 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #    validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.89,
                  lambda = 0.00278,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm3 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #     validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.74,
                  lambda = 0.003344,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm4 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #    validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.65,
                  lambda = 0.003807,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm5 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #       validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.51,
                  lambda = 0.004852,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm6 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #      validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.47,
                  lambda = 0.005265,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm7 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #     validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.46,
                  lambda = 0.005379,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm8 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #      validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.38,
                  lambda =  0.006511,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm9 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #        validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.32,
                  lambda =  0.007732,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm10 <- h2o.glm ( x=x,
                   y=y,
                   training_frame= train,
                   #      validation_frame= valid,
                   family= "binomial",
                   nfolds= nfolds,
                   alpha=0.2,
                   lambda =  0.01237,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 1)

glm11 <- h2o.glm ( x=x,
                   y=y,
                   training_frame= train,
                   #       validation_frame= valid,
                   family= "binomial",
                   nfolds= nfolds,
                   alpha=0.1,
                   lambda =0.02474,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 1)

nn1  <- h2o.deeplearning(x=x,
                         y=y,
                         training_frame = train,
                         #        validation_frame = valid,
                         nfolds= nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         epochs = 7 ,
                         epsilon = 1.0E-8   ,
                         hidden = c(500) ,
                         l1= 3.5E-5 ,
                         l2= 5.9E-5  ,
                         rho= 0.9 , 
                         activation = "RectifierWithDropout",
                         seed = 1)


nn2  <- h2o.deeplearning(x=x,
                         y=y,
                         training_frame = train,
                         #        validation_frame = valid,
                         nfolds= nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         epochs = 8 ,
                         epsilon = 1.0E-10,
                         hidden = c(500) ,
                         l1= 6.6E-5 ,
                         l2= 9.1E-5 ,
                         rho= 0.99 , 
                         activation = "RectifierWithDropout",
                         seed = 1)

nn3  <- h2o.deeplearning(x=x,
                         y=y,
                         training_frame = train,
                         #           validation_frame = valid,
                         nfolds= nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         epochs = 4 ,
                         epsilon = 1.0E-8 ,
                         hidden = c(200,200) ,
                         l1= 3.0E-5 ,
                         l2= 5.7E-5 ,
                         rho= 0.999  , 
                         activation = "RectifierWithDropout",
                         seed = 1)

nn4  <- h2o.deeplearning(x=x,
                         y=y,
                         training_frame = train,
                         #         validation_frame = valid,
                         nfolds= nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         epochs = 6 ,
                         epsilon = 1.0E-8 ,
                         hidden = c(100,100) ,
                         l1= 6.2E-5 ,
                         l2= 3.6E-5 ,
                         rho=  0.99 , 
                         activation = "RectifierWithDropout",
                         seed = 1)

nn5  <- h2o.deeplearning(x=x,
                         y=y,
                         training_frame = train,
                         #        validation_frame = valid,
                         nfolds= nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         epochs = 6 ,
                         epsilon = 1.0E-8 ,
                         hidden = c(20, 20),
                         l1= 4.0E-5 ,
                         l2= 9.4E-5,
                         rho= 0.99 , 
                         activation = "RectifierWithDropout",
                         seed = 1)

nn6  <- h2o.deeplearning(x=x,
                         y=y,
                         training_frame = train,
                         #         validation_frame = valid,
                         nfolds= nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         epochs = 6 ,
                         epsilon = 1.0E-10  ,
                         hidden = c(500) ,
                         l1= 9.5E-5 ,
                         l2= 8.0E-6 ,
                         rho= 0.999  , 
                         activation = "RectifierWithDropout",
                         seed = 1)

nn7  <- h2o.deeplearning(x=x,
                         y=y,
                         training_frame = train,
                         #        validation_frame = valid,
                         nfolds= nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         epochs = 2 ,
                         epsilon =  1.0E-8,
                         hidden = c(500) ,
                         l1= 1.7E-5  ,
                         l2= 6.4E-5 ,
                         rho= 0.99  , 
                         activation = "RectifierWithDropout",
                         seed = 1)

gbm1 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #        valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.4 ,
                      col_sample_rate_change_per_level = 0.98 ,
                      col_sample_rate_per_tree = 0.82 ,
                      histogram_type = "UniformAdaptive" ,
                      max_depth = 37  ,
                      min_split_improvement =  1.0E-4 ,
                      nbins = 512  ,
                      nbins_cats = 16 ,
                      sample_rate = 0.58 
)


gbm2 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #       valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.85,
                      col_sample_rate_change_per_level = 1.03 ,
                      col_sample_rate_per_tree = 0.97 ,
                      histogram_type = "Random"  ,
                      max_depth = 30 ,
                      min_split_improvement = 0  ,
                      nbins = 128 ,
                      nbins_cats = 512 ,
                      sample_rate = 0.49
)

gbm3 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #       valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.24 ,
                      col_sample_rate_change_per_level = 0.96 ,
                      col_sample_rate_per_tree =  1,
                      histogram_type = "Random"  ,
                      max_depth = 29  ,
                      min_split_improvement =  1.0E-6 ,
                      nbins = 16  ,
                      nbins_cats = 512,
                      sample_rate = 0.58
)


gbm4 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #      valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.23 ,
                      col_sample_rate_change_per_level = 1.1 ,
                      col_sample_rate_per_tree = 0.73 ,
                      histogram_type = "Random"  ,
                      max_depth = 25 ,
                      min_split_improvement = 1.0E-4 ,
                      nbins = 64  ,
                      nbins_cats = 4096,
                      sample_rate = 0.48
)

gbm5 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #       valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.81 ,
                      col_sample_rate_change_per_level = 0.99 ,
                      col_sample_rate_per_tree = 0.87 ,
                      histogram_type = "Random"  ,
                      max_depth = 33 ,
                      min_split_improvement = 1.0E-4   ,
                      nbins = 128 ,
                      nbins_cats = 64 ,
                      sample_rate = 0.57
)

gbm6 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #       valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.76 ,
                      col_sample_rate_change_per_level = 0.92 ,
                      col_sample_rate_per_tree = 0.83 ,
                      histogram_type = "UniformAdaptive" ,
                      max_depth = 27  ,
                      min_split_improvement = 0  ,
                      nbins = 64  ,
                      nbins_cats = 16 ,
                      sample_rate = 0.27
)

gbm7 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #       valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.94 ,
                      col_sample_rate_change_per_level = 0.91 ,
                      col_sample_rate_per_tree = 0.81 ,
                      histogram_type = "RoundRobin" ,
                      max_depth = 39 ,
                      min_split_improvement = 0 ,
                      nbins = 64  ,
                      nbins_cats = 16 ,
                      sample_rate = 0.35
)

gbm8 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #        valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.49 ,
                      col_sample_rate_change_per_level = 0.98 ,
                      col_sample_rate_per_tree = 0.67 ,
                      histogram_type = "Random" ,
                      max_depth = 31  ,
                      min_split_improvement = 1.0E-8  ,
                      nbins = 128  ,
                      nbins_cats = 1024,
                      sample_rate = 0.39 
)

gbm9 <-       h2o.gbm(x=x,
                      y=y,
                      train= train,
                      #         valid = valid,
                      stopping_rounds = 5,
                      stopping_tolerance = 0.01,
                      stopping_metric = "AUC",
                      
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      col_sample_rate = 0.22 ,
                      col_sample_rate_change_per_level = 1.07 ,
                      col_sample_rate_per_tree = 0.83 ,
                      histogram_type = "QuantilesGlobal" ,
                      max_depth = 30  ,
                      min_split_improvement =  1.0E-8 ,
                      nbins = 256  ,
                      nbins_cats = 16 ,
                      sample_rate = 0.82 
)

gbm10 <-       h2o.gbm(x=x,
                       y=y,
                       train= train,
                       #      valid = valid,
                       stopping_rounds = 5,
                       stopping_tolerance = 0.01,
                       stopping_metric = "AUC",
                       
                       nfolds = nfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       col_sample_rate = 0.74 ,
                       col_sample_rate_change_per_level = 1.06 ,
                       col_sample_rate_per_tree = 0.68 ,
                       histogram_type = "Random" ,
                       max_depth = 29  ,
                       min_split_improvement = 1.0E-8  ,
                       nbins = 256  ,
                       nbins_cats = 32 ,
                       sample_rate = 0.31 
)

rf1 <-  h2o.randomForest(x=x,
                         y=y,
                         training_frame = train,
                         #         validation_frame = valid,
                         nfolds = nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         col_sample_rate_change_per_level = 1.08 ,
                         col_sample_rate_per_tree = 0.99,
                         histogram_type = "UniformAdaptive",
                         max_depth = 29 ,
                         min_split_improvement =  1.0E-6 ,
                         nbins = 128 ,
                         nbins_cats = 4096 ,
                         sample_rate = 0.75 )

rf2 <-  h2o.randomForest(x=x,
                         y=y,
                         training_frame = train,
                         #         validation_frame = valid,
                         nfolds = nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         col_sample_rate_change_per_level = 1.04 ,
                         col_sample_rate_per_tree = 0.95,
                         histogram_type = "QuantilesGlobal" ,
                         max_depth = 37  ,
                         min_split_improvement =  1.0E-4,
                         nbins = 64 ,
                         nbins_cats = 32 ,
                         sample_rate = 0.95 )


rf3 <-  h2o.randomForest(x=x,
                         y=y,
                         training_frame = train,
                         #      validation_frame = valid,
                         nfolds = nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         col_sample_rate_change_per_level = 1 ,
                         col_sample_rate_per_tree = 0.95 ,
                         histogram_type = "RoundRobin" ,
                         max_depth =  25 ,
                         min_split_improvement = 1.0E-4 ,
                         nbins = 256 ,
                         nbins_cats = 512 ,
                         sample_rate = 0.92)


rf4 <-  h2o.randomForest(x=x,
                         y=y,
                         training_frame = train,
                         #       validation_frame = valid,
                         nfolds = nfolds,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         col_sample_rate_change_per_level = 1.07 ,
                         col_sample_rate_per_tree = 0.95 ,
                         histogram_type = "QuantilesGlobal" ,
                         max_depth = 32  ,
                         min_split_improvement = 1.0E-6 ,
                         nbins = 32 ,
                         nbins_cats = 256 ,
                         sample_rate = 0.58)


# Train a stacked ensemble using the GBM and RF above

ensemble_a <- h2o.stackedEnsemble(x = x,
                                  y = y,
                                  training_frame = train,
                                  #     validation_frame = valid,
                                  metalearner_nfolds = 10,
                                  
                                  model_id = "my_ensemble_binomial",
                                  base_models = list(glm1, glm2, glm3, glm4,
                                                     glm5, glm6, glm7, glm8,
                                                     glm9, glm10, glm11,
                                                     nn1, nn2, nn3, nn4,
                                                     nn5, nn6, nn7,
                                                     gbm1, gbm2, gbm3, gbm4,
                                                     gbm5, gbm6, gbm7, gbm8,
                                                     gbm9, gbm10,
                                                     rf1, rf2, rf3, rf4))
######################################################################
# END###
#erase Y in test set because oddly it pulls errors (maybe bacause of NA?)
dtest_a.test$poor <- NULL
#pred_a <- as.data.frame(h2o.predict(aml_a@leader, dtest_a.test)$p1)
pred_a <- as.data.frame(h2o.predict(ensemble_a, dtest_a.test)$p1)

#uncomment if you 
#pred_a <- abs(pred_a) #because lower bound of Y is 0 the prediction has some negative outcomes near 0.00...
submit_a <- data.frame( id = combi_a[8204:12244,c("ID")], country=combi_a[8204:12244, c("country")], poor = pred_a)
names(submit_a)[names(submit_a)=="country"] <- "country"
names(submit_a)[names(submit_a)=="p0"] <- "poor"


####removing dataframes, free memory
rm(combi_a_indiv)
rm(nzv)
rm(drop.cols)

rm(combi_a)
rm(combi_a_test)
rm(combi_a_train)
rm(dtest_a)
rm(dtrain_a)
rm(dtrain_a.test)
rm(dtrain_a.train)
rm(dtest_a.test)
rm(aml_a)
rm(pred_a)
rm(train_a_hhold)
rm(test_a_hhold)
rm(test_ind_a)
rm(train_ind_a)
rm(trainIndex)
rm(x)
rm(y)
rm(tsne_a)
rm(gbm1)
rm(gbm2)
rm(gbm3)
rm(gbm4)
rm(gbm5)
rm(gbm6)
rm(gbm7)
rm(gbm8)
rm(gbm9)
rm(gbm10)
rm(gbm11)
rm(glm1)
rm(glm2)
rm(glm3)
rm(glm4)
rm(glm5)
rm(glm6)
rm(glm7)
rm(glm8)
rm(glm9)
rm(glm10)
rm(glm11)
rm(nn1)
rm(nn2)
rm(nn3)
rm(nn4)
rm(nn5)
rm(nn6)
rm(nn7)
rm(rf1)
rm(rf2)
rm(rf3)
rm(rf4)
rm(ensemble_a)

h2o.removeAll()
gc()
###########################################################################


##########################
h2o.removeAll()
gc()

train_b_hhold <- fread("./data/B_hhold_train.csv", stringsAsFactors = TRUE)
test_b_hhold <- fread("./data/B_hhold_test.csv", stringsAsFactors = TRUE)
combi_b = bind_rows(train_b_hhold, test_b_hhold)

train_ind_b <- fread("./data/B_indiv_train.csv", stringsAsFactors = TRUE)
test_ind_b <- fread("./data/B_indiv_test.csv", stringsAsFactors = TRUE)

combi_b_indiv <- bind_rows(train_ind_b, test_ind_b)

combi_b_indiv <- subset( combi_b_indiv, select = -c(iid, poor,country ) )
combi_b_indiv <- combi_b_indiv[!duplicated(combi_b_indiv$id), ]

combi_b <- join(combi_b, combi_b_indiv, by="id", type="inner")
#preprocessing
nzv <- nearZeroVar(combi_b[, -c("country")], saveMetrics= TRUE)

drop.cols <- rownames(nzv)[nzv$nzv == TRUE]
combi_b <- as.data.frame(combi_b)
combi_b <- combi_b[,!names(combi_b) %in% drop.cols]
combi_b <- setDT(combi_b)
###########################################
nzv <- nearZeroVar(combi_b[, -c("country")], saveMetrics= TRUE)

drop.cols <- rownames(nzv)[nzv$nzv == TRUE]
combi_b <- as.data.frame(combi_b)
combi_b <- combi_b[,!names(combi_b) %in% drop.cols]
combi_b <- setDT(combi_b)
###################################
# preprocessing

pp_hpc <- preProcess(combi_b[, -1], 
                     method = c("center", "scale", "YeoJohnson"))
pp_hpc
combi_b <- predict(pp_hpc, newdata = combi_b)
###########################################################################################################
########################################################
##############################################################
########################################################
##############################################################
########################################################






# save ID
id   <- combi_b$id
id   <- as.data.frame(id)
country <- combi_b$country
country <- as.data.frame(country)
poor <- combi_b$poor
poor <- as.numeric(poor)
poor <- as.factor(poor)
poor <- as.data.frame(poor)
#################################


#eliminate target
combi_b$poor <- NULL

###################################################################
#load feature selection 
RFE_b <- readRDS("./data/RFE_B.rds")
RFE_b <- RFE_b

combi_b <- as.data.frame(combi_b)
combi_b <- combi_b[, RFE_b]


#crazy feature interactions##
################################ sum 
combi_b$sum1 <-   combi_b$wJthinfa   +    combi_b$qrOrXLPM
combi_b$sum2 <-   combi_b$wJthinfa   +    combi_b$dnmwvCng
combi_b$sum3 <-   combi_b$wJthinfa   +    combi_b$ldnyeZwD
combi_b$sum4 <-   combi_b$wJthinfa   +    combi_b$VyHofjLM
combi_b$sum5 <-   combi_b$wJthinfa   +    combi_b$rCVqiShm
combi_b$sum6 <-   combi_b$wJthinfa   +    combi_b$GrLBZowF
combi_b$sum7 <-   combi_b$wJthinfa   +    combi_b$NjDdhqIe
combi_b$sum8 <-   combi_b$wJthinfa   +    combi_b$NBWkerdL
combi_b$sum9 <-   combi_b$wJthinfa   +    combi_b$IOMvIGQS

combi_b$sum10 <-   combi_b$qrOrXLPM   +    combi_b$dnmwvCng
combi_b$sum12 <-   combi_b$qrOrXLPM   +    combi_b$ldnyeZwD
combi_b$sum13 <-   combi_b$qrOrXLPM   +    combi_b$VyHofjLM
combi_b$sum14 <-   combi_b$qrOrXLPM   +    combi_b$rCVqiShm
combi_b$sum15 <-   combi_b$qrOrXLPM   +    combi_b$GrLBZowF
combi_b$sum16 <-   combi_b$qrOrXLPM   +    combi_b$NjDdhqIe
combi_b$sum17 <-   combi_b$qrOrXLPM   +    combi_b$NBWkerdL
combi_b$sum18 <-   combi_b$qrOrXLPM   +    combi_b$IOMvIGQS
################################diff
combi_b$diff1 <-   combi_b$wJthinfa   -    combi_b$qrOrXLPM
combi_b$diff2 <-   combi_b$wJthinfa   -    combi_b$dnmwvCng
combi_b$diff3 <-   combi_b$wJthinfa   -    combi_b$ldnyeZwD
combi_b$diff4 <-   combi_b$wJthinfa   -    combi_b$VyHofjLM
combi_b$diff5 <-   combi_b$wJthinfa   -    combi_b$rCVqiShm
combi_b$diff6 <-   combi_b$wJthinfa   -    combi_b$GrLBZowF
combi_b$diff7 <-   combi_b$wJthinfa   -    combi_b$NjDdhqIe
combi_b$diff8 <-   combi_b$wJthinfa   -    combi_b$NBWkerdL
combi_b$diff9 <-   combi_b$wJthinfa    -   combi_b$IOMvIGQS

combi_b$diff10 <-   combi_b$qrOrXLPM   -    combi_b$dnmwvCng
combi_b$diff12 <-   combi_b$qrOrXLPM   -    combi_b$ldnyeZwD
combi_b$diff13 <-   combi_b$qrOrXLPM   -    combi_b$VyHofjLM
combi_b$diff14 <-   combi_b$qrOrXLPM   -    combi_b$rCVqiShm
combi_b$diff15 <-   combi_b$qrOrXLPM   -    combi_b$GrLBZowF
combi_b$diff16 <-   combi_b$qrOrXLPM   -    combi_b$NjDdhqIe
combi_b$diff17 <-   combi_b$qrOrXLPM   -    combi_b$NBWkerdL
combi_b$diff18 <-   combi_b$qrOrXLPM   -    combi_b$IOMvIGQS
##################################mul
combi_b$mul1 <-   combi_b$wJthinfa   *    combi_b$qrOrXLPM
combi_b$mul2 <-   combi_b$wJthinfa   *    combi_b$dnmwvCng
combi_b$mul3 <-   combi_b$wJthinfa   *    combi_b$ldnyeZwD
combi_b$mul4 <-   combi_b$wJthinfa   *    combi_b$VyHofjLM
combi_b$mul5 <-   combi_b$wJthinfa   *    combi_b$rCVqiShm
combi_b$mul6 <-   combi_b$wJthinfa   *    combi_b$GrLBZowF
combi_b$mul7 <-   combi_b$wJthinfa   *    combi_b$NjDdhqIe
combi_b$mul8 <-   combi_b$wJthinfa   *    combi_b$NBWkerdL
combi_b$mul9 <-   combi_b$wJthinfa   *    combi_b$IOMvIGQS

combi_b$mul10 <-   combi_b$qrOrXLPM   *    combi_b$dnmwvCng
combi_b$mul12 <-   combi_b$qrOrXLPM   *    combi_b$ldnyeZwD
combi_b$mul13 <-   combi_b$qrOrXLPM   *    combi_b$VyHofjLM
combi_b$mul14 <-   combi_b$qrOrXLPM   *    combi_b$rCVqiShm
combi_b$mul15 <-   combi_b$qrOrXLPM   *    combi_b$GrLBZowF
combi_b$mul16 <-   combi_b$qrOrXLPM   *    combi_b$NjDdhqIe
combi_b$mul17 <-   combi_b$qrOrXLPM   *    combi_b$NBWkerdL
combi_b$mul18 <-   combi_b$qrOrXLPM   *    combi_b$IOMvIGQS
#########################################div
combi_b$div1 <-   combi_b$wJthinfa   /    combi_b$qrOrXLPM
combi_b$div2 <-   combi_b$wJthinfa   /    combi_b$dnmwvCng
combi_b$div3 <-   combi_b$wJthinfa   /    combi_b$ldnyeZwD
combi_b$div4 <-   combi_b$wJthinfa   /    combi_b$VyHofjLM
combi_b$div5 <-   combi_b$wJthinfa   /    combi_b$rCVqiShm
combi_b$div6 <-   combi_b$wJthinfa   /    combi_b$GrLBZowF
combi_b$div7 <-   combi_b$wJthinfa   /    combi_b$NjDdhqIe
combi_b$div8 <-   combi_b$wJthinfa   /    combi_b$NBWkerdL
combi_b$div9 <-   combi_b$wJthinfa   /    combi_b$IOMvIGQS

combi_b$div10 <-   combi_b$qrOrXLPM   /    combi_b$dnmwvCng
combi_b$div12 <-   combi_b$qrOrXLPM   /    combi_b$ldnyeZwD
combi_b$div13 <-   combi_b$qrOrXLPM   /    combi_b$VyHofjLM
combi_b$div14 <-   combi_b$qrOrXLPM   /    combi_b$rCVqiShm
combi_b$div15 <-   combi_b$qrOrXLPM   /    combi_b$GrLBZowF
combi_b$div16 <-   combi_b$qrOrXLPM   /    combi_b$NjDdhqIe
combi_b$div17 <-   combi_b$qrOrXLPM   /    combi_b$NBWkerdL
combi_b$div18 <-   combi_b$qrOrXLPM   /    combi_b$IOMvIGQS
#########################################################mean

combi_b$mean1 <- sapply(  combi_b$wJthinfa   +    combi_b$qrOrXLPM , mean)
combi_b$mean2 <- sapply(  combi_b$wJthinfa   +    combi_b$dnmwvCng , mean)
combi_b$mean3 <- sapply(  combi_b$wJthinfa   +    combi_b$ldnyeZwD , mean)
combi_b$mean4 <- sapply(  combi_b$wJthinfa   +    combi_b$VyHofjLM , mean)
combi_b$mean5 <- sapply(  combi_b$wJthinfa   +    combi_b$rCVqiShm , mean)
combi_b$mean6 <- sapply(  combi_b$wJthinfa   +    combi_b$GrLBZowF , mean)
combi_b$mean7 <- sapply(  combi_b$wJthinfa   +    combi_b$NjDdhqIe , mean)
combi_b$mean8 <- sapply(  combi_b$wJthinfa   +    combi_b$NBWkerdL , mean)
combi_b$mean9 <- sapply(  combi_b$wJthinfa   +    combi_b$IOMvIGQS , mean)

combi_b$mean10 <-  sapply( combi_b$qrOrXLPM   +    combi_b$dnmwvCng , mean)
combi_b$mean12 <-  sapply( combi_b$qrOrXLPM   +    combi_b$ldnyeZwD , mean)
combi_b$mean13 <-  sapply( combi_b$qrOrXLPM   +    combi_b$VyHofjLM , mean)
combi_b$mean14 <-  sapply( combi_b$qrOrXLPM   +    combi_b$rCVqiShm , mean)
combi_b$mean15 <-  sapply( combi_b$qrOrXLPM   +    combi_b$GrLBZowF , mean)
combi_b$mean16 <-  sapply( combi_b$qrOrXLPM   +    combi_b$NjDdhqIe , mean)
combi_b$mean17 <-  sapply( combi_b$qrOrXLPM   +    combi_b$NBWkerdL , mean)
combi_b$mean18 <-  sapply( combi_b$qrOrXLPM   +    combi_b$IOMvIGQS , mean)
#############################################################################
##############################################################################

################################ sum 
combi_b$sum1 <-   combi_b$wJthinfa   +    combi_b$qrOrXLPM
combi_b$sum2 <-   combi_b$wJthinfa   +    combi_b$dnmwvCng
combi_b$sum3 <-   combi_b$wJthinfa   +    combi_b$ldnyeZwD
combi_b$sum4 <-   combi_b$wJthinfa   +    combi_b$VyHofjLM
combi_b$sum5 <-   combi_b$wJthinfa   +    combi_b$rCVqiShm
combi_b$sum6 <-   combi_b$wJthinfa   +    combi_b$GrLBZowF
combi_b$sum7 <-   combi_b$wJthinfa   +    combi_b$NjDdhqIe
combi_b$sum8 <-   combi_b$wJthinfa   +    combi_b$NBWkerdL
combi_b$sum9 <-   combi_b$wJthinfa   +    combi_b$IOMvIGQS

combi_b$sum10 <-   combi_b$qrOrXLPM   +    combi_b$dnmwvCng
combi_b$sum12 <-   combi_b$qrOrXLPM   +    combi_b$ldnyeZwD
combi_b$sum13 <-   combi_b$qrOrXLPM   +    combi_b$VyHofjLM
combi_b$sum14 <-   combi_b$qrOrXLPM   +    combi_b$rCVqiShm
combi_b$sum15 <-   combi_b$qrOrXLPM   +    combi_b$GrLBZowF
combi_b$sum16 <-   combi_b$qrOrXLPM   +    combi_b$NjDdhqIe
combi_b$sum17 <-   combi_b$qrOrXLPM   +    combi_b$NBWkerdL
combi_b$sum18 <-   combi_b$qrOrXLPM   +    combi_b$IOMvIGQS
################################diff
combi_b$diff1 <-   combi_b$wJthinfa   -    combi_b$qrOrXLPM
combi_b$diff2 <-   combi_b$wJthinfa   -    combi_b$dnmwvCng
combi_b$diff3 <-   combi_b$wJthinfa   -    combi_b$ldnyeZwD
combi_b$diff4 <-   combi_b$wJthinfa   -    combi_b$VyHofjLM
combi_b$diff5 <-   combi_b$wJthinfa   -    combi_b$rCVqiShm
combi_b$diff6 <-   combi_b$wJthinfa   -    combi_b$GrLBZowF
combi_b$diff7 <-   combi_b$wJthinfa   -    combi_b$NjDdhqIe
combi_b$diff8 <-   combi_b$wJthinfa   -    combi_b$NBWkerdL
combi_b$diff9 <-   combi_b$wJthinfa    -   combi_b$IOMvIGQS

combi_b$diff10 <-   combi_b$qrOrXLPM   -    combi_b$dnmwvCng
combi_b$diff12 <-   combi_b$qrOrXLPM   -    combi_b$ldnyeZwD
combi_b$diff13 <-   combi_b$qrOrXLPM   -    combi_b$VyHofjLM
combi_b$diff14 <-   combi_b$qrOrXLPM   -    combi_b$rCVqiShm
combi_b$diff15 <-   combi_b$qrOrXLPM   -    combi_b$GrLBZowF
combi_b$diff16 <-   combi_b$qrOrXLPM   -    combi_b$NjDdhqIe
combi_b$diff17 <-   combi_b$qrOrXLPM   -    combi_b$NBWkerdL
combi_b$diff18 <-   combi_b$qrOrXLPM   -    combi_b$IOMvIGQS
##################################mul
combi_b$mul1 <-   combi_b$wJthinfa   *    combi_b$qrOrXLPM
combi_b$mul2 <-   combi_b$wJthinfa   *    combi_b$dnmwvCng
combi_b$mul3 <-   combi_b$wJthinfa   *    combi_b$ldnyeZwD
combi_b$mul4 <-   combi_b$wJthinfa   *    combi_b$VyHofjLM
combi_b$mul5 <-   combi_b$wJthinfa   *    combi_b$rCVqiShm
combi_b$mul6 <-   combi_b$wJthinfa   *    combi_b$GrLBZowF
combi_b$mul7 <-   combi_b$wJthinfa   *    combi_b$NjDdhqIe
combi_b$mul8 <-   combi_b$wJthinfa   *    combi_b$NBWkerdL
combi_b$mul9 <-   combi_b$wJthinfa   *    combi_b$IOMvIGQS

combi_b$mul10 <-   combi_b$qrOrXLPM   *    combi_b$dnmwvCng
combi_b$mul12 <-   combi_b$qrOrXLPM   *    combi_b$ldnyeZwD
combi_b$mul13 <-   combi_b$qrOrXLPM   *    combi_b$VyHofjLM
combi_b$mul14 <-   combi_b$qrOrXLPM   *    combi_b$rCVqiShm
combi_b$mul15 <-   combi_b$qrOrXLPM   *    combi_b$GrLBZowF
combi_b$mul16 <-   combi_b$qrOrXLPM   *    combi_b$NjDdhqIe
combi_b$mul17 <-   combi_b$qrOrXLPM   *    combi_b$NBWkerdL
combi_b$mul18 <-   combi_b$qrOrXLPM   *    combi_b$IOMvIGQS
#########################################div
combi_b$div1 <-   combi_b$wJthinfa   /    combi_b$qrOrXLPM
combi_b$div2 <-   combi_b$wJthinfa   /    combi_b$dnmwvCng
combi_b$div3 <-   combi_b$wJthinfa   /    combi_b$ldnyeZwD
combi_b$div4 <-   combi_b$wJthinfa   /    combi_b$VyHofjLM
combi_b$div5 <-   combi_b$wJthinfa   /    combi_b$rCVqiShm
combi_b$div6 <-   combi_b$wJthinfa   /    combi_b$GrLBZowF
combi_b$div7 <-   combi_b$wJthinfa   /    combi_b$NjDdhqIe
combi_b$div8 <-   combi_b$wJthinfa   /    combi_b$NBWkerdL
combi_b$div9 <-   combi_b$wJthinfa   /    combi_b$IOMvIGQS

combi_b$div10 <-   combi_b$qrOrXLPM   /    combi_b$dnmwvCng
combi_b$div12 <-   combi_b$qrOrXLPM   /    combi_b$ldnyeZwD
combi_b$div13 <-   combi_b$qrOrXLPM   /    combi_b$VyHofjLM
combi_b$div14 <-   combi_b$qrOrXLPM   /    combi_b$rCVqiShm
combi_b$div15 <-   combi_b$qrOrXLPM   /    combi_b$GrLBZowF
combi_b$div16 <-   combi_b$qrOrXLPM   /    combi_b$NjDdhqIe
combi_b$div17 <-   combi_b$qrOrXLPM   /    combi_b$NBWkerdL
combi_b$div18 <-   combi_b$qrOrXLPM   /    combi_b$IOMvIGQS
#########################################################mean

combi_b$mean1 <- sapply(  combi_b$wJthinfa   +    combi_b$qrOrXLPM , mean)
combi_b$mean2 <- sapply(  combi_b$wJthinfa   +    combi_b$dnmwvCng , mean)
combi_b$mean3 <- sapply(  combi_b$wJthinfa   +    combi_b$ldnyeZwD , mean)
combi_b$mean4 <- sapply(  combi_b$wJthinfa   +    combi_b$VyHofjLM , mean)
combi_b$mean5 <- sapply(  combi_b$wJthinfa   +    combi_b$rCVqiShm , mean)
combi_b$mean6 <- sapply(  combi_b$wJthinfa   +    combi_b$GrLBZowF , mean)
combi_b$mean7 <- sapply(  combi_b$wJthinfa   +    combi_b$NjDdhqIe , mean)
combi_b$mean8 <- sapply(  combi_b$wJthinfa   +    combi_b$NBWkerdL , mean)
combi_b$mean9 <- sapply(  combi_b$wJthinfa   +    combi_b$IOMvIGQS , mean)

combi_b$mean10 <-  sapply( combi_b$qrOrXLPM   +    combi_b$dnmwvCng , mean)
combi_b$mean12 <-  sapply( combi_b$qrOrXLPM   +    combi_b$ldnyeZwD , mean)
combi_b$mean13 <-  sapply( combi_b$qrOrXLPM   +    combi_b$VyHofjLM , mean)
combi_b$mean14 <-  sapply( combi_b$qrOrXLPM   +    combi_b$rCVqiShm , mean)
combi_b$mean15 <-  sapply( combi_b$qrOrXLPM   +    combi_b$GrLBZowF , mean)
combi_b$mean16 <-  sapply( combi_b$qrOrXLPM   +    combi_b$NjDdhqIe , mean)
combi_b$mean17 <-  sapply( combi_b$qrOrXLPM   +    combi_b$NBWkerdL , mean)
combi_b$mean18 <-  sapply( combi_b$qrOrXLPM   +    combi_b$IOMvIGQS , mean)
#############################################################################
##############################################################################


###############################################################

#####################################################################
# create dummy variables
#combi_b.new <- dummy.data.frame(combi_b , sep = ".")
#run t SNE on the whole set perplexity 600

#tsne_b <- Rtsne(combi_b.new, dims = 3, perplexity=250, verbose=TRUE, max_iter = 5000)
#plot(tsne_a$Y)
#ts <- tsne_b$Y
#saveRDS(ts, "./data/tsne_b.rds")
tsne_b <- readRDS("./data/tsne_b.rds")

####################################################################
####################################################################
tsne_b  <- as.data.frame(tsne_b)
combi_b <- bind_cols(id, combi_b)
combi_b <- bind_cols(combi_b, country)
combi_b <- bind_cols(combi_b, tsne_b)
combi_b <- bind_cols(combi_b, poor)
##############################################################################
###############################################################################
#eliminate character columns

#split again
#combi_a$poor <- as.numeric(combi_a$poor)
dtrain_b =combi_b[1:3255 ,]
dtest_b = combi_b[3256:4859 ,]
#########################################
##### eliminate id
dtrain_b$id <- NULL
#eliminate character columns
dtrain_b <- as.data.frame(dtrain_b)
dtrain_b[sapply(dtrain_b, is.character)] <- list(NULL)
################################################################################
############################################################################
# weights to try to balance the learning, the dataset is very imbalanced
# but this solution didn't worked it out
#index <- c(0, 1)
#values <- c(1, 10)
#dtrain_b$weights <- values[match(dtrain_b$poor, index)]

dtrain_b$poor <- as.factor(dtrain_b$poor)

# Create a stratified random sample to create train and validation sets

#trainIndex   <- createDataPartition(dtrain_b$poor , p=0.85, list=FALSE, times=1)
#dtrain_b.train        <- dtrain_b[ trainIndex, ]
#dtrain_b.test         <- dtrain_b[-trainIndex, ]
#############################################################################
#############################################################################

# Identify predictors and response
y <- "poor"
x <- setdiff(names(dtrain_b), y)

#feed the h2o cloud
h2o.init(nthreads = -1, max_mem_size = "16g" )
train <- as.h2o(dtrain_b)

dtest_b.test <- as.h2o(dtest_b)


#############################################################
###############################################################
#automl function 
#aml_b <- h2o.automl(x = x, y = y,
#                    training_frame = train,
#                    #                    validation_frame = dtrain_b.test,
#                    #                    weights_column = "weights",  
#                    nfolds = 10,
#                    seed = 1234,
#                    max_runtime_secs = 600)
###################################################################
####################################################################
####################################################################
nfolds <- 10


glm1 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #     validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.05,
                  lambda = 0.01568,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm2 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #    validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.08,
                  lambda = 0.01357,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm3 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #   validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.07,
                  lambda = 0.01413,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm4 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #   validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.1,
                  lambda = 0.01192,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm5 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #    validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.06,
                  lambda =0.01434,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm6 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #    validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.12,
                  lambda =0.0109,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm7 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #     validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha=0.18,
                  lambda = 0.009168,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm8 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #     validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha= 0.2,
                  lambda = 0.008644,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm9 <- h2o.glm ( x=x,
                  y=y,
                  training_frame= train,
                  #      validation_frame= valid,
                  family= "binomial",
                  nfolds= nfolds,
                  alpha= 0.23,
                  lambda = 0.007875,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

glm10 <- h2o.glm ( x=x,
                   y=y,
                   training_frame= train,
                   #     validation_frame= valid,
                   family= "binomial",
                   nfolds= nfolds,
                   alpha=0.28,
                   lambda =  0.006776,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 1)

glm11 <- h2o.glm ( x=x,
                   y=y,
                   training_frame= train,
                   #    validation_frame= valid,
                   family= "binomial",
                   nfolds= nfolds,
                   alpha=0.32,
                   lambda = 0.006212,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 1)

nn1 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #      validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 6 ,
                       epsilon= 1.0E-8 ,
                       hidden= c(20,20) ,
                       rho= 0.9 ,
                       input_dropout_ratio = ,
                       l1 = 9.4e-05 , 
                       l2 = 9.5e-05 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)


nn2 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #      validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 4 ,
                       epsilon= 1.0E-8 ,
                       hidden= c(20,20) ,
                       rho= 0.9 ,
                       
                       l1 = 5.5e-05 , 
                       l2 = 7e-06 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

nn3 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #      validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 2 ,
                       epsilon= 1.0E-4 ,
                       hidden= c(200,200) ,
                       rho= 0.95 ,
                       
                       l1 = 1e-05 , 
                       l2 = 5e-06 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

nn4 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #     validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 6 ,
                       epsilon= 1.0E-10 ,
                       hidden= c(50,50) ,
                       rho= 0.99 ,
                       
                       l1 = 4.1e-05 , 
                       l2 = 9.3e-05 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

nn5 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #    validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 4 ,
                       epsilon= 1.0E-8 ,
                       hidden= c(20,20) ,
                       rho= 0.95 ,
                       
                       l1 = 8.4e-05 , 
                       l2 = 8.9e-05 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

nn6 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #      validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 8 ,
                       epsilon= 1.0E-8 ,
                       hidden= c(50,50,50) ,
                       rho= 0.95 ,
                       
                       l1 = 9.9e-05 , 
                       l2 = 7.7e-05 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

nn7 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #     validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 6 ,
                       epsilon= 1.0E-10 ,
                       hidden= c(100,100) ,
                       rho= 0.999 ,
                       
                       l1 = 8.8e-05 , 
                       l2 = 7.2e-05 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

nn8 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #      validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 4 ,
                       epsilon= 1.0E-10 ,
                       hidden= c(50,50, 50) ,
                       rho= 0.999 ,
                       
                       l1 = 8.6e-05 , 
                       l2 = 2.1e-05 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

nn9 <-h2o.deeplearning(x=x,
                       y=y,
                       training_frame = train,
                       #       validation_frame = valid,
                       nfolds= nfolds,
                       balance_classes = TRUE,
                       epochs= 6 ,
                       epsilon= 1.0E-8 ,
                       hidden= c(100,100) ,
                       rho= 0.9 ,
                       
                       l1 = 9.9e-05 , 
                       l2 = 2e-05 ,
                       activation = "RectifierWithDropout",
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

nn10 <-h2o.deeplearning(x=x,
                        y=y,
                        training_frame = train,
                        #      validation_frame = valid,
                        nfolds= nfolds,
                        balance_classes = TRUE,
                        epochs= 4 ,
                        epsilon= 1.0E-8 ,
                        hidden= c(50,50) ,
                        rho= 0.9 ,
                        
                        l1 = 2.6e-05 , 
                        l2 = 8.5e-05 ,
                        activation = "RectifierWithDropout",
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE,
                        seed = 1)


gbm1 <-    h2o.gbm(x=x,
                   y=y,
                   training_frame = train,
                   #     validation_frame = valid,
                   nfolds = nfolds,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   stopping_rounds = 5,
                   
                   
                   stopping_tolerance = 0.01,
                   stopping_metric = "AUC",
                   # weights_column = "weights",
                   col_sample_rate= 0.22,
                   col_sample_rate_change_per_level = 0.9,
                   col_sample_rate_per_tree = 0.53 ,
                   histogram_type = "UniformAdaptive",
                   max_depth = 10 ,
                   min_split_improvement = 1.0E-4 ,
                   nbins = 64 ,
                   nbins_cats = 2048 ,
                   sample_rate = 0.21,
                   seed = 1   )

gbm2 <-   h2o.gbm(x=x,
                  y=y,
                  training_frame = train,
                  #    validation_frame = valid,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  stopping_rounds = 5,
                  stopping_tolerance = 0.01,
                  stopping_metric = "AUC",
                  #  weights_column = "weights",
                  col_sample_rate= 0.28 ,
                  col_sample_rate_change_per_level = 0.9 ,
                  col_sample_rate_per_tree = 0.72 ,
                  histogram_type = "Random" ,
                  max_depth = 12 ,
                  min_split_improvement = 0,
                  nbins = 1024 ,
                  nbins_cats = 256 ,
                  sample_rate = 0.28, 
                  seed = 1   )

gbm3 <-   h2o.gbm(x=x,
                  y=y,
                  training_frame = train,
                  #     validation_frame = valid,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  stopping_rounds = 5,
                  stopping_tolerance = 0.01,
                  stopping_metric = "AUC",
                  #  weights_column = "weights",
                  col_sample_rate= 0.32 ,
                  col_sample_rate_change_per_level = 0.92 ,
                  col_sample_rate_per_tree = 0.37 ,
                  histogram_type = "Random" ,
                  max_depth = 11 ,
                  min_split_improvement = 1.0E-8 ,
                  nbins = 1024,
                  nbins_cats = 4096 ,
                  sample_rate = 0.35,
                  seed = 1   )

gbm4 <-   h2o.gbm(x=x,
                  y=y,
                  training_frame = train,
                  #     validation_frame = valid,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  stopping_rounds = 5,
                  
                  stopping_tolerance = 0.01,
                  stopping_metric = "AUC",
                  #  weights_column = "weights",
                  col_sample_rate= 0.97 ,
                  col_sample_rate_change_per_level = 1.06 ,
                  col_sample_rate_per_tree = 0.32 ,
                  histogram_type = "Random" ,
                  max_depth = 13 ,
                  min_split_improvement =  1.0E-4 ,
                  nbins = 256 ,
                  nbins_cats = 64 ,
                  sample_rate = 0.26, 
                  seed = 1   )

gbm5 <-   h2o.gbm(x=x,
                  y=y,
                  training_frame = train,
                  #   validation_frame = valid,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  stopping_rounds = 5,
                  stopping_tolerance = 0.01,
                  stopping_metric = "AUC",
                  #  weights_column = "weights",
                  col_sample_rate= 0.65 ,
                  col_sample_rate_change_per_level = 0.92 ,
                  col_sample_rate_per_tree = 0.63  ,
                  histogram_type = "QuantilesGlobal" ,
                  max_depth = 10,
                  min_split_improvement = 1.0E-6 ,
                  nbins = 64 ,
                  nbins_cats =  2048,
                  sample_rate = 0.23, 
                  seed = 1   )

gbm6 <-   h2o.gbm(x=x,
                  y=y,
                  training_frame = train,
                  #     validation_frame = valid,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  stopping_rounds = 5,
                  stopping_tolerance = 0.01,
                  stopping_metric = "AUC",
                  #   weights_column = "weights",
                  col_sample_rate= 0.6,
                  col_sample_rate_change_per_level = 1.09 ,
                  col_sample_rate_per_tree = 0.71 ,
                  histogram_type = "Random" ,
                  max_depth = 22 ,
                  min_split_improvement = 0 ,
                  nbins = 128 ,
                  nbins_cats = 128 ,
                  sample_rate = 0.27,
                  seed = 1   )

gbm7 <-   h2o.gbm(x=x,
                  y=y,
                  training_frame = train,
                  #   validation_frame = valid,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  stopping_rounds = 5,
                  
                  stopping_tolerance = 0.01,
                  stopping_metric = "AUC",
                  #   weights_column = "weights",
                  col_sample_rate= 1 ,
                  col_sample_rate_change_per_level = 0.94,
                  col_sample_rate_per_tree =  0.71,
                  histogram_type = "Random",
                  max_depth = 25 ,
                  min_split_improvement = 1.0E-4,
                  nbins = 512 ,
                  nbins_cats = 64 ,
                  sample_rate = 0.39, 
                  seed = 1   )



gbm8 <-   h2o.gbm(x=x,
                  y=y,
                  training_frame = train,
                  #   validation_frame = valid,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  stopping_rounds = 5,
                  stopping_tolerance = 0.01,
                  stopping_metric = "AUC",
                  # weights_column = "weights",
                  col_sample_rate= 0.33 ,
                  col_sample_rate_change_per_level = 1.06 ,
                  col_sample_rate_per_tree = 0.22  ,
                  histogram_type = "Random" ,
                  max_depth = 24 ,
                  min_split_improvement =  1.0E-6 ,
                  nbins = 512 ,
                  nbins_cats = 128,
                  sample_rate = 0.39, 
                  seed = 1   )








# Train a stacked ensemble using the GBM and RF above
ensemble_a <- h2o.stackedEnsemble(x = x,
                                  y = y,
                                  training_frame = train,
                                  #     validation_frame = valid,
                                  metalearner_nfolds = 10,
                                  
                                  model_id = "my_ensemble_binomial",
                                  base_models = list(glm1, glm2, glm3, glm4,
                                                     glm5, glm6, glm7, glm8,
                                                     glm9, glm10, glm11, 
                                                     nn1, nn2, nn3, nn4, nn5,
                                                     nn6, nn7, nn8, nn9, nn10,
                                                     gbm1, gbm2, gbm3, gbm4,
                                                     gbm5, gbm6, gbm7, gbm8))
#################################################################
###################################################################

#erase Y in test set because oddly it pulls errors (maybe bacause of NA?)
dtest_b.test$poor <- NULL
dtest_b.test$ogUWmSXZ <- NULL #eliminate trouble columns
dtest_b.test$GshWgURK <- NULL 
#pred_b <- as.data.frame(h2o.predict(aml_b@leader, dtest_b.test)$p1)
pred_b <- as.data.frame(h2o.predict(ensemble_a, dtest_b.test)$p1)

pred_b <- abs(pred_b) #because lower bound of Y is 0 the prediction has some negative outcomes near 0.00...
submit_b <- data.frame( id = combi_b[3256:4859,c("id")], country=combi_b[3256:4859, c("country")], poor = pred_b)
names(submit_b)[names(submit_b)=="country"] <- "country"
names(submit_b)[names(submit_b)=="p0"] <- "poor"

####removing dataframes
rm(combi_b_indiv)
rm(nzv)
rm(drop.cols)
rm(pp_hpc)

rm(combi_b)
rm(combi_b_test)
rm(combi_b_train)
rm(dtest_b)
rm(dtrain_b)
rm(dtrain_b.test)
rm(dtrain_b.train)
rm(dtest_b.test)
rm(aml_b)
rm(pred_b)
rm(train_b_hhold)
rm(test_b_hhold)
rm(test_ind_b)
rm(train_ind_b)
rm(trainIndex)
rm(x)
rm(y)
rm(tsne_b)
rm(gbm1)
rm(gbm2)
rm(gbm3)
rm(gbm4)
rm(gbm5)
rm(gbm6)
rm(gbm7)
rm(gbm8)
rm(glm1)
rm(glm2)
rm(glm3)
rm(glm4)
rm(glm5)
rm(glm6)
rm(glm7)
rm(glm8)
rm(glm9)
rm(glm10)
rm(glm11)
rm(nn1)
rm(nn2)
rm(nn3)
rm(nn4)
rm(nn5)
rm(nn6)
rm(nn7)
rm(nn8)
rm(nn9)
rm(nn10)
rm(ensemble_a)
h2o.removeAll()
gc()

########################################################################

##########################
h2o.removeAll()
gc()

train_c_hhold <- fread("./data/C_hhold_train.csv", stringsAsFactors = TRUE)
test_c_hhold <- fread("./data/C_hhold_test.csv", stringsAsFactors = TRUE)
combi_c = bind_rows(train_c_hhold, test_c_hhold)

train_ind_c <- fread("./data/C_indiv_train.csv", stringsAsFactors = TRUE)
test_ind_c <- fread("./data/C_indiv_test.csv", stringsAsFactors = TRUE)

combi_c_indiv <- bind_rows(train_ind_c, test_ind_c)

combi_c_indiv <- subset( combi_c_indiv, select = -c(iid, poor,country ) )
combi_c_indiv <- combi_c_indiv[!duplicated(combi_c_indiv$id), ]

combi_c <- join(combi_c, combi_c_indiv, by="id", type="inner")

#preprocessing
nzv <- nearZeroVar(combi_c[, -c("country")], saveMetrics= TRUE)

drop.cols <- rownames(nzv)[nzv$nzv == TRUE]
combi_c <- as.data.frame(combi_c)
combi_c <- combi_c[,!names(combi_c) %in% drop.cols]
combi_c <- setDT(combi_c)

pp_hpc <- preProcess(combi_c[, -1], 
                     method = c("center", "scale", "YeoJohnson"))
pp_hpc
combi_c <- predict(pp_hpc, newdata = combi_c)

#split again
combi_c$poor <- as.numeric(combi_c$poor)
dtrain_c =combi_c[1:6469 ,]
dtest_c = combi_c[6470:9656 ,]

dtrain_c <- as.data.frame(dtrain_c)
dtrain_c[sapply(dtrain_c, is.character)] <- list(NULL)

poor <- dtrain_c$poor
poor <- as.data.frame(poor)
#############################
#load feature selection 
RFE_c <- readRDS("./data/RFE_c.rds")


dtrain_c <- dtrain_c[, RFE_c]
dtrain_c<- bind_cols(dtrain_c, poor)

#index <- c(0, 1)
#values <- c(1, 5.60)
#dtrain_c$weights <- values[match(dtrain_c$poor, index)]

dtrain_c$poor <- as.factor(dtrain_c$poor)

# Create a stratified random sample to create train and validation sets

#trainIndex   <- createDataPartition(dtrain_c$poor , p=0.85, list=FALSE, times=1)
#dtrain_c.train        <- dtrain_c[ trainIndex, ]
#dtrain_c.test         <- dtrain_c[-trainIndex, ]
#dtrain_c.train <- dtrain_c

dtrain_c.train <- as.data.frame(dtrain_c.train)
dtrain_c.test <- as.data.frame(dtrain_c.test)

# Identify predictors and response
y <- "poor"
x <- setdiff(names(dtrain_c), y)

#feed the h2o cloud
h2o.init(nthreads = -1,  max_mem_size = "16g")
dtrain_c.train <- as.h2o(dtrain_c)

dtest_c.test <- as.h2o(dtest_c)

#autoencoders


#automl function, for country C is ok to use it, you will get 
# 0.18 logloss
aml_c <- h2o.automl(x = x, y = y,
                    training_frame = dtrain_c.train,
                    #  validation_frame = dtrain_c.test,
                    #  weights_column = "weights",
                    nfolds = 10,
                    seed = 1234,
                    max_runtime_secs = 1200)

#erase Y in test set because oddly it pulls errors (maybe bacause of NA?)
dtest_c.test$poor <- NULL
pred_c <- as.data.frame(h2o.predict(aml_c@leader, dtest_c.test)$p1)


pred_c <- abs(pred_c) #because lower bound of Y is 0 the prediction has some negative outcomes near 0.00...
submit_c <- data.frame( id = combi_c[6470:9656,c("id")], country=combi_c[6470:9656, c("country")], poor = pred_c)
names(submit_c)[names(submit_c)=="country"] <- "country"
names(submit_c)[names(submit_c)=="p0"] <- "poor"

####removing dataframes
rm(combi_c_indiv)
rm(nzv)
rm(drop.cols)
rm(pp_hpc)

rm(combi_c)
rm(combi_c_test)
rm(combi_c_train)
rm(dtest_c)
rm(dtrain_c)
rm(dtrain_c.test)
rm(dtrain_c.train)
rm(dtest_c.test)
rm(aml_c)
rm(pred_c)
rm(train_c_hhold)
rm(test_c_hhold)
rm(test_ind_c)
rm(train_ind_c)
rm(trainIndex)
rm(x)
rm(y)
###########################################
# merging for final submission
submit_temp <- bind_rows(submit_a, submit_b)
submit_final <- bind_rows(submit_temp, submit_c)
write.csv(submit_final, file = "./results/poverty_final.csv", row.names = FALSE)