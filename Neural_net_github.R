# Neural Net hyperparameter search if you want to search a reliable network
# architecture uncomment the code, if the hyper_params still persist in the
# memory backend 
# rm(hyper_params)
# h2o.removeALL
# h2o.shutdown
# gc()
# re initialize the backend

######################################################################################
###########################################################################
############################################################################
hyper_params = list( 
   #HIDDEN LAYERS SEARCH
  #hidden = list(c(20,20), c(50,50), c(50,50,50), c(100,100), c(100,100,100),
  #              c(200, 200), c(200,200,200), c(500), c(500,500),
  #              c(1000), c(1000,1000)),
  hidden = list(c(20,20), c(50,50), c(50,50,50), c(100,100), c(200,200), c(500)),
  
  epochs = seq(2,8,1),
  rho = c(0.9,0.95,0.99,0.999),
  epsilon = c(1e-10,1e-8,1e-6,1e-4),
  ##################################################################
  # activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  
  #input_dropout_ratio=c(0,0.05,0.1,0.2),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6)
  
  
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 120 minutes
  max_runtime_secs = 7200,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 10,                
  stopping_metric = "AUC",
  stopping_tolerance = 0.1
)
############################################################################
############################################################################
############################################################################

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "deeplearning",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid", 
  
  ## standard model parameters
  x=x,
  y=y,
  training_frame = train,
  validation_frame = valid,
  nfolds= 2,
  #balance_classes = TRUE,
  ###### trying something better####### hyperparameter search 2#####
 
  activation = "RectifierWithDropout",
  
 
  
  
  
  ## early stopping once the validation AUC doesn't improve by at least 0.1% for 5 consecutive scoring events
  stopping_rounds = 2, 
  stopping_tolerance = 1e-1, 
  stopping_metric = "AUC", 
  
  
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 12345                                                             
)
## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid

## Sort the grid models by AUC
sortedGrid <- h2o.getGrid("final_grid", sort_by = "auc", decreasing = TRUE)    
sortedGrid



