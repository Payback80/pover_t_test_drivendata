# Pover T test driven data competition 
The World Bank is aiming to end extreme poverty by 2030. Crucial to this goal are techniques for determining which poverty reduction strategies work and which do not. But measuring poverty reduction requires measuring poverty in the first place, and it turns out that measuring poverty is pretty hard. The World Bank helps developing countries measure poverty by conducting in-depth household surveys with a subset of the country's population in order to get a clearer picture of a household's poverty status.


I was able to achieve top 5% in this competition against 2310 other data scientists world-wide. The problem was to model 3 different datasets
My solution pipeline was: 

pre process and normalize data
feature space reduction with Recursive Feature Elimination (RFE) based on random forest and 10 fold CV this also gave me the features ordered by importance, 
feature clustering and reduction with the t-distributed stochastic neighbor embedding algorithm (t-SNE) and added it as new features into the dataset
basic feature interaction due the fact the dataset in anonymized 
for the final model i used a stacked ensemble of around 10 each gradient boosting trees, random forest, neural networks, elastic net linear model and stacked with a 10 folds GBM, all the models and parameters tuning have done under H2O.ai framework and R
with this code you will be able to obtain 0.1650 mean logloss error 

##instructions: 
the main file is poverty_github.r you should be able to run it without problems, just change the "fread" directory lines, RFE, TSNE of your local machine and download the mid processing files: 
RFE/a/b/b and tsne a/b.
if you have forgotten the ratios of the 3 countries is:
 0.46 A
 0.18 B
 0.36 C
 So you can calculate the mean logloss again.
 I have commented in details just the first dataset, you could refer to it for countries B and C 

file RFE_github.r shows how to perform the feature selection
file tsne_github shows the how to of a viable t-sne features rapresentation
file Neural_net_github.r shows how to find a neural netwtork architecture and hyperparameters search, you should be able to perform the others algorithms optimization from this sketch, if you really really really want drop me a line!

#what didn't worked globally so far (feel free to try it):

PCA
GLRM
naive bayes models
k means clustering 

##what didn't worked for balance the B country:

creation of synthetic data with the SMOTE algorithm
upsampling the minority class
upsampling the whole dataset
upsampling the whole dataset and upsample the minority class  
autoencoders to add decoded features as new features
autoencoders to model the minority class 
autoencorders to model the outliers (MSE < 0.001)





## Author

* **Andrea Mariani** - *Initial work* - [Payback80](https://github.com/Payback80)
* ** linkedin** (https://www.linkedin.com/in/andrea-mariani-353381/)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details


