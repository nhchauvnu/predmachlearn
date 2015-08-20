# Practical machine learning course project
#### by Chau Nguyen

## Load and check data
First we check if the data files exist on the local disk. If not
we download them. Then we load the data into R.

```r
if (!file.exists('pml-training.csv')) {
	print("Download training data set")
	download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',
		dest='pml-training.csv', quiet=F, method='curl')
}
# Load training data set
tr = read.csv('pml-training.csv')

if (!file.exists('pml-testing.csv')) {
	print("Download testing data set")
	download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',
		dest='pml-testing.csv', quiet=F, method='curl')
}
# Load testing data set
te = read.csv('pml-testing.csv')
```

## Data cleaning
We count the number of NA values on all variables of
the training and testing data sets. Variables with too many
NA values will be removed from the data sets.

```r
# natr is a vector contains number of NA in each variables of the training set
natr = sapply(1:ncol(tr), function(i) {length(which(is.na(tr[,i])))})
# Show NAs in the training set in percent
natr/nrow(tr)*100
```

```
##   [1]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
##   [8]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
##  [15]  0.00000  0.00000  0.00000 97.93089 97.93089  0.00000 97.93089
##  [22] 97.93089  0.00000 97.93089 97.93089  0.00000 97.93089 97.93089
##  [29] 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089
##  [36] 97.93089  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
##  [43]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
##  [50] 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089
##  [57] 97.93089 97.93089 97.93089  0.00000  0.00000  0.00000  0.00000
##  [64]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
##  [71]  0.00000  0.00000  0.00000  0.00000 97.93089 97.93089 97.93089
##  [78] 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089  0.00000
##  [85]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
##  [92]  0.00000 97.93089 97.93089  0.00000 97.93089 97.93089  0.00000
##  [99] 97.93089 97.93089  0.00000  0.00000 97.93089 97.93089 97.93089
## [106] 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089
## [113]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [120]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [127]  0.00000  0.00000  0.00000  0.00000 97.93089 97.93089  0.00000
## [134] 97.93089 97.93089  0.00000 97.93089 97.93089  0.00000  0.00000
## [141] 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089 97.93089
## [148] 97.93089 97.93089 97.93089  0.00000  0.00000  0.00000  0.00000
## [155]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
```

```r
# nate is a vector contains number of NA in each variables of the testing set
nate = sapply(1:ncol(te), function(i) {length(which(is.na(te[,i])))})
# Show NAs in the testing set in percent
nate/nrow(tr)*100
```

```
##   [1] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
##   [8] 0.0000000 0.0000000 0.0000000 0.0000000 0.1019264 0.1019264 0.1019264
##  [15] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
##  [22] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
##  [29] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
##  [36] 0.1019264 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
##  [43] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
##  [50] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
##  [57] 0.1019264 0.1019264 0.1019264 0.0000000 0.0000000 0.0000000 0.0000000
##  [64] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.1019264 0.1019264
##  [71] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
##  [78] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.0000000
##  [85] 0.0000000 0.0000000 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
##  [92] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
##  [99] 0.1019264 0.1019264 0.1019264 0.0000000 0.1019264 0.1019264 0.1019264
## [106] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
## [113] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
## [120] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.1019264 0.1019264
## [127] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
## [134] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.0000000
## [141] 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264 0.1019264
## [148] 0.1019264 0.1019264 0.1019264 0.0000000 0.0000000 0.0000000 0.0000000
## [155] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
```

```r
# Which variables on the training set has many NAs?
settr = which(natr>0)
length(settr)
```

```
## [1] 67
```

```r
settr
```

```
##  [1]  18  19  21  22  24  25  27  28  29  30  31  32  33  34  35  36  50
## [18]  51  52  53  54  55  56  57  58  59  75  76  77  78  79  80  81  82
## [35]  83  93  94  96  97  99 100 103 104 105 106 107 108 109 110 111 112
## [52] 131 132 134 135 137 138 141 142 143 144 145 146 147 148 149 150
```

```r
# Which variables on the testing set has many NAs?
sette = which(nate>0)
length(sette)
```

```
## [1] 100
```

```r
sette
```

```
##   [1]  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28
##  [18]  29  30  31  32  33  34  35  36  50  51  52  53  54  55  56  57  58
##  [35]  59  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  87
##  [52]  88  89  90  91  92  93  94  95  96  97  98  99 100 101 103 104 105
##  [69] 106 107 108 109 110 111 112 125 126 127 128 129 130 131 132 133 134
##  [86] 135 136 137 138 139 141 142 143 144 145 146 147 148 149 150
```

```r
# Which columns on the two sets cannot be used?
setall = union(settr, sette)

# Eliminate the bad variables we have smaller training and testing
# data sets
tr = tr[, -setall]
te = te[, -setall]
```

## Train models for prediction
After remove "bad" variables we have a new training and a new test data set,
refers as TR and TE data sets, prepectively. To predict the 20 test cases
in TE, we first divide the TR set into 
a training set and a testing set. We refer them as TR.train and TR.test data
sets, respectively. We build three models based on CART, C5.0 and Random
Forest methods on TR.train, calculate the accuracy of the models on TR.test, then choose
the most appropriate model to predict on TE.

The TR and TE data sets contain data for 6 users.
We therefore peform two test scenarios for comparison of accuracy. The first one is
training and testing on data of each individual user (FILTER case), and the second one is 
training and testing on data of all users (ALL case). 

The following R code performs what we have described.

```r
library(caret)
library(rpart)
library(dplyr)
library(doMC)

# Set seed value
set.seed(53234)

# Train and test for each user, or all users if username is AllUsers
usertraintest = function(username, method) {
	# Filter by user name
	print(paste0('Train and test user: ', username, ', method: ', method))
	if (username!='AllUsers') {
		trfilter = tr[tr$user_name==username,]
		tefilter = te[te$user_name==username,]
	}
	else {
		trfilter = tr
		tefilter = te
	}

	index = createDataPartition(trfilter$classe, p=0.6, list=FALSE)

	# Remove X, user_name, date, cvtd_timestamp, and new_window 
	# (all of them are factor variables)
	tr1 = trfilter[index, -c(1,2,5,6)]
	te1 = trfilter[-index, -c(1,2,5,6)]
	te2 = tefilter[, -c(1,2,5,6)]

	# Use multicore feature
	registerDoMC(cores=40)	# We run this project on a 40-core machine
	# Set train control option: Using 10-fold cross-validation, repeat 10 times
	tc = trainControl(method="repeatedcv", number=10, repeats=10, allowParallel=TRUE)
	# Train model on TR.train
	t = system.time(fit <- train(classe ~ ., data=tr1, method=method, trControl=tc))
	# Report time for training and testing
	print(paste0('Elapse time = ', format(t['elapsed'], digits=2), 's'))
	# Print out model for AllUsers only, otherwise the report will be too long
	if (username=='AllUsers') print(fit)
	# Test model in TR.test
	res1 = predict(fit, te1[,-56])
	m1 = confusionMatrix(te1$classe, res1)
	acc = format(m1$overall['Accuracy'], digits=3)
	ci95 = paste0('(', format(m1$overall['AccuracyLower'], digits=3), ',',
		format(m1$overall['AccuracyUpper'], digits=3),')')
	print(paste0('Accuracy: ', acc, ', 95% CI: ', ci95))

	# Predict on TE
	res2 = predict(fit, te2)
	# Return results
	data.frame(problem_id=te2$problem_id, answer_filter=res2)
}

# Train, test and predict
mypredict = function (method) {
	print(paste0("----- Applying ", method, " method -----"))
	users = as.character(unique(tr$user_name))
	results = NULL
	sapply(users, function(u) {
		results <<- rbind(results, usertraintest(u, method))
	})
	results = arrange(results, problem_id)

	resall = usertraintest('AllUsers', method)
	resall = arrange(resall, problem_id)
	results = cbind(results, answer_all=resall$answer)
	rm(resall)
	data.frame(results)
}

t1 = system.time(res1 <- mypredict('rpart'))
```

```
## [1] "----- Applying rpart method -----"
## [1] "Train and test user: carlitos, method: rpart"
## [1] "Elapse time = 2.8s"
## [1] "Accuracy: 0.685, 95% CI: (0.658,0.71)"
## [1] "Train and test user: pedro, method: rpart"
## [1] "Elapse time = 2.7s"
## [1] "Accuracy: 0.989, 95% CI: (0.981,0.995)"
## [1] "Train and test user: adelmo, method: rpart"
## [1] "Elapse time = 3s"
## [1] "Accuracy: 0.687, 95% CI: (0.663,0.71)"
## [1] "Train and test user: charles, method: rpart"
## [1] "Elapse time = 3s"
## [1] "Accuracy: 0.664, 95% CI: (0.639,0.689)"
## [1] "Train and test user: eurico, method: rpart"
## [1] "Elapse time = 2.9s"
## [1] "Accuracy: 0.66, 95% CI: (0.632,0.686)"
## [1] "Train and test user: jeremy, method: rpart"
## [1] "Elapse time = 3s"
## [1] "Accuracy: 0.697, 95% CI: (0.672,0.721)"
## [1] "Train and test user: AllUsers, method: rpart"
## [1] "Elapse time = 12s"
## CART 
## 
## 11776 samples
##    55 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 10598, 10599, 10598, 10598, 10599, 10598, ... 
## Resampling results across tuning parameters:
## 
##   cp          Accuracy   Kappa       Accuracy SD  Kappa SD  
##   0.03921452  0.5467842  0.41830298  0.04738216   0.07344077
##   0.05546986  0.4530352  0.26794763  0.06970768   0.11393153
##   0.11271951  0.3230059  0.05908317  0.03829321   0.05841242
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.03921452. 
## [1] "Accuracy: 0.525, 95% CI: (0.514,0.536)"
```

```r
t2 = system.time(res2 <- mypredict('C5.0'))
```

```
## [1] "----- Applying C5.0 method -----"
## [1] "Train and test user: carlitos, method: C5.0"
## [1] "Elapse time = 34s"
## [1] "Accuracy: 0.997, 95% CI: (0.992,0.999)"
## [1] "Train and test user: pedro, method: C5.0"
## [1] "Elapse time = 19s"
## [1] "Accuracy: 0.998, 95% CI: (0.993,1)"
## [1] "Train and test user: adelmo, method: C5.0"
## [1] "Elapse time = 13s"
## [1] "Accuracy: 0.999, 95% CI: (0.996,1)"
## [1] "Train and test user: charles, method: C5.0"
## [1] "Elapse time = 30s"
## [1] "Accuracy: 1, 95% CI: (0.997,1)"
## [1] "Train and test user: eurico, method: C5.0"
## [1] "Elapse time = 25s"
## [1] "Accuracy: 1, 95% CI: (0.997,1)"
## [1] "Train and test user: jeremy, method: C5.0"
## [1] "Elapse time = 17s"
## [1] "Accuracy: 1, 95% CI: (0.997,1)"
## [1] "Train and test user: AllUsers, method: C5.0"
## [1] "Elapse time = 621s"
## C5.0 
## 
## 11776 samples
##    55 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 10601, 10599, 10598, 10598, 10597, 10598, ... 
## Resampling results across tuning parameters:
## 
##   model  winnow  trials  Accuracy   Kappa      Accuracy SD  Kappa SD   
##   rules  FALSE    1      0.9878989  0.9846940  0.004164138  0.005265967
##   rules  FALSE   10      0.9973930  0.9967024  0.001579486  0.001998000
##   rules  FALSE   20      0.9982676  0.9978088  0.001224558  0.001549028
##   rules   TRUE    1      0.9880439  0.9848778  0.003841605  0.004858834
##   rules   TRUE   10      0.9975119  0.9968528  0.001570355  0.001986370
##   rules   TRUE   20      0.9981913  0.9977122  0.001269577  0.001605972
##   tree   FALSE    1      0.9816914  0.9768438  0.004728425  0.005978736
##   tree   FALSE   10      0.9962976  0.9953167  0.001929613  0.002441103
##   tree   FALSE   20      0.9973846  0.9966917  0.001604267  0.002029470
##   tree    TRUE    1      0.9819549  0.9771767  0.004743423  0.005998144
##   tree    TRUE   10      0.9964420  0.9954994  0.001772394  0.002242084
##   tree    TRUE   20      0.9974184  0.9967345  0.001443030  0.001825348
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were trials = 20, model = rules
##  and winnow = FALSE. 
## [1] "Accuracy: 0.999, 95% CI: (0.998,0.999)"
```

```r
res = merge(res1, res2, by.x='problem_id', by.y='problem_id', all=T)
t3 = system.time(res3 <- mypredict('rf'))
```

```
## [1] "----- Applying rf method -----"
## [1] "Train and test user: carlitos, method: rf"
## [1] "Elapse time = 46s"
## [1] "Accuracy: 0.998, 95% CI: (0.994,1)"
## [1] "Train and test user: pedro, method: rf"
## [1] "Elapse time = 35s"
## [1] "Accuracy: 1, 95% CI: (0.996,1)"
## [1] "Train and test user: adelmo, method: rf"
## [1] "Elapse time = 53s"
## [1] "Accuracy: 1, 95% CI: (0.998,1)"
## [1] "Train and test user: charles, method: rf"
## [1] "Elapse time = 48s"
## [1] "Accuracy: 0.999, 95% CI: (0.996,1)"
## [1] "Train and test user: eurico, method: rf"
## [1] "Elapse time = 41s"
## [1] "Accuracy: 0.999, 95% CI: (0.995,1)"
## [1] "Train and test user: jeremy, method: rf"
## [1] "Elapse time = 54s"
## [1] "Accuracy: 1, 95% CI: (0.997,1)"
## [1] "Train and test user: AllUsers, method: rf"
## [1] "Elapse time = 621s"
## Random Forest 
## 
## 11776 samples
##    55 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 10598, 10599, 10598, 10599, 10598, 10599, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD   Kappa SD   
##    2    0.9955418  0.9943606  0.0022913987  0.002898995
##   28    0.9988960  0.9986036  0.0009309518  0.001177563
##   55    0.9954400  0.9942317  0.0021130786  0.002673410
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 28. 
## [1] "Accuracy: 0.999, 95% CI: (0.998,0.999)"
```

```r
res = merge(res, res3, by.x='problem_id', by.y='problem_id', all=T)
names(res) = c('problem_id', 'rpart_filter', 'rpart_all',
	'C5.0_filter', 'C5.0_all', 'rf_filter', 'rf_all')
rm(res1);rm(res2);rm(res3)
res
```

```
##    problem_id rpart_filter rpart_all C5.0_filter C5.0_all rf_filter rf_all
## 1           1            B         C           B        B         B      B
## 2           2            A         A           A        A         A      A
## 3           3            C         C           B        B         B      B
## 4           4            A         A           A        A         A      A
## 5           5            A         A           A        A         A      A
## 6           6            E         C           E        E         E      E
## 7           7            C         C           D        D         D      D
## 8           8            C         A           B        B         B      B
## 9           9            A         A           A        A         A      A
## 10         10            A         A           A        A         A      A
## 11         11            B         C           B        B         B      B
## 12         12            C         C           C        C         C      C
## 13         13            B         C           B        B         B      B
## 14         14            A         A           A        A         A      A
## 15         15            E         C           E        E         E      E
## 16         16            D         A           E        E         E      E
## 17         17            A         A           A        A         A      A
## 18         18            B         A           B        B         B      B
## 19         19            B         A           B        B         B      B
## 20         20            B         C           B        B         B      B
```

## Conclusion and prediction results
We see that the CART (rpart) method is worst for accuracy when testing on
TR.test data set for FILTER and ALL scenarios.

The C5.0 and Random Forests methods have very similar accuracy and much better than
that of CART method, on the TR.test data set for both FILTER and ALL scenarios.
However the C5.0 total running time is lower than that of Random Forest
1.2 times.

On the prediction results, the C5.0 and Random Forests
give the same prediction for both FILTER and ALL scenarios. Therefore we can conclude that
in this project the C5.0 is the most appropriate method for accuracy and performance. We
use C5.0 prediction results (column C5.0_filter) for submission.


```r
# Submission code
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(res$C5.0_filter)
```
