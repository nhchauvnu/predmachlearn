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
	# Print out model
	print(fit)
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
## CART 
## 
## 1869 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1679, 1683, 1683, 1681, 1684, 1683, ... 
## Resampling results across tuning parameters:
## 
##   cp         Accuracy   Kappa      Accuracy SD  Kappa SD 
##   0.2156433  0.7508785  0.6817215  0.07893147   0.1027053
##   0.2668129  0.5874792  0.4654168  0.09819991   0.1316113
##   0.3019006  0.3884358  0.1823839  0.10986725   0.1658200
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.2156433. 
## [1] "Accuracy: 0.685, 95% CI: (0.658,0.71)"
## [1] "Train and test user: pedro, method: rpart"
## [1] "Elapse time = 2.7s"
## CART 
## 
## 1568 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1410, 1411, 1411, 1412, 1411, 1413, ... 
## Resampling results across tuning parameters:
## 
##   cp           Accuracy   Kappa      Accuracy SD  Kappa SD  
##   0.006756757  0.9815697  0.9768858  0.008823799  0.01106669
##   0.238597973  0.7401504  0.6732190  0.153918501  0.19365060
##   0.250844595  0.5319614  0.3995104  0.162508207  0.22570965
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.006756757. 
## [1] "Accuracy: 0.989, 95% CI: (0.981,0.995)"
## [1] "Train and test user: adelmo, method: rpart"
## [1] "Elapse time = 2.9s"
## CART 
## 
## 2336 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 2103, 2101, 2102, 2102, 2103, 2103, ... 
## Resampling results across tuning parameters:
## 
##   cp         Accuracy   Kappa      Accuracy SD  Kappa SD 
##   0.2486255  0.7569716  0.6865007  0.08458927   0.1098581
##   0.2730605  0.5671288  0.4389521  0.09198423   0.1203022
##   0.2828345  0.4031575  0.1831774  0.09850499   0.1733872
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.2486255. 
## [1] "Accuracy: 0.687, 95% CI: (0.663,0.71)"
## [1] "Train and test user: charles, method: rpart"
## [1] "Elapse time = 2.8s"
## CART 
## 
## 2124 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1911, 1912, 1912, 1911, 1912, 1910, ... 
## Resampling results across tuning parameters:
## 
##   cp         Accuracy   Kappa      Accuracy SD  Kappa SD 
##   0.2424242  0.7575389  0.6915652  0.09061928   0.1163591
##   0.2664141  0.5604512  0.4369779  0.09865881   0.1281323
##   0.2821970  0.3355000  0.1213479  0.10225924   0.1525295
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.2424242. 
## [1] "Accuracy: 0.664, 95% CI: (0.639,0.689)"
## [1] "Train and test user: eurico, method: rpart"
## [1] "Elapse time = 2.8s"
## CART 
## 
## 1845 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1660, 1661, 1659, 1661, 1661, 1660, ... 
## Resampling results across tuning parameters:
## 
##   cp         Accuracy   Kappa      Accuracy SD  Kappa SD 
##   0.2443439  0.7445941  0.6747014  0.08824225   0.1126419
##   0.2639517  0.5741498  0.4564074  0.09411240   0.1209288
##   0.2654600  0.3866137  0.1807218  0.09407122   0.1610208
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.2443439. 
## [1] "Accuracy: 0.66, 95% CI: (0.632,0.686)"
## [1] "Train and test user: jeremy, method: rpart"
## [1] "Elapse time = 2.9s"
## CART 
## 
## 2045 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1841, 1842, 1840, 1840, 1840, 1841, ... 
## Resampling results across tuning parameters:
## 
##   cp         Accuracy   Kappa      Accuracy SD  Kappa SD  
##   0.2324365  0.7693950  0.6971842  0.07524166   0.09977617
##   0.2526158  0.6042563  0.4771288  0.08151139   0.10902279
##   0.2899851  0.4203978  0.1523653  0.09146991   0.18755531
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.2324365. 
## [1] "Accuracy: 0.697, 95% CI: (0.672,0.721)"
## [1] "Train and test user: AllUsers, method: rpart"
## [1] "Elapse time = 11s"
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
## C5.0 
## 
## 1869 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1683, 1681, 1681, 1682, 1683, 1683, ... 
## Resampling results across tuning parameters:
## 
##   model  winnow  trials  Accuracy   Kappa      Accuracy SD  Kappa SD   
##   rules  FALSE    1      0.9945482  0.9931092  0.006654273  0.008409649
##   rules  FALSE   10      0.9981303  0.9976367  0.003668777  0.004637816
##   rules  FALSE   20      0.9983980  0.9979751  0.003437186  0.004345117
##   rules   TRUE    1      0.9964159  0.9954706  0.005209036  0.006578199
##   rules   TRUE   10      0.9985555  0.9981742  0.002930361  0.003704108
##   rules   TRUE   20      0.9988784  0.9985824  0.002661790  0.003364411
##   tree   FALSE    1      0.9929956  0.9911467  0.006854394  0.008662962
##   tree   FALSE   10      0.9978063  0.9972264  0.003958215  0.005005105
##   tree   FALSE   20      0.9981809  0.9977005  0.003499598  0.004423890
##   tree    TRUE    1      0.9942751  0.9927631  0.006503176  0.008218931
##   tree    TRUE   10      0.9985012  0.9981054  0.002854098  0.003607699
##   tree    TRUE   20      0.9989299  0.9986473  0.002520730  0.003186177
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were trials = 20, model = tree
##  and winnow = TRUE. 
## [1] "Accuracy: 0.997, 95% CI: (0.992,0.999)"
## [1] "Train and test user: pedro, method: C5.0"
## [1] "Elapse time = 19s"
## C5.0 
## 
## 1568 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1410, 1412, 1411, 1411, 1410, 1412, ... 
## Resampling results across tuning parameters:
## 
##   model  winnow  trials  Accuracy   Kappa      Accuracy SD  Kappa SD   
##   rules  FALSE    1      0.9975771  0.9969619  0.004508942  0.005653668
##   rules  FALSE   10      0.9989147  0.9986392  0.002576630  0.003230686
##   rules  FALSE   20      0.9988506  0.9985587  0.002628572  0.003295974
##   rules   TRUE    1      0.9977723  0.9972065  0.004273698  0.005358776
##   rules   TRUE   10      0.9988535  0.9985624  0.003310111  0.004150074
##   rules   TRUE   20      0.9988535  0.9985624  0.003310111  0.004150074
##   tree   FALSE    1      0.9959851  0.9949657  0.005907579  0.007406676
##   tree   FALSE   10      0.9982782  0.9978411  0.003374243  0.004230535
##   tree   FALSE   20      0.9982798  0.9978431  0.003372008  0.004227720
##   tree    TRUE    1      0.9973280  0.9966496  0.005038681  0.006316715
##   tree    TRUE   10      0.9983451  0.9979248  0.004103443  0.005145076
##   tree    TRUE   20      0.9983451  0.9979247  0.004002331  0.005018386
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were trials = 10, model = rules
##  and winnow = FALSE. 
## [1] "Accuracy: 0.998, 95% CI: (0.993,1)"
## [1] "Train and test user: adelmo, method: C5.0"
## [1] "Elapse time = 13s"
## C5.0 
## 
## 2336 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 2103, 2102, 2102, 2102, 2104, 2102, ... 
## Resampling results across tuning parameters:
## 
##   model  winnow  trials  Accuracy   Kappa      Accuracy SD  Kappa SD   
##   rules  FALSE    1      0.9992298  0.9990191  0.001959433  0.002495657
##   rules  FALSE   10      0.9992298  0.9990192  0.001959433  0.002495495
##   rules  FALSE   20      0.9992726  0.9990736  0.001830466  0.002331277
##   rules   TRUE    1      0.9984597  0.9980380  0.002887543  0.003678582
##   rules   TRUE   10      0.9990164  0.9987471  0.002180384  0.002777676
##   rules   TRUE   20      0.9989734  0.9986924  0.002202950  0.002806425
##   tree   FALSE    1      0.9992298  0.9990191  0.001959433  0.002495657
##   tree   FALSE   10      0.9992726  0.9990736  0.001830466  0.002331277
##   tree   FALSE   20      0.9992726  0.9990736  0.001830466  0.002331277
##   tree    TRUE    1      0.9984597  0.9980380  0.002887543  0.003678582
##   tree    TRUE   10      0.9991873  0.9989648  0.001989323  0.002534260
##   tree    TRUE   20      0.9992730  0.9990740  0.001829746  0.002330408
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were trials = 20, model = tree
##  and winnow = TRUE. 
## [1] "Accuracy: 0.999, 95% CI: (0.996,1)"
## [1] "Train and test user: charles, method: C5.0"
## [1] "Elapse time = 31s"
## C5.0 
## 
## 2124 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1912, 1912, 1911, 1912, 1911, 1912, ... 
## Resampling results across tuning parameters:
## 
##   model  winnow  trials  Accuracy   Kappa      Accuracy SD  Kappa SD   
##   rules  FALSE    1      0.9937844  0.9921756  0.005428230  0.006832355
##   rules  FALSE   10      0.9972214  0.9965019  0.003673991  0.004625511
##   rules  FALSE   20      0.9973631  0.9966805  0.003370201  0.004242653
##   rules   TRUE    1      0.9948205  0.9934801  0.004361760  0.005490147
##   rules   TRUE   10      0.9973624  0.9966796  0.003162202  0.003981054
##   rules   TRUE   20      0.9977400  0.9971550  0.003100579  0.003903554
##   tree   FALSE    1      0.9929845  0.9911695  0.006037237  0.007597155
##   tree   FALSE   10      0.9968925  0.9960881  0.004082906  0.005139869
##   tree   FALSE   20      0.9976924  0.9970950  0.003245058  0.004085164
##   tree    TRUE    1      0.9936900  0.9920584  0.005364789  0.006747760
##   tree    TRUE   10      0.9972220  0.9965032  0.003416458  0.004300577
##   tree    TRUE   20      0.9975040  0.9968580  0.003239090  0.004077387
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were trials = 20, model = rules
##  and winnow = TRUE. 
## [1] "Accuracy: 1, 95% CI: (0.997,1)"
## [1] "Train and test user: eurico, method: C5.0"
## [1] "Elapse time = 26s"
## C5.0 
## 
## 1845 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1661, 1661, 1660, 1660, 1660, 1660, ... 
## Resampling results across tuning parameters:
## 
##   model  winnow  trials  Accuracy   Kappa      Accuracy SD  Kappa SD   
##   rules  FALSE    1      0.9960989  0.9950683  0.005110131  0.006459586
##   rules  FALSE   10      0.9976145  0.9969849  0.004247893  0.005369417
##   rules  FALSE   20      0.9982635  0.9978051  0.003078353  0.003890546
##   rules   TRUE    1      0.9968542  0.9960223  0.004767451  0.006029069
##   rules   TRUE   10      0.9983742  0.9979451  0.003810509  0.004817027
##   rules   TRUE   20      0.9984829  0.9980824  0.003615928  0.004571285
##   tree   FALSE    1      0.9960989  0.9950683  0.005110131  0.006459586
##   tree   FALSE   10      0.9977221  0.9971204  0.003709958  0.004689885
##   tree   FALSE   20      0.9981013  0.9976001  0.003645296  0.004607231
##   tree    TRUE    1      0.9967996  0.9959531  0.004762344  0.006022638
##   tree    TRUE   10      0.9988064  0.9984910  0.003137538  0.003967065
##   tree    TRUE   20      0.9988607  0.9985598  0.002703025  0.003417686
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were trials = 20, model = tree
##  and winnow = TRUE. 
## [1] "Accuracy: 1, 95% CI: (0.997,1)"
## [1] "Train and test user: jeremy, method: C5.0"
## [1] "Elapse time = 17s"
## C5.0 
## 
## 2045 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1841, 1840, 1842, 1841, 1842, 1839, ... 
## Resampling results across tuning parameters:
## 
##   model  winnow  trials  Accuracy   Kappa      Accuracy SD  Kappa SD   
##   rules  FALSE    1      0.9977969  0.9971446  0.003063637  0.003971455
##   rules  FALSE   10      0.9988732  0.9985401  0.002496152  0.003234338
##   rules  FALSE   20      0.9988732  0.9985401  0.002496152  0.003234338
##   rules   TRUE    1      0.9977969  0.9971446  0.003063637  0.003971455
##   rules   TRUE   10      0.9988245  0.9984768  0.002521446  0.003267195
##   rules   TRUE   20      0.9988245  0.9984768  0.002521446  0.003267195
##   tree   FALSE    1      0.9983350  0.9978427  0.002627194  0.003404350
##   tree   FALSE   10      0.9992157  0.9989846  0.002058692  0.002665352
##   tree   FALSE   20      0.9992644  0.9990478  0.002018105  0.002612738
##   tree    TRUE    1      0.9983350  0.9978427  0.002627194  0.003404350
##   tree    TRUE   10      0.9992647  0.9990480  0.002017607  0.002612460
##   tree    TRUE   20      0.9992647  0.9990480  0.002017607  0.002612460
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were trials = 10, model = tree
##  and winnow = TRUE. 
## [1] "Accuracy: 1, 95% CI: (0.997,1)"
## [1] "Train and test user: AllUsers, method: C5.0"
## [1] "Elapse time = 579s"
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
## Random Forest 
## 
## 1869 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1682, 1681, 1682, 1683, 1682, 1681, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9967339  0.9958716  0.004498099  0.005686069
##   28    0.9980199  0.9974973  0.003196244  0.004040244
##   55    0.9975932  0.9969587  0.003348771  0.004231647
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 28. 
## [1] "Accuracy: 0.998, 95% CI: (0.994,1)"
## [1] "Train and test user: pedro, method: rf"
## [1] "Elapse time = 39s"
## Random Forest 
## 
## 1568 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1412, 1411, 1410, 1412, 1410, 1412, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9966184  0.9957583  0.004200105  0.005268957
##   28    1.0000000  1.0000000  0.000000000  0.000000000
##   55    0.9989788  0.9987197  0.002826467  0.003543497
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 28. 
## [1] "Accuracy: 1, 95% CI: (0.996,1)"
## [1] "Train and test user: adelmo, method: rf"
## [1] "Elapse time = 57s"
## Random Forest 
## 
## 2336 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 2103, 2102, 2102, 2103, 2102, 2103, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9955073  0.9942760  0.004609687  0.005874334
##   28    0.9979036  0.9973291  0.002752966  0.003507369
##   55    0.9990587  0.9988005  0.002068814  0.002636642
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 55. 
## [1] "Accuracy: 1, 95% CI: (0.998,1)"
## [1] "Train and test user: charles, method: rf"
## [1] "Elapse time = 49s"
## Random Forest 
## 
## 2124 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1913, 1911, 1912, 1911, 1911, 1912, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9963754  0.9954376  0.004061458  0.005111948
##   28    0.9975981  0.9969766  0.003313100  0.004170177
##   55    0.9977389  0.9971539  0.003378337  0.004252277
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 55. 
## [1] "Accuracy: 1, 95% CI: (0.997,1)"
## [1] "Train and test user: eurico, method: rf"
## [1] "Elapse time = 43s"
## Random Forest 
## 
## 1845 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1661, 1660, 1659, 1661, 1660, 1661, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9990244  0.9987668  0.002598890  0.003283714
##   28    0.9988076  0.9984924  0.002731277  0.003452920
##   55    0.9978851  0.9973258  0.004213026  0.005327537
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2. 
## [1] "Accuracy: 0.998, 95% CI: (0.994,1)"
## [1] "Train and test user: jeremy, method: rf"
## [1] "Elapse time = 47s"
## Random Forest 
## 
## 2045 samples
##   55 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 1839, 1841, 1841, 1842, 1839, 1840, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9964794  0.9954396  0.003685670  0.004774424
##   28    0.9994130  0.9992392  0.001743070  0.002260027
##   55    0.9984837  0.9980352  0.003005609  0.003894880
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 28. 
## [1] "Accuracy: 0.998, 95% CI: (0.994,1)"
## [1] "Train and test user: AllUsers, method: rf"
## [1] "Elapse time = 649s"
## Random Forest 
## 
## 11776 samples
##    55 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 10600, 10598, 10599, 10599, 10597, 10599, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9953807  0.9941567  0.001882120  0.002380992
##   28    0.9984121  0.9979915  0.001204786  0.001523935
##   55    0.9967732  0.9959183  0.001777666  0.002248829
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 28. 
## [1] "Accuracy: 0.999, 95% CI: (0.999,1)"
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
1.3 times.

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
