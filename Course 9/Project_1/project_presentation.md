Project: Shiny Application and Reproducible Pitch
========================================================
author: Ailyn
date: 22/12/2017
autosize: true

Background
========================================================

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. 

In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

Goal
========================================================

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.


```r
    # - Load Data
    if(!exists("input_training")){
        input_training <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),
                                   na.strings=c("NA","#DIV/0!",""))
        input_testing <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),
                                  na.strings=c("NA","#DIV/0!",""))
        
    }
    table(input_training$classe)
```

```

   A    B    C    D    E 
5580 3797 3422 3216 3607 
```

Predict Test Cases
========================================================

1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 

B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 

Levels: A B C D E


More information: https://github.com/ailynb/datasciencecoursera/tree/master/Course%209/Project_1

Prediction Project: https://github.com/ailynb/datasciencecoursera/tree/master/Course%208/Project_1

========================================================
