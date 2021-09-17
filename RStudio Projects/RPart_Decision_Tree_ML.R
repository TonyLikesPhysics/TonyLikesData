install.packages("rpart")
install.packages("rpart.plot")

install.packages("devtools")
devtools::install_github("devanshagr/CrossValidation")

library(rpart)
library(rpart.plot)

moody_train <-read.csv("M2021train.csv",stringsAsFactors = FALSE)
moody_submit <- read.csv("M2021test-submission-file.csv",stringsAsFactors = FALSE)

# Model 1
tree1 <- rpart(Grade~Score+Major+Seniority+Texting+Questions+Attendance, 
              control = rpart.control(minbucket = 250),data = moody_train)
rpart.plot(tree1)
CrossValidation::cross_validate(moody_train, tree1, 2, 0.8)
#  accuracy_subset accuracy_all
#1       0.8230322    0.8378236
#2       0.8225040    0.8330692


# Model 2
tree2 <- rpart(Grade~Score+Major+Seniority+Texting+Questions+Attendance, 
               control = rpart.control(minsplit = 200, minbucket = 100),
               data = moody_train)
rpart.plot(tree2)
CrossValidation::cross_validate(moody_train, tree2, 2, 0.8)
#  accuracy_subset accuracy_all
#1       0.8320127    0.8320127
#2       0.8166931    0.8166931


# Model 3
tree3 <- rpart(Grade~Score+Major+Seniority+Texting+Questions+Attendance, 
               control = rpart.control(minsplit = 100, minbucket = 200),
               data = moody_train)
rpart.plot(tree3)
CrossValidation::cross_validate(moody_train, tree3, 2, 0.8)
#  accuracy_subset accuracy_all
#1       0.8341257    0.8547279
#2       0.8658214    0.8652932

#Model 4
tree4 <- rpart(Grade~Score+Major+Seniority+Texting+Questions+Attendance, 
               control = rpart.control(minsplit = 100, minbucket = 250),
               data = moody_train)
rpart.plot(tree4)
CrossValidation::cross_validate(moody_train, tree4, 2, 0.8)
#  accuracy_subset accuracy_all
#1       0.8082409    0.8198627
#2       0.8225040    0.8367670

# Model 3
tree5 <- rpart(Grade~Score+Major+Seniority+Texting+Questions+Attendance, 
               control = rpart.control(minsplit = 50, minbucket = 200),
               data = moody_train)
rpart.plot(tree5)
CrossValidation::cross_validate(moody_train, tree5, 2, 0.8)
#  accuracy_subset accuracy_all
#1       0.8182779    0.8420497
#2       0.8378236    0.8600106

# Model 6
tree6 <- rpart(Grade~Score+Major+Seniority+Texting+Questions+Attendance, 
               control = rpart.control(minsplit = 200, minbucket = 200),
               data = moody_train)
rpart.plot(tree6)
CrossValidation::cross_validate(moody_train, tree6, 2, 0.8)
#  accuracy_subset accuracy_all
#1       0.8325409    0.8515584
#2       0.8166931    0.8351823

# Model 7
tree7 <- rpart(Grade~Score+Major+Seniority+Texting+Questions+Attendance, 
               control = rpart.control(minsplit = 200, minbucket = 170, 
                                       maxdepth = 30),
               data = moody_train)
rpart.plot(tree7)
CrossValidation::cross_validate(moody_train, tree7, 5, 0.8)
#  accuracy_subset accuracy_all
#1       0.8489171    0.8483888
#2       0.8304279    0.8304279
#3       0.8637084    0.8637084
#4       0.8304279    0.8304279
#5       0.8293714    0.8288431

decision_ML <-predict(tree7, newdata = test, type = "class")
moody_submit$Grade <-decision_ML
write.csv(moody_submit, "asilva_moody_submit.csv", row.names = FALSE)

# Model 7
tree8 <- rpart(Grade~Score+Major+Seniority+Questions, 
               control = rpart.control(minsplit = 300, minbucket = 100, 
                                       maxdepth = 30),
               data = moody_train)
rpart.plot(tree8)
CrossValidation::cross_validate(moody_train, tree8, 5, 0.8)