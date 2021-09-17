install.packages("Metrics")
install.packages("lmvar")
install.packages("devtools")

library(Metrics)
library(lmvar)

#*********************** Data ***********************#
earnings_train <- read.csv("Earnings_Train2021.csv")
earnings_test <- read.csv("Earnings_Test_Students.csv")
earnings_submit<- read.csv("earning_submission.csv")

random_train <-earnings_train[sample(nrow(earnings_train)),]

train_set <-random_train[1:8000,]
test_set  <-random_train[8001:9995,]



#*********************** Plots **********************#
plot(earnings_data$GPA,earnings_data$Earnings,
     main = "Earnings vs. GPA",
     ylab ="Earnings", 
     xlab = "GPA",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(earnings_data$Height,earnings_data$Earnings,
     main = "Earnings vs. Height",
     ylab ="Earnings", 
     xlab = "Height",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(earnings_data$Number_Of_Professional_Connections,earnings_data$Earnings,
     main = "Earnings vs. Professional Connections",
     ylab ="Earnings", 
     xlab = "Professional Connections",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(earnings_data$Number_Of_Parking_Tickets,earnings_data$Earnings,
     main = "Earnings vs. Parking Tickets",
     ylab ="Earnings", 
     xlab = "Parking Tickets",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(earnings_data$Graduation_Year,earnings_data$Earnings,
     main = "Earnings vs. Graduation Year",
     ylab ="Earnings", 
     xlab = "Graduation Year",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(earnings_data$Number_Of_Credits,earnings_data$Earnings,
     main = "Earnings vs. Credits",
     ylab ="Earnings", 
     xlab = "Credits",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(earnings_data$GPA,earnings_data$Earnings,
     main = "Earnings vs. GPA",
     ylab ="Earnings", 
     xlab = "GPA",
     pch=1, cex=0.75,col=c("steelblue4"))

#******************* Simple Fit Test ********************#
simple.fit<-lm(Earnings~GPA,data = train_set,x=TRUE, y=TRUE)
predicted_earnings.simple <- predict(simple.fit,test_set)
mse(predicted_earnings.simple, test_set$Earnings)
summary(simple.fit)
plot(train_set$GPA,train_set$Earnings,
     main = "Earnings vs. GPA with Simple Fit ",
     ylab ="Earnings", 
     xlab = "GPA")
abline(simple.fit,col="red")

#******************* Multiple Fit Test ********************#
multi.fit <-lm(Earnings~Major * GPA * Number_Of_Professional_Connections,
                                        data = train_set, x=TRUE, y=TRUE)
predicted_earnings.multi <-predict(multi.fit,test_set)
mse(predicted_earnings.multi,test_set$Earnings)
summary(multi.fit)

#mse = 119532.9
#p-value: < 2.2e-16

#******************** Cross Validation ********************#
cv.lm(multi.fit, m=5)

#Mean absolute error        :  76.20457 
#Sample standard deviation  :  7.202027 
#Mean squared error         :  53069.38 
#Sample standard deviation  :  45547.43 
#Root mean squared error    :  218.1455 
#Sample standard deviation  :  78.04489 


#**************** Write Model to Submission File *****************#
earnings_test$Earnings <- predict(multi.fit, earnings_test)
earnings_submit$Earnings <- earnings_test$Earnings
write.csv(earnings_submit, "asilva_earnings_submit.csv", row.names = FALSE)


#******************* Multiple Fit Test 2 ********************#
multi.fit2 <-lm(Earnings~Major * GPA * Number_Of_Professional_Connections,
               data = train_set, x=TRUE, y=TRUE)
predicted_earnings.multi2 <-predict(multi.fit2,test_set)
mse(predicted_earnings.multi2,test_set$Earnings)
summary(multi.fit2)
#******************** Cross Validation ********************#
cv.lm(multi.fit2, m=5)