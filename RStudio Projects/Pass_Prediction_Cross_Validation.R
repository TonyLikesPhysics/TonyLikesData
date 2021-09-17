train_data <-read.csv("M2021train.csv",stringsAsFactors = TRUE)
test_data <- read.csv("M2021test-students.csv",stringsAsFactors = TRUE)
sample_submit <- read.csv("M2021test-submission-file.csv",stringsAsFactors = TRUE)
sample_submit2 <- read.csv("M2021test-submission-file.csv",stringsAsFactors = TRUE)

boxplot(train_data$Score~train_data$Grade,
        main="Student Score Distribution by Grade",
        xlab="Student Grade", 
        ylab = "Student Score",
        col= c("purple1", "royalblue1"))

mosaicplot(train_data$Questions~train_data$Grade, 
           main="Grade by Question Habits",
           xlab="Asks Questions", 
           ylab="Grade",
           col= c( "skyblue1", "royalblue1"))

mosaicplot(train_data$Texting~train_data$Grade, 
           main="Grade by Texting Habits",
           xlab="Texts During Class", 
           ylab="Grade",
           col= c( "violetred3", "violetred4"))

mosaicplot(train_data$Seniority~train_data$Grade, 
           main="Grade by Seniority",
           xlab="College Year", 
           ylab="Grade",
           col= c( "purple1", "purple4"))

mosaicplot(train_data$Major~train_data$Grade, 
           main="Grade by Major",
           xlab="Major", 
           ylab="Grade",
           col= c( "hotpink1", "hotpink3"))

mosaicplot(train_data$Major~train_data$Seniority, 
           main="Grade by Major",
           xlab="Major", 
           ylab="Seniority",
           col= c( "pink1", "pink2","pink3","pink4"))

plot(train_data$Score,train_data$Attendance,
           main = "Score vs. Attendance",
           xlab ="Student Score", 
           ylab = "Attendance",
           pch=1, cex=0.75,col=c("steelblue4"))

barplot(tapply(train_data$Score, train_data$Grade, mean),
          main = "Average Student Score by Grade",
          xlab ="Grade", 
          ylab = "Average Score",
          ylim = c(0,100),
          col= c("lightblue1", "steelblue1"))

barplot(tapply(train_data$Score, train_data$Major, mean),
          main = "Average Student Score by Major",
          xlab ="Major", 
          ylab = "Average Score",
          ylim = c(0,100),
          col= c("lightblue1", "steelblue1"))

barplot(tapply(train_data$Attendance, train_data$Major, mean),
          main = "Average Attendance by Major",
          xlab ="Major", 
          ylab = "Average Attendance",
          ylim = c(0,100),
          col= c("lightblue1", "steelblue1"))

barplot(tapply(train_data$Attendance, train_data$Seniority, mean),
        main = "Average Attendance by Seniority",
        xlab ="College Year", 
        ylab = "Average Attendance",
        ylim = c(0,100),
        col= c("lightblue1", "steelblue1"))

barplot(tapply(train_data$Score, train_data$Seniority, mean),
        main = "Average Student Score by College Year",
        xlab = "College Year", 
        ylab = "Average Score",
        ylim = c(0,100),
        col= c("lightblue1", "steelblue1"))

mosaicplot(train_data$Major~train_data$Texting, 
           main="Texting Habits by Major",
           xlab="Major", 
           ylab="Texting",
           col= c( "hotpink1", "hotpink3"))

mosaicplot(train_data$Seniority~train_data$Texting, 
           main="Texting Habits by Seniority",
           xlab="College Year", 
           ylab="Texting",
           col= c( "hotpink2", "hotpink4"))

mosaicplot(train_data$Major~train_data$Questions, 
           main="Question Habits by Major",
           xlab="Major", 
           ylab="Ask Questions",
           col= c( "purple1", "purple4"))

mosaicplot(train_data$Seniority~train_data$Questions, 
           main="Question Habits by Seniority",
           xlab="College Year", 
           ylab="Ask Questions",
           col= c( "purple1", "purple4"))

mosaicplot(Cs$Seniority~Cs$Questions, 
           main="CS Major Questions by Seniority",
           xlab="College Year", 
           ylab="Ask Questions",
           col= c( "purple1", "purple4"))

mosaicplot(Cs$Seniority~Cs$Texting, 
           main="CS Major Texting by Seniority",
           xlab="College Year", 
           ylab="Texting",
           col= c( "hotpink1", "hotpink3"))

plot(Cs$Score,Cs$Attendance,
     main = "CS Major Score vs. Attendance",
     xlab ="Student Score", 
     ylab = "Attendance",
     pch=1, cex=0.75,col=c("steelblue4"))

barplot(tapply(Cs$Score, Cs$Grade, mean),
        main = "Average CS Student Score by Grade",
        xlab ="Grade", 
        ylab = "Average Score",
        ylim = c(0,100),
        col= c("lightblue1", "steelblue1"))

barplot(table(Cs$Grade),
        main = "Number of CS Students by Grade",
        xlab ="Grade", 
        ylab = "Number of Students",
        #ylim = c(0,100),
        col= c("lightblue1", "steelblue1"))

boxplot(Cs$Score~Cs$Grade,
        main="CS Student Score Distribution by Grade",
        xlab="Student Grade", 
        ylab = "Student Score",
        col= c("purple1", "royalblue1"))

high_fail <- train_data[train_data$Grade=="Fail" & train_data$Score>80,]
fail <- train_data[train_data$Grade=="Fail",]
Cs <- train_data[train_data$Major=="Cs",]


summary(fail)
summary(high_fail)
summary(train_data)
summary(train_data[train_data$Grade=="Fail",])
summary(train_data[train_data$Major=="Cs",])
summary(train_data[train_data$Score < 30,])


#************************** Model One *******************************#
train <-train_data[sample(1:nrow(train_data)),]
training <- train[1:1000,]
testing<-train[2001:3000,]

my_prediction <- training
decision <- rep('Pass',nrow(my_prediction))
decision[my_prediction$Major=="Cs" & my_prediction$Score<20]<-"Fail"
decision[my_prediction$Seniority=="Senior" &my_prediction$Questions=="Rarely"
         & my_prediction$Attendance==0]<-"Fail"
decision[ my_prediction$Questions=="Rarely" & my_prediction$Attendance==0]<-"Fail"
#decision[my_prediction$Questions=="Rarely"& my_prediction$Texting=="Always"
        # &my_prediction$Score<50]<-"Fail"
decision[my_prediction$Major=="Communication" & my_prediction$Score<40]<-"Fail"
decision[my_prediction$Questions=="Always"& my_prediction$Score>50]<-"Pass"
#decision[my_prediction$Texting=="Rarely"& my_prediction$Questions=="Always"
        # & my_prediction$Score>51]<-"Pass"
decision[my_prediction$Texting=="Always"& my_prediction$Score<50]<-"Fail"
my_prediction$Grade<-as.factor(decision)
error1<-mean(train_data$Grade != my_prediction$Grade)
error1
cross_valid_error1<-mean(testing$Grade != my_prediction$Grade)
cross_valid_error1
#********************************************************************#


#************************** Model Two *******************************#
#*This was the model I used to submit to Kaggle.
train2 <-train_data[sample(1:nrow(train_data)),]
training2 <- train2[1:2000,]
testing2<-train2[2001:4000,]

my_prediction2 <- training2
decision2 <- rep('Pass',nrow(my_prediction2))
decision2[my_prediction2$Major=="Cs" & my_prediction2$Score<50]<-"Fail"
decision2[my_prediction2$Seniority=="Senior"& my_prediction2$Attendance==0]<-"Fail"
decision2[my_prediction2$Questions=="Rarely" & my_prediction2$Attendance==0]<-"Fail"
decision2[my_prediction2$Major=="Communication" & my_prediction2$Score<40]<-"Fail"
decision2[my_prediction2$Questions=="Rarely"& my_prediction2$Score<50]<-"Fail"
decision2[my_prediction2$Questions=="Always"& my_prediction2$Score>50]<-"Pass"
decision2[my_prediction2$Texting=="Rarely"& my_prediction2$Score>51]<-"Pass"
my_prediction2$Grade<-as.factor(decision2)
error2<-mean(training2$Grade != my_prediction2$Grade)
error2
cross_valid_error2<-mean(testing2$Grade != my_prediction2$Grade)
cross_valid_error2
#**********************************************************************#


#************************** Model Three *******************************#
#* This was the model I used by mistake in the first submission. It was for an
#* attempt at finding where over-fitting might be happening in other models.
train3 <-train_data[sample(1:nrow(train_data)),]
training3 <- train3[1:2000,]
testing3<-train3[2001:4000,]
my_prediction3 <- training3

decision3 <- rep('Pass',nrow(my_prediction3))
decision3[my_prediction3$Major=="Cs" 
          & my_prediction3$Score<20 
          &my_prediction3$Texting=="Always"]<-"Fail" 
decision3[my_prediction3$Seniority=="Senior"
          &my_prediction3$Questions=="Rarely"
         & my_prediction3$Attendance==0]<-"Fail"
decision3[my_prediction3$Texting=="Rarely"
         & my_prediction3$Questions=="Always"
         & my_prediction3$Score>50]<-"Pass"

my_prediction3$Grade<-as.factor(decision3)
error3<-mean(training3$Grade != my_prediction3$Grade)
error3
cross_valid_error3<-mean(testing3$Grade != my_prediction3$Grade)
cross_valid_error3

cross_valid_error3-error3
#**********************************************************************#

# First Incorrect Submission using wrong model
test <-test_data
decision5 <- rep('Pass',nrow(test))
decision5[test$Major=="Cs" 
          & test$Score<20 
          &test$Texting=="Always"]<-"Fail" 
decision5[test$Seniority=="Senior"
          &test$Questions=="Rarely"
          & test$Attendance==0]<-"Fail"
decision5[test$Texting=="Rarely"
          & test$Questions=="Always"
          & test$Score>50]<-"Pass"
test$Grade<-as.factor(decision5)
sample_submit$Grade = test$Grade
summary(sample_submit)
write.csv(sample_submit, file = "a_silva_submission.csv", row.names = FALSE)


# *** Corrected submission using model I thought was my optimal one ***#
#I lost the optimal one :(
test2 <-test_data

decision_2 <- rep('Pass',nrow(test2))
decision_2[test2$Major=="Cs" & test2$Score<50]<-"Fail"
decision_2[test2$Seniority=="Senior"& test2$Attendance==0]<-"Fail"
decision_2[test2$Questions=="Rarely" & test2$Attendance==0]<-"Fail"
decision_2[test2$Major=="Communication" & test2$Score<40]<-"Fail"
decision_2[test2$Questions=="Rarely"& test2$Score<50]<-"Fail"
decision_2[test2$Questions=="Always"& test2$Score>50]<-"Pass"
decision_2[test2$Texting=="Rarely"& test2$Score>51]<-"Pass"
test2$Grade<-as.factor(decision_2)

test2$Grade<-as.factor(decision_2)
sample_submit2$Grade = test2$Grade

summary(sample_submit2)

write.csv(sample_submit2, file = "asilva_submission.csv", row.names = FALSE)

