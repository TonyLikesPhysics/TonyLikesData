student_data<-read.csv("moody2020b.csv")

boxplot(student_data$score~student_data$grade,
        main="Student Score Distribution by Grade",
        xlab="Student Letter Grade", 
        ylab = "Student Score",
        col= c("lightblue1", "skyblue1", "steelblue1", "royalblue1", "navyblue"))

student_f<-student_data[student_data$grade=="F",]
student_a<-student_data[student_data$grade=="A",]
student_b<-student_data[student_data$grade=="B",]
student_c<-student_data[student_data$grade=="C",]
student_d<-student_data[student_data$grade=="D",]


# Dataframe varied to plot with differing conditions
student_optimal<-student_data[student_data$texting=="always" 
                           & student_data$questions=="never",]

mosaicplot(student_data$questions~student_data$grade, 
           main="Frequency of Student Questions by Grade",
           xlab="Student Letter Grade", 
           ylab="Question Frequency",
           col= c("lightblue1", "skyblue1", "steelblue2", "royalblue1", "navyblue"))


plot(student_data)
nrow(student_data)
               
# The barplot function varied for producing all the average score plots with 
# differing conditions
barplot(tapply(student_data$score, student_data$grade, mean),
        main = "Average Student Score",
        xlab ="Letter Grade", 
        ylab = "Average Score",
        ylim = c(0,100),
        col= c("lightblue1", "skyblue1", "steelblue1", "royalblue1", "navyblue"))

barplot(table(student_data$grade), 
        main = "Overall Class Grade Distribution",
        xlab ="Letter Grade", 
        ylab = "Number of Students",
        col= c("lightblue1", "skyblue1", "steelblue1", "royalblue1", "navyblue"))

plot(student_data$score,student_data$participation,
     main = "Participation vs. Score",
     xlab ="Student Score", 
     ylab = "Participation Index",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(student_a$score,student_a$participation,
     main = "Participation vs. Score: Grade A",
     xlab ="Student Score", 
     ylab = "Participation Index",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(student_b$score,student_b$participation,
     main = "Participation vs. Score: Grade B",
     xlab ="Student Score", 
     ylab = "Participation Index",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(student_c$score,student_c$participation,
     main = "Participation vs. Score: Grade C",
     xlab ="Student Score", 
     ylab = "Participation Index",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(student_d$score,student_d$participation,
     main = "Participation vs. Score: Grade D",
     xlab ="Student Score", 
     ylab = "Participation Index",
     pch=1, cex=0.75,col=c("steelblue4"))

plot(student_f$score,student_f$participation,
     main = "Participation vs. Score: Grade F",
     xlab ="Student Score", 
     ylab = "Participation Index",
     pch=1, cex=0.75,col=c("steelblue4"))