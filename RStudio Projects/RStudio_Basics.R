firstName<-c("Ethan", "Captain", "John","Selina");
lastName<-c("Hunt","John","Wick","Kyle");
sex<-c("MALE","MALE","MALE","FEMALE");
score<-c(97, 88, 85, 92);
stu_df<-data.frame(firstName, lastName, sex, score)
stu_df
#square brackets are for direct operations on a data frame
#adding a new column to the existing data frame
stu_df$age<-c(38, 50, 45, 24)
stu_df<-stu_df[1:4, c("sex", "score", "age")]
stu_df
#for certain columns subset(moody_df, SCORE>50)
# tapply 