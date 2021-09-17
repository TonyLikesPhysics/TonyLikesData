shooting_data<-read.csv("fatal-police-shootings-data-3.csv")

black_data<-shooting_data[shooting_data$race=="B"
                          & shooting_data$age != 0,]
white_data<-shooting_data[shooting_data$race=="W" 
                          & shooting_data$age != 0,]

# num of rows black_data = 1439
# num of rows white_data = 2751
nrow(black_data)
nrow(white_data)

black_data.age<-black_data$age
white_data.age<-white_data$age

#sd.black.age = 11.46521
#sd.white.age = 13.29735
sd.black.age <- sd(black_data.age)
sd.white.age <- sd(white_data.age)

#mean.black.age = 32.69215
#mean.white.age = 39.99709
mean.black.age <- mean(black_data.age)
mean.white.age <- mean(white_data.age)


length.black <- length(black_data.age)
length.white <- length(white_data.age)


#sd.black.white = 0.3944915
sd.black.white <- sqrt((sd.black.age^2)/length.black 
                     + (sd.white.age^2)/length.white)

# zeta = 18.51737
zeta <- (mean.white.age - mean.black.age)/sd.black.white

# p-value = 0
p_value = 1-pnorm(zeta)


barplot(tapply(shooting_data$age, shooting_data$race, mean),
        names.arg=c("Unknown","Asian","Black", "Hispanic", 
                    "Native \n American","Other","White"),
        main = "Average Age of Persons Shot by Police Since 2015",
        xlab ="Reported Race", 
        ylab = "Average Age",
        ylim = c(0,40),
        col= c("lightblue3", "skyblue2", "hotpink",
               "steelblue3", "royalblue3", "navyblue", "plum"))
