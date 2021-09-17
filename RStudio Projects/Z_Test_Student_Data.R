student_data<-read.csv("moody2020b.csv")

# Creating the data frames
ask_always.data<-subset(student_data, student_data$questions=="always")
ask_never.data<-subset(student_data,student_data$questions=="never")

# Get number of students = 836
nrow(student_data)

# Scores for question values always and never 
ask_always.score <- ask_always.data$score
ask_never.score <- ask_never.data$score
ask_always.score

# Standard deviation of the scores for question values always and never
# sd.ask_always = 25.9949
# sd.ask_never = 25.68241
sd.ask_always <- sd(ask_always.score)
sd.ask_never <- sd(ask_never.score)


# mean of the scores for question values always and never
# mean.ask_always = 51.08277
# mean.ask_never = 56.32474
mean.ask_always <- mean(ask_always.score)
mean.ask_never <- mean(ask_never.score)


# Getting number of rows for normalization
# length.ask_always = 296
# length.ask_never = 287
length.ask_always <- length(ask_always.score)
length.ask_never <- length(ask_never.score)


# Checking that length() counts the rows
nrow(ask_always.data)

# Getting standard deviation of the means
# sd.questions = 2.14035
sd.questions <- sqrt((sd.ask_always^2)/length.ask_always 
                     + (sd.ask_never^2)/length.ask_never)


# z-score 
# zeta = 2.449118
zeta <- (mean.ask_never - mean.ask_always)/sd.questions


# Getting the one sided p-value: 
# p_value = 0.007160334
p_value = 1-pnorm(zeta)
p_value

