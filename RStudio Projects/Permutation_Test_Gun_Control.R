install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
gun_data<-read.csv("Gun_Control.csv")
# I created another dataframe for the permuteation test 
# so I could compare how the function might modify the base dataframe
gun_data_mod<-read.csv("Gun_Control.csv")

# Create the data frames
strict_gun_laws.data <-subset(gun_data, gun_data$Gun_Laws =="Strict_Gun_Laws")
medium_gun_laws.data <-subset(gun_data, gun_data$Gun_Laws =="Medium_Gun_Laws")
loose_gun_laws.data <-subset(gun_data, gun_data$Gun_Laws =="Loose_Gun_Laws")

# Count the rows
nrow(gun_data)
nrow(strict_gun_laws.data)
nrow(medium_gun_laws.data)
nrow(loose_gun_laws.data)

# Monetary damages variables
strict_gun_laws.cost <- strict_gun_laws.data$Monetary_Damage
medium_gun_laws.cost <- medium_gun_laws.data$Monetary_Damage
loose_gun_laws.cost <- loose_gun_laws.data$Monetary_Damage

# Calculate the averages of monetary damages
mean.strict_gun_laws <- mean(strict_gun_laws.cost)
mean.medium_gun_laws<- mean(medium_gun_laws.cost)
mean.loose_gun_laws <- mean(loose_gun_laws.cost)

# Print the mean values
mean.strict_gun_laws
mean.medium_gun_laws
mean.loose_gun_laws

# Conduct permutation tests: Null Hypothesis: The average monetary damage of 
# robberies in places with strict gun laws is the same as the average monetary 
# damage of robberies in places with loose gun laws.

# Strict, Medium
PermutationTestSecond::Permutation(gun_data_mod,
                                   "Gun_Laws", "Monetary_Damage", 10000,
                                   "Strict_Gun_Laws", "Medium_Gun_Laws")
# Medium, Loose
PermutationTestSecond::Permutation(gun_data_mod,
                                   "Gun_Laws", "Monetary_Damage", 10000,
                                   "Medium_Gun_Laws","Loose_Gun_Laws")
# Strict, Loose
PermutationTestSecond::Permutation(gun_data_mod,
                                   "Gun_Laws", "Monetary_Damage", 10000,
                                   "Strict_Gun_Laws", "Loose_Gun_Laws")

# Data frame for barplot robbery cost < $1000
robbery_df<-gun_data[gun_data$Monetary_Damage<1000,]
# Barplot # of robberies by gun control laws 
barplot(table(robbery_df$Gun_Laws, useNA = "always"),
                          names.arg=c("Medium","Strict","Loose"),
                          main = "Number of Robberies with Damages Below $1000",
                          xlab ="Gun Control Level", 
                          ylab = "Number of Robberies",
                          ylim = c(0,3500),
                          col= c("royalblue2", "navyblue","skyblue1"))



