install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
minimarket_data<-read.csv("HomeworkMarket.csv")


minimarket_data_princeton <- minimarket_data[minimarket_data$Location == "Princeton",]

# Data frames for general comparison of popcorn and cola
with_popcorn <- minimarket_data[minimarket_data$SoftDrinks=="Cola",]
with_cola <- minimarket_data[minimarket_data$Snacks=="Popcorn",]

# Data frames for comparison of popcorn and cola specific to Princeton location
with_popcorn_princeton <- minimarket_data_princeton[minimarket_data$SoftDrinks=="Cola",]
with_cola_princeton <- minimarket_data_princeton[minimarket_data$Snacks=="Popcorn",]


mosaicplot(minimarket_data$Snacks~minimarket_data$SoftDrinks, 
           main="Minimarket Snacks Sold with Softdrinks",
           xlab="Snacks", 
           ylab="Soft Drinks",
           col= c( "royalblue3","lightblue3"))

mosaicplot(minimarket_data_princeton$Snacks~minimarket_data_princeton$SoftDrinks, 
           main="Princeton Minimarket Snacks Sold with Softdrinks",
           xlab="Snacks", 
           ylab="Soft Drinks",
           col= c("purple2","hotpink2"))

#********* The Princeton Popcorn/Cola Test ***********#
minimarket_data$slice  <- 0
minimarket_data$bar    <- 0
minimarket_data[minimarket_data$Snacks =="Popcorn",]$bar <-1
minimarket_data[minimarket_data$SoftDrinks=="Cola" 
                & minimarket_data$Location == "Princeton",]$slice <-1
PermutationTestSecond::Permutation(minimarket_data, "slice", "bar", 100000, "0", "1")


#********* The General Popcorn/Cola Test ***********#
minimarket_data$slice  <- 0
minimarket_data$bar    <- 0
minimarket_data[minimarket_data$Snacks =="Popcorn",]$bar <-1
minimarket_data[minimarket_data$SoftDrinks=="Cola",]$slice <-1
PermutationTestSecond::Permutation(minimarket_data, "slice", "bar", 100000, "0", "1")



#********* The General Cola/Popcorn Test ***********#
minimarket_data$slice  <- 0
minimarket_data$bar    <- 0
minimarket_data[minimarket_data$SoftDrinks=="Cola",]$bar <-1
minimarket_data[minimarket_data$Snacks=="Popcorn",]$slice <-1
PermutationTestSecond::Permutation(minimarket_data, "slice", "bar", 100000, "0", "1")

