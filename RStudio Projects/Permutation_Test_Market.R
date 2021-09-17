install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
market_data<-read.csv("Minimarket.csv")
market_cat_data<-read.csv("Minimarket_cat.csv")

# Data frames for comparison of items sold
with_bread <- market_data[market_data$BREAD==1,]
with_butter <- market_data[market_data$BUTTER==1,]
with_coffee <- market_data[market_data$COFFEE==1,]
with_tea <- market_data[market_data$TEA==1,]
with_cookies <- market_data[market_data$COOKIES==1,]

# Cutting self-comparison
with_bread$BREAD <-NULL
with_butter$BUTTER <-NULL
with_coffee$COFFEE <-NULL
with_tea$TEA <-NULL
with_cookies$COOKIES <-NULL

# ********** Bar graphs for comparing frequency of items sold **********
barplot(colSums(with_bread, na.rm=FALSE),
        names.arg=c("Butter","Cookies","Coffee","Tea"),
        main = "Frequency of Items Sold with Bread",
        xlab ="Merchendise", 
        ylab = "Sales",
        ylim = c(2000,2600),
        xpd = FALSE,
        border = c("white"),
        col= c("violetred2"))

barplot(colSums(with_butter, na.rm=FALSE),
        names.arg=c("Bread","Cookies","Coffee","Tea"),
        main = "Frequency of Items Sold with Butter",
        xlab ="Merchendise", 
        ylab = "Sales",
        ylim = c(2000,2600),
        xpd = FALSE,
        border = c("white"),
        col= c("turquoise3"))

barplot(colSums(with_coffee, na.rm=FALSE),
        names.arg=c("Bread","Butter","Cookies","Tea"),
        main = "Frequency of Items Sold with Coffee",
        xlab ="Merchendise", 
        ylab = "Sales",
        ylim = c(2000,2600),
        xpd = FALSE,
        border = c("white"),
        col= c("purple2"))

barplot(colSums(with_cookies, na.rm=FALSE),
        names.arg=c("Bread","Butter","Coffee","Tea"),
        main = "Frequency of Items Sold with Cookies",
        xlab ="Merchendise", 
        ylab = "Sales",
        ylim = c(2000,2600),
        xpd = FALSE,
        border = c("white"),
        col= c("chartreuse3"))

barplot(colSums(with_tea, na.rm=FALSE),
        names.arg=c("Bread","Butter","Cookies","Coffee"),
        main = "Frequency of Items Sold with Tea",
        xlab ="Merchendise", 
        ylab = "Sales",
        ylim = c(2000,2600),
        xpd = FALSE,
        border = c("white"),
        col= c("tomato2"))


# *************** The bread/butter tests ***************
bread_with_butter <- subset(market_data, market_data$BREAD == 1)
bread_no_butter <- subset(market_data, market_data$BREAD == 0)
mean.bread_with_butter = mean(bread_with_butter$BUTTER)
mean.bread_no_butter = mean(bread_no_butter$BUTTER)
mean.bread_with_butter # mean: 0.5085331
mean.bread_no_butter   # mean: 0.4927445
PermutationTestSecond::Permutation(market_data, "BREAD", "BUTTER", 10000, 0, 1)
# p-value: 0.0591
butter_with_bread <- subset(market_data, market_data$BUTTER == 1)
butter_no_bread <- subset(market_data, market_data$BUTTER == 0)
mean.butter_with_bread = mean(butter_with_bread$BREAD)
mean.butter_no_bread = mean(butter_no_bread$BREAD)
mean.butter_with_bread # mean: 0.5165085
mean.butter_no_bread   # mean: 0.5007245
PermutationTestSecond::Permutation(market_data, "BUTTER", "BREAD", 10000, 0, 1)
# p-value: 0.054


# *************** The tea/cookies tests *****************
tea_with_cookies <- subset(market_data, market_data$TEA == 1)
tea_no_cookies <- subset(market_data, market_data$TEA == 0)
mean.tea_with_cookies = mean(tea_with_cookies$COOKIES)
mean.tea_no_cookies = mean(tea_no_cookies$COOKIES)
mean.tea_with_cookies # mean: 0.5132762
mean.tea_no_cookies   # mean: 0.4879444
PermutationTestSecond::Permutation(market_data, "TEA", "COOKIES", 10000, 0, 1)
# p-value: 0.0063
cookies_with_tea <- subset(market_data, market_data$COOKIES == 1)
cookies_no_tea <- subset(market_data, market_data$COOKIES == 0)
mean.cookies_with_tea = mean(cookies_with_tea$TEA)
mean.cookies_no_tea = mean(cookies_no_tea$TEA)
mean.cookies_with_tea # mean: 0.5069172
mean.cookies_no_tea   # mean: 0.4815887
PermutationTestSecond::Permutation(market_data, "COOKIES", "TEA", 10000, 0, 1)
# p-value: 0.0055


# *************** The butter/cookies tests ***************
butter_with_cookies <- subset(market_data, market_data$BUTTER == 1)
butter_no_cookies <- subset(market_data, market_data$BUTTER == 0)
mean.butter_with_cookies = mean(butter_with_cookies$COOKIES)
mean.butter_no_cookies = mean(butter_no_cookies$COOKIES)
mean.butter_with_cookies # mean: 0.5068097
mean.butter_no_cookies   # mean: 0.4941006
PermutationTestSecond::Permutation(market_data, "BUTTER", "COOKIES", 10000, 0, 1)
# p-value: 0.102
cookies_with_butter <- subset(market_data, market_data$COOKIES == 1)
cookies_no_butter <- subset(market_data, market_data$COOKIES == 0)
mean.cookies_with_butter = mean(cookies_with_butter$BUTTER)
mean.cookies_no_butter = mean(cookies_no_butter$BUTTER)
mean.cookies_with_butter # mean: 0.5071237
mean.cookies_no_butter   # mean: 0.4944146
PermutationTestSecond::Permutation(market_data, "COOKIES", "BUTTER", 10000, 0, 1)
# p-value: 0.0987


# *************** The coffee/bread tests *****************
coffee_with_bread <- subset(market_data, market_data$COFFEE == 1)
coffee_no_bread <- subset(market_data, market_data$COFFEE == 0)
mean.coffee_with_bread = mean(coffee_with_bread$BREAD)
mean.coffee_no_bread = mean(coffee_no_bread$BREAD)
mean.coffee_with_bread # mean: 0.5103061
mean.coffee_no_bread  # mean: 0.5069758
PermutationTestSecond::Permutation(market_data, "COFFEE", "BREAD", 10000, 0, 1)
# p-value: 0.3639
bread_with_coffee <- subset(market_data, market_data$BREAD == 1)
bread_no_coffee<- subset(market_data, market_data$BREAD == 0)
mean.bread_with_coffee = mean(bread_with_coffee$COFFEE)
mean.bread_no_coffee = mean(bread_no_coffee$COFFEE)
mean.bread_with_coffee # mean: 0.4979683
mean.bread_no_coffee  #mean:  0.4946372
PermutationTestSecond::Permutation(market_data, "BREAD", "COFFEE", 10000, 0, 1)
# p-value: 0.369

