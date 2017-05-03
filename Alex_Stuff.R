setwd("/Users/alexandrafunk/Desktop/")

library(ggplot2)
library(fBasics)

dat0 <- read.csv("Team5_data.csv")

#summary(dat0$DEP_TIME_BLK)

dat <- dat0[ lapply(dat0, function(x) sum(is.na(x)) / length(x) ) < 0.3 ]

####################################################################################

#str(dat)

#dat$AIRLINE_ID = as.character(dat$AIRLINE_ID)

ggplot(dat,aes(x=dat$UNIQUE_CARRIER)) + geom_bar() + labs(x="UNIQUE_CARRIER")

#####################################
#nums only
nums = sapply(dat, is.numeric)
dat2 = dat[,nums]
names(dat2)

basicStats(dat2)[c("Mean", "Stdev", "Median", "Minimum", "Maximum", "nobs", "NAs"),]

#print boxplots for the 60 numeric variables in dataset
par(mfrow=c(2,5))
for (i in 1:length(dat2)) {
  boxplot(dat2[,i], main=names(dat2[i]), type="l")
  hist(dat2[,i], main=names(dat2[i]), type="l")
}

###########################################
#Variable transformations
ggplot(dat,aes(x=dat$ARR_DEL15)) + geom_bar() + labs(x="ARR DELAY")
ggplot(dat,aes(x=dat$DEP_DEL15)) + geom_bar() + labs(x="DEP DELAY")

#dummy variables for unique carrier
unique = factor(dat$UNIQUE_CARRIER)
d_unique = model.matrix(~unique)
df_unique = data.frame(d_unique)

#dummy variables for month
month = factor(dat$MONTH)
d_month = model.matrix(~month)
df_month = data.frame(d_month)

#dummy variables for day of week
day = factor(dat$DAY_OF_WEEK)
d_day = model.matrix(~day)
df_day = data.frame(d_day)

#dummy variables for day of week
org = factor(dat$ORIGIN_AIRPORT_ID)
d_org = model.matrix(~org)
df_org = data.frame(d_org)

#dummy variables for day of week
des = factor(dat$DEST_AIRPORT_ID)
d_des = model.matrix(~des)
df_des = data.frame(d_des)

df_dummies = merge(df_unique, df_month, by=0)
df_dummies1 = merge(df_dummies, df_day, by=0)
df_dummies2 = merge(df_dummies1, df_org, by=0)
df_dummies3 = merge(df_dummies2, df_des, by=0)

write.csv(df_dummies3, file = "dummy.csv")

#########START HERE FOR MODELING#########################
d_dat <- read.csv("dummy.csv")

arr_del15 = dat$ARR_DEL15
d_dat$ARR_DEL15 = arr_del15

## 75% of the sample size
smp_size <- floor(0.75 * nrow(d_dat))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(d_dat)), size = smp_size)

train <- d_dat[train_ind, ]
test <- d_dat[-train_ind, ]

model1 = glm(ARR_DEL15 ~.,family = binomial(link="logit"), data=train)
summary(model1)

#step(model1,direction = "both", steps = 100, k = 2)

model2 = glm(ARR_DEL15 ~uniqueAS+uniqueDL+uniqueUA+uniqueWN+month3+month4+month6+month9+month10+month11+month12+day3+
             day4+des10434+des10581+des10821+des11278+des11823+des12094+des12191+des12339+des13342+des13577+des13851+
             des14307+des14683+des14696,family = binomial(link="logit"), data=train)

summary(model2)

step(model2,direction = "both", steps = 100, k = 2)
