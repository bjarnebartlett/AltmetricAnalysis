################################################
################################################
########## MODELLING ALTMETRIC SCORES ##########
################################################
################################################

# 2019.10.23 Regression of altmetric scores and predictor variables

###############
### SUMMARY ###
###############

# We are looking at altmetric scores of publications in 7 idiotypic journals
# And looking at the gender of the authors of those publications
# To see if there are any interesting patterns in terms of altmetrics & gender bias


##############
### SET UP ###
##############

setwd("/Users/juliefortin/Documents/UBC/Projects/Altmetrics/AltmetricAnalysis/")

# install.packages("data.table")
# install.packages("lme4")
# install.packages("sjPlot")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("visreg")
library(data.table)
library(lme4)
library(sjPlot)
library(ggplot2)
library(tidyverse)
library(visreg)


#################
### LOAD DATA ###
#################

genderizedfiles <- list.files("./Data100319/")
genderizedfiles <- genderizedfiles[which(substr(genderizedfiles,nchar(genderizedfiles)-3,nchar(genderizedfiles))==".tsv")]

mergeddata <- c()
for (file in genderizedfiles) {
  filename <- paste("./Data100319/",file,sep="")
  journaldata <- fread(filename)
  mergeddata <- rbind(mergeddata, journaldata)
}

mergeddata$pubmonth <- as.factor(mergeddata$pubmonth)
mergeddata$firstgender <- as.factor(mergeddata$firstgender)
mergeddata$lastgender <- as.factor(mergeddata$lastgender)
mergeddata$type <- as.factor(mergeddata$type)
mergeddata$`5day` <- as.numeric(mergeddata$`5day`)
mergeddata$`1week` <- as.numeric(mergeddata$`1week`)
mergeddata$`1month` <- as.numeric(mergeddata$`1month`)
mergeddata$`1week` <- as.numeric(mergeddata$`1week`)
mergeddata$`1year` <- as.numeric(mergeddata$`1year`)

# Change "None" or NAs to 0
mergeddata[,9:20][mergeddata[,9:20]=="None"] <- 0
mergeddata[,9:20][is.na(mergeddata[,9:20])] <- 0


###################
### EXPLORATION ###
###################

# Altmetric scores
hist(mergeddata$`5day`, col="#cbd5e8")
hist(mergeddata$`1year`, col="#cbd5e8")
summary(mergeddata$`5day`)
summary(mergeddata$`1year`)
hist(mergeddata[`5day`<25]$`5day`, col="#cbd5e8", main="Histogram of articles with 5day score <25")
hist(mergeddata[`1year`<100]$`1year`, col="#cbd5e8", main="Histogram of articles with 1year score <100")

# Article timing
boxplot(`1year`~pubmonth, data=mergeddata, main="Month of publication")
hist(mergeddata$pubyear, col="#cbd5e8")

# Author genders
barplot(table(mergeddata$firstgender), col="#cbd5e8", main="First author gender")
barplot(table(mergeddata$lastgender), col="#cbd5e8", main="Last author gender")
slices <- c(mean(mergeddata$propmales),mean(mergeddata$propfemales),mean(mergeddata$propunknown))
pie(slices, labels=c("Male","Female","Unknown"), main="Author list gender balance")
plot(mergeddata$propmales, mergeddata$propfemales) # lots that are at 0 because format was nofirstname, or because of unknown authors

# Number of authors
hist(mergeddata$numauthors, col="#cbd5e8")
hist(mergeddata[numauthors<25]$numauthors, col="#cbd5e8", main="Histogram of articles with <25 authors")
summary(mergeddata$numauthors)

# Type of article
barplot(table(mergeddata$type), col="#cbd5e8", las=2, main="Type of article")

# Journal
pie(table(mergeddata$journal), labels=c("bioRxiv","Cell","Nature","NEJM","PLoS ONE","PNAS","Science"), main="Journal")


#################
### DATA PREP ###
#################

data <- mergeddata[-which(authors_format=="nofirstname"),]

# melt altmetric scores
nonscore_cols <- c("id","altmetric_id","authors","journal","type","doi",
                   "date","pubyear","pubmonth",
                   "males","females","unknown","propmales","propfemales","propunknown","numauthors",
                   "firstgender","firstgenderprob","lastgender","lastgenderprob",
                   "authors_format","cleaned_authors")
score_cols <- c("5day","1year")
meltdata <- melt(data, id.vars=nonscore_cols, measure.vars=score_cols, variable.name="scoretime", value.name="score")
head(meltdata)

# melt gender
nongender_cols <- c("id","altmetric_id","authors","journal","type","doi",
                    "date","pubyear","pubmonth",
                    "males","females","unknown","propmales","propfemales","propunknown","numauthors",
                    "authors_format","cleaned_authors",
                    "scoretime","score")
gender_cols <- c("firstgender","lastgender")
meltdata <- melt(meltdata, id.vars=nongender_cols, measure.vars=gender_cols)
meltdata$gender <- paste(substr(meltdata$variable,1,1),substr(meltdata$value,1,1),sep="")
meltdata$variable <- NULL
meltdata$value <- NULL
head(meltdata)

# abbreviate names
meltdata[journal=="Proceedings of the National Academy of Sciences of the United States of America"]$journal <- "PNAS"
meltdata[journal=="New England Journal of Medicine"]$journal <- "NEJM"
unique(meltdata$journal)

# change class
meltdata$journal <- as.factor(meltdata$journal)
meltdata$gender <- as.factor(meltdata$gender)

# for articles with only 1 author, treat as first author, but don't duplicate as last author
toremove <- which(meltdata$numauthors==1 & (meltdata$gender=="l"|meltdata$gender=="lf"|meltdata$gender=="lm"))
meltdata <- meltdata[-toremove,]


#################
### MODELLING ###
#################

##### poisson glm with all interactions #####

poissondata1 <- meltdata

# m_poisson1 <- glm(score ~  pubyear*gender*scoretime*journal +
#                       propfemales +
#                       numauthors +
#                       pubmonth,
#              data=poissondata1,
#              family=poisson)
# saveRDS(m_poisson1,"./DataAnalysis/AltmetricScoreModeling/m_poisson1.RDS")
m_poisson1 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_poisson1.RDS")
summary(m_poisson1)

predicted <- predict(m_poisson1,type="response")
plot(predicted, meltdata$score-predicted)
plot(meltdata$score-predicted)

# But the poisson doesn't behave super well when there are too many 0s
# which is definitely the case here
# So one recommended way of dealing with this is splitting it up to model 0s and other counts separately
# http://www.stat.cmu.edu/~brian/463-663/week04/checking-poissonness.pdf

# make some plots
plot_model(m_poisson1,type="pred",terms=c("scoretime","journal","gender","pubyear"))
plot_model(m_poisson1,type="pred",terms=c("scoretime","journal","gender"))
plot_model(m_poisson1,type="pred",terms=c("propfemales"))
plot_model(m_poisson1,type="pred",terms=c("numauthors"))
plot_model(m_poisson1,type="pred",terms=c("pubmonth"))
plot_model(m_poisson1,type="pred",terms=c("gender"))
plot_model(m_poisson1,type="pred",terms=c("pubyear"))
plot_model(m_poisson1,type="pred",terms=c("scoretime"))
plot_model(m_poisson1,type="pred",terms=c("journal"))
plot_model(m_poisson1,type="pred",terms=c("gender","journal"))

##### logistic1 split data into 0 and non-zero #####

# On second thought, should split up because so many 0s
# First, do logistic on those without a score and those with a score
logisticdata <- meltdata
logisticdata[score>0]$score <- 1

# m_logist1 <- glm(score ~  pubyear*gender*scoretime*journal +
#                             propfemales +
#                             numauthors +
#                             pubmonth,
#                           data=logisticdata,
#                           family=binomial)
# saveRDS(m_logist1,"./DataAnalysis/AltmetricScoreModeling/m_logist1.RDS")
m_logist1 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist1.RDS")
summary(m_logist1)

##### logistic2 split data into 0 and non-zero, using only 1-year scores #####

# There aren't enough 5-day scores > 0, so let's just work with 1-year scores
logisticdata2 <- meltdata[scoretime=="1year"]
logisticdata2[score>0]$score <- 1

# m_logist2 <- glm(score ~  pubyear*gender*journal +
#                             propfemales +
#                             numauthors +
#                             pubmonth,
#                           data=logisticdata2,
#                           family=binomial)
# saveRDS(m_logist2,"./DataAnalysis/AltmetricScoreModeling/m_logist2.RDS")
m_logist2 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist2.RDS")
summary(m_logist2)

# logistic regression has assumptions
# 1. outcome is binary
# 2. relationship between logit of outcome and each of predictors is linear
# 3. no influential values in continuous predictors
# 4. no high correlations (multicollinearity) between predictors
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

# 1. is outcome binary?
unique(logisticdata2$score)
# yes, just 0 or 1

# 2. is logit x predictors linear? (could check by scatter plot or box Tidwell test)
probabilities <- predict(m_logist2, type="response")
logit <- log(probabilities/(1-probabilities))
ggplot(logisticdata2, aes(logit,pubyear)) +
  geom_point() +
  theme_bw()
# pretty linear, no need to transform (not running with smooth loess because takes FOREVER to run with this many points)
# also this is an int, not a numeric, so it's fine
ggplot(logisticdata2, aes(logit,propfemales)) +
  geom_point() +
  theme_bw()
# not really linear...  i don't know what to make of this...
ggplot(logisticdata2, aes(logit,numauthors)) +
  geom_point() +
  theme_bw()
# kind of odd but pretty linear
# this is also an int, not numeric

# actually, zia said the way they go about doing this on the sthda website is wrong
# it's not the predictor vs the logit that matters, what matters is the partial residuals
# that is, you need to take into account the effect of everything else on the response, 
# so can't just do predictor x logit(response)

visreg(m_logist2, "propfemales", type="conditional")
# looks linear so there's no point in transforming the propfemales variable
# also this is the only one that is actually continuous so only one that we need to check

# 3. are there influential values in continuous predictors?
plot(m_logist2, 4)
# cook's distance shows outliers
plot(m_logist2, 5)
# lots of points with high leverage, and lots of points with high std residuals, but none outside cook's distance lines

# 4. is there high correlation (multicollinearity) between predictors?
# need to look only at continuous vars, so can't use car::vif(m_logist1)
m_logist2_contvars <- data.frame(logisticdata2$pubyear,logisticdata2$numauthors,logisticdata2$propfemales)
head(m_logist2_contvars)
dim(m_logist2_contvars)
faraway::vif(m_logist2_contvars)
# values are close to 1, which means the predictors are not correlated


##### lm1 on 1-year scores, with all interaction terms #####

# Next, do linear regression on those with a score
lineardata <- meltdata[scoretime=="1year" & score>0]
lineardata$logscore <- log(lineardata$score)

# m_linear1 <- lm(logscore ~ pubyear*gender*journal +
#                              propfemales +
#                              numauthors +
#                              pubmonth,
#                            data=lineardata)
# saveRDS(m_linear1,"./DataAnalysis/AltmetricScoreModeling/m_linear1.RDS")
m_linear1 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear1.RDS")
summary(m_linear1)

# linear regression assumptions
# 1. linear relationship between outcome and predictor
# 2. normally distributed residuals
# 3. homogeneity of residuals variance (homoscedasticity)
# 4. independence of residuals error terms
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

par(mfrow=c(2,2))
plot(m_linear1)

# 1. is predictor x outcome linear?
# top left: residuals vs fitted
# lack of distinct pattern means linear
# in this case, there is a bit of a pattern, which means that the logged score is not quite linear
# but the cutoff is probably due to having selected only points with scores >0
# considering that, the pattern is relatively indistinct?
# for high fitted values, residuals are negative. Basically the model overestimates high scores?
# there are relatively few data points that have very high scores, which could explain this weirdness

# 2. are residuals normally distributed?
# top right: normal q-q plot
# straight line is good
# in this case, the points follow the straight line, which means that the residuals are normally distributed

# 3. is the variance of the residuals homogeneous?
# bottom left: scale-location
# horizontal line with spread points is good
# in this case, there is a slight dip in the line and clusters of points, 
# which means that there can be a bit of heteroscedasticity
# then again, it's could still just be the fact that there are relatively few points with high fitted values

# 4. are the residual terms independent? i.e. are there outliers/influential points?
# bottom right: residuals vs leverage
# points outside of cook's distance (dashed red line) are bad
# in this case, there are several points that have high leverage (right of plot) but they aren't big outliers (top and bottom right corner)
# also, there are lots of points that are outliers (top & bottom) but are not highly influential
# there are a few points that are awful close to cook's line

##### lm2 with sqrt transformed y instead of log transformed #####

lineardata2 <- lineardata
lineardata2$sqrtscore <- sqrt(lineardata2$score)

# m_linear2 <- lm(sqrtscore ~ pubyear*gender*journal +
#                               propfemales +
#                               numauthors +
#                               pubmonth,
#                             data=lineardata2)
# saveRDS(m_linear2,"./DataAnalysis/AltmetricScoreModeling/m_linear2.RDS")
m_linear2 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear2.RDS")
summary(m_linear2)

par(mfrow=c(2,2))
plot(m_linear2)

# nope, the original log transform was better

##### lm3 with high leverage points removed #####

which(hatvalues(m_linear1)>0.95)
# when i originally ran this with scoretime as an interaction term
# there were high leverage outliers (points outside cook's line)
# so i tried a new model removing those points
# but now that isn't a problem anymore
# lineardata3 <- lineardata[-c(35,207,275,70728,70787),]
# m_linear3 <- lm(logscore ~ pubyear*gender*scoretime*journal +
#                              propfemales +
#                              numauthors +
#                              pubmonth,
#                            data=lineardata3)
# saveRDS(m_linear3,"./DataAnalysis/AltmetricScoreModeling/m_linear3.RDS")
# m_linear3 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear3.RDS")
# summary(m_linear3)
# 
# par(mfrow=c(2,2))
# plot(m_linear3)

# not much change, looks to me like the assumptions are more or less met as described above

# m_linear3_contvars <- data.frame(lineardata2$pubyear,lineardata2$numauthors,lineardata2$propfemales)
# head(m_linear3_contvars)
# dim(m_linear3_contvars)
# faraway::vif(m_linear3_contvars)

##### lm4 with pubyear as a factor #####

# that was with pubyear as an int, should be a factor
lineardata4 <- lineardata
lineardata4$pubyear <- as.factor(lineardata4$pubyear)
# m_linear4 <- lm(logscore ~ pubyear*gender*journal +
#                   propfemales +
#                   numauthors +
#                   pubmonth,
#                 data=lineardata4)
# saveRDS(m_linear4,"./DataAnalysis/AltmetricScoreModeling/m_linear4.RDS")
m_linear4 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear4.RDS")
summary(m_linear4)

# problem arises when i code pubyear as a factor
# some coefs are NA
summary(lineardata4[journal=="bioRxiv",c(1,4,8,9:22)])
# there are 0 articles in 2011 or 2012 for bioRxiv, and only 6 in 2013

##### lm5 without biorxiv #####

# there are 0 articles in 2011 or 2012 for bioRxiv, and only 6 in 2013
# this makes sense - bioRxiv was only founded in nov 2013 (https://en.wikipedia.org/wiki/BioRxiv)
# this causes problems because model tries to use bioRxiv 2011 as base case

lineardata5 <- lineardata4[journal!="bioRxiv",]
lineardata5$journal <- factor(lineardata5$journal)

# m_linear5 <- lm(logscore ~ pubyear*gender*journal +
#                   propfemales +
#                   numauthors +
#                   pubmonth, # zia put -1 here on another version - to get rid of intercept? do i need to?
#                 data=lineardata5)
# saveRDS(m_linear5,"./DataAnalysis/AltmetricScoreModeling/m_linear5.RDS")
m_linear5 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear5.RDS")
summary(m_linear5)

par(mfrow=c(2,2))
plot(m_linear5)
# looks pretty good

##### lm6 for just biorxiv #####

lineardata6 <- lineardata4[journal=="bioRxiv",]
lineardata6 <- lineardata6[pubyear!=2013,]
lineardata6$journal <- factor(lineardata6$journal)
lineardata6$pubyear <- factor(lineardata6$pubyear)

# m_linear6 <- lm(logscore ~ pubyear*gender +
#                   propfemales +
#                   numauthors +
#                   pubmonth,
#                 data=lineardata6)
# saveRDS(m_linear6,"./DataAnalysis/AltmetricScoreModeling/m_linear6.RDS")
m_linear6 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear6.RDS")
summary(m_linear6)

par(mfrow=c(2,2))
plot(m_linear6)

# observation with leverage 1: 21579
lineardata6[21579,]

##### lm7 for biorxiv removed point with leverage 1 #####

lineardata7 <- lineardata6[-21579,]

# m_linear7 <- lm(logscore ~ pubyear*gender +
#                   propfemales +
#                   numauthors +
#                   pubmonth,
#                 data=lineardata7)
# saveRDS(m_linear7,"./DataAnalysis/AltmetricScoreModeling/m_linear7.RDS")
m_linear7 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear7.RDS")
summary(m_linear7)

# still get an NA for 2018 lf! why?
summary(lineardata7[pubyear==2014])
# no lf for 2014

##### lm8 removed 2014 from biorxiv #####

lineardata8 <- lineardata7[pubyear!=2014,]

# m_linear8 <- lm(logscore ~ pubyear*gender +
#                   propfemales +
#                   numauthors +
#                   pubmonth,
#                 data=lineardata8)
# saveRDS(m_linear8,"./DataAnalysis/AltmetricScoreModeling/m_linear8.RDS")
m_linear8 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear8.RDS")
summary(m_linear8)

par(mfrow=c(2,2))
plot(m_linear8)

##### logistic3 with pubyear as factor #####

logisticdata3 <- logisticdata2
logisticdata3$pubyear <- as.factor(logisticdata3$pubyear)

# m_logist3 <- glm(score ~  pubyear*gender*journal +
#                             propfemales +
#                             numauthors +
#                             pubmonth,
#                           data=logisticdata3,
#                           family=binomial)
# saveRDS(m_logist3,"./DataAnalysis/AltmetricScoreModeling/m_logist3.RDS")
m_logist3 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist3.RDS")
summary(m_logist3)

##### logistic4 without biorxiv #####

logisticdata4 <- logisticdata3[journal!="bioRxiv",]
logisticdata4$journal <- factor(logisticdata4$journal)
logisticdata4$pubyear <- as.factor(logisticdata4$pubyear)

# m_logist4 <- glm(score ~  pubyear*gender*journal +
#                             propfemales +
#                             numauthors +
#                             pubmonth,
#                           data=logisticdata4,
#                           family=binomial)
# saveRDS(m_logist4,"./DataAnalysis/AltmetricScoreModeling/m_logist4.RDS")
m_logist4 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist4.RDS")
summary(m_logist4)

# logistic regression has assumptions
# 1. outcome is binary
# 2. relationship between logit of outcome and each of predictors is linear
# 3. no influential values in continuous predictors
# 4. no high correlations (multicollinearity) between predictors
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

# 1. is outcome binary?
unique(logisticdata4$score)
# yes, just 0 or 1

# 2. is logit x predictors linear? (could check by scatter plot or box Tidwell test)
probabilities <- predict(m_logist4, type="response")
logit <- log(probabilities/(1-probabilities))
ggplot(logisticdata4, aes(logit,pubyear)) +
  geom_point() +
  theme_bw()
# pretty linear, no need to transform (not running with smooth loess because takes FOREVER to run with this many points)
# also this is an int, not a numeric, so it's fine
ggplot(logisticdata4, aes(logit,propfemales)) +
  geom_point() +
  theme_bw()
# not really linear...  i don't know what to make of this...
ggplot(logisticdata4, aes(logit,numauthors)) +
  geom_point() +
  theme_bw()
# kind of odd but pretty linear
# this is also an int, not numeric

# actually, zia said the way they go about doing this on the sthda website is wrong
# it's not the predictor vs the logit that matters, what matters is the partial residuals
# that is, you need to take into account the effect of everything else on the response, 
# so can't just do predictor x logit(response)

visreg(m_logist4, "propfemales", type="conditional")
# looks linear so there's no point in transforming the propfemales variable
# also this is the only one that is actually continuous so only one that we need to check

# 3. are there influential values in continuous predictors?
plot(m_logist4, 4)
# cook's distance shows outliers
plot(m_logist4, 5)
# lots of points with high leverage, and lots of points with high std residuals, but none outside cook's distance lines

# 4. is there high correlation (multicollinearity) between predictors?
# need to look only at continuous vars, so can't use car::vif(m_logist1)
m_logist4_contvars <- data.frame(logisticdata4$numauthors,logisticdata4$propfemales)
head(m_logist4_contvars)
dim(m_logist4_contvars)
faraway::vif(m_logist4_contvars)
# values are close to 1, which means the predictors are not correlated

##### logistic5 just biorxiv #####

logisticdata5 <- logisticdata3[journal=="bioRxiv",]
logisticdata5 <- logisticdata5[pubyear %in% c(2015:2018),]
logisticdata5$journal <- factor(logisticdata5$journal)
logisticdata5$pubyear <- as.factor(logisticdata5$pubyear)

# m_logist5 <- glm(score ~  pubyear*gender +
#                             propfemales +
#                             numauthors +
#                             pubmonth,
#                           data=logisticdata5,
#                           family=binomial)
# saveRDS(m_logist5,"./DataAnalysis/AltmetricScoreModeling/m_logist5.RDS")
m_logist5 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist5.RDS")
summary(m_logist5)

# 1. is outcome binary?
unique(logisticdata5$score)
# yes, just 0 or 1

# 2. is logit x predictors linear? (could check by scatter plot or box Tidwell test)

visreg(m_logist5, "propfemales", type="conditional")
# looks linear so there's no point in transforming the propfemales variable
# also this is the only one that is actually continuous so only one that we need to check

# 3. are there influential values in continuous predictors?
plot(m_logist5, 4)
# cook's distance shows outliers
plot(m_logist5, 5)
# lots of points with high leverage, and lots of points with high std residuals, but none outside cook's distance lines

# 4. is there high correlation (multicollinearity) between predictors?
# need to look only at continuous vars, so can't use car::vif(m_logist1)
m_logist5_contvars <- data.frame(logisticdata5$numauthors,logisticdata5$propfemales)
head(m_logist5_contvars)
dim(m_logist5_contvars)
faraway::vif(m_logist5_contvars)
# values are close to 1, which means the predictors are not correlated

table(logisticdata5[pubyear==2018,]$score)
# only 4 article that have no score, the rest don't?!?
# maybe it's because 2018 is weird...

##### logistic6 without biorxiv til 2017 #####

logisticdata6 <- logisticdata3[journal!="bioRxiv",]
logisticdata6 <- logisticdata6[pubyear!=2018,]
logisticdata6$journal <- factor(logisticdata6$journal)
logisticdata6$pubyear <- as.factor(logisticdata6$pubyear)

# m_logist6 <- glm(score ~  pubyear*gender*journal +
#                             propfemales +
#                             numauthors +
#                             pubmonth,
#                           data=logisticdata6,
#                           family=binomial)
# saveRDS(m_logist6,"./DataAnalysis/AltmetricScoreModeling/m_logist6.RDS")
m_logist6 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist6.RDS")
summary(m_logist6)

# 1. is outcome binary?
unique(logisticdata6$score)
# yes, just 0 or 1

# 2. is logit x predictors linear? (could check by scatter plot or box Tidwell test)

visreg(m_logist6, "propfemales", type="conditional")
# looks linear so there's no point in transforming the propfemales variable
# also this is the only one that is actually continuous so only one that we need to check

# 3. are there influential values in continuous predictors?
plot(m_logist6, 4)
# cook's distance shows outliers
plot(m_logist6, 5)
# lots of points with high leverage, and lots of points with high std residuals, but none outside cook's distance lines

# 4. is there high correlation (multicollinearity) between predictors?
# need to look only at continuous vars, so can't use car::vif(m_logist1)
m_logist6_contvars <- data.frame(logisticdata6$numauthors,logisticdata6$propfemales)
head(m_logist6_contvars)
dim(m_logist6_contvars)
faraway::vif(m_logist6_contvars)
# values are close to 1, which means the predictors are not correlated

##### logistic7 just biorxiv til 2017 #####

logisticdata7 <- logisticdata3[journal=="bioRxiv",]
logisticdata7 <- logisticdata7[pubyear %in% c(2015:2017),]
logisticdata7$journal <- factor(logisticdata7$journal)
logisticdata7$pubyear <- factor(logisticdata7$pubyear)

# m_logist7 <- glm(score ~  pubyear*gender +
#                             propfemales +
#                             numauthors +
#                             pubmonth,
#                           data=logisticdata7,
#                           family=binomial)
# saveRDS(m_logist7,"./DataAnalysis/AltmetricScoreModeling/m_logist7.RDS")
m_logist7 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist7.RDS")
summary(m_logist7)

# 1. is outcome binary?
unique(logisticdata7$score)
# yes, just 0 or 1

# 2. is logit x predictors linear? (could check by scatter plot or box Tidwell test)

visreg(m_logist7, "propfemales", type="conditional")
# looks linear so there's no point in transforming the propfemales variable
# also this is the only one that is actually continuous so only one that we need to check

# 3. are there influential values in continuous predictors?
plot(m_logist7, 4)
# cook's distance shows outliers
plot(m_logist7, 5)
# lots of points with high leverage, and lots of points with high std residuals, but none outside cook's distance lines

# 4. is there high correlation (multicollinearity) between predictors?
# need to look only at continuous vars, so can't use car::vif(m_logist1)
m_logist7_contvars <- data.frame(logisticdata7$numauthors,logisticdata7$propfemales)
head(m_logist7_contvars)
dim(m_logist7_contvars)
faraway::vif(m_logist7_contvars)
# values are close to 1, which means the predictors are not correlated


#############
### PLOTS ###
#############

par(mfrow=c(1,1))

# can't really plot predictions for logistic glm

# predictions for lm without biorxiv
plot_model(m_linear5,type="pred",terms=c("pubyear","gender","journal"))
plot_model(m_linear5,type="pred",terms=c("pubmonth"))
plot_model(m_linear5,type="pred",terms=c("propfemales"))
plot_model(m_linear5,type="pred",terms=c("numauthors"))

# predictions for lm with only biorxiv
plot_model(m_linear8,type="pred",terms=c("pubyear","gender"))
plot_model(m_linear8,type="pred",terms=c("pubmonth"))
plot_model(m_linear8,type="pred",terms=c("propfemales"))
plot_model(m_linear8,type="pred",terms=c("numauthors"))





