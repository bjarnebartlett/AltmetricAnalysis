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
# install.packages("multcomp")
# install.packages("ggplot2")
# install.packages("ggpubr")
library(data.table)
library(lme4)
library(sjPlot)
library(ggplot2)
library(tidyverse)
library(visreg)
library(multcomp)
library(ggplot2)
library(ggpubr)


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

# Collinearity
plot(mergeddata$numauthors, mergeddata$propfemales)

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

# added 2020.01.23
library(MASS)
rm_linear5 <- rlm(logscore ~ pubyear*gender*journal +
                                        propfemales +
                                        numauthors +
                                        pubmonth,
                                      data=lineardata5)
summary(rm_linear5)
###

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


################
### POST HOC ###
################

# Final models are:

# m_linear5: 
# lm of articles from all journals except bioRxiv,
# for all years, 
# with year as factor
# with gender*journal*year 
# and propfemales, numauthors and month as additional factors

# m_linear8:
# lm of articles from bioRxiv,
# for years 2015-2018
# with year as factor
# with gender*year
# and propfemales, numauthors and month as additional factors


##### Are all factors important #####

# according to Tanadini's thesis p. 39 (https://ora.ox.ac.uk/objects/uuid:73c52d36-2e8a-4e04-92e0-a67ed93d7090/download_file?file_format=pdf&safe_filename=Thesis_Tanadini_2016.pdf&type_of_work=Thesis)
# we want to confirm that an independent variable of interest actually has an effect
# can do this by creating a version of the model without the variable and doing an anova

summary(m_linear5)
# mult R2: 0.2611
# adjusted R2: 0.2589

m_linear5b <- lm(logscore ~ pubyear*gender*journal + numauthors + pubmonth,
                data=lineardata5)
anova(m_linear5, m_linear5b)
# via anova, found that propfemales was not significant but numauthors and pubmonth was
saveRDS(m_linear5b,"./DataAnalysis/AltmetricScoreModeling/m_linear5b.RDS")
m_linear5b <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear5b.RDS")
summary(m_linear5b)
# mult R2: 0.2611
# adj R2: 0.2589

summary(m_linear8)
# mult R2: 0.05691
# adj R2: 0.05587
# let's see if removing additional factors (just keeping interaction) makes a dif
m_linear8b <- lm(logscore ~ pubyear*gender + numauthors + pubmonth,
                data=lineardata8)
anova(m_linear8, m_linear8b)
# via anova, found all independent vars were significant - keep the main model
saveRDS(m_linear8b,"./DataAnalysis/AltmetricScoreModeling/m_linear8b.RDS")
m_linear8b <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear8b.RDS")
summary(m_linear8b)
# mult R2: 0.05627
# adj R2: 0.05526

##### Sensitivity to unknown authors #####

# we have a pretty large number of unknown authors
# that are coded as f or l for first author unknown/last author unknown
# let's see if the models change if we didn't have unknowns

# 1. Assume all unknown authors are evenly split between male and female
lineardata5c <- lineardata5
lineardata5c[gender=="f"]$gender <- sample(c("ff","fm"),length(lineardata5c[gender=="f"]$gender), replace=TRUE) # for unknown authors, randomly assign ff or fm
lineardata5c[gender=="l"]$gender <- sample(c("lf","lm"),length(lineardata5c[gender=="l"]$gender), replace=TRUE) # for unknown authors, randomly assign ff or fm
lineardata5c$gender <- factor(lineardata5c$gender)
levels(lineardata5c$gender)
m_linear5c <- lm(logscore ~ pubyear*gender*journal + numauthors + pubmonth,
                data=lineardata5c)
saveRDS(m_linear5c,"./DataAnalysis/AltmetricScoreModeling/m_linear5c.RDS")
m_linear5c <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear5c.RDS")
summary(m_linear5c)
# R2 0.2488 0.2573

lineardata8c <- lineardata8
lineardata8c[gender=="f"]$gender <- sample(c("ff","fm"),length(lineardata8c[gender=="f"]$gender), replace=TRUE) # for unknown authors, randomly assign ff or fm
lineardata8c[gender=="l"]$gender <- sample(c("lf","lm"),length(lineardata8c[gender=="l"]$gender), replace=TRUE) # for unknown authors, randomly assign ff or fm
lineardata8c$gender <- factor(lineardata8c$gender)
levels(lineardata8c$gender)
m_linear8c <- lm(logscore ~ pubyear*gender + numauthors + propfemales + pubmonth,
                 data=lineardata8c)
saveRDS(m_linear8c,"./DataAnalysis/AltmetricScoreModeling/m_linear8c.RDS")
m_linear8c <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear8c.RDS")
summary(m_linear8c)
# 0.0563 0.05549

# 2. Assume all unknown authors are split according to the proportions observed in the known authors
lineardata5d <- lineardata5
probff <- count(lineardata5[gender=="ff"])/count(lineardata5[gender=="ff"|gender=="fm"])
probfm <- 1-probff
problf <- count(lineardata5[gender=="lf"])/count(lineardata5[gender=="lf"|gender=="lm"])
problm <- 1-problf
lineardata5d[gender=="f"]$gender <- sample(c("ff","fm"),length(lineardata5[gender=="f"]$gender),prob=c(probff,probfm), replace=TRUE)
lineardata5d[gender=="l"]$gender <- sample(c("lf","lm"),length(lineardata5[gender=="l"]$gender),prob=c(problf,problm), replace=TRUE)
lineardata5d$gender <- factor(lineardata5d$gender)
table(lineardata5d$gender)
m_linear5d <- lm(logscore ~ pubyear*gender*journal + numauthors + pubmonth,
                 data=lineardata5d)
saveRDS(m_linear5d,"./DataAnalysis/AltmetricScoreModeling/m_linear5d.RDS")
m_linear5d <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear5d.RDS")
summary(m_linear5d)
# R2 0.2589 0.2574

lineardata8d <- lineardata8
probff <- count(lineardata8[gender=="ff"])/count(lineardata8[gender=="ff"|gender=="fm"])
probfm <- 1-probff
problf <- count(lineardata8[gender=="lf"])/count(lineardata8[gender=="lf"|gender=="lm"])
problm <- 1-problf
lineardata8d[gender=="f"]$gender <- sample(c("ff","fm"),length(lineardata8[gender=="f"]$gender),prob=c(probff,probfm), replace=TRUE)
lineardata8d[gender=="l"]$gender <- sample(c("lf","lm"),length(lineardata8[gender=="l"]$gender),prob=c(problf,problm), replace=TRUE)
lineardata8d$gender <- factor(lineardata8d$gender)
table(lineardata8d$gender)
m_linear8d <- lm(logscore ~ pubyear*gender + numauthors + propfemales + pubmonth,
                 data=lineardata8d)
saveRDS(m_linear8d,"./DataAnalysis/AltmetricScoreModeling/m_linear8d.RDS")
m_linear8d <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear8d.RDS")
summary(m_linear8d)
# R2 0.05632 0.05551

# How do I compare the models? Goal is to see whether missing data has a big impact on the modeled relationships
# Maybe just look at R2?
# 5b: 0.2611 0.2589
# 5c: 0.2488 0.2573
# 5d: 0.2589 0.2574
# 5b might be best?
# Looking at R2 will tell us about the fit, but not how different the fitted models are?
# Maybe look at AIC?
# AIC needs to be done on models run on the same data
# Maybe look at model coefs?
head(data.frame(coef(m_linear5b)))
head(data.frame(coef(m_linear5c)))
head(data.frame(coef(m_linear5d)))

head(data.frame(coef(m_linear8b)))
head(data.frame(coef(m_linear8c)))
head(data.frame(coef(m_linear8d)))


##### Is there a difference between male and female authors #####

# Following post hoc assessment p.40 Tanadini's thesis
lineardata5d <- lineardata5
lineardata5d$puby.gend.jour <- interaction(lineardata5d$pubyear, lineardata5d$gender, lineardata5d$journal)
m_linear5d2 <- lm(logscore ~ puby.gend.jour + numauthors + pubmonth, data=lineardata5d)
coef(m_linear5d2)

ind.ff <- grep(pattern = "ff", x = names(coef(m_linear5d2)))
ind.fm <- grep(pattern = "fm", x = names(coef(m_linear5d2)))
vec_f <- rep(0, times = length(coef(m_linear5d2)))
vec_f[ind.ff] <- 1/length(ind.ff)
vec_f[ind.fm] <- -1/length(ind.fm)
ind.lf <- grep(pattern = "lf", x = names(coef(m_linear5d2)))
ind.lm <- grep(pattern = "lm", x = names(coef(m_linear5d2)))
vec_l <- rep(0, times = length(coef(m_linear5d2)))
vec_l[ind.lf] <- 1/length(ind.lf)
vec_l[ind.lm] <- -1/length(ind.lm)
M5 <- rbind("first female vs first male" = vec_f,
            "last female vs last male" = vec_l)
colnames(M5) <- names(coef(m_linear5d2))

posthoc5 <- glht(m_linear5d2, linfct = M5)
summary(posthoc5)

# for biorxiv
lineardata8d <- lineardata8
lineardata8d$puby.gend <- interaction(lineardata8d$pubyear, lineardata8d$gender)
m_linear8d2 <- lm(logscore ~ puby.gend + numauthors + propfemales + pubmonth, data=lineardata8d)
coef(m_linear8d2)

ind.ff <- grep(pattern = "ff", x = names(coef(m_linear8d2)))
ind.fm <- grep(pattern = "fm", x = names(coef(m_linear8d2)))
vec_f <- rep(0, times = length(coef(m_linear8d2)))
vec_f[ind.ff] <- 1/length(ind.ff)
vec_f[ind.fm] <- -1/length(ind.fm)
ind.lf <- grep(pattern = "lf", x = names(coef(m_linear8d2)))
ind.lm <- grep(pattern = "lm", x = names(coef(m_linear8d2)))
vec_l <- rep(0, times = length(coef(m_linear8d2)))
vec_l[ind.lf] <- 1/length(ind.lf)
vec_l[ind.lm] <- -1/length(ind.lm)
M8 <- rbind("first female vs first male" = vec_f,
            "last female vs last male" = vec_l)
colnames(M8) <- names(coef(m_linear8d2))

posthoc8 <- glht(m_linear8d2, linfct = M8)
summary(posthoc8)

##### Is there a difference between male and female authors, within journal, between 2011 and 2018? ####

fffmCell2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.ff.Cell", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.fm.Cell", nomatch = 0))
fffmCell2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.ff.Cell", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.fm.Cell", nomatch = 0))
lflmCell2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lf.Cell", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lm.Cell", nomatch = 0))
lflmCell2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lf.Cell", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lm.Cell", nomatch = 0))

fffmNat2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.ff.Nature", nomatch = 0) -
                match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.fm.Nature", nomatch = 0))
fffmNat2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.ff.Nature", nomatch = 0) -
                match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.fm.Nature", nomatch = 0))
lflmNat2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lf.Nature", nomatch = 0) -
                match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lm.Nature", nomatch = 0))
lflmNat2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lf.Nature", nomatch = 0) -
                match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lm.Nature", nomatch = 0))

fffmNEJM2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.ff.NEJM", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.fm.NEJM", nomatch = 0))
fffmNEJM2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.ff.NEJM", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.fm.NEJM", nomatch = 0))
lflmNEJM2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lf.NEJM", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lm.NEJM", nomatch = 0))
lflmNEJM2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lf.NEJM", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lm.NEJM", nomatch = 0))

fffmPLOS2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.ff.PLoS ONE", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.fm.PLoS ONE", nomatch = 0))
fffmPLOS2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.ff.PLoS ONE", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.fm.PLoS ONE", nomatch = 0))
lflmPLOS2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lf.PLoS ONE", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lm.PLoS ONE", nomatch = 0))
lflmPLOS2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lf.PLoS ONE", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lm.PLoS ONE", nomatch = 0))

fffmPNAS2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.ff.PNAS", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.fm.PNAS", nomatch = 0))
fffmPNAS2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.ff.PNAS", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.fm.PNAS", nomatch = 0))
lflmPNAS2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lf.PNAS", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lm.PNAS", nomatch = 0))
lflmPNAS2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lf.PNAS", nomatch = 0) -
                 match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lm.PNAS", nomatch = 0))

fffmSci2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.ff.Science", nomatch = 0) -
                    match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.fm.Science", nomatch = 0))
fffmSci2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.ff.Science", nomatch = 0) -
                    match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.fm.Science", nomatch = 0))
lflmSci2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lf.Science", nomatch = 0) -
                    match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lm.Science", nomatch = 0))
lflmSci2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lf.Science", nomatch = 0) -
                    match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lm.Science", nomatch = 0))

M <- rbind(fffmCell2018, fffmCell2011, lflmCell2018, lflmCell2011,
           fffmNat2018,  fffmNat2011,  lflmNat2018,  lflmNat2011,
           fffmNEJM2018, fffmNEJM2011, lflmNEJM2018, lflmNEJM2011,
           fffmPLOS2018, fffmPLOS2011, lflmPLOS2018, lflmPLOS2011,
           fffmPNAS2018, fffmPNAS2011, lflmPNAS2018, lflmPNAS2011,
           fffmSci2018,  fffmSci2011,  lflmSci2018,  lflmSci2011)
colnames(M) <- names(coef(m_linear5d2))

posthocdif <- glht(m_linear5d2, linfct = M)
summary(posthocdif)
plot(posthocdif)

# bioRxiv
fffmbio2018 <- (match(x = names(coef(m_linear8d2)), table = "puby.gend2018.ff", nomatch = 0) -
                match(x = names(coef(m_linear8d2)), table = "puby.gend2018.fm", nomatch = 0))
fffmbio2015 <- (match(x = names(coef(m_linear8d2)), table = "puby.gend2015.ff", nomatch = 0) -
                match(x = names(coef(m_linear8d2)), table = "puby.gend2015.fm", nomatch = 0))
lflmbio2018 <- (match(x = names(coef(m_linear8d2)), table = "puby.gend2018.lf", nomatch = 0) -
                match(x = names(coef(m_linear8d2)), table = "puby.gend2018.lm", nomatch = 0))
lflmbio2015 <- (match(x = names(coef(m_linear8d2)), table = "puby.gend2015.lf", nomatch = 0) -
                match(x = names(coef(m_linear8d2)), table = "puby.gend2015.lm", nomatch = 0))

Mb <- rbind(fffmbio2018, fffmbio2015, lflmbio2018, lflmbio2015)
colnames(Mb) <- names(coef(m_linear8d2))

posthocdifb <- glht(m_linear8d2, linfct = Mb)
summary(posthocdifb)
plot(posthocdifb)

# prep for plot
d <- data.frame(confint(posthocdif)$confint)
db <- data.frame(confint(posthocdifb)$confint)
d <- rbind(db,d)
head(d)
d$year <- c(2018,2015,2018,2015,rep(c(2018,2011),times=12))
d$fl <- rep(c("First author","First author","Last author","Last author"), times=7)
d$journal <- as.factor(c(rep("bioRxiv",4),rep("Cell",4),rep("Nature",4),rep("NEJM",4),rep("PLOS",4),rep("PNAS",4),rep("Science",4)))
tail(d)

##### Is there a difference between genders within journals (regardless of years) #####

lineardata5e <- lineardata5
lineardata5e$gend.jour <- interaction(lineardata5e$gender, lineardata5e$journal)
m_linear5e <- lm(logscore ~ pubyear*gend.jour + numauthors + pubmonth, data=lineardata5e)
coef(m_linear5e)

cell.ff <- grep(pattern = "ff.Cell", x = names(coef(m_linear5e)))
cell.fm <- grep(pattern = "fm.Cell", x = names(coef(m_linear5e)))
vec_cellf <- rep(0, times = length(coef(m_linear5e)))
vec_cellf[cell.ff] <- 1/length(cell.ff)
vec_cellf[cell.fm] <- -1/length(cell.fm)
cell.lf <- grep(pattern = "lf.Cell", x = names(coef(m_linear5e)))
cell.lm <- grep(pattern = "lm.Cell", x = names(coef(m_linear5e)))
vec_celll <- rep(0, times = length(coef(m_linear5e)))
vec_celll[cell.lf] <- 1/length(cell.lf)
vec_celll[cell.lm] <- -1/length(cell.lm)
Mcell <- rbind("first female vs first male" = vec_cellf,
            "last female vs last male" = vec_celll)
colnames(Mcell) <- names(coef(m_linear5e))

posthoccell <- glht(m_linear5e, linfct = Mcell)
summary(posthoccell)

nat.ff <- grep(pattern = "ff.Nature", x = names(coef(m_linear5e)))
nat.fm <- grep(pattern = "fm.Nature", x = names(coef(m_linear5e)))
vec_natf <- rep(0, times = length(coef(m_linear5e)))
vec_natf[nat.ff] <- 1/length(nat.ff)
vec_natf[nat.fm] <- -1/length(nat.fm)
nat.lf <- grep(pattern = "lf.Nature", x = names(coef(m_linear5e)))
nat.lm <- grep(pattern = "lm.Nature", x = names(coef(m_linear5e)))
vec_natl <- rep(0, times = length(coef(m_linear5e)))
vec_natl[nat.lf] <- 1/length(nat.lf)
vec_natl[nat.lm] <- -1/length(nat.lm)
Mnat <- rbind("first female vs first male" = vec_natf,
               "last female vs last male" = vec_natl)
colnames(Mnat) <- names(coef(m_linear5e))

posthocnat <- glht(m_linear5e, linfct = Mnat)
summary(posthocnat)
plot(posthocnat)

nejm.ff <- grep(pattern = "ff.NEJM", x = names(coef(m_linear5e)))
nejm.fm <- grep(pattern = "fm.NEJM", x = names(coef(m_linear5e)))
vec_nejmf <- rep(0, times = length(coef(m_linear5e)))
vec_nejmf[nejm.ff] <- 1/length(nejm.ff)
vec_nejmf[nejm.fm] <- -1/length(nejm.fm)
nejm.lf <- grep(pattern = "lf.NEJM", x = names(coef(m_linear5e)))
nejm.lm <- grep(pattern = "lm.NEJM", x = names(coef(m_linear5e)))
vec_nejml <- rep(0, times = length(coef(m_linear5e)))
vec_nejml[nejm.lf] <- 1/length(nejm.lf)
vec_nejml[nejm.lm] <- -1/length(nejm.lm)
Mnejm <- rbind("first female vs first male" = vec_nejmf,
              "last female vs last male" = vec_nejml)
colnames(Mnejm) <- names(coef(m_linear5e))

posthocnejm <- glht(m_linear5e, linfct = Mnejm)
summary(posthocnejm)

plos.ff <- grep(pattern = "ff.PLoS ONE", x = names(coef(m_linear5e)))
plos.fm <- grep(pattern = "fm.PLoS ONE", x = names(coef(m_linear5e)))
vec_plosf <- rep(0, times = length(coef(m_linear5e)))
vec_plosf[plos.ff] <- 1/length(plos.ff)
vec_plosf[plos.fm] <- -1/length(plos.fm)
plos.lf <- grep(pattern = "lf.PLoS ONE", x = names(coef(m_linear5e)))
plos.lm <- grep(pattern = "lm.PLoS ONE", x = names(coef(m_linear5e)))
vec_plosl <- rep(0, times = length(coef(m_linear5e)))
vec_plosl[plos.lf] <- 1/length(plos.lf)
vec_plosl[plos.lm] <- -1/length(plos.lm)
Mplos <- rbind("first female vs first male" = vec_plosf,
              "last female vs last male" = vec_plosl)
colnames(Mplos) <- names(coef(m_linear5e))

posthocplos <- glht(m_linear5e, linfct = Mplos)
summary(posthocplos)

pnas.ff <- grep(pattern = "ff.PNAS", x = names(coef(m_linear5e)))
pnas.fm <- grep(pattern = "fm.PNAS", x = names(coef(m_linear5e)))
vec_pnasf <- rep(0, times = length(coef(m_linear5e)))
vec_pnasf[pnas.ff] <- 1/length(pnas.ff)
vec_pnasf[pnas.fm] <- -1/length(pnas.fm)
pnas.lf <- grep(pattern = "lf.PNAS", x = names(coef(m_linear5e)))
pnas.lm <- grep(pattern = "lm.PNAS", x = names(coef(m_linear5e)))
vec_pnasl <- rep(0, times = length(coef(m_linear5e)))
vec_pnasl[pnas.lf] <- 1/length(pnas.lf)
vec_pnasl[pnas.lm] <- -1/length(pnas.lm)
Mpnas <- rbind("first female vs first male" = vec_pnasf,
              "last female vs last male" = vec_pnasl)
colnames(Mpnas) <- names(coef(m_linear5e))

posthocpnas <- glht(m_linear5e, linfct = Mpnas)
summary(posthocpnas)

sci.ff <- grep(pattern = "ff.Science", x = names(coef(m_linear5e)))
sci.fm <- grep(pattern = "fm.Science", x = names(coef(m_linear5e)))
vec_scif <- rep(0, times = length(coef(m_linear5e)))
vec_scif[sci.ff] <- 1/length(sci.ff)
vec_scif[sci.fm] <- -1/length(sci.fm)
sci.lf <- grep(pattern = "lf.Science", x = names(coef(m_linear5e)))
sci.lm <- grep(pattern = "lm.Science", x = names(coef(m_linear5e)))
vec_scil <- rep(0, times = length(coef(m_linear5e)))
vec_scil[sci.lf] <- 1/length(sci.lf)
vec_scil[sci.lm] <- -1/length(sci.lm)
Msci <- rbind("first female vs first male" = vec_scif,
              "last female vs last male" = vec_scil)
colnames(Msci) <- names(coef(m_linear5e))

posthocsci <- glht(m_linear5e, linfct = Msci)
summary(posthocsci)


##### Posthoc for logistic #####

logisticdata4b <- logisticdata4
logisticdata4b$puby.gend.jour <- interaction(logisticdata4b$pubyear, logisticdata4b$gender, logisticdata4b$journal)
m_logist4b <- glm(score ~ puby.gend.jour + numauthors + propfemales + pubmonth, data=logisticdata4b, family=binomial)
saveRDS(m_logist4b,"./DataAnalysis/AltmetricScoreModeling/m_logist4b.RDS")
# m_logist4b <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist4b.RDS")
coef(m_logist4b)

ind.ff <- grep(pattern = "ff", x = names(coef(m_logist4b)))
ind.fm <- grep(pattern = "fm", x = names(coef(m_logist4b)))
vec_f <- rep(0, times = length(coef(m_logist4b)))
vec_f[ind.ff] <- 1/length(ind.ff)
vec_f[ind.fm] <- -1/length(ind.fm)
ind.lf <- grep(pattern = "lf", x = names(coef(m_logist4b)))
ind.lm <- grep(pattern = "lm", x = names(coef(m_logist4b)))
vec_l <- rep(0, times = length(coef(m_logist4b)))
vec_l[ind.lf] <- 1/length(ind.lf)
vec_l[ind.lm] <- -1/length(ind.lm)
M4 <- rbind("first female vs first male" = vec_f,
            "last female vs last male" = vec_l)
colnames(M4) <- names(coef(m_logist4b))

posthoc4 <- glht(m_logist4b, linfct = M4)
summary(posthoc4)

logisticdata7b <- logisticdata7
logisticdata7b$puby.gend.jour <- interaction(logisticdata7b$pubyear, logisticdata7b$gender, logisticdata7b$journal)
m_logist7b <- glm(score ~ puby.gend.jour + numauthors + propfemales + pubmonth, data=logisticdata7b, family=binomial)
saveRDS(m_logist7b,"./DataAnalysis/AltmetricScoreModeling/m_logist7b.RDS")
# m_logist7b <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist7b.RDS")
coef(m_logist7b)

ind.ff <- grep(pattern = "ff", x = names(coef(m_logist7b)))
ind.fm <- grep(pattern = "fm", x = names(coef(m_logist7b)))
vec_f <- rep(0, times = length(coef(m_logist7b)))
vec_f[ind.ff] <- 1/length(ind.ff)
vec_f[ind.fm] <- -1/length(ind.fm)
ind.lf <- grep(pattern = "lf", x = names(coef(m_logist7b)))
ind.lm <- grep(pattern = "lm", x = names(coef(m_logist7b)))
vec_l <- rep(0, times = length(coef(m_logist7b)))
vec_l[ind.lf] <- 1/length(ind.lf)
vec_l[ind.lm] <- -1/length(ind.lm)
M7 <- rbind("first female vs first male" = vec_f,
            "last female vs last male" = vec_l)
colnames(M7) <- names(coef(m_logist7b))

posthoc7 <- glht(m_logist7b, linfct = M7)
summary(posthoc7)

# is data (in this case, the )
# log ratio between first author female and male scores = -0.2425
# std err = 0.1033
# sigma sq = 0.1033^2*n (where n is sample size)
# but I don't know what n was in glht????
# is it just the n of the model?
# 0.1033^2 = 0.01067089 which is very close to the value in vcov 0.0106683318
# same for last author, 0.1211^2 = 0.01466521 which is close to vcov 0.0146728393
# so maybe i don't need to multiply by n becacuse it needs to be var of the sample???
# so the unbiased backtransform would be 

# exp(fitted(lm.0) * (1 / nobs(mod.2) * sum(exp(resid(lm.0)))) # missing a closing bracket and mod.2 is a mistake?
exp(fitted(m_logist7b) * (1 / nobs(m_logist7b) * sum(exp(resid(m_logist7b)))))
# ??????

#############
### PLOTS ###
#############

##### Plots for dif bw female and male for each journal for start year and end year #####

pf <- ggplot(data=d[which(d$fl=="First author"),], aes(x=year, y=Estimate, color=journal)) +
  theme_bw() +
  scale_color_discrete() +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  geom_line() +
  theme(
    legend.position="right",
    plot.title = element_text(hjust=0.5)
  ) +
  ggtitle("Difference between female and male first authors")
pf

pbio <- ggplot(data=d[which(d$journal=="bioRxiv"),], aes(x=year, y=Estimate, color=fl)) +
  theme_bw() +
  geom_point(position=position_dodge(.7)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2, position=position_dodge(.7)) +
  theme(
    legend.position = "none", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("bioRxiv") +
  scale_x_continuous(name="Year", limits=c(2010,2020)) +
  scale_y_continuous(name="Difference in logscore", limits=c(-2,2))
pbio

pcell <- ggplot(data=d[which(d$journal=="Cell"),], aes(x=year, y=Estimate, color=fl)) +
  theme_bw() +
  geom_point(position=position_dodge(.7)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2, position=position_dodge(.7)) +
  theme(
    legend.position = "none", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("Cell") +
  scale_x_continuous(limits=c(2010,2020)) +
  scale_y_continuous(limits=c(-2.5,2.5))
pcell

pnat <- ggplot(data=d[which(d$journal=="Nature"),], aes(x=year, y=Estimate, color=fl)) +
  theme_bw() +
  geom_point(position=position_dodge(.7)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2, position=position_dodge(.7)) +
  theme(
    legend.position = "none", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("Nature") +
  scale_x_continuous(limits=c(2010,2020)) +
  scale_y_continuous(limits=c(-2.5,2.5))
pnat

pnejm <- ggplot(data=d[which(d$journal=="NEJM"),], aes(x=year, y=Estimate, color=fl)) +
  theme_bw() +
  geom_point(position=position_dodge(.7)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2, position=position_dodge(.7)) +
  theme(
    legend.position = "none", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("NEJM") +
  scale_x_continuous(limits=c(2010,2020)) +
  scale_y_continuous(limits=c(-2.5,2.5))
pnejm

pplos <- ggplot(data=d[which(d$journal=="PLOS"),], aes(x=year, y=Estimate, color=fl)) +
  theme_bw() +
  geom_point(position=position_dodge(.7)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2, position=position_dodge(.7)) +
  theme(
    legend.position = "none", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("PLOS") +
  scale_x_continuous(limits=c(2010,2020)) +
  scale_y_continuous(limits=c(-2.5,2.5))
pplos

ppnas <- ggplot(data=d[which(d$journal=="PNAS"),], aes(x=year, y=Estimate, color=fl)) +
  theme_bw() +
  geom_point(position=position_dodge(.7)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2, position=position_dodge(.7)) +
  theme(
    legend.position = "none", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("PNAS") +
  scale_x_continuous(limits=c(2010,2020)) +
  scale_y_continuous(limits=c(-2.5,2.5))
ppnas

psci <- ggplot(data=d[which(d$journal=="Science"),], aes(x=year, y=Estimate, color=fl)) +
  theme_bw() +
  geom_point(position=position_dodge(.7)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2, position=position_dodge(.7)) +
  theme(
    legend.position = "right", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(0,2,0,0,"cm")
  ) +
  ggtitle("Science") +
  scale_x_continuous(limits=c(2010,2020)) +
  scale_y_continuous(limits=c(-2.5,2.5))
psci

# create grob of legend
tmp <- ggplot_gtable(ggplot_build(psci))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
plegend <- tmp$grobs[[leg]]
psci <- psci + theme(legend.position = "none")

fig1 <- as_ggplot(egg::ggarrange(pbio,pcell,pnat,pnejm,nrow=1))
fig2 <- as_ggplot(egg::ggarrange(pplos,ppnas,psci,nrow=1))
fig <- egg::ggarrange(fig1,fig2,nrow=2)


##### Plots for dif bw female and male for each journal #####

par(mar = c(5.1, 10.1, 4.1, 2.1))
plot(posthoc5, xlim=c(-0.25,0.25), main="Dif in logscore for all journals except bioRxiv")
d_allwithoutbio <- data.frame(confint(posthoc5)$confint)
d_allwithoutbio$position <- c("First","Last")
p_allwithoutbio <- ggplot(data=d_allwithoutbio, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-1,1)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("All journals except bioRxiv")
p_allwithoutbio

dbio2 <- data.frame(confint(posthoc8)$confint)
dbio2$position <- c("First","Last")
pbio2 <- ggplot(data=dbio2, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-2,2)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("bioRxiv")
  

dcell2 <- data.frame(confint(posthoccell)$confint)
dcell2$position <- c("First","Last")
pcell2 <- ggplot(data=dcell2, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-2,2)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("Cell")

dnat2 <- data.frame(confint(posthocnat)$confint)
dnat2$position <- c("First","Last")
pnat2 <- ggplot(data=dnat2, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-2,2)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("Nature")

dnejm2 <- data.frame(confint(posthocnejm)$confint)
dnejm2$position <- c("First","Last")
pnejm2 <- ggplot(data=dnejm2, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-2,2)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("NEJM")

dplos2 <- data.frame(confint(posthocplos)$confint)
dplos2$position <- c("First","Last")
pplos2 <- ggplot(data=dplos2, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-2,2)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("PLoS ONE")

dpnas2 <- data.frame(confint(posthocpnas)$confint)
dpnas2$position <- c("First","Last")
ppnas2 <- ggplot(data=dpnas2, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-2,2)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("PNAS")

dsci2 <- data.frame(confint(posthocsci)$confint)
dsci2$position <- c("First","Last")
psci2 <- ggplot(data=dsci2, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-2,2)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("Science")

pall <- egg::ggarrange(pbio2, pcell2, pnat2, pnejm2, pplos2, ppnas2, psci2, p_allwithoutbio,nrow=2)

dlogist4 <- data.frame(confint(posthoc4)$confint)
dlogist4$position <- c("First","Last")
plogist4 <- ggplot(data=dlogist4, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-20,20)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("All except bioRxiv")
plogist4

dlogist7 <- data.frame(confint(posthoc7)$confint)
dlogist7$position <- c("First","Last")
plogist7 <- ggplot(data=dlogist7, aes(x=position, y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2) +
  theme_bw() +
  scale_y_continuous(limits=c(-1,1)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ggtitle("bioRxiv")
plogist7

##### OLD #####
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


















fffmSci2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.fm.Science", nomatch = 0))
fffmSci2017 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2017.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2017.fm.Science", nomatch = 0))
fffmSci2016 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2016.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2016.fm.Science", nomatch = 0))
fffmSci2015 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2015.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2015.fm.Science", nomatch = 0))
fffmSci2014 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2014.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2014.fm.Science", nomatch = 0))
fffmSci2013 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2013.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2013.fm.Science", nomatch = 0))
fffmSci2012 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2012.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2012.fm.Science", nomatch = 0))
fffmSci2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.fm.Science", nomatch = 0))
lflmSci2018 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2018.lm.Science", nomatch = 0))
lflmSci2017 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2017.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2017.lm.Science", nomatch = 0))
lflmSci2016 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2016.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2016.lm.Science", nomatch = 0))
lflmSci2015 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2015.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2015.lm.Science", nomatch = 0))
lflmSci2014 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2014.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2014.lm.Science", nomatch = 0))
lflmSci2013 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2013.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2013.lm.Science", nomatch = 0))
lflmSci2012 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2012.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2012.lm.Science", nomatch = 0))
lflmSci2011 <- (match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5d2)), table = "puby.gend.jour2011.lm.Science", nomatch = 0))
Mat <- rbind(fffmSci2018, fffmSci2017, fffmSci2016, fffmSci2015, fffmSci2014, fffmSci2013, fffmSci2012, fffmSci2011,  
           lflmSci2018, lflmSci2017, lflmSci2016, lflmSci2015, lflmSci2014, lflmSci2013, lflmSci2012, lflmSci2011)
colnames(Mat) <- names(coef(m_linear5d2))

posthocd <- glht(m_linear5d2, linfct = Mat)
summary(posthocd)
plot(posthocd)

# prep for plot
d2 <- data.frame(confint(posthocd)$confint)
d2$year <- rep(2018:2011,times=2)
d2$position <- c(rep("First author", times=8),rep("Last author", times=8))
d2$journal <- as.factor(rep("Science",times=16))
tail(d2)

psci3 <- ggplot(data=d2, aes(x=year, y=Estimate, color=fl)) +
  theme_bw() +
  geom_point(position=position_dodge(.7)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=.2, position=position_dodge(.7)) +
  theme(
    legend.position = "right", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(0,2,0,0,"cm")
  ) +
  ggtitle("Science") +
  scale_x_continuous(limits=c(2010,2020)) +
  scale_y_continuous(limits=c(-2.5,2.5))
psci3


##########
# What's going on with Science???


meltdata <- readRDS("./DataAnalysis/AltmetricScoreModeling/meltdata.RDS")

scidata <- meltdata[which(meltdata$journal=="Science"),]
colnames(scidata) # pubyear is 8, score is 20, gender is 21
scidata <- scidata[,c(8,20,21)]
scidata2 <- aggregate(scidata$score~scidata$gender*scidata$pubyear,FUN=mean)
colnames(scidata2) <- c("gender","pubyear","meanscore")
scidata3 <- scidata2[which(!(scidata2$gender %in% c("f","l"))),]
ggplot(data=scidata3, aes(x=pubyear,y=meanscore,color=gender)) +
  geom_line() +
  ggtitle("Science")
 
natdata <- meltdata[which(meltdata$journal=="Nature"),]
colnames(natdata) # pubyear is 8, score is 20, gender is 21
natdata <- natdata[,c(8,20,21)]
natdata2 <- aggregate(natdata$score~natdata$gender*natdata$pubyear,FUN=mean)
colnames(natdata2) <- c("gender","pubyear","meanscore")
natdata3 <- natdata2[which(!(natdata2$gender %in% c("f","l"))),]
ggplot(data=natdata3, aes(x=pubyear,y=meanscore,color=gender)) +
  geom_line() +
  ggtitle("Nature")

plosdata <- meltdata[which(meltdata$journal=="PLoS ONE"),]
colnames(plosdata) # pubyear is 8, score is 20, gender is 21
plosdata <- plosdata[,c(8,20,21)]
plosdata2 <- aggregate(plosdata$score~plosdata$gender*plosdata$pubyear,FUN=mean)
colnames(plosdata2) <- c("gender","pubyear","meanscore")
plosdata3 <- plosdata2[which(!(plosdata2$gender %in% c("f","l"))),]
ggplot(data=plosdata3, aes(x=pubyear,y=meanscore,color=gender)) +
  geom_line() +
  ggtitle("PLOS")

pnasdata <- meltdata[which(meltdata$journal=="PNAS"),]
colnames(pnasdata) # pubyear is 8, score is 20, gender is 21
pnasdata <- pnasdata[,c(8,20,21)]
pnasdata2 <- aggregate(pnasdata$score~pnasdata$gender*pnasdata$pubyear,FUN=mean)
colnames(pnasdata2) <- c("gender","pubyear","meanscore")
pnasdata3 <- pnasdata2[which(!(pnasdata2$gender %in% c("f","l"))),]
ggplot(data=pnasdata3, aes(x=pubyear,y=meanscore,color=gender)) +
  geom_line() +
  ggtitle("PNAS")

biodata <- meltdata[which(meltdata$journal=="bioRxiv"),]
colnames(biodata) # pubyear is 8, score is 20, gender is 21
biodata <- biodata[,c(8,20,21)]
biodata2 <- aggregate(biodata$score~biodata$gender*biodata$pubyear,FUN=mean)
colnames(biodata2) <- c("gender","pubyear","meanscore")
biodata3 <- biodata2[which(!(biodata2$gender %in% c("f","l"))),]
ggplot(data=biodata3, aes(x=pubyear,y=meanscore,color=gender)) +
  geom_line() +
  ggtitle("bioRxiv")

nejmdata <- meltdata[which(meltdata$journal=="NEJM"),]
colnames(nejmdata) # pubyear is 8, score is 20, gender is 21
nejmdata <- nejmdata[,c(8,20,21)]
nejmdata2 <- aggregate(nejmdata$score~nejmdata$gender*nejmdata$pubyear,FUN=mean)
colnames(nejmdata2) <- c("gender","pubyear","meanscore")
nejmdata3 <- nejmdata2[which(!(nejmdata2$gender %in% c("f","l"))),]
ggplot(data=nejmdata3, aes(x=pubyear,y=meanscore,color=gender)) +
  geom_line() +
  ggtitle("NEJM")

celldata <- meltdata[which(meltdata$journal=="Cell"),]
colnames(celldata) # pubyear is 8, score is 20, gender is 21
celldata <- celldata[,c(8,20,21)]
celldata2 <- aggregate(celldata$score~celldata$gender*celldata$pubyear,FUN=mean)
colnames(celldata2) <- c("gender","pubyear","meanscore")
celldata3 <- celldata2[which(!(celldata2$gender %in% c("f","l"))),]
ggplot(data=celldata3, aes(x=pubyear,y=meanscore,color=gender)) +
  geom_line() +
  ggtitle("Cell")

#######

# max score in science is > 8000 (Obama paper)
lineardata5f <- lineardata5[which(lineardata5$score<2000),]
max(lineardata5f$score)
dim(lineardata5); dim(lineardata5f)
# this eliminated 73 articles
lineardata5f$puby.gend.jour <- interaction(lineardata5f$pubyear, lineardata5f$gender, lineardata5f$journal)

m_linear5f <- lm(logscore ~ puby.gend.jour +
                  propfemales +
                  numauthors +
                  pubmonth, # zia put -1 here on another version - to get rid of intercept? do i need to?
                data=lineardata5f)
summary(m_linear5f)
coef(m_linear5f)


fffmSci2018 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2018.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2018.fm.Science", nomatch = 0))
fffmSci2017 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2017.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2017.fm.Science", nomatch = 0))
fffmSci2016 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2016.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2016.fm.Science", nomatch = 0))
fffmSci2015 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2015.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2015.fm.Science", nomatch = 0))
fffmSci2014 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2014.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2014.fm.Science", nomatch = 0))
fffmSci2013 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2013.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2013.fm.Science", nomatch = 0))
fffmSci2012 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2012.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2012.fm.Science", nomatch = 0))
fffmSci2011 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2011.ff.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2011.fm.Science", nomatch = 0))
lflmSci2018 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2018.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2018.lm.Science", nomatch = 0))
lflmSci2017 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2017.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2017.lm.Science", nomatch = 0))
lflmSci2016 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2016.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2016.lm.Science", nomatch = 0))
lflmSci2015 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2015.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2015.lm.Science", nomatch = 0))
lflmSci2014 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2014.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2014.lm.Science", nomatch = 0))
lflmSci2013 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2013.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2013.lm.Science", nomatch = 0))
lflmSci2012 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2012.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2012.lm.Science", nomatch = 0))
lflmSci2011 <- (match(x = names(coef(m_linear5f)), table = "puby.gend.jour2011.lf.Science", nomatch = 0) -
                  match(x = names(coef(m_linear5f)), table = "puby.gend.jour2011.lm.Science", nomatch = 0))
Mat <- rbind(fffmSci2018, fffmSci2017, fffmSci2016, fffmSci2015, fffmSci2014, fffmSci2013, fffmSci2012, fffmSci2011,  
             lflmSci2018, lflmSci2017, lflmSci2016, lflmSci2015, lflmSci2014, lflmSci2013, lflmSci2012, lflmSci2011)
colnames(Mat) <- names(coef(m_linear5f))
Matf <- Mat[1:8,]
Matl <- Mat[9:16,]

posthocf <- glht(m_linear5f, linfct = Matf)
summary(posthocf)
plot(posthocf, main="dif in logscore for first author")

posthocl <- glht(m_linear5f, linfct = Matl)
summary(posthocl)
plot(posthocl, main="dif in logscore for last author")








#####
# robust model

fffmCell2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.ff.Cell", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.fm.Cell", nomatch = 0))
fffmCell2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.ff.Cell", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.fm.Cell", nomatch = 0))
lflmCell2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lf.Cell", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lm.Cell", nomatch = 0))
lflmCell2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lf.Cell", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lm.Cell", nomatch = 0))

fffmNat2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.ff.Nature", nomatch = 0) -
                  match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.fm.Nature", nomatch = 0))
fffmNat2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.ff.Nature", nomatch = 0) -
                  match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.fm.Nature", nomatch = 0))
lflmNat2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lf.Nature", nomatch = 0) -
                  match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lm.Nature", nomatch = 0))
lflmNat2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lf.Nature", nomatch = 0) -
                  match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lm.Nature", nomatch = 0))

fffmNEJM2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.ff.NEJM", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.fm.NEJM", nomatch = 0))
fffmNEJM2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.ff.NEJM", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.fm.NEJM", nomatch = 0))
lflmNEJM2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lf.NEJM", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lm.NEJM", nomatch = 0))
lflmNEJM2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lf.NEJM", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lm.NEJM", nomatch = 0))

fffmPLOS2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.ff.PLoS ONE", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.fm.PLoS ONE", nomatch = 0))
fffmPLOS2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.ff.PLoS ONE", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.fm.PLoS ONE", nomatch = 0))
lflmPLOS2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lf.PLoS ONE", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lm.PLoS ONE", nomatch = 0))
lflmPLOS2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lf.PLoS ONE", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lm.PLoS ONE", nomatch = 0))

fffmPNAS2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.ff.PNAS", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.fm.PNAS", nomatch = 0))
fffmPNAS2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.ff.PNAS", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.fm.PNAS", nomatch = 0))
lflmPNAS2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lf.PNAS", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lm.PNAS", nomatch = 0))
lflmPNAS2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lf.PNAS", nomatch = 0) -
                   match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lm.PNAS", nomatch = 0))

fffmSci2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.ff.Science", nomatch = 0) -
                  match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.fm.Science", nomatch = 0))
fffmSci2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.ff.Science", nomatch = 0) -
                  match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.fm.Science", nomatch = 0))
lflmSci2018 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lf.Science", nomatch = 0) -
                  match(x = names(coef(rm_linear5)), table = "puby.gend.jour2018.lm.Science", nomatch = 0))
lflmSci2011 <- (match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lf.Science", nomatch = 0) -
                  match(x = names(coef(rm_linear5)), table = "puby.gend.jour2011.lm.Science", nomatch = 0))

M <- rbind(fffmCell2018, fffmCell2011, lflmCell2018, lflmCell2011,
           fffmNat2018,  fffmNat2011,  lflmNat2018,  lflmNat2011,
           fffmNEJM2018, fffmNEJM2011, lflmNEJM2018, lflmNEJM2011,
           fffmPLOS2018, fffmPLOS2011, lflmPLOS2018, lflmPLOS2011,
           fffmPNAS2018, fffmPNAS2011, lflmPNAS2018, lflmPNAS2011,
           fffmSci2018,  fffmSci2011,  lflmSci2018,  lflmSci2011)
colnames(M) <- names(coef(rm_linear5))

posthocdif <- glht(rm_linear5, linfct = M)
summary(posthocdif)
plot(posthocdif)
