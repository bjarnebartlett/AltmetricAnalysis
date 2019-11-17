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
# install.packages("olsrr")
# install.packages("tidyverse")
# install.packages("broom")
# install.packages("car")
# install.packages("faraway")
# install.packages("visreg")
library(data.table)
library(lme4)
# library(sjPlot)
library(ggplot2)
# library(olsrr)
# library(tidyverse)
# library(broom)
# library(car)
# library(faraway)
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
# looked at figures again with this new dataset
# removes many unknowns but doesn't make a huge difference

# melt altmetric scores
nonscore_cols <- c("id","altmetric_id","authors","journal","type","doi",
                   "date","pubyear","pubmonth",
                   "males","females","unknown","propmales","propfemales","propunknown","numauthors",
                   "firstgender","firstgenderprob","lastgender","lastgenderprob",
                   "authors_format","cleaned_authors")
score_cols <- c("5day","1year")
meltdata <- melt(data, id.vars = nonscore_cols, measure.vars = score_cols)
meltdata$value <- as.numeric(meltdata$value)
colnames(meltdata)[23] <- "scoretime"
colnames(meltdata)[24] <- "score"
head(meltdata)

# melt gender
nongender_cols <- c("id","altmetric_id","authors","journal","type","doi",
                    "date","pubyear","pubmonth",
                    "males","females","unknown","propmales","propfemales","propunknown","numauthors",
                    "authors_format","cleaned_authors",
                    "scoretime","score")
gender_cols <- c("firstgender","lastgender")
meltdata <- melt(meltdata, id.vars = nongender_cols, measure.vars = gender_cols)
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

#################
### MODELLING ###
#################

# First attempt
# poisson because score is non-negative (theoretically) unbounded
# m_poisson1 <- glm(score ~  pubyear*gender*scoretime*journal +
#                       propfemales +
#                       numauthors +
#                       pubmonth,
#              data=meltdata,
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

#################

# On second thought, should split up because so many 0s
# First, do logistic on those without a score and those with a score
logisticdata <- meltdata
logisticdata[score>0]$score <- 1

# uncomment as needed
# m_logist1 <- glm(score ~  pubyear*gender*scoretime*journal +
#                             propfemales +
#                             numauthors +
#                             pubmonth,
#                           data=logisticdata,
#                           family=binomial)
# saveRDS(m_logist1,"./DataAnalysis/AltmetricScoreModeling/m_logist1.RDS")
# m_logist1 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_logist1.RDS")
summary(m_logist1)

# logistic regression has assumptions
# 1. outcome is binary
# 2. relationship between logit of outcome and each of predictors is linear
# 3. no influential values in continuous predictors
# 4. no high correlations (multicollinearity) between predictors
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

# 1. is outcome binary?
unique(logisticdata$score)
# yes, just 0 or 1

# 2. is logit x predictors linear? (could check by scatter plot or box Tidwell test)
probabilities <- predict(m_logist1, type="response")
logit <- log(probabilities/(1-probabilities))
ggplot(logisticdata, aes(logit,pubyear)) +
  geom_point() +
  theme_bw()
# pretty linear, no need to transform (not running with smooth loess because takes FOREVER to run with this many points)
# also this is an int, not a numeric, so it's fine
ggplot(logisticdata, aes(logit,propfemales)) +
  geom_point() +
  theme_bw()
# not really linear...  i don't know what to make of this...
ggplot(logisticdata, aes(logit,numauthors)) +
  geom_point() +
  theme_bw()
# kind of odd but pretty linear
# this is also an int, not numeric

# actually, zia said the way they go about doing this on the sthda website is wrong
# it's not the predictor vs the logit that matters, what matters is the partial residuals
# that is, you need to take into account the effect of everything else on the response, 
# so can't just do predictor x logit(response)

visreg(m_logist1, "propfemales", type="conditional")
# looks linear so there's no point in transforming the propfemales variable
# also this is the only one that is actually continuous so only one that we need to check

# 3. are there influential values in continuous predictors?
plot(m_logist1, 4)
# cook's distance shows outliers
plot(m_logist1, 5)
# lots of points with high leverage, and lots of points with high std residuals, but none outside cook's distance lines

# 4. is there high correlation (multicollinearity) between predictors?
# need to look only at continuous vars, so can't use car::vif(m_logist1)
m_logist1_contvars <- data.frame(logisticdata$pubyear,logisticdata$numauthors,logisticdata$propfemales)
head(m_logist1_contvars)
dim(m_logist1_contvars)
faraway::vif(m_logist1_contvars)
# values are close to 1, which means the predictors are not correlated

##########

# Next, do linear regression on those with a score
lineardata <- meltdata[score>0]
lineardata$logscore <- log(lineardata$score)

# uncomment as needed
# m_linear1 <- lm(logscore ~ pubyear*gender*scoretime*journal +
#                              propfemales +
#                              numauthors +
#                              pubmonth,
#                            data=lineardata)
# saveRDS(m_linear1,"./DataAnalysis/AltmetricScoreModeling/m_linear1.RDS")
# m_linear1 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear1.RDS")
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

# could try to solve linearity and heteroscedasticity issues by 

lineardata$sqrtscore <- sqrt(lineardata$score)

# m_linear2 <- lm(sqrtscore ~ pubyear*gender*scoretime*journal +
#                               propfemales +
#                               numauthors +
#                               pubmonth,
#                             data=lineardata)
# saveRDS(m_linear2,"./DataAnalysis/AltmetricScoreModeling/m_linear2.RDS")
# m_linear2 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear2.RDS")
summary(m_linear2)

par(mfrow=c(2,2))
plot(m_linear2)

# nope, the original log transform was better
# how about removing the points close to cook's line from the original version
which(hatvalues(m_linear1)>0.95)
lineardata2 <- lineardata[-c(35,207,275,70728,70787),]
m_linear3 <- lm(logscore ~ pubyear*gender*scoretime*journal +
                             propfemales +
                             numauthors +
                             pubmonth,
                           data=lineardata2)
saveRDS(m_linear3,"./DataAnalysis/AltmetricScoreModeling/m_linear3.RDS")
m_linear3 <- readRDS("./DataAnalysis/AltmetricScoreModeling/m_linear3.RDS")
summary(m_linear3)

par(mfrow=c(2,2))
plot(m_linear3)

# not much change, looks to me like the assumptions are more or less met as described above

