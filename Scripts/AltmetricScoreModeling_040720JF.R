################################################
################################################
########## MODELLING ALTMETRIC SCORES ##########
################################################
################################################

# 2020.04.02
# Julie Fortin
# julie.fortin@ubc.ca

###############
### SUMMARY ###
###############

# We are looking at altmetric scores of publications in 7 idiotypic journals
# And looking at the gender of the authors of those publications
# To see if there are any interesting patterns in terms of altmetric scores & gender bias


##############
### SET UP ###
##############

setwd("/Users/juliefortin/Documents/UBC/Projects/Altmetrics/AltmetricAnalysis/")

# install.packages("data.table")
# install.packages("visreg")
# install.packages("tidyverse")
# install.packages("multcomp")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("sjPlot")
# install.packages("car")
# install.packages("robustbase")
# install.packages("quantreg")
library(data.table)
library(visreg)
library(tidyverse)
library(multcomp)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(car)
library(robustbase)
library(quantreg)


#################
### FUNCTIONS ###
#################

## This function reads each journal's file, merges them all into a single dataframe and returns the merged dataframe
afn.loaddata <- function(files) {
  
  # Create a merged dataset from all journals
  alldata <- c()
  for (file in files) {
    filename <- paste("./GenderizedData/",file,sep="")
    journaldata <- fread(filename)
    alldata <- rbind(alldata, journaldata)
  }
  
  # Make sure variables are correct class
  alldata$pubmonth <- as.factor(alldata$pubmonth)
  alldata$firstgender[which(alldata$firstgender=="")] <- "unknown"
  alldata$firstgender <- as.factor(alldata$firstgender)
  alldata$lastgender[which(alldata$lastgender=="")] <- "unknown"
  alldata$lastgender <- as.factor(alldata$lastgender)
  alldata$type <- as.factor(alldata$type)
  alldata$`5day` <- as.numeric(alldata$`5day`)
  alldata$`1year` <- as.numeric(alldata$`1year`)
  alldata$journal[which(alldata$journal=="Proceedings of the National Academy of Sciences of the United States of America")] <- "PNAS"
  alldata$journal[which(alldata$journal=="New England Journal of Medicine")] <- "NEJM"
  alldata$journal <- as.factor(alldata$journal)
  alldata$pubyear <- as.factor(alldata$pubyear)
  
  # Change "None" or NAs to 0
  alldata[,9:20][alldata[,9:20]=="None"] <- 0
  alldata[,9:20][is.na(alldata[,9:20])] <- 0
  
  # No point keeping articles we are unable to genderize
  alldata <- alldata[-which(authors_format=="nofirstname"),]
  
  return(alldata)
  
}

## This function reads the merged dataframe, separates first author male/female and last author male/female, and returns the melted dataframe
afn.cleanmergeddata <- function(datatoclean) {
  
  # melt dataframe by author gender
  nongender_cols <- c("id","altmetric_id","authors","journal","type","doi","date","pubyear","pubmonth",
                      "males","females","unknown","propmales","propfemales","propunknown","numauthors","authors_format","cleaned_authors",
                      "5day","1year")
  gender_cols <- c("firstgender","lastgender")
  cleandata <- melt(datatoclean, id.vars=nongender_cols, measure.vars=gender_cols) # each article gets two rows: one for first author one for last author
  cleandata$gender <- paste(substr(cleandata$variable,1,1),substr(cleandata$value,1,1),sep="") %>%
    as.factor() # this codes gender as ff-first author is female, fm-first author is male, lf-last author is female, lm-last author is male
  cleandata$variable <- NULL # these have now been accounted for in new "gender"variable
  cleandata$value <- NULL
  
  # for articles with only 1 author, treat as first author, but don't duplicate as last author
  toremove <- which(cleandata$numauthors==1 & (cleandata$gender=="l"|cleandata$gender=="lf"|cleandata$gender=="lm"))
  cleandata <- cleandata[-toremove,]
  
  return(cleandata)
  
}

## This function creates a matrix to help simultaneously test linear hypotheses 
## (difference in score vs no-score between first/last author female/male in different journals in different years)
afn.createlogisticmatrix <- function(model, journals) {
  
  matrix <- c()
  rownames <- c()
  
  if (journals[1]=="bioRxiv") {
    years <- 2015:2018
  } else {
    years <- 2011:2018
  }
  
  for (journal in journals) {
    
    for (genderindex in 1:2) {
      
      gender1 <- c("ff","lf")[genderindex]
      gender2 <- c("fm","lm")[genderindex]
      
      for (year in years) {
        
        if (journal == "bioRxiv") {
          table1 <- paste("puby.gend",year,".",gender1,sep="")
          table2 <- paste("puby.gend",year,".",gender2,sep="")
        } else {
          table1 <- paste("puby.gend.jour",year,".",gender1,".",journal,sep="")
          table2 <- paste("puby.gend.jour",year,".",gender2,".",journal,sep="")
        }
        
        row <- match(x=names(coef(model)), table=table1, nomatch=0) - match(x=names(coef(model)), table=table2, nomatch=0)
        rowname <- paste(gender1,gender2,journal,year,sep="")
        
        matrix <- rbind(matrix, row)
        rownames <- c(rownames,rowname)
        
      }
    }
  }
  
  colnames(matrix) <- names(coef(model))
  rownames(matrix) <- rownames
  
  return(matrix)
  
}

## This function creates a matrix to help simultaneously test linear hypotheses 
## (difference in log(score) between first/last author female/male in different journals in different years)
afn.createlinearmatrix <- function(model, journals) {
  
  matrix <- c()
  rownames <- c()
  
  if (journals[1]=="bioRxiv") {
    years <- 2015:2018
  } else {
    years <- 2011:2018
  }
  
  for (journal in journals) {
    
    for (genderindex in 1:2) {
      
      gender1 <- c("ff","lf")[genderindex]
      gender2 <- c("fm","lm")[genderindex]
      
      for (year in years) {
        
        if (journal == "bioRxiv") {
          table1 <- paste("puby.gend",year,".",gender1,sep="")
          table2 <- paste("puby.gend",year,".",gender2,sep="")
        } else {
          table1 <- paste("puby.gend.jour",year,".",gender1,".",journal,sep="")
          table2 <- paste("puby.gend.jour",year,".",gender2,".",journal,sep="")
        }
        
        row <- match(x=names(coef(model)), table=table1, nomatch=0) - match(x=names(coef(model)), table=table2, nomatch=0)
        rowname <- paste(gender1,gender2,journal,year,sep="")
        
        matrix <- rbind(matrix, row)
        rownames <- c(rownames,rowname)
        
      }
    }
  }
  
  colnames(matrix) <- names(coef(model))
  rownames(matrix) <- rownames
  
  return(matrix)
  
}

## This function makes a plot of the difference between male & female author scores through the time series, for a single journal
afn.makeaplot <- function(df, journal, author) {
  
  df <- df[which(df$journal==journal & df$author==author),]
  
  plot <- ggplot(data=df, aes(x=year, y=Estimate)) +
    theme_bw() +
    geom_ribbon(aes(ymin=lwr,ymax=upr), fill="#6aa4c8") +
    geom_line() +
    theme(
      legend.position = "none", legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    ggtitle(journal) +
    coord_cartesian(xlim=c(2010,2020), ylim=c(-2.5,2.5)) + # this here so that when values go beyond -2.5,2.5 the error bars go to the fig margins
    scale_x_continuous(name="Year", breaks=c(2010,2012,2014,2016,2018,2020)) +
    scale_y_continuous(name="Difference in log odds ratio") +
    geom_hline(yintercept=0, linetype="dashed",color="dimgray") #+
    # scale_x_continuous(name="Year", breaks=c(2010,2012,2014,2016,2018,2020), limits=c(2010,2020)) +
    # scale_y_continuous(name="Difference in probability", limits=c(-2.5,2.5))
  
  plot
  
  return(plot)
  
}

## This function puts together the plots from afn.makeaplot for all journals to generate a figure
afn.makefigure <- function(df, author, type) {
  
  journals <- c("bioRxiv","Cell","Nature","NEJM","PLoS ONE","PNAS","Science")
  plots <- vector(mode="list", length=length(journals))
  if (type == "logistic") {
    yaxis = "Difference in log odds ratio"
  } else if (type == "linear" | type == "quantile") {
    yaxis = "Difference in log(score)"
  }
  
  for (i in 1:7) {
    journal <- journals[i]
    plots[[i]] <- afn.makeaplot(df,journal,author)
  }
  
  fig <- as_ggplot(egg::ggarrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],nrow=2))
  fig <- annotate_figure(fig, 
                         bottom="Year",
                         left=yaxis,
                         top=text_grob(paste(author,": difference between female and male scores",sep=""),size=16))
  
  return(fig)
  
}


#################
### LOAD DATA ###
#################

## We have one csv file per journal that contains article information, altmetric score and gender information
## Read all of them in and merge into one dataframe
genderizedfiles <- list.files("./GenderizedData/")
mergeddata <- afn.loaddata(genderizedfiles)


###################
### EXPLORATION ###
###################

## Total number of articles we were able to genderize
totalgenderized <- dim(mergeddata)[[1]]
totalgenderized
# 208,804 articles total
totalscore0 <- dim(mergeddata[which(`1year`==0),])[[1]]
totalscore0
# 139,035 articles with a 1-year altmetric score of 0
propscore0 <- totalscore0/totalgenderized
propscore0
# 66.5% of articles have a 1-year altmetric score of 0

## Quantiles
quantile(mergeddata$`1year`[which(mergeddata$`1year`>0)],c(0.1,0.25,0.5,0.75,0.9,0.99))
# Median score of articles with a 1-year score > 0 is 3.35, 90th percentile is 52.5

## Altmetric scores
attach(mergeddata)
hist(`5day`, col="#6aa4c8")
summary(`5day`)
summary(`1year`)
hist(mergeddata$`1year`, col="#6aa4c8", main="1-year scores",xlab="score")
hist(log(mergeddata$`1year`), col="#6aa4c8", main="log(1-year scores)",xlab="log(score)")
# Lots of 0s, log transforming 1-year scores makes it look a bit closer to normal

## Article timing
boxplot(`1year`~pubmonth, data=mergeddata, main="Month of publication")
# No obvious seasonal trend
hist(as.numeric(pubyear), col="#6aa4c8")
# All years 2011-2018 represented

## Author genders
barplot(table(firstgender), col=c("#ffb8a2","#1c3678","#e9edee"), main="First author gender", ylim=c(0,150000))
barplot(table(lastgender), col=c("#ffb8a2","#1c3678","#e9edee"), main="Last author gender", ylim=c(0,150000))
# More male than female first authors as expected, even more pronounced for last authors
slices <- c(mean(propunknown), mean(propmales),mean(propfemales))
pie(slices, labels=c("Unknown", "Male","Female"), main="Author list gender balance", col=c("#e9edee","#1c3678","#ffb8a2"))
# Gender balance more malethan female authors

## Number of authors
hist(numauthors, col="#6aa4c8")
# Many articles with very few authors, few articles with very many authors
hist(mergeddata[numauthors<25]$numauthors, col="#6aa4c8", main="Histogram of articles with <25 authors")
# Still very large number of articles with 1-2 authors, log transforming makes it look a bit closer to normal
summary(numauthors)

## Type of article
barplot(table(type), col="#6aa4c8", las=2, main="Type of article")
# Almost all are articles (as opposed to books, chapters, news, ...)

## Journal
pie(table(journal), labels=c("bioRxiv","Cell","Nature","NEJM","PLoS ONE","PNAS","Science"), main="Journal",
    col=c("#1a9988","#e9edee","#6aa4c8","#eb5600","#a2ffe8","#1c3678","#ffb8a2"))
# More than half of articles are from PLOS ONE

## Collinearity
plot(numauthors, propfemales)
# Not much evidence of a relationship between number of authors and proportion of female authors

detach(mergeddata)


#################
### DATA PREP ###
#################

## Melt data by first/last author gender
meltdata <- afn.cleanmergeddata(mergeddata)

## In data exploration, we saw how a very large number of articles have scores of 0 after 1-year
## We will only use 1-year scores then (as this problem was worse with say 5-day scores)
## But we will separate data into articles that have 0 vs >0 after 1 year and run logistic model
binarydata <- meltdata
binarydata$score <- binarydata$`1year`
binarydata[score>0]$score <- 1

## bioRxiv started in 2015, so the time series of data for bioRxiv is shorter than the rest of the journals
## So we will code it separately
binarydata_bio <- binarydata[journal=="bioRxiv" & pubyear %in% c(2015:2018)]
binarydata_bio$pubyear <- factor(binarydata_bio$pubyear)
binarydata_oth <- binarydata[journal!="bioRxiv"]

## Now, of all articles that have scores >0 after 1 year
## We can take these articles and run linear model
lineardata <- meltdata
lineardata$score <- lineardata$`1year`
lineardata <- lineardata[score>0]
lineardata$logscore <- log(lineardata$score) # from data exploration, we saw log-transforming the score helped

## Again, separate into bioRxiv and non-bioRxiv journals
lineardata_bio <- lineardata[journal=="bioRxiv" & pubyear %in% c(2015:2018)]
lineardata_oth <- lineardata[journal!="bioRxiv"]


#################
### MODELLING ###
#################

## Logistic (binary score/no-score) model for bioRxiv only
binarydata_bio$puby.gend <- interaction(binarydata_bio$pubyear, binarydata_bio$gender) # create an interaction term
logisticmodel_bio <- glm(score ~  puby.gend +
                                  propfemales +
                                  numauthors +
                                  pubmonth,
                         data=binarydata_bio,
                         family=binomial)
# saveRDS(logisticmodel_bio, "./Models/logisticmodel_bio.RDS")
# logisticmodel_bio <- readRDS("./Models/logisticmodel_bio.RDS")
summary(logisticmodel_bio)

## Logistic model for all other journals
binarydata_oth$puby.gend.jour <- interaction(binarydata_oth$pubyear, binarydata_oth$gender, binarydata_oth$journal)
logisticmodel_oth <- glm(score ~  puby.gend.jour +
                                  propfemales +
                                  numauthors +
                                  pubmonth,
                         data=binarydata_oth,
                         family=binomial)
# saveRDS(logisticmodel_oth, "./Models/logisticmodel_oth.RDS")
# logisticmodel_oth <- readRDS("./Models/logisticmodel_oth.RDS")
summary(logisticmodel_oth)

## Linear (score > 0) model for bioRxiv only
lineardata_bio$puby.gend <- interaction(lineardata_bio$pubyear, lineardata_bio$gender)
linearmodel_bio <- lm(logscore ~ puby.gend +
                                 propfemales +
                                 numauthors +
                                 pubmonth,
                      data=lineardata_bio)
# saveRDS(linearmodel_bio, "./Models/linearmodel_bio.RDS")
# linearmodel_bio <- readRDS("./Models/linearmodel_bio.RDS")
summary(linearmodel_bio)

## Linear model for all other journals
lineardata_oth$puby.gend.jour <- interaction(lineardata_oth$pubyear, lineardata_oth$gender, lineardata_oth$journal)
linearmodel_oth <- lm(logscore ~ puby.gend.jour +
                                 propfemales +
                                 numauthors +
                                 pubmonth,
                      data=lineardata_oth)
# saveRDS(linearmodel_oth, "./Models/linearmodel_oth.RDS")
# linearmodel_oth <- readRDS("./Models/linearmodel_oth.RDS")
summary(linearmodel_oth)


######################
### MODEL CHECKING ###
######################

## Let's check that the assumptions for logistic models hold

## 1. is outcome binary?
unique(binarydata_bio$score)
unique(binarydata_oth$score)
# yes, just 0 or 1. And as we've defined it, 0 = score=0, 1 = score>0

## 2. is logit x predictors linear? look at partial residuals for continuous variables
crPlots(logisticmodel_bio, "propfemales")
crPlots(logisticmodel_bio, "numauthors")
crPlots(logisticmodel_oth, "propfemales")
crPlots(logisticmodel_oth, "numauthors")
# There are some outliers, also shown in residual vs leverage plot

## 3. are there influential values in continuous predictors?
plot(logisticmodel_bio, 5)
plot(logisticmodel_oth, 5)
# lots of points with high leverage, and lots of points with high std residuals, but none outside cook's distance lines

## 4. is there high correlation (multicollinearity) between predictors?
faraway::vif(data.frame(binarydata_bio$numauthors,binarydata_bio$propfemales))
faraway::vif(data.frame(binarydata_oth$numauthors,binarydata_oth$propfemales))
# values are close to 1, which means the predictors are not correlated

## Conclusion: there are some potential issues with outliers (seen in partial residual & residual vs leverage plots)
## Let's see if estimates are robust
logisticmodel_bio_rob <- glmrob(score ~  puby.gend + propfemales + numauthors + pubmonth, data=binarydata_bio, family=binomial)
logisticmodel_oth_rob <- glmrob(score ~  puby.gend.jour + propfemales + numauthors + pubmonth, data=binarydata_oth, family=binomial)
# Get singularity error, which has to do with the interaction term (error disappears if interaction is removed)
# Instead, to see if the outliers really have an impact we can remove the points that are close to cook's lines
# outliers are 45856, 19297, 42789
binarydata_bio_outliers <- binarydata_bio[-c(45856,19297,42789,22487),]
logisticmodel_bio_outliers <- glm(score ~  puby.gend +
                                   propfemales +
                                   numauthors +
                                   pubmonth,
                                 data=binarydata_bio_outliers,
                                 family=binomial)
summary(logisticmodel_bio_outliers)
summary(logisticmodel_bio)
# Very little difference in coefficients
binarydata_oth_outliers <- binarydata_oth[-c(122252,190864),]
logisticmodel_oth_outliers <- glm(score ~  puby.gend.jour +
                                    propfemales +
                                    numauthors +
                                    pubmonth,
                                  data=binarydata_oth_outliers,
                                  family=binomial)
plot(logisticmodel_oth_outliers, 5)
summary(logisticmodel_oth_outliers)
summary(logisticmodel_oth)
# Very little difference in coefficients
# So not too much concern about outliers in logistic models

## Now let's check if linear assumptions hold
par(mfrow=c(2,2)) # plot in a 2x2 square
plot(linearmodel_bio)
plot(linearmodel_oth)
par(mfrow=c(1,1)) # change it back for future plots
# 1. is it linear? 
# residual plot looks more or less equal on either side of 0 except straight line due to truncated data (scores > 0)
# 2. is it normal?
# qq-plot 
# data can be assumed to be normal
# 3. is it homoskedastic?
# scale-location plot lines looks pretty flat until fitted value of about 3.5 for bio, 5 for others
# homoskedastic for the majority of points, except for the few very large fitted values
# 4. are there highly influential outliers?
# the residuals vs leverage plots show no points outside of Cook's distance
# it looks like there are not points we need to worry about

## Conclusion: the main concern here is that, because scores are truncated (scores > 0), the error distribution is not normal
## We could overcome this issue by defining the model with a different distribution,
## or fitting a distribution-free model (quantile methods)
## or doing inference with non-parametric bootstrapping
## or other specific approaches developed for non-parametric inference
quantilemodel_bio <- rq(logscore ~ puby.gend + propfemales + numauthors + pubmonth, data=lineardata_bio, tau=0.5)
summary(quantilemodel_bio)
quantilemodel_oth <- rq(logscore ~ puby.gend.jour + propfemales + numauthors + pubmonth, data=lineardata_oth, tau=0.5)
summary(quantilemodel_oth)
# warning non-positive fis:
# This is generally harmless, leading to a somewhat conservative (larger) estimate of the standard errors, however if the
# reported number of non-positive fis is large relative to the sample size then it is an indication of misspecification of the model.
# (http://www.econ.uiuc.edu/~roger/research/rq/FAQ)
# In our case, this is way smaller than sample size, so should be okay


################
### POST HOC ###
################

## Goodness of fit

1 - logisticmodel_bio$deviance / logisticmodel_bio$null.deviance # 0.6373 (https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms)
1 - logisticmodel_oth$deviance / logisticmodel_oth$null.deviance # 0.2909 # doesn't work if out-of-sample
summary(linearmodel_bio) # Mult 0.05691 Adj 0.05587
summary(linearmodel_oth) # Mult 0.2611 Adj 0.2589

# For quantile regression
# https://stats.stackexchange.com/questions/129200/r-squared-in-quantile-regression
# suggests Koenker & Machado 1999 method
rho <- function(u,tau=.5)u*(tau - (u < 0))
quantilemodel_bio_0 <- rq(logscore ~ 1, data=lineardata_bio, tau=0.5)
R1 <- 1 - quantilemodel_bio$rho/quantilemodel_bio_0$rho # 0.03
quantilemodel_oth_0 <- rq(logscore ~ 1, data=lineardata_oth, tau=0.5)
R1 <- 1 - quantilemodel_oth$rho/quantilemodel_oth_0$rho # 0.14
# both a bit lower than the regular lm

## Multiple comparisons
## We start with hypotheses that the difference in score/probability of obtaining a score
## between male and female first and last authors is 0
## We can use glht from the multcomp package to test these hypotheses for each journal*year combination
## In order to use glht to test multiple hypotheses at once, we need to create a matrix of the hypotheses

logisticmatrix_bio <- afn.createlogisticmatrix(logisticmodel_bio,"bioRxiv")
logisticposthoc_bio <- glht(logisticmodel_bio, linfct = logisticmatrix_bio)
summary(logisticposthoc_bio)
plot(logisticposthoc_bio)
confint(logisticposthoc_bio)
# bioRxiv 2018 has very wide confidence intervals because there are only 2 articles that have scores of 0, and 7772 with scores >0

logisticmatrix_oth <- afn.createlogisticmatrix(logisticmodel_oth,c("Cell","Nature","NEJM","PLoS ONE","PNAS","Science"))
logisticposthoc_oth <- glht(logisticmodel_oth, linfct = logisticmatrix_oth)
summary(logisticposthoc_oth)
plot(logisticposthoc_oth)
confint(logisticposthoc_oth)

linearmatrix_bio <- afn.createlinearmatrix(linearmodel_bio,"bioRxiv")
linearposthoc_bio <- glht(linearmodel_bio, linfct = linearmatrix_bio)
summary(linearposthoc_bio)
plot(linearposthoc_bio)
confint(linearposthoc_bio)

linearmatrix_oth <- afn.createlinearmatrix(linearmodel_oth,c("Cell","Nature","NEJM","PLoS ONE","PNAS","Science"))
linearposthoc_oth <- glht(linearmodel_oth, linfct = linearmatrix_oth)
summary(linearposthoc_oth)
plot(linearposthoc_oth)
confint(linearposthoc_oth)

quantilematrix_bio <- afn.createlinearmatrix(quantilemodel_bio,"bioRxiv")
quantileposthoc_bio <- glht(quantilemodel_bio, linfct = quantilematrix_bio, coef=coef(quantilemodel_bio), vcov=summary.rq(quantilemodel_bio, se="boot", R=1000, cov=T)$cov)
summary(quantileposthoc_bio)
plot(quantileposthoc_bio)
confint(quantileposthoc_bio)

quantilematrix_oth <- afn.createlinearmatrix(quantilemodel_oth,c("Cell","Nature","NEJM","PLoS ONE","PNAS","Science"))
quantileposthoc_oth <- glht(quantilemodel_oth, linfct = quantilematrix_oth, coef=coef(quantilemodel_oth), vcov=summary.rq(quantilemodel_oth, se="boot", R=1000, cov=T)$cov)
summary(quantileposthoc_oth)
plot(quantileposthoc_oth)
confint(quantileposthoc_oth)


#########################
### PLOT INTERACTIONS ###
#########################

# Prep dfs for plots
logisticdf <- rbind(data.frame(confint(logisticposthoc_bio)$confint), data.frame(confint(logisticposthoc_oth)$confint))
logisticdf$author <- substr(rownames(logisticdf),1,1)
logisticdf$author[logisticdf$author=="f"] <- "First author"
logisticdf$author[logisticdf$author=="l"] <- "Last author"
logisticdf$author <- factor(logisticdf$author)
logisticdf$year <- as.numeric(substr(rownames(logisticdf),nchar(rownames(logisticdf))-3,nchar(rownames(logisticdf)))) # last 4 chars are year
logisticdf$journal <- as.factor(substr(rownames(logisticdf),5,nchar(rownames(logisticdf))-4)) # middle chars are journal

lineardf <- rbind(data.frame(confint(linearposthoc_bio)$confint), data.frame(confint(linearposthoc_oth)$confint))
lineardf$author <- substr(rownames(lineardf),1,1)
lineardf$author[lineardf$author=="f"] <- "First author"
lineardf$author[lineardf$author=="l"] <- "Last author"
lineardf$author <- factor(lineardf$author)
lineardf$year <- as.numeric(substr(rownames(lineardf),nchar(rownames(lineardf))-3,nchar(rownames(lineardf)))) # last 4 chars are year
lineardf$journal <- as.factor(substr(rownames(lineardf),5,nchar(rownames(lineardf))-4)) # middle chars are journal

quantiledf <-  rbind(data.frame(confint(quantileposthoc_bio)$confint), data.frame(confint(quantileposthoc_oth)$confint))
quantiledf$author <- substr(rownames(quantiledf),1,1)
quantiledf$author[quantiledf$author=="f"] <- "First author"
quantiledf$author[quantiledf$author=="l"] <- "Last author"
quantiledf$author <- factor(quantiledf$author)
quantiledf$year <- as.numeric(substr(rownames(quantiledf),nchar(rownames(quantiledf))-3,nchar(rownames(quantiledf)))) # last 4 chars are year
quantiledf$journal <- as.factor(substr(rownames(quantiledf),5,nchar(rownames(quantiledf))-4)) # middle chars are journal

# Logistic
logisticfig_first <- afn.makefigure(logisticdf, "First author", "logistic")
ggsave("./Figures/logistic_firstauthor.tiff",logisticfig_first,device=tiff(),width=8,height=6,units="in",dpi=900)
logisticfig_last <- afn.makefigure(logisticdf, "Last author", "logistic")
ggsave("./Figures/logistic_lastauthor.tiff",logisticfig_last,device=tiff(),width=8,height=6,units="in",dpi=900)

# Linear
linearfig_first <- afn.makefigure(lineardf, "First author", "linear")
ggsave("./Figures/linear_firstauthor.tiff",linearfig_first,device=tiff(),width=8,height=6,units="in",dpi=900)
linearfig_last <- afn.makefigure(lineardf, "Last author", "linear")
ggsave("./Figures/linear_lastauthor.tiff",linearfig_last,device=tiff(),width=8,height=6,units="in",dpi=900)

# Quantile
quantilefig_first <- afn.makefigure(quantiledf, "First author", "quantile")
ggsave("./Figures/quantile_firstauthor.tiff",quantilefig_first,device=tiff(),width=8,height=6,units="in",dpi=900)
quantilefig_last <- afn.makefigure(quantiledf, "Last author", "quantile")
ggsave("./Figures/quantile_lastauthor.tiff",quantilefig_last,device=tiff(),width=8,height=6,units="in",dpi=900)


#######################
### PLOT OTHER VARS ###
#######################

set_theme(theme_bw())

# Logisitc - bioRxiv
logistic_month_bio <- plot_model(logisticmodel_bio,type="pred",terms=c("pubmonth"),
                               axis.title=c("Month of publication","Probability of score"),
                               title="")
ggsave("./Figures/logistic_month_bio.tiff",logistic_month_bio,device=tiff(),width=8,height=6,units="in",dpi=900)
logistic_propf_bio <- plot_model(logisticmodel_bio,type="pred",terms=c("propfemales"),
                               axis.title=c("Proportion of female authors","Probability of score"),
                               title="")
ggsave("./Figures/logistic_propf_bio.tiff",logistic_propf_bio,device=tiff(),width=8,height=6,units="in",dpi=900)
logistic_numauth_bio <- plot_model(logisticmodel_bio,type="pred",terms=c("numauthors"),
                                 axis.title=c("Number of authors","Probability of score"),
                                 title="")
ggsave("./Figures/logistic_numauth_bio.tiff",logistic_numauth_bio,device=tiff(),width=8,height=6,units="in",dpi=900)

# Logisitc - non-bioRxiv
logistic_month_oth <- plot_model(logisticmodel_oth,type="pred",terms=c("pubmonth"),
                                 axis.title=c("Month of publication","Probability of score"),
                                 title="")
ggsave("./Figures/logistic_month_oth.tiff",logistic_month_oth,device=tiff(),width=8,height=6,units="in",dpi=900)
logistic_propf_oth <- plot_model(logisticmodel_oth,type="pred",terms=c("propfemales"),
                                 axis.title=c("Proportion of female authors","Probability of score"),
                                 title="")
ggsave("./Figures/logistic_propf_oth.tiff",logistic_propf_oth,device=tiff(),width=8,height=6,units="in",dpi=900)
logistic_numauth_oth <- plot_model(logisticmodel_oth,type="pred",terms=c("numauthors"),
                                   axis.title=c("Number of authors","Probability of score"),
                                   title="")
ggsave("./Figures/logistic_numauth_oth.tiff",logistic_numauth_oth,device=tiff(),width=8,height=6,units="in",dpi=900)


# Linear - bioRxiv
linear_month_bio <- plot_model(linearmodel_bio,type="pred",terms=c("pubmonth"),
                               axis.title=c("Month of publication","log(score)"),
                               title="")
ggsave("./Figures/linear_month_bio.tiff",linear_month_bio,device=tiff(),width=8,height=6,units="in",dpi=900)
linear_propf_bio <- plot_model(linearmodel_bio,type="pred",terms=c("propfemales"),
                               axis.title=c("Proportion of female authors","log(score)"),
                               title="")
ggsave("./Figures/linear_propf_bio.tiff",linear_propf_bio,device=tiff(),width=8,height=6,units="in",dpi=900)
linear_numauth_bio <- plot_model(linearmodel_bio,type="pred",terms=c("numauthors"),
                                 axis.title=c("Number of authors","log(score)"),
                                 title="")
ggsave("./Figures/linear_numauth_bio.tiff",linear_numauth_bio,device=tiff(),width=8,height=6,units="in",dpi=900)

# Linear - non-bioRxiv
linear_month_oth <- plot_model(linearmodel_oth,type="pred",terms=c("pubmonth"),
           axis.title=c("Month of publication","log(score)"),
           title="")
ggsave("./Figures/linear_month_oth.tiff",linear_month_oth,device=tiff(),width=8,height=6,units="in",dpi=900)
linear_propf_oth <- plot_model(linearmodel_oth,type="pred",terms=c("propfemales"),
           axis.title=c("Proportion of female authors","log(score)"),
           title="")
ggsave("./Figures/linear_propf_oth.tiff",linear_propf_oth,device=tiff(),width=8,height=6,units="in",dpi=900)
linear_numauth_oth <- plot_model(linearmodel_oth,type="pred",terms=c("numauthors"),
           axis.title=c("Number of authors","log(score)"),
           title="")
ggsave("./Figures/linear_numauth_oth.tiff",linear_numauth_oth,device=tiff(),width=8,height=6,units="in",dpi=900)

# Plot hist of freq in different years to show that there are fewer articles considered in 2018
# This is just total # of articles in each category
freqbio <- summary(mergeddata[journal=="bioRxiv",]$pubyear)
freqcell <- summary(mergeddata[journal=="Cell",]$pubyear)
freqnat <- summary(mergeddata[journal=="Nature",]$pubyear)
freqnejm <- summary(mergeddata[journal=="NEJM",]$pubyear)
freqplos <- summary(mergeddata[journal=="PLoS ONE",]$pubyear)
freqpnas <- summary(mergeddata[journal=="PNAS",]$pubyear)
freqsci <- summary(mergeddata[journal=="Science",]$pubyear)
freqdata <- rbind(freqbio, freqcell, freqnat, freqnejm, freqplos, freqpnas, freqsci)
rownames(freqdata) <- c("bioRxiv", "Cell", "Nature", "NEJM", "PLoS ONE", "PNAS", "Science")
meltfreqdata <- reshape2::melt(freqdata)
colnames(meltfreqdata) <- c("Journal","Year","Frequency")
freqplot <- ggplot(meltfreqdata, aes(fill=Journal, y=Frequency, x=Year)) +
  theme_bw() +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4")) +
  ggtitle("Number of articles - total") +
  theme(plot.title = element_text(hjust=0.5))
ggsave("./Figures/freqplot_totalarticles.tiff",freqplot,device=tiff(),width=8,height=6,units="in",dpi=900)

# now total # of articles that have score 0
freqbio0 <- summary(mergeddata[which(journal=="bioRxiv"&`1year`==0),]$pubyear)
freqcell0 <- summary(mergeddata[which(journal=="Cell"&`1year`==0),]$pubyear)
freqnat0 <- summary(mergeddata[which(journal=="Nature"&`1year`==0),]$pubyear)
freqnejm0 <- summary(mergeddata[which(journal=="NEJM"&`1year`==0),]$pubyear)
freqplos0 <- summary(mergeddata[which(journal=="PLoS ONE"&`1year`==0),]$pubyear)
freqpnas0 <- summary(mergeddata[which(journal=="PNAS"&`1year`==0),]$pubyear)
freqsci0 <- summary(mergeddata[which(journal=="Science"&`1year`==0),]$pubyear)
freqdata0 <- rbind(freqbio0, freqcell0, freqnat0, freqnejm0, freqplos0, freqpnas0, freqsci0)
rownames(freqdata0) <- c("bioRxiv", "Cell", "Nature", "NEJM", "PLoS ONE", "PNAS", "Science")
meltfreqdata0 <- reshape2::melt(freqdata0)
colnames(meltfreqdata0) <- c("Journal","Year","Frequency")
freqplot0 <- ggplot(meltfreqdata0, aes(fill=Journal, y=Frequency, x=Year)) +
  theme_bw() +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4")) +
  ggtitle("Number of articles with score = 0") +
  theme(plot.title = element_text(hjust=0.5))
ggsave("./Figures/freqplot_articlesequal0.tiff",freqplot0,device=tiff(),width=8,height=6,units="in",dpi=900)

# now total # of articles that have score >0
freqbio1 <- summary(mergeddata[which(journal=="bioRxiv"&`1year`>0),]$pubyear)
freqcell1 <- summary(mergeddata[which(journal=="Cell"&`1year`>0),]$pubyear)
freqnat1 <- summary(mergeddata[which(journal=="Nature"&`1year`>0),]$pubyear)
freqnejm1 <- summary(mergeddata[which(journal=="NEJM"&`1year`>0),]$pubyear)
freqplos1 <- summary(mergeddata[which(journal=="PLoS ONE"&`1year`>0),]$pubyear)
freqpnas1 <- summary(mergeddata[which(journal=="PNAS"&`1year`>0),]$pubyear)
freqsci1 <- summary(mergeddata[which(journal=="Science"&`1year`>0),]$pubyear)
freqdata1 <- rbind(freqbio1, freqcell1, freqnat1, freqnejm1, freqplos1, freqpnas1, freqsci1)
rownames(freqdata1) <- c("bioRxiv", "Cell", "Nature", "NEJM", "PLoS ONE", "PNAS", "Science")
meltfreqdata1 <- reshape2::melt(freqdata1)
colnames(meltfreqdata1) <- c("Journal","Year","Frequency")
freqplot1 <- ggplot(meltfreqdata1, aes(fill=Journal, y=Frequency, x=Year)) +
  theme_bw() +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4")) +
  ggtitle("Number of articles with score > 0") +
  theme(plot.title = element_text(hjust=0.5))
ggsave("./Figures/freqplot_articlesabove0.tiff",freqplot1,device=tiff(),width=8,height=6,units="in",dpi=900)


#####################
### BACKTRANSFORM ###
#####################

## We want to translate these model results into more interpretable terms
## We know how the Altmetric scores are calculated (e.g. a tweet is worth 0.25, a news article is with 8)
## We want to calculate what the difference in scores for female/male first authors in Science in 2017 and 2018 translates to
## Are male authors being Tweeted once more? 100 more times?


## Rerun models removing intercepts (makes it easier to do smearing estimate) (-1 removes intercept)
logisticmodel_bio_nointerc <- glm(score ~  puby.gend + propfemales + numauthors + pubmonth - 1, data=binarydata_bio, family=binomial)
# saveRDS(logisticmodel_bio_nointerc, "./Models/logisticmodel_bio_nointerc.RDS")
# logisticmodel_bio_nointerc <- readRDS("./Models/logisticmodel_bio_nointerc.RDS")

logisticmodel_oth_nointerc <- glm(score ~  puby.gend.jour + propfemales + numauthors + pubmonth - 1, data=binarydata_oth, family=binomial)
# saveRDS(logisticmodel_oth_nointerc, "./Models/logisticmodel_oth_nointerc.RDS")
# logisticmodel_oth_nointerc <- readRDS("./Models/logisticmodel_oth_nointerc.RDS")

linearmodel_bio_nointerc <- lm(logscore ~ puby.gend + propfemales + numauthors + pubmonth - 1, data=lineardata_bio)
# saveRDS(linearmodel_bio_nointerc, "./Models/linearmodel_bio_nointerc.RDS")
# linearmodel_bio_nointerc <- readRDS("./Models/linearmodel_bio_nointerc.RDS")

linearmodel_oth_nointerc <- lm(logscore ~ puby.gend.jour + propfemales + numauthors + pubmonth - 1, data=lineardata_oth)
# saveRDS(linearmodel_oth_nointerc, "./Models/linearmodel_oth_nointerc.RDS")
# linearmodel_oth_nointerc <- readRDS("./Models/linearmodel_oth_nointerc.RDS")

quantilemodel_bio_nointerc <- rq(logscore ~ puby.gend + propfemales + numauthors + pubmonth -1, data=lineardata_bio, tau=0.5)
# saveRDS(quantilemodel_bio_nointerc, "./Models/quantilemodel_bio_nointerc.RDS")
# quantilemodel_bio_nointerc <- readRDS("./Models/quantilemodel_bio_nointerc.RDS")

quantilemodel_oth_nointerc <- rq(logscore ~ puby.gend.jour + propfemales + numauthors + pubmonth -1, data=lineardata_oth, tau=0.5)
# saveRDS(quantilemodel_oth_nointerc, "./Models/quantilemodel_oth_nointerc.RDS")
# quantilemodel_oth_nointerc <- readRDS("./Models/quantilemodel_oth_nointerc.RDS")

## We are interested in backtransforming the difference in scores for Science in 2017 & 2018
summary(linearmodel_oth_nointerc)
# puby.gend.jour2017.ff.Science   1.2510277
# puby.gend.jour2018.ff.Science   1.0787635
# puby.gend.jour2017.fm.Science   1.9666501
# puby.gend.jour2018.fm.Science   2.9624915
# taking the exp(coefficient) produces a biased backtransform (https://www.biorxiv.org/content/10.1101/179358v1)
# let's do a proper unbiased backtransform (https://www.biorxiv.org/content/biorxiv/suppl/2017/08/21/179358.DC1/179358-1.pdf)
#### 1) Data following a normal distribution
# sigma.sq <- summary(lm.0)$sigma^2
# exp(fitted(lm.0) + sigma.sq / 2)
sciff2017 <- exp(1.2510277 + summary(linearmodel_oth_nointerc)$sigma^2/2)
scifm2017 <- exp(1.9666501 + summary(linearmodel_oth_nointerc)$sigma^2/2)
sciff2017 - scifm2017
sciff2018 <- exp(1.0787635 + summary(linearmodel_oth_nointerc)$sigma^2/2)
scifm2018 <- exp(2.9624915 + summary(linearmodel_oth_nointerc)$sigma^2/2)
sciff2018 - scifm2018

# Instead of backtransforming
# If I just want to know the difference between scores for male and female
# For the specific case of Science
# I can just rerun my model on untransformed data
linearmodel_untransf <- lm(score ~ puby.gend.jour + propfemales + numauthors + pubmonth, data=lineardata_oth)
summary(linearmodel_untransf)

# Calculate dif manually
# Sci 2018 ff - 49.73238
# Sci 2018 fm - 138.70413
# Intercept   - -1.29051
49.73238 - 138.70413
# -88.97175
# 2017
34.38098 - 56.52243

# Calculate dif through multcomp
linearmatrix_untrans <- afn.createlinearmatrix(linearmodel_untransf,c("Cell","Nature","NEJM","PLoS ONE","PNAS","Science"))
linearposthoc_untransf <- glht(linearmodel_untransf, linfct = linearmatrix_untrans)
linearposthoc_untransf
# fffmScience2017 == 0  -22.1414
# fffmScience2018 == 0  -88.9717

# For quantile model, we aren't making distributional assumptions, so we can use the naive backtransform
quantilemodel_oth_nointerc$coefficients
# Sci 2018 ff 4.353965e-01
# Sci 2018 fm 3.074395e+00
exp(4.353965e-01) - exp(3.074395e+00)


#####################
