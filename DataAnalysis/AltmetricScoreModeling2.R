################################################
################################################
########## MODELLING ALTMETRIC SCORES ##########
################################################
################################################

# 2019.10.23 Regression of altmetric scores and predictor variables
# 2020.01.21 Trying to clean up code and make prettier plots for presentation next week
# 2020.03.26 Tidying & making figs for paper

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
# install.packages("visreg")
# install.packages("tidyverse")
# install.packages("multcomp")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("sjPlot")
library(data.table)
library(visreg)
library(tidyverse)
library(multcomp)
library(ggplot2)
library(ggpubr)
library(sjPlot)


#################
### FUNCTIONS ###
#################

afn.loaddata <- function(files) {
  
  # Create a merged dataset from all journals
  alldata <- c()
  for (file in files) {
    filename <- paste("./Data100319/",file,sep="")
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
  
  return(alldata)
  
}

afn.cleanmergeddata <- function(datatoclean) {
  
  # no point keeping articles we are unable to genderize
  cleandata <- datatoclean[-which(authors_format=="nofirstname"),]
  
  # melt dataframe by author gender
  nongender_cols <- c("id","altmetric_id","authors","journal","type","doi","date","pubyear","pubmonth",
                      "males","females","unknown","propmales","propfemales","propunknown","numauthors","authors_format","cleaned_authors",
                      "5day","1year")
  gender_cols <- c("firstgender","lastgender")
  cleandata <- melt(cleandata, id.vars=nongender_cols, measure.vars=gender_cols)
  cleandata$gender <- paste(substr(cleandata$variable,1,1),substr(cleandata$value,1,1),sep="") %>%
    as.factor()
  cleandata$variable <- NULL
  cleandata$value <- NULL
  
  # for articles with only 1 author, treat as first author, but don't duplicate as last author
  toremove <- which(cleandata$numauthors==1 & (cleandata$gender=="l"|cleandata$gender=="lf"|cleandata$gender=="lm"))
  cleandata <- cleandata[-toremove,]
  
  return(cleandata)
  
}

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
    # coord_cartesian(xlim=c(2010,2020), ylim=c(-2.5,2.5)) +
    # scale_x_continuous(name="Year", breaks=c(2010,2012,2014,2016,2018,2020)) +
    # scale_y_continuous(name="Difference in probability") +
    geom_hline(yintercept=0, linetype="dashed",color="dimgray") +
    scale_x_continuous(name="Year", breaks=c(2010,2012,2014,2016,2018,2020), limits=c(2010,2020)) +
    scale_y_continuous(name="Difference in probability", limits=c(-2.5,2.5))
  
  plot
  
  return(plot)
  
}

afn.makefigure <- function(df, author, type) {
  
  journals <- c("bioRxiv","Cell","Nature","NEJM","PLoS ONE","PNAS","Science")
  plots <- vector(mode="list", length=length(journals))
  if (type == "logistic") {
    yaxis = "Difference in logit probability"
  } else if (type == "linear") {
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

# We have one csv file per journal that contains article information and genderized authors
genderizedfiles <- list.files("./Data100319/")
genderizedfiles <- genderizedfiles[which(substr(genderizedfiles,nchar(genderizedfiles)-3,nchar(genderizedfiles))==".tsv")]

mergeddata <- afn.loaddata(genderizedfiles)


###################
### EXPLORATION ###
###################

# Altmetric scores
attach(mergeddata)
hist(`5day`, col="#cbd5e8")
hist(`1year`, col="#cbd5e8")
summary(`5day`)
summary(`1year`)
hist(mergeddata[`5day`<25]$`5day`, col="#cbd5e8", main="Articles with 5day score <25")
hist(mergeddata[`1year`<100]$`1year`, col="#cbd5e8", main="Articles with 1year score <100")
hist(mergeddata$`1year`, col="#6aa4c8", main="1-year scores",xlab="score")
hist(log(mergeddata$`1year`), col="#6aa4c8", main="log(1-year scores)",xlab="log(score)")

# Article timing
boxplot(`1year`~pubmonth, data=mergeddata, main="Month of publication")
hist(as.numeric(pubyear), col="#cbd5e8")

# Author genders
barplot(table(firstgender), col=c("#e9edee","#ffb8a2","#1c3678"), main="First author gender", ylim=c(0,150000))
barplot(table(lastgender), col=c("#e9edee","#ffb8a2","#1c3678"), main="Last author gender", ylim=c(0,150000))
slices <- c(mean(propmales),mean(propfemales),mean(propunknown))
pie(slices, labels=c("Male","Female","Unknown"), main="Author list gender balance")
plot(propmales, propfemales) # lots that are at 0 because format was nofirstname, or because of unknown authors

# Number of authors
hist(numauthors, col="#cbd5e8")
hist(mergeddata[numauthors<25]$numauthors, col="#cbd5e8", main="Histogram of articles with <25 authors")
summary(numauthors)

# Type of article
barplot(table(type), col="#cbd5e8", las=2, main="Type of article")

# Journal
pie(table(journal), labels=c("bioRxiv","Cell","Nature","NEJM","PLoS ONE","PNAS","Science"), main="Journal",
    col=c("#1a9988","#e9edee","#6aa4c8","#eb5600","#a2ffe8","#1c3678","#ffb8a2"))

# Collinearity
plot(numauthors, propfemales)

detach(mergeddata)


#################
### DATA PREP ###
#################

# melt data by author gender
meltdata <- afn.cleanmergeddata(mergeddata)

# separate data into score and no-score after 1 year
binarydata <- meltdata
binarydata$score <- binarydata$`1year`
binarydata[score>0]$score <- 1

# separate binary data into bioRxiv and non-bioRxiv journals
binarydata_bio <- binarydata[journal=="bioRxiv" & pubyear %in% c(2015:2018)]
binarydata_oth <- binarydata[journal!="bioRxiv"]

# take all data that have score > 0 after 1 year
lineardata <- meltdata
lineardata$score <- lineardata$`1year`
lineardata <- lineardata[score>0]
lineardata$logscore <- log(lineardata$score)

# separate linear data into bioRxiv and non-bioRxiv journals
lineardata_bio <- lineardata[journal=="bioRxiv" & pubyear %in% c(2015:2018)]
lineardata_oth <- lineardata[journal!="bioRxiv"]


#################
### MODELLING ###
#################

# Logistic (binary score/no-score)
binarydata_bio$puby.gend <- interaction(binarydata_bio$pubyear, binarydata_bio$gender)
# logisticmodel_bio <- glm(score ~  puby.gend +
#                                   propfemales +
#                                   numauthors +
#                                   pubmonth,
#                          data=binarydata_bio,
#                          family=binomial)
# saveRDS(logisticmodel_bio, "./DataAnalysis/AltmetricScoreModeling/logisticmodel_bio.RDS")
logisticmodel_bio <- readRDS("./DataAnalysis/AltmetricScoreModeling/logisticmodel_bio.RDS")
summary(logisticmodel_bio)

binarydata_oth$puby.gend.jour <- interaction(binarydata_oth$pubyear, binarydata_oth$gender, binarydata_oth$journal)
# logisticmodel_oth <- glm(score ~  puby.gend.jour +
#                                   propfemales +
#                                   numauthors +
#                                   pubmonth,
#                          data=binarydata_oth,
#                          family=binomial)
# saveRDS(logisticmodel_oth, "./DataAnalysis/AltmetricScoreModeling/logisticmodel_oth.RDS")
logisticmodel_oth <- readRDS("./DataAnalysis/AltmetricScoreModeling/logisticmodel_oth.RDS")
summary(logisticmodel_oth)
# gives warning: fitted probabilities numerically 0 or 1 occurred

# Linear (score > 0)
lineardata_bio$puby.gend <- interaction(lineardata_bio$pubyear, lineardata_bio$gender)
# linearmodel_bio <- lm(logscore ~ puby.gend +
#                                  propfemales +
#                                  numauthors +
#                                  pubmonth,
#                       data=lineardata_bio)
# saveRDS(linearmodel_bio, "./DataAnalysis/AltmetricScoreModeling/linearmodel_bio.RDS")
linearmodel_bio <- readRDS("./DataAnalysis/AltmetricScoreModeling/linearmodel_bio.RDS")
summary(linearmodel_bio)

lineardata_oth$puby.gend.jour <- interaction(lineardata_oth$pubyear, lineardata_oth$gender, lineardata_oth$journal)
# linearmodel_oth <- lm(logscore ~ puby.gend.jour +
#                                  propfemales +
#                                  numauthors +
#                                  pubmonth,
#                       data=lineardata_oth)
# saveRDS(linearmodel_oth, "./DataAnalysis/AltmetricScoreModeling/linearmodel_oth.RDS")
linearmodel_oth <- readRDS("./DataAnalysis/AltmetricScoreModeling/linearmodel_oth.RDS")
summary(linearmodel_oth)


######################
### MODEL CHECKING ###
######################

# Logistic assumptions

# 1. is outcome binary?
unique(binarydata_bio$score)
unique(binarydata_oth$score)
# yes, just 0 or 1

# 2. is logit x predictors linear? look at partial residuals for continuous variable
visreg(logisticmodel_bio, "propfemales", type="conditional")
visreg(logisticmodel_oth, "propfemales", type="conditional")
# looks linear so there's no point in transforming the propfemales variable
# also this is the only one that is actually continuous so only one that we need to check

# 3. are there influential values in continuous predictors?
plot(logisticmodel_bio, 5)
plot(logisticmodel_oth, 5)
# lots of points with high leverage, and lots of points with SUPER high std residuals, but none outside cook's distance lines

# 4. is there high correlation (multicollinearity) between predictors?
faraway::vif(data.frame(binarydata_bio$numauthors,binarydata_bio$propfemales))
faraway::vif(data.frame(binarydata_oth$numauthors,binarydata_oth$propfemales))
# values are close to 1, which means the predictors are not correlated; also numauthors isn't technically continuous

# Linear assumptions

par(mfrow=c(2,2))
plot(linearmodel_bio)
plot(linearmodel_oth)
# 1. is it linear? 
# residual plot looks more or less equal on either side of 0 except straight line due to cutoff of scores=0
# residuals negative for high fitted values: model is predicting higher scores than actual
# 2. is it normal?
# qq-plot looks quite straight in both cases
# data can be assumed to be normal
# 3. is it homoskedastic?
# scale-location plot lines looks pretty flat until fitted value of about 3.5 for bio, 5 for others
# homoskedastic for the majority of points, except for the few very large fitted values
# 4. are there highly influential outliers?
# the residuals vs leverage plots show no points outside of Cook's distance
# it looks like there are not points we need to worry about


################
### POST HOC ###
################

# R2

1 - logisticmodel_bio$deviance / logisticmodel_bio$null.deviance # 0.6373 # doesn't work if out-of-sample
1 - logisticmodel_oth$deviance / logisticmodel_oth$null.deviance # 0.2909 # doesn't work if out-of-sample
summary(linearmodel_bio) # Mult 0.05691 Adj 0.05587
summary(linearmodel_oth) # Mult 0.2611 Adj 0.2589

# Are all factors important
# according to Tanadini's thesis p. 39 (https://ora.ox.ac.uk/objects/uuid:73c52d36-2e8a-4e04-92e0-a67ed93d7090/download_file?file_format=pdf&safe_filename=Thesis_Tanadini_2016.pdf&type_of_work=Thesis)
# we want to confirm that an independent variable of interest actually has an effect
# can do this by creating a version of the model without the variable and doing an anova

anova(logisticmodel_bio, glm(score ~ pubyear*gender + propfemales + numauthors, data=binarydata_bio, family=binomial))
anova(logisticmodel_bio, glm(score ~ pubyear*gender + propfemales + pubmonth, data=binarydata_bio, family=binomial))
anova(logisticmodel_bio, glm(score ~ pubyear*gender + numauthors + pubmonth, data=binarydata_bio, family=binomial))
anova(logisticmodel_bio, glm(score ~ pubyear*gender + numauthors, data=binarydata_bio, family=binomial))
anova(logisticmodel_bio, glm(score ~ pubyear*gender + pubmonth, data=binarydata_bio, family=binomial))
anova(logisticmodel_bio, glm(score ~ pubyear*gender + propfemales, data=binarydata_bio, family=binomial))
# in all cases, the most complex model (logisticmodel_bio) had the lowest score, so we should keep it
anova(logisticmodel_oth, glm(score ~ pubyear*gender*journal + propfemales + numauthors, data=binarydata_oth, family=binomial))
anova(logisticmodel_oth, glm(score ~ pubyear*gender*journal + propfemales + pubmonth, data=binarydata_oth, family=binomial))
anova(logisticmodel_oth, glm(score ~ pubyear*gender*journal + numauthors + pubmonth, data=binarydata_oth, family=binomial))
anova(logisticmodel_oth, glm(score ~ pubyear*gender*journal + numauthors, data=binarydata_oth, family=binomial))
anova(logisticmodel_oth, glm(score ~ pubyear*gender*journal + pubmonth, data=binarydata_oth, family=binomial))
anova(logisticmodel_oth, glm(score ~ pubyear*gender*journal + propfemales, data=binarydata_oth, family=binomial))
# in all cases, the most complex model (logisticmodel_oth) had the lowest score, so we should keep it
anova(linearmodel_bio, lm(logscore ~ pubyear*gender + propfemales + numauthors, data=lineardata_bio))
anova(linearmodel_bio, lm(logscore ~ pubyear*gender + propfemales + pubmonth, data=lineardata_bio))
anova(linearmodel_bio, lm(logscore ~ pubyear*gender + numauthors + pubmonth, data=lineardata_bio))
anova(linearmodel_bio, lm(logscore ~ pubyear*gender + numauthors, data=lineardata_bio))
anova(linearmodel_bio, lm(logscore ~ pubyear*gender + pubmonth, data=lineardata_bio))
anova(linearmodel_bio, lm(logscore ~ pubyear*gender + propfemales, data=lineardata_bio))
# in all cases, the most complex model (linearmodel_bio) had the lowest score, so we should keep it
anova(linearmodel_oth, lm(logscore ~ puby*gend*jour + propfemales + numauthors, data=lineardata_oth))
anova(linearmodel_oth, lm(logscore ~ pubyear*gender + propfemales + pubmonth, data=lineardata_oth))
anova(linearmodel_oth, lm(logscore ~ pubyear*gender + numauthors + pubmonth, data=lineardata_oth))
anova(linearmodel_oth, lm(logscore ~ pubyear*gender + numauthors, data=lineardata_oth))
anova(linearmodel_oth, lm(logscore ~ pubyear*gender + pubmonth, data=lineardata_oth))
anova(linearmodel_oth, lm(logscore ~ pubyear*gender + propfemales, data=lineardata_oth))
# in all cases, the most complex model (linearmodel_oth) had the lowest score, so we should keep it

# Multiple comparisons

par(mfrow=c(1,1))

logisticmatrix_bio <- afn.createlogisticmatrix(logisticmodel_bio,"bioRxiv")
logisticposthoc_bio <- glht(logisticmodel_bio, linfct = logisticmatrix_bio)
summary(logisticposthoc_bio)
plot(logisticposthoc_bio)

logisticmatrix_oth <- afn.createlogisticmatrix(logisticmodel_oth,c("Cell","Nature","NEJM","PLoS ONE","PNAS","Science"))
logisticposthoc_oth <- glht(logisticmodel_oth, linfct = logisticmatrix_oth)
summary(logisticposthoc_oth)
plot(logisticposthoc_oth)

linearmatrix_bio <- afn.createlinearmatrix(linearmodel_bio,"bioRxiv")
linearposthoc_bio <- glht(linearmodel_bio, linfct = linearmatrix_bio)
summary(linearposthoc_bio)
plot(linearposthoc_bio)

linearmatrix_oth <- afn.createlinearmatrix(linearmodel_oth,c("Cell","Nature","NEJM","PLoS ONE","PNAS","Science"))
linearposthoc_oth <- glht(linearmodel_oth, linfct = linearmatrix_oth)
summary(linearposthoc_oth)
plot(linearposthoc_oth)


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

# Logistic

logisticfig_first <- afn.makefigure(logisticdf, "First author", "logistic")
ggsave("./DataAnalysis/Figures/logistic_firstauthor.tiff",logisticfig_first,device=tiff(),width=8,height=6,units="in",dpi=900)
logisticfig_last <- afn.makefigure(logisticdf, "Last author", "logistic")
ggsave("./DataAnalysis/Figures/logistic_lastauthor_ylimfixed.tiff",logisticfig_last,device=tiff(),width=8,height=6,units="in",dpi=900)

# Linear

linearfig_first <- afn.makefigure(lineardf, "First author", "linear")
ggsave("./DataAnalysis/Figures/linear_firstauthor.tiff",linearfig_first,device=tiff(),width=8,height=6,units="in",dpi=900)
linearfig_last <- afn.makefigure(lineardf, "Last author", "linear")
ggsave("./DataAnalysis/Figures/linear_lastauthor.tiff",linearfig_last,device=tiff(),width=8,height=6,units="in",dpi=900)


#######################
### PLOT OTHER VARS ###
#######################

set_theme(theme_bw())

# Logisitc - bioRxiv
logistic_month_bio <- plot_model(logisticmodel_bio,type="pred",terms=c("pubmonth"),
                               axis.title=c("Month of publication","Probability of score"),
                               title="")
ggsave("./DataAnalysis/Figures/logistic_month_bio.tiff",logistic_month_bio,device=tiff(),width=8,height=6,units="in",dpi=900)
logistic_propf_bio <- plot_model(logisticmodel_bio,type="pred",terms=c("propfemales"),
                               axis.title=c("Proportion of female authors","Probability of score"),
                               title="")
ggsave("./DataAnalysis/Figures/logistic_propf_bio.tiff",logistic_propf_bio,device=tiff(),width=8,height=6,units="in",dpi=900)
logistic_numauth_bio <- plot_model(logisticmodel_bio,type="pred",terms=c("numauthors"),
                                 axis.title=c("Number of authors","Probability of score"),
                                 title="")
ggsave("./DataAnalysis/Figures/logistic_numauth_bio.tiff",logistic_numauth_bio,device=tiff(),width=8,height=6,units="in",dpi=900)

# Logisitc - non-bioRxiv
logistic_month_oth <- plot_model(logisticmodel_oth,type="pred",terms=c("pubmonth"),
                                 axis.title=c("Month of publication","Probability of score"),
                                 title="")
ggsave("./DataAnalysis/Figures/logistic_month_oth.tiff",logistic_month_oth,device=tiff(),width=8,height=6,units="in",dpi=900)
logistic_propf_oth <- plot_model(logisticmodel_oth,type="pred",terms=c("propfemales"),
                                 axis.title=c("Proportion of female authors","Probability of score"),
                                 title="")
ggsave("./DataAnalysis/Figures/logistic_propf_oth.tiff",logistic_propf_oth,device=tiff(),width=8,height=6,units="in",dpi=900)
logistic_numauth_oth <- plot_model(logisticmodel_oth,type="pred",terms=c("numauthors"),
                                   axis.title=c("Number of authors","Probability of score"),
                                   title="")
ggsave("./DataAnalysis/Figures/logistic_numauth_oth.tiff",logistic_numauth_oth,device=tiff(),width=8,height=6,units="in",dpi=900)


# Linear - bioRxiv
linear_month_bio <- plot_model(linearmodel_bio,type="pred",terms=c("pubmonth"),
                               axis.title=c("Month of publication","log(score)"),
                               title="")
ggsave("./DataAnalysis/Figures/linear_month_bio.tiff",linear_month_bio,device=tiff(),width=8,height=6,units="in",dpi=900)
linear_propf_bio <- plot_model(linearmodel_bio,type="pred",terms=c("propfemales"),
                               axis.title=c("Proportion of female authors","log(score)"),
                               title="")
ggsave("./DataAnalysis/Figures/linear_propf_bio.tiff",linear_propf_bio,device=tiff(),width=8,height=6,units="in",dpi=900)
linear_numauth_bio <- plot_model(linearmodel_bio,type="pred",terms=c("numauthors"),
                                 axis.title=c("Number of authors","log(score)"),
                                 title="")
ggsave("./DataAnalysis/Figures/linear_numauth_bio.tiff",linear_numauth_bio,device=tiff(),width=8,height=6,units="in",dpi=900)

# Linear - non-bioRxiv
linear_month_oth <- plot_model(linearmodel_oth,type="pred",terms=c("pubmonth"),
           axis.title=c("Month of publication","log(score)"),
           title="")
ggsave("./DataAnalysis/Figures/linear_month_oth.tiff",linear_month_oth,device=tiff(),width=8,height=6,units="in",dpi=900)
linear_propf_oth <- plot_model(linearmodel_oth,type="pred",terms=c("propfemales"),
           axis.title=c("Proportion of female authors","log(score)"),
           title="")
ggsave("./DataAnalysis/Figures/linear_propf_oth.tiff",linear_propf_oth,device=tiff(),width=8,height=6,units="in",dpi=900)
linear_numauth_oth <- plot_model(linearmodel_oth,type="pred",terms=c("numauthors"),
           axis.title=c("Number of authors","log(score)"),
           title="")
ggsave("./DataAnalysis/Figures/linear_numauth_oth.tiff",linear_numauth_oth,device=tiff(),width=8,height=6,units="in",dpi=900)

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
  scale_fill_manual(values=c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4"))
ggsave("./DataAnalysis/Figures/freqplot.tiff",freqplot,device=tiff(),width=8,height=6,units="in",dpi=900)

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
  scale_fill_manual(values=c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4"))
ggsave("./DataAnalysis/Figures/freqplot0.tiff",freqplot0,device=tiff(),width=8,height=6,units="in",dpi=900)

# now total # of articles that have score 1
freqbio1 <- summary(mergeddata[which(journal=="bioRxiv"&`1year`==1),]$pubyear)
freqcell1 <- summary(mergeddata[which(journal=="Cell"&`1year`==1),]$pubyear)
freqnat1 <- summary(mergeddata[which(journal=="Nature"&`1year`==1),]$pubyear)
freqnejm1 <- summary(mergeddata[which(journal=="NEJM"&`1year`==1),]$pubyear)
freqplos1 <- summary(mergeddata[which(journal=="PLoS ONE"&`1year`==1),]$pubyear)
freqpnas1 <- summary(mergeddata[which(journal=="PNAS"&`1year`==1),]$pubyear)
freqsci1 <- summary(mergeddata[which(journal=="Science"&`1year`==1),]$pubyear)
freqdata1 <- rbind(freqbio1, freqcell1, freqnat1, freqnejm1, freqplos1, freqpnas1, freqsci1)
rownames(freqdata1) <- c("bioRxiv", "Cell", "Nature", "NEJM", "PLoS ONE", "PNAS", "Science")
meltfreqdata1 <- reshape2::melt(freqdata1)
colnames(meltfreqdata1) <- c("Journal","Year","Frequency")
freqplot1 <- ggplot(meltfreqdata1, aes(fill=Journal, y=Frequency, x=Year)) +
  theme_bw() +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4"))
ggsave("./DataAnalysis/Figures/freqplot1.tiff",freqplot1,device=tiff(),width=8,height=6,units="in",dpi=900)




##############
### CHECKS ###
##############

# Why are error margins so wide in 2018 in the logistic model outputs?

binarybio2018 <- binarydata_bio[pubyear==2018,]
length(which(binarybio2018$score==1)) # 15347
length(which(binarybio2018$score==0)) # 4
binaryoth2018 <- binarydata_oth[pubyear==2018,]
length(which(binaryoth2018$score==1)) # 18833
length(which(binaryoth2018$score==0)) # 508
# By 2018, almost all articles get a score > 0 (e.g. are tweeted about at least once.)

# Why does Science all of a sudden see a difference in male and female scores in 2017-2018?

# Plotting sci scores through time
scidata <- meltdata[which(meltdata$journal=="Science"),]
colnames(scidata) # pubyear is 8, score is 20, gender is 21
scidata <- scidata[,c(8,20,21)]
scidata2 <- aggregate(scidata$`1year`~scidata$gender*scidata$pubyear,FUN=mean)
colnames(scidata2) <- c("gender","pubyear","meanscore")
scidata3 <- scidata2[which(!(scidata2$gender %in% c("fu","lu"))),]
scidata3$gender <- as.character(scidata3$gender)
scidata3$gender[which(scidata3$gender == "ff")] <- "First author female"
scidata3$gender[which(scidata3$gender == "fm")] <- "First author male"
scidata3$gender[which(scidata3$gender == "lf")] <- "Last author female"
scidata3$gender[which(scidata3$gender == "lm")] <- "Last author male"
ggplot(data=scidata3, aes(x=pubyear,y=meanscore,group=gender,colour=gender)) +
  geom_line() +
  scale_color_manual(values=c("#eb5600","#1c3678","#ffb8a2","#6aa4c8")) +
  theme(plot.title=element_text(hjust=0.5),
        legend.title=element_blank()) +
  ggtitle("Science")

natdata <- meltdata[which(meltdata$journal=="Nature"),]
colnames(natdata) # pubyear is 8, score is 20, gender is 21
natdata <- natdata[,c(8,20,21)]
natdata2 <- aggregate(natdata$`1year`~natdata$gender*natdata$pubyear,FUN=mean)
colnames(natdata2) <- c("gender","pubyear","meanscore")
natdata3 <- natdata2[which(!(natdata2$gender %in% c("fu","lu"))),]
natdata3$gender <- as.character(natdata3$gender)
natdata3$gender[which(natdata3$gender == "ff")] <- "First author female"
natdata3$gender[which(natdata3$gender == "fm")] <- "First author male"
natdata3$gender[which(natdata3$gender == "lf")] <- "Last author female"
natdata3$gender[which(natdata3$gender == "lm")] <- "Last author male"
ggplot(data=natdata3, aes(x=pubyear,y=meanscore,group=gender, colour=gender)) +
  geom_line() +
  scale_color_manual(values=c("#eb5600","#1c3678","#ffb8a2","#6aa4c8")) +
  theme(plot.title=element_text(hjust=0.5),
        legend.title=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Nature")


# Backtransforming

# Response scale (this is the naive backtransform! Just doing it for the presentation, but should fix for paper)
lineardf2 <- lineardf
lineardf2$Estimate <- exp(lineardf2$Estimate)
lineardf2$lwr <- exp(lineardf2$lwr)
lineardf2$upr <- exp(lineardf2$upr)
linearfig_first2 <- afn.makefigure(lineardf2, "First author", "linear")

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

#####################

