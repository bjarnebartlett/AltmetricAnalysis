#####################################################
#####################################################
########## PARSE NAMES FROM ALTMETRIC DATA ##########
#####################################################
#####################################################

# 2019.10.20 Trying to parse names from Altmetric data in order to look at gender

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
# install.packages("glmmTMB")
# install.packages("effects")
# install.packages("sjPlot")
library(data.table)
library(lme4)
library(glmmTMB)
library(effects)
library(sjPlot)

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
mergeddata$alltime <- as.numeric(mergeddata$alltime)
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
hist(mergeddata$alltime, col="#cbd5e8") # very skewed
summary(mergeddata$`5day`)
summary(mergeddata$`1year`)
summary(mergeddata$alltime)
hist(mergeddata[`5day`<25]$`5day`, col="#cbd5e8", main="Histogram of articles with 5day score <25")
hist(mergeddata[`1year`<100]$`1year`, col="#cbd5e8", main="Histogram of articles with 1year score <100")
hist(mergeddata[alltime<100]$alltime, col="#cbd5e8", main="Histogram of articles with alltime score <100")

# Article timing
boxplot(alltime~pubmonth, data=mergeddata, main="Month of publication")
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


################
### ANALYSIS ###
################

data <- mergeddata[-which(authors_format=="nofirstname"),]
# looked at figures again with this new dataset
# removes many unknowns but doesn't make a huge difference

boxplot(data$`5day`~data$firstgender, main="5day altmetric score by first author gender")
boxplot(data$`5day`~data$lastgender, main="5day altmetric score by last author gender")
boxplot(data$`1year`~data$firstgender, main="1year altmetric score by first author gender")
boxplot(data$`1year`~data$lastgender, main="1year altmetric score by last author gender")

# melt altmetric scores
nonscore_cols <- c("id","altmetric_id","authors","journal","type","doi",
                   "date","pubyear","pubmonth",
                   "males","females","unknown","propmales","propfemales","propunknown","numauthors",
                   "firstgender","firstgenderprob","lastgender","lastgenderprob",
                   "authors_format","cleaned_authors")
score_cols <- c("5day","1year","alltime")
d <- melt(data, id.vars = nonscore_cols, measure.vars = score_cols)
d$value <- as.numeric(d$value)
colnames(d)[23] <- "scoretime"
colnames(d)[24] <- "score"
head(d)

# melt gender
nongender_cols <- c("id","altmetric_id","authors","journal","type","doi",
                    "date","pubyear","pubmonth",
                    "males","females","unknown","propmales","propfemales","propunknown","numauthors",
                    "authors_format","cleaned_authors",
                    "scoretime","score")
gender_cols <- c("firstgender","lastgender")
d2 <- melt(d, id.vars = nongender_cols, measure.vars = gender_cols)
d2$flauth <- paste(substr(d2$variable,1,1),substr(d2$value,1,1),sep="")
d2$variable <- NULL
d2$value <- NULL
head(d2)

d2[journal=="Proceedings of the National Academy of Sciences of the United States of America"]$journal <- "PNAS"
d2[journal=="New England Journal of Medicine"]$journal <- "NEJM"
unique(d2$journal)

model <- glm(score ~  pubyear*flauth*scoretime*journal +
                      propfemales +
                      numauthors +
                      pubmonth,
             data=d2,
             family=poisson)
saveRDS(model,"poissonmodel.RDS")
summary(model)

effects <- effect("pubyear*flauth*scoretime*journal",model)
plot(effects)

e2 <- predictorEffect("numauthors",model)
plot(e2)

plot_model(model,type="pred",terms=c("scoretime","journal","flauth","pubyear"))
plot_model(model,type="pred",terms=c("scoretime","journal","flauth"))
plot_model(model,type="pred",terms=c("propfemales"))
plot_model(model,type="pred",terms=c("numauthors"))
plot_model(model,type="pred",terms=c("pubmonth"))
plot_model(model,type="pred",terms=c("flauth"))
plot_model(model,type="pred",terms=c("pubyear"))
plot_model(model,type="pred",terms=c("scoretime"))
plot_model(model,type="pred",terms=c("journal"))
plot_model(model,type="pred",terms=c("flauth","journal"))



