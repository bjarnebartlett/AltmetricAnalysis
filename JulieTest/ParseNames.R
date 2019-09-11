#####################################################
#####################################################
########## PARSE NAMES FROM ALTMETRIC DATA ##########
#####################################################
#####################################################

# 2019.08.01 Trying to parse names from Altmetric data in order to look at gender

###############
### SUMMARY ###
###############

# We are looking at altmetric scores of publications in 7 idiotypic journals
# And looking at the gender of the authors of those publications
# To see if there are any interesting patterns in terms of altmetrics & gender bias

# Citation
# Kamil Wais, Gender Prediction Methods Based on First Names with genderizeR, The R Journal,Vol. 8/1, Aug. 2016, https://journal.r-project.org/archive/2016-1/wais.pdf

##############
### SET UP ###
##############

setwd("/Users/juliefortin/Documents/UBC/Projects/Altmetrics/AltmetricAnalysis/")

# install.packages("data.table")
# install.packages("genderizeR")
# install.packages("dplyr")
# install.packages("lubridate")
library(data.table)
library(genderizeR)
library(dplyr)
library(lubridate)


#################
### FUNCTIONS ###
#################

afn.loaddata <- function(filename) {
  
  data <- fread(filename,sep="\t")
  data <- data[,1:8] # get rid of excess cols
  colnames(data) <- c("id",
                      "altmetric_id",
                      "authors",
                      "journal",
                      "type",
                      "doi",
                      "altmetric_score",
                      "date"
  )
  # print(head(data))
  
  return(data)
}

afn.cleandata <- function(data) {
  
  # Remove entries that have no authors listed
  cleandata <- data[which(!is.na(data$authors)),]
  cleandata <- cleandata[which(cleandata$authors!="None"),]
  cleandata <- cleandata[which(cleandata$authors!="['']"),]
  cleandata <- cleandata[which(cleandata$authors!=""),]
  cleandata <- cleandata[which(cleandata$authors!="NA"),]
  cleandata <- cleandata[which(cleandata$authors!="[', ']"),]
  
  # Remove entries that have no date
  cleandata <- cleandata[which(cleandata$date!="None"),]
  cleandata <- cleandata[which(!is.na(cleandata$date)),]
  
  # Remove apparent duplicates
  # cleandata <- afn.checkduplicates(cleandata)
  
  # Split date into year and month
  pubyear <- year(as.Date(cleandata$date))
  pubmonth <- month(as.Date(cleandata$date))
  cleandata <- cbind(cleandata,pubyear,pubmonth)
  
  return(cleandata)
  
}

afn.subsetdata <- function(data, year) {
  
  subset <- data[which(data$pubyear>=year),]
  subset <- subset[which(subset$pubyear<2020),] # don't take things that aren't published yet
  
  return(subset)
  
}

afn.assessonejournal <- function(journal_data, outfile) {
  
  newjournaldata <- c()
  
  for (i in 1:#dim(journal_data)[[1]]) { # go through all articles
       3) { # for testing purposes to keep within 1000 api limit
    # pull data for just one article
    
    # print(paste("On article",i,"of journal",journal_data$journal[i]))
    journal_article <- journal_data[i,]
    
    # extract authors
    article_authors <- afn.getauthors(journal_article)
    authors_gendered <- article_authors[[1]] # all authors
    authors_order <- article_authors[[2]] # ordered vector of all authors
    authors_num <- article_authors[[3]] # number of authors
    
    # looking at all authors of the article
    article_stats <- afn.getgenderstats(authors_gendered, authors_num)
    
    # now just first and last author
    firstauthor <- authors_order[1]
    lastauthor <- authors_order[length(authors_order)]
    firstgender <- authors_gendered$gender[1]
    firstgenderprob <- authors_gendered$probability[1]
    lastgender <- authors_gendered$gender[authors_num]
    lastgenderprob <- authors_gendered$probability[authors_num]
    
    firstlast_stats <- data.frame(firstgender, firstgenderprob, lastgender, lastgenderprob)
    article_stats <- cbind(article_stats,firstlast_stats)
    
    # now put everything together
    journal_article <- cbind(journal_article, article_stats)
    newjournaldata <- rbind(newjournaldata, journal_article)
  }
  
  fwrite(newjournaldata, outfile, sep="\t", append=TRUE)
  
  return(newjournaldata)
  
}

afn.getauthors <- function(article) {
  
  authors <- article$authors
  cleanedauthors <- afn.cleanauthors(authors)
  numauthors <- cleanedauthors[[2]]
  cleanedauthors <- cleanedauthors[[1]]
  
  authorsformat <- afn.checkformat(cleanedauthors)
  firstthree <- authorsformat[[1]]
  format <- authorsformat[[2]]
  
  allgenderizednames <-  firstthree
  
  if (numauthors > 3) {
    for (i in 4:numauthors) {
      
      author <- cleanedauthors[i]
      names <- strsplit(author," ")
      
      if (length(names[[1]])>1) {
        # there is only one term, most likely it is a last name 
        # don't bother genderizing
        if (format == "firstlast") {
          genderizedname <- unique(findGivenNames(names[[1]][1],apikey="d92354e95b4ff49e7944cd9395e4f908"))
        } else if (format == "lastfirst") {
          genderizedname <- unique(findGivenNames(names[[1]][2],apikey="d92354e95b4ff49e7944cd9395e4f908"))
        } else {
          genderizedname <- data.frame(name=names[[1]][1], gender=NA, probability=NA, count=NA, country_id=NA)
        }
        allgenderizednames <- rbind(allgenderizednames, genderizedname)
      }
    }
  }
  
  return(list(allgenderizednames, cleanedauthors, numauthors))
  
}

afn.getgenderstats <- function(names, numauthors) {
  
  males <- length(which(names$gender=="male"))
  females <- length(which(names$gender=="female"))
  unknown <- numauthors-(males+females)
  percmales <- males/numauthors
  percfemales <- females/numauthors
  percunknown <- unknown/numauthors
  
  genderstats <- data.frame(males,females, unknown, percmales, percfemales, percunknown, numauthors)
  
  return(genderstats)
  
}

afn.cleanauthors <- function(authorsvector) {
  
  authors <- gsub("\\\\xa0"," ",authorsvector)
  authors <- gsub("[^[:alnum:]^\'^-]"," ",authors)
  
  authorsvec <- strsplit(authors,"\' ")[[1]]
  authorsvec <- gsub(" \'", "", authorsvec)
  
  authors <- gsub("^von ","von",authorsvec)
  authors <- gsub(" von "," von",authors)
  authors <- gsub("Van de ","VanDe",authors)
  authors <- gsub("[A-Z] ","",authors) # get rid of beginning/middle initials
  authors <- gsub("[A-Z][A-Z]","",authors) # get rid of FM initials
  authors <- gsub(" [A-Z]$","",authors) # get rid of initials at end
  authors <- gsub("  ", " ", authors)
  authors <- gsub("- "," ",authors)
  authors <- gsub(" -"," ",authors)
  authors <- trimws(authors)
  
  numauthors <- length(authorsvec)
  
  return(list(authors, numauthors))
  
}

afn.checkformat <- function(authorvec) {
  
  # Name format should be one of:
  # First Last
  # First Middle Last
  # Last First
  # Last First Middle
  
  # First name is only ever first or second term
  # So can ignore third term
  
  formats <- c()
  genderedauthors <- data.frame(name=character(),gender=factor(),probability=numeric(),count=numeric(),country_id=factor())
  
  # let's try to determine pattern from first 3 authors
  if (length(authorvec) < 3) {
    loops <- length(authorvec)
  } else if (length(authorvec) >= 3) {
    loops <- 3
  }
  
  for (i in 1:loops) {
    
    formattest <- afn.testfirstfew(authorvec, i, confthreshold)
    format <- formattest[[1]]
    genderedauthor <- formattest[[2]]
    
    formats <- c(formats, format)
    genderedauthors <- rbind(genderedauthors, genderedauthor)
  }
  
  # checked first 3 authors
  # based on these, does it seem like format is firstlast or lastfirst?
  numfirstlast <- length(which(formats=="firstlast"))
  numlastfirst <- length(which(formats=="lastfirst"))
  numnofirst <- length(which(formats=="nofirstname"))
  numunknown <- length(which(formats=="unknown"))
  
  df <- data.frame(firstlast=numfirstlast, 
                   lastfirst=numlastfirst, 
                   nofirstname=numnofirst,
                   unknown=numunknown)
  df <- sort(df,decreasing=TRUE)
  
  if (df[1,1] > df[1,2]) { # there is a unique max
    authorformat <- colnames(df)[1]
  } else { # there is no unique max
    authorformat <- "firstlast" # best guess - from looking at tsv's, most articles seem to have firstlast format
  }
  
  return(list(genderedauthors, authorformat)) # return the genderized ones so we don't have to do them again, plus the format
  
}

afn.testfirstfew <- function(authors, i, confthresh) {
  
  author <- authors[i]
  names <- strsplit(author," ")
  
  if (length(names[[1]])>1) { # if there is only one term, most likely it is a last name so don't bother genderizing
    
    term1 <- names[[1]][1]
    term2 <- names[[1]][2]
    
    genderedterm1 <- unique(findGivenNames(term1,apikey="d92354e95b4ff49e7944cd9395e4f908"))
    
    if (dim(genderedterm1)[[1]]==0) { # if there are no rows
      
      format <- "unknown"
      genderedauthor <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
      
    } else {
      
      prob1 <- as.numeric(genderedterm1$probability)
      
      if (prob1 > confthresh) { # if term is 90% prob male or female, it's likely a first name, don't bother genderizing next terms
        
        format <- "firstlast"
        genderedauthor <- genderedterm1
        
      } else { # if uncertain that term1 is firstname, check term2
        
        genderedterm2 <- unique(findGivenNames(term2,apikey="d92354e95b4ff49e7944cd9395e4f908"))
        prob2 <- as.numeric(genderedterm2$probability)
        
        if (dim(genderedterm2)[[1]]==0) { # if there are no rows
          format <- "unknown"
          genderedauthor <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
        } else if ((prob2 > prob1) && (prob2 > probabilitythreshold)) { # need to be confident enough in gender
          format <- "lastfirst"
          genderedauthor <- genderedterm2
        } else if ((prob1 > prob2) && (prob1 > probabilitythreshold)) {
          format <- "firstlast"
          genderedauthor <- genderedterm1
        } else { # if not sufficiently confident in either term, leave author as unknown
          format <- "unknown"
          genderedauthor <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
        }
      }
    }
  } else {
    format <- "nofirstname"
    genderedauthor <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
  }
  
  return(list(format, genderedauthor))
  
}


#################
### LOAD DATA ###
#################

subsetyear <- 2011

sink("./JulieTest/JournalLevelCleaning.txt")

fname_biorxiv <- "./Data091019/bioRxiv.tsv"
alldata_biorxiv <- afn.loaddata(fname_biorxiv)
print(paste("There are",dim(alldata_biorxiv)[[1]],"articles in BioRxiv"))
cleandata_biorxiv <- afn.cleandata(alldata_biorxiv)
print(paste("There are",dim(cleandata_biorxiv)[[1]],"articles in BioRxiv after cleaning (i.e. removing articles with no date/no author)"))
data_biorxiv <- afn.subsetdata(cleandata_biorxiv,subsetyear)
print(paste("There are",dim(data_biorxiv)[[1]],"articles in BioRxiv between",subsetyear,"and end of 2019"))
print("***")

fname_cell <- "./Data091019/Cell.tsv"
alldata_cell <- afn.loaddata(fname_cell)
print(paste("There are",dim(alldata_cell)[[1]],"articles in Cell"))
cleandata_cell <- afn.cleandata(alldata_cell)
print(paste("There are",dim(cleandata_cell)[[1]],"articles in Cell after cleaning (i.e. removing articles with no date/no author)"))
data_cell <- afn.subsetdata(cleandata_cell,subsetyear)
print(paste("There are",dim(data_cell)[[1]],"articles in Cell between",subsetyear,"and end of 2019"))
print("***")

fname_nature <- "./Data091019/Nature.tsv"
alldata_nature <- afn.loaddata(fname_nature)
print(paste("There are",dim(alldata_nature)[[1]],"articles in Nature"))
cleandata_nature <- afn.cleandata(alldata_nature)
print(paste("There are",dim(cleandata_nature)[[1]],"articles in Nature after cleaning (i.e. removing articles with no date/no author)"))
data_nature <- afn.subsetdata(cleandata_nature,subsetyear)
print(paste("There are",dim(data_nature)[[1]],"articles in Nature between",subsetyear,"and end of 2019"))
print("***")

fname_nejm <- "./Data091019/NEJM.tsv"
alldata_nejm <- afn.loaddata(fname_nejm)
print(paste("There are",dim(alldata_nejm)[[1]],"articles in NEJM"))
cleandata_nejm <- afn.cleandata(alldata_nejm)
print(paste("There are",dim(cleandata_nejm)[[1]],"articles in NEJM after cleaning (i.e. removing articles with no date/no author)"))
data_nejm <- afn.subsetdata(cleandata_nejm,subsetyear)
print(paste("There are",dim(data_nejm)[[1]],"articles in NEJM between",subsetyear,"and end of 2019"))
print("***")

fname_pnas <- "./Data091019/PNAS.tsv"
alldata_pnas <- afn.loaddata(fname_pnas)
print(paste("There are",dim(alldata_pnas)[[1]],"articles in PNAS"))
cleandata_pnas <- afn.cleandata(alldata_pnas)
print(paste("There are",dim(cleandata_pnas)[[1]],"articles in PNAS after cleaning (i.e. removing articles with no date/no author)"))
data_pnas <- afn.subsetdata(cleandata_pnas,subsetyear)
print(paste("There are",dim(data_pnas)[[1]],"articles in PNAS between",subsetyear,"and end of 2019"))
print("***")

fname_science <- "./Data091019/Science.tsv"
alldata_science <- afn.loaddata(fname_science)
print(paste("There are",dim(alldata_science)[[1]],"articles in Science"))
cleandata_science <- afn.cleandata(alldata_science)
print(paste("There are",dim(cleandata_science)[[1]],"articles in Science after cleaning (i.e. removing articles with no date/no author)"))
data_science <- afn.subsetdata(cleandata_science,subsetyear)
print(paste("There are",dim(data_science)[[1]],"articles in Science between",subsetyear,"and end of 2019"))
print("***")

fname_plos <- "./Data091019/PLoS ONE.tsv"
alldata_plos <- afn.loaddata(fname_plos)
print(paste("There are",dim(alldata_plos)[[1]],"articles in PLoS ONE"))
cleandata_plos <- afn.cleandata(alldata_plos)
print(paste("There are",dim(cleandata_plos)[[1]],"articles in PLoS ONE after cleaning (i.e. removing articles with no date/no author)"))
data_plos <- afn.subsetdata(cleandata_plos,subsetyear)
print(paste("There are",dim(data_plos)[[1]],"articles in PLoS ONE between",subsetyear,"and end of 2019"))
print("***")

sink()

######################
### GENDERIZE DATA ###
######################

probabilitythreshold <- 0.75
confthreshold <- 0.9

outname_biorxiv <- "JulieTest/GenderizedByJournal/NewBioRxiv.tsv"
newdata_biorxiv <- afn.assessonejournal(data_biorxiv,outname_biorxiv)

outname_cell <- "JulieTest/GenderizedByJournal/NewCell.tsv"
newdata_cell <- afn.assessonejournal(data_cell,outname_cell)

outname_nature <- "JulieTest/GenderizedByJournal/NewNature.tsv"
newdata_nature <- afn.assessonejournal(data_nature,outname_nature)

outname_nejm <- "JulieTest/GenderizedByJournal/NewNEJM.tsv"
newdata_nejm <- afn.assessonejournal(data_nejm,outname_nejm)

outname_pnas <- "JulieTest/GenderizedByJournal/NewPNAS.tsv"
newdata_pnas <- afn.assessonejournal(data_pnas,outname_pnas)

outname_science <- "JulieTest/GenderizedByJournal/NewScience.tsv"
newdata_science <- afn.assessonejournal(data_science,outname_science)

outname_plos <- "JulieTest/GenderizedByJournal/NewPLoSONE.tsv"
newdata_plos <- afn.assessonejournal(data_plos,outname_plos)





