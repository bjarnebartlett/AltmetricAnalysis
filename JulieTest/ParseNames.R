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
  data <- data[,1:7] # get rid of excess cols
  colnames(data) <- c("id",
                      "altmetric_id",
                      "authors",
                      "journal",
                      "type",
                      "altmetric_score",
                      "date"
  )
  print(head(data))
  
  return(data)
}

afn.getauthors <- function(article) {
  
  authors <- article$authors
  authors <- gsub("\\\\xa0"," ",authors)
  authors <- gsub("[^[:alnum:]^\'^-]"," ",authors)
  # authors <- gsub("\\\\.* ", " ", authors)
  
  authorsvec <- strsplit(authors,"\' ")[[1]]
  authorsvec <- gsub(" \'", "", authorsvec)
  numauthors <- length(authorsvec)
  
  authors <- textPrepare(authors)
  
  return(list(authors, authorsvec, numauthors))
  
}


afn.findnames <- function(authors) {
  
  names <- findGivenNames(authors)
  
  freqnames <- names[count > 100] # removes words that are unlikely to be names
  genderednames <- freqnames[probability > 0.75] # removes names that don't confidently indicate gender
  
  if (TRUE %in% duplicated(genderednames)) {
    genderednames <- distinct(genderednames) # for some reason some results are duplicated; this fixes that
  }
  
  genderednames <- genderize(authors, genderednames)
  
  return(genderednames)
  
}

afn.getgenderstats <- function(names, numauthors) {
  
  males <- length(which(names$gender=="male"))
  females <- length(which(names$gender=="female"))
  unknown <- numauthors-(males+females)
  percmales <- males/numauthors
  percfemales <- females/numauthors
  percunknown <- unknown/numauthors
  
  genderstats <- data.frame(males,females, unknown, percmales, percfemales, percunknown)
  
  
  return(genderstats)
  
}

afn.assessonejournal <- function(journal_data, outfile) {
  
  newjournaldata <- c()
  
  for (i in 1:#dim(journal_data)[[1]]) { # go through all articles
       2) { # for testing purposes to keep within 1000 api limit
    # pull data for just one article
    journal_article <- journal_data[i,]
    
    # extract authors
    article_authors <- afn.getauthors(journal_article)
    authors_text <- article_authors[[1]] # all authors
    authors_order <- article_authors[[2]] # ordered vector of all authors
    authors_num <- article_authors[[3]] # number of authors
    
    # looking at all authors of the article
    allauthors_names <- afn.findnames(authors_text)
    article_stats <- afn.getgenderstats(allauthors_names, authors_num)
    
    # now just first and last author
    firstauthor <- authors_order[1]
    lastauthor <- authors_order[length(authors_order)]
    firstnames <- afn.findnames(firstauthor)
    firstgender <- firstnames$gender[1] # !!! How do I pick if there's more than one?
    lastnames <- afn.findnames(lastauthor)
    lastgender <- lastnames$gender[1]
    firstlast_stats <- data.frame(firstgender, lastgender)
    article_stats <- cbind(article_stats,firstlast_stats)
    
    # now put everything together
    journal_article <- cbind(journal_article, article_stats, authors_num)
    newjournaldata <- rbind(newjournaldata, journal_article)
  }
  
  fwrite(newjournaldata, outfile, sep="\t", append=TRUE)
  
  return(newjournaldata)
  
}

afn.subsetdata <- function(data, year) {
  
  pubyear <- year(as.Date(data$date))
  cbind(data,pubyear)
  
  subset <- data[which(pubyear>=year),]
  subset <- subset[which(pubyear<2020),] # don't take things that aren't published yet
  
  return(subset)
  
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
  cbind(cleandata,pubyear,pubmonth)
  
  return(cleandata)

}

afn.checkduplicates <- function(data) {
  
  exclude_dupcheck <- cleandata[which(cleandata$journal=="Nature" & cleandata$type=="news"),] # nature has a lot of science writers writing repeatedly for news pieces; these are unlikely to be duplicates
  include_dupcheck <- cleandata[-which(cleandata$journal=="Nature" & cleandata$type=="news"),] 
  
  rowstoremove <- c()
  duplicated <- which(duplicated(include_dupcheck$authors))
  
  for (i in duplicated) {
    dupauthor <- include_dupcheck$authors[i]
    dupid <- include_dupcheck$id[i]
    duplicates <- include_dupcheck[which(include_dupcheck$authors==dupauthor),] # get the whole rows for those authors
    
    for (j in 1:(dim(duplicates)[[1]]-1)) {
      
      dup1 <- duplicates[j,]
      dup2 <- duplicates[j+1,]
      
      date1 <- as.Date(dup1$date) # get the date of the first pub
      date2 <- as.Date(dup2$date) # get the date of the second pub
      tdif <- abs(difftime(date1, date2, units="days"))
      
      if (tdif < 365) { # if the two pubs are within a year of each other and have the same authors/journal, probably duplicates
        if (dup1$id!=dupid) {
          r <- which(include_dupcheck$id==dup1$id) # find the row number from the original df
          rowstoremove <- c(rowstoremove, r) # need to do this via rowstoremove cause if i remove as i go, the correct row nums change
        }
        if (dup2$id!=dupid) { # this is not an elseif b/c it could be that neither of the two dups being compared are the original dup from duplicated
          r <- which(include_dupcheck$id==dup2$id)
          rowstoremove <- c(rowstoremove, r)
        }
      }
      
    }
  }
  rowstoremove <- unique(rowstoremove)
  include_dupcheck <- include_dupcheck[-rowstoremove,]
  
  cleandata <- rbind(exclude_dupcheck, include_dupcheck)
  
  return(cleandata)
}



#################
### LOAD DATA ###
#################

subsetyear <- 2011

fname_biorxiv <- "bioRxiv.tsv"
alldata_biorxiv <- afn.loaddata(fname_biorxiv)
data_biorxiv <- afn.subsetdata(alldata_biorxiv,subsetyear)
data_biorxiv <- afn.cleandata(data_biorxiv)

fname_cell <- "Cell.tsv"
alldata_cell <- afn.loaddata(fname_cell)
data_cell <- afn.subsetdata(alldata_cell,subsetyear)
data_cell <- afn.cleandata(data_cell)

fname_nature <- "Nature.tsv"
alldata_nature <- afn.loaddata(fname_nature)
data_nature <- afn.subsetdata(alldata_nature,subsetyear)
data_nature <- afn.cleandata(data_nature)

fname_nejm <- "NEJM.tsv"
alldata_nejm <- afn.loaddata(fname_nejm)
data_nejm <- afn.subsetdata(alldata_nejm,subsetyear)
data_nejm <- afn.cleandata(data_nejm)

fname_pnas <- "PNAS.tsv"
alldata_pnas <- afn.loaddata(fname_pnas)
data_pnas <- afn.subsetdata(alldata_pnas,subsetyear)
data_pnas <- afn.cleandata(data_pnas)

fname_science <- "Science.tsv"
alldata_science <- afn.loaddata(fname_science)
data_science <- afn.subsetdata(alldata_science,subsetyear)
data_science <- afn.cleandata(data_science)


###################
### GET AUTHORS ###
###################

a <- afn.getauthors(data_biorxiv)
a

######################
### GENDERIZE DATA ###
######################

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







# PROBLEM: genderizeR has a limit of 1000 records per day


