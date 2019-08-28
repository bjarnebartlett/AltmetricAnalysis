#####################################################
#####################################################
########## PARSE NAMES FROM ALTMETRIC JSON ##########
#####################################################
#####################################################

# 2019.08.01 Trying to parse names from Altmetric data in order to look at gender

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
# install.packages("genderizeR")
library(data.table)
library(genderizeR)
library(dplyr)


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
  
  authorsvec <- strsplit(authors,",")[[1]]
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
       3) {
    journal_article <- journal_data[i,]
    article_authors <- afn.getauthors(journal_article)
    article_authors_text <- article_authors[[1]]
    article_authors_order <- article_authors[[2]]
    article_authors_num <- article_authors[[3]]
    article_authors_names <- afn.findnames(article_authors_text)
    article_stats <- afn.getgenderstats(article_authors_names, article_authors_num)
    journal_article <- cbind(journal_article, article_stats)
    newjournaldata <- rbind(newjournaldata, journal_article)
  }
  
  fwrite(newjournaldata, outfile, sep="\t")
  
  return(newjournaldata)
  
}


#################
### LOAD DATA ###
#################


fname_biorxiv <- "bioRxiv.tsv"
data_biorxiv <- afn.loaddata(fname_biorxiv)
fname_cell <- "Cell.tsv"
data_cell <- afn.loaddata(fname_cell)
fname_nature <- "Nature.tsv"
data_nature <- afn.loaddata(fname_nature)
fname_nejm <- "NEJM.tsv"
data_nejm <- afn.loaddata(fname_nejm)
fname_pnas <- "PNAS.tsv"
data_pnas <- afn.loaddata(fname_pnas)
fname_science <- "Science.tsv"
data_science <- afn.loaddata(fname_science)


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


