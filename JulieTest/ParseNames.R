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
  
  authorsvec <- strsplit(authors,",")
  numauthors <- length(authorsvec[[1]])
  
  authors <- textPrepare(authors)
  
  return(authors, numauthors)
  
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

#################
### LOAD DATA ###
#################

fname_cell <- "JulieTest/DataByJournal/Cell.tsv"
data_cell <- afn.loaddata(fname_cell)





# read data
# pull authors
# create vector of authors for each paper
# for each author vector, check to see how names are listed
# pull given names
# genderize given names

# PROBLEM: genderizeR has a limit of 1000 records per day


