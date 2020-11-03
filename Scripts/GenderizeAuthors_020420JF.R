#######################################
#######################################
########## GENDERIZE AUTHORS ##########
#######################################
#######################################

# 2020.04.02
# Julie Fortin
# julie.fortin@ubc.ca

###############
### SUMMARY ###
###############

# We are looking at altmetric scores of publications in 7 idiotypic journals
# And looking at the gender of the authors of those publications
# To see if there are any interesting patterns in terms of altmetrics & gender bias

# Citation for genderizeR package
# Kamil Wais, Gender Prediction Methods Based on First Names with genderizeR, The R Journal,Vol. 8/1, Aug. 2016, https://journal.r-project.org/archive/2016-1/wais.pdf


##############
### SET UP ###
##############

## Load packages
# install.packages("data.table")
# install.packages("genderizeR")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("here")
library(data.table)
library(genderizeR)
library(dplyr)
library(lubridate)
library(stringr)
library(here)

## Set global variables
subsetstartyear <- 2011 # Look at articles starting in 2011 (the year Altmetric started keeping scores)
subsetendyear <- 2019 # Analysis was done end of 2019; subsetting years < 2019 will allow us to use 1-year scores from 2018
confthreshold <- 0.95 # If genderizeR probability for a name is above this threshold, we will assume it is a first name (we do this to avoid making unnecessary api requests)
countthreshold <- 100 # Setting a count threshold of 100 increases reliability that words found are actually names (as in https://cran.microsoft.com/snapshot/2016-10-05/web/packages/genderizeR/vignettes/tutorial.html)
apikey <- "[insert api key here]" # We purchased a subscription via store.genderize.io to genderize all the names in our dataset 


#################
### FUNCTIONS ###
#################

## This function reads in a tsv file which contains all articles for a single journal, cleans columns, and returns a dataframe
afn.loaddata <- function(filename) {
  
  # Read in data
  data <- fread(filename,sep="\t")
  
  # Fix data columns
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
  
  return(data)
  
}

## This function reads a single journal's dataframe, removes articles with no author/date, and returns the cleaned dataframe
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
  
  # Create new vars for year and month
  pubyear <- year(as.Date(cleandata$date))
  pubmonth <- month(as.Date(cleandata$date))
  cleandata <- cbind(cleandata,pubyear,pubmonth)
  
  return(cleandata)
  
}

## This function reads a cleaned dataframe, selects only articles within a specified year range, and returns the subsetted dataframe
afn.subsetdata <- function(data, startyear, endyear) {
  
  # Select publications within given year range
  subset <- data[which(data$pubyear>=startyear),]
  subset <- subset[which(subset$pubyear<endyear),] # don't take things that aren't published yet
  
  return(subset)
  
}

## This function reads a subsetted dataframe, genderizes the authors of each article, and writes out a new tsv file with the original data plus gender data
## This process happens in several steps; hence other functions are called within it
## 1. Extracts, cleans and genderizes authors using afn.getauthors
## 2. Calculates gender stats from genderized names using afn.getgenderstats
## 3. Binds gender stats and first/last author gender/probability to each article
## 4. Writes output file
afn.assessonejournal <- function(journal_data, outfile) {
  
  newjournaldata <- c()
  
  # Loop through all articles (each article is a row)
  for (i in 1:dim(journal_data)[[1]]) {

    # Pull data for just one article
    print(paste("On article",i,"of journal",journal_data$journal[i]))
    journal_article <- journal_data[i,]
    
    # Extract authors
    article_authors <- afn.getauthors(journal_article)
    authors_gendered <- article_authors[[1]] # all authors
    authors_format <- article_authors[[2]] # what did the function determine was the format (firstname lastname, last first, etc)
    authors_order <- article_authors[[3]] # ordered vector of all authors
    authors_num <- article_authors[[4]] # number of authors
    
    # Looking at all authors of the article
    article_stats <- afn.getgenderstats(authors_gendered, authors_num)
    
    # Now just first and last author
    firstauthor <- authors_order[1]
    lastauthor <- authors_order[length(authors_order)]
    firstgender <- authors_gendered$gender[1]
    firstgenderprob <- authors_gendered$probability[1]
    lastgender <- authors_gendered$gender[authors_num]
    lastgenderprob <- authors_gendered$probability[authors_num]
    
    firstlast_stats <- data.frame(firstgender, firstgenderprob, lastgender, lastgenderprob)
    article_stats <- cbind(article_stats,firstlast_stats)
    
    # Could also be helpful to add cleaned authors as a variable to see how it was cleaned
    cleaned_authors <- authors_order[1]
    if (length(authors_order)>1) {
      for (j in 2:length(authors_order)) {
        cleaned_authors <- paste(cleaned_authors,authors_order[j],sep=", ")
      }
    }
    
    # Now put everything together
    journal_article <- cbind(journal_article, article_stats, authors_format, cleaned_authors)
    newjournaldata <- rbind(newjournaldata, journal_article)
    assign("newjournaldata",newjournaldata,envir = .GlobalEnv)
  }
  
  # Write output
  fwrite(newjournaldata, outfile, sep="\t", append=TRUE)
  
  return(newjournaldata)
  
}

## This function reads a single article, cleans the authors, determines author format, genderizes the authors, and returns all this information to afn.assessonejournal
## This process happens in several steps; hence other functions are called within it
## 1. Takes the author variable from the article
## 2. Cleans the authors using afn.cleanauthors
## 3. Determines the author format (e.g. "Firstname Lastname" or "Lastname, F") using afn.checkformat
## 4. Genderizes authors by calling genderizeR only on terms that are assumed to be first names (as determined by step 3)
## 5. Returns genderized names, format, cleaned authors, and number of authors to afn.assessonejournal
afn.getauthors <- function(article) {
  
  # Extract authors variable
  authors <- article$authors
  
  # Authors as scraped by Altmetric need to be cleaned
  cleanedauthors <- afn.cleanauthors(authors)
  numauthors <- cleanedauthors[[2]]
  cleanedauthors <- cleanedauthors[[1]]
  
  # Author format (i.e. Last First, Last F, First M Last, etc.) is inconsistent; figure out format for first three authors
  authorsformat <- afn.checkformat(cleanedauthors)
  firstthree <- authorsformat[[1]] # returns the first three authors
  format <- authorsformat[[2]] # returns the format as determined by looking at the first three authors
  numauthors <- authorsformat[[3]] # returns the number of authors
  nondupauthors <- authorsformat[[4]] # returns the full list of authors, with duplicates removed
  
  # Assume the format is the same for the rest of the author list & genderize remaining authors 
  allgenderizednames <- firstthree
  if (numauthors > 3) {
    for (i in 4:numauthors) { # already have first three authors, only need to do 4th onward
      
      author <- nondupauthors[i]
      author <- gsub("^[A-z] ","",author) # get rid of initials
      author <- gsub(" [A-z]$","",author)
      
      names <- strsplit(author," ") # split names into first and last
      
      if (length(names[[1]])>1) { # there is only one term, most likely it is a last name, don't bother genderizing
        
        if (format == "firstlast") {
          name1 <- textPrepare(names[[1]][1]) # only look at the first term as it should be the first name
          if (length(name1)>0) { # needs to have found a name
            genderizedname <- findGivenNames(name1,apikey=apikey)[1]
          } else {
            genderizedname <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
          }
          
        } else if (format == "lastfirst") {
          name2 <- textPrepare(names[[1]][2]) # only look at the second term as it should be the first name
          if (length(name2)>0) { # needs to have found a name
            genderizedname <- findGivenNames(name2,apikey=apikey)[1]
          } else {
            genderizedname <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
          }
          
        } else { # format wasn't firstlast or lastfirst; didn't include a first name
          genderizedname <- data.frame(name=names[[1]][1], gender=NA, probability=NA, count=NA, country_id=NA)
        }
        
        allgenderizednames <- rbind(allgenderizednames, genderizedname)
      }
    }
  }
  
  return(list(allgenderizednames, format, nondupauthors, numauthors))
  
}

## This function reads genderized authors and number of authors (from afn.getauthors), counts the number of male/female/unknown and proportion of male/female/unknown, and returns this information to afn.assessonejournal
afn.getgenderstats <- function(names, numauthors) {
  
  # Get counts for each
  males <- length(which(names$gender=="male"))
  females <- length(which(names$gender=="female"))
  unknown <- numauthors-(males+females)
  propmales <- males/numauthors
  propfemales <- females/numauthors
  propunknown <- unknown/numauthors
  
  # Create output
  genderstats <- data.frame(males, females, unknown, propmales, propfemales, propunknown, numauthors)
  
  return(genderstats)
  
}

## This function reads the author variable of an article, cleans the strings, separates terms and initials, counts the number of authors and returns this information to afn.getauthors
afn.cleanauthors <- function(authorsvector) {
  
  # Remove strange characters
  authors <- gsub("\\\\xa0"," ",authorsvector)
  authors <- gsub("[^[:alnum:]^\'^-]"," ",authors)
  authors <- gsub("[[:digit:]+]","",authors)
  authors <- gsub("-[[:lower:]] ","- ",authors)
  
  # Split author list into vector
  authorsvec <- strsplit(authors,"\' ")[[1]]
  
  # Clean white space and quotation marks/dashes
  authorsvec <- gsub(" \'", "", authorsvec)
  authorsvec <- gsub("   "," ",authorsvec)
  authorsvec <- gsub("  ", " ", authorsvec)
  authorsvec <- gsub("- "," ",authorsvec)
  authorsvec <- gsub(" -"," ",authorsvec)
  
  # Clean terms that aren't standalone names 
  authorsvec <- gsub("^von ","von",authorsvec)
  authorsvec <- gsub(" von "," von",authorsvec)
  authorsvec <- gsub("Van de ","VanDe",authorsvec)
  authorsvec <- gsub("^de ","de",authorsvec)
  authorsvec <- gsub(" de "," de",authorsvec)
  authorsvec <- gsub("Jr.","",authorsvec)
  authorsvec <- gsub("Sr.","",authorsvec)
  
  # Loop through all authors
  for (i in 1:length(authorsvec)) {
    
    # Look at one author at a time
    auth <- authorsvec[i]
    
    # Could have two initials in the name (e.g. Smith, JA or Smith, J A) - find these cases and replace with a single letter
    initials <- str_extract(auth,"[A-Z]{2}") # extract more than one consecutive uppercase letter
    spacedinitials <- str_extract(auth,"[A-Z] [A-Z] ")
    spacedinitials2 <- str_extract(auth,"[A-Z] [A-Z]$")
    dashedinitials <- str_extract(auth,"[A-Z]-[A-Z] ")
    dashedinitials2 <- str_extract(auth,"[A-Z]-[A-Z]$")
    if (!is.na(initials)) {
      initial <- substr(initials,1,1) # take only the first one
      auth <- gsub(initials,initial,auth) # turn FM initials into just F
      authorsvec[i] <- auth
    }
    if (!is.na(spacedinitials)) {
      initial <- substr(spacedinitials,1,2)
      auth <- gsub(spacedinitials,initial,auth)
      authorsvec[i] <- auth
    }
    if (!is.na(spacedinitials2)) {
      initial <- substr(spacedinitials2,1,1)
      auth <- gsub(spacedinitials2,initial,auth)
      authorsvec[i] <- auth
    }
    if (!is.na(dashedinitials)) {
      initial <- substr(dashedinitials,1,1)
      auth <- gsub(dashedinitials,paste(initial," ",sep=""),auth) # takes just first initial and adds space
      authorsvec[i] <- auth
    }
    if (!is.na(dashedinitials2)) {
      initial <- substr(dashedinitials2,1,1)
      auth <- gsub(dashedinitials2,paste(initial," ",sep=""),auth) # takes just first initial and adds space
      authorsvec[i] <- auth
    }
  }
  
  # Clean remaining vector
  authorsvec <- gsub(" [A-z] "," ",authorsvec) # get rid of any lone initials (most likely middle initial)
  authorsvec <- trimws(authorsvec) # remove white space
  authorsvec <- authorsvec[which(authorsvec!="")] # don't include authors that are blank
  
  # Get the number of authors
  numauthors <- length(authorsvec)
  
  return(list(authorsvec, numauthors))
  
}

## This function reads the cleaned authors (from afn.cleanauthors), checks author format and returns the three first authors genderized, author format, number of authors and original cleaned authors to afn.getauthors
## This process happens in several steps; hence other functions
## 1. Takes the cleaned authors from afn.cleanauthors
## 2. Removes duplicated author names using afn.checkduplicateauthors
## 3. Pulls the first three authors (or fewer if there are fewer than three total authors)
## 4. Checks the format of those three authors using afn.testfirstfew
## 5. Determines which format is the most frequent in the first three authors
## 6. Returns the genderized first three authors, format, number of authors and original cleaned authors
afn.checkformat <- function(authorvec) {
  
  # Many articles have authors duplicated, clean and count authors
  nonduplicatedauthors <- afn.checkduplicateauthors(authorvec)
  authorvec <- nonduplicatedauthors[[1]]
  numauthors <- nonduplicatedauthors[[2]]
  
  # Name format should be one of:
  # First Last; First Middle Last; Last First; Last First Middle
  # First name is only ever first or second term
  # So we can ignore third term
  
  formats <- c()
  genderedauthors <- data.frame(name=character(),gender=factor(),probability=numeric(),count=numeric(),country_id=factor())
  
  # Let's determine the pattern from the first 3 authors
  if (length(authorvec)==0) { # all authors were removed by afn.checkduplicatedauthors, which means they were all nofirstname
    genderedauthors <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
    authorformat <- "nofirstname"
  } else {
    
    # If there are 1 or 2 authors, only look at 1 or 2. If there are 3 or more, only look at 3.
    if (length(authorvec) < 3) {
      loops <- length(authorvec)
    } else if (length(authorvec) >= 3) {
      loops <- 3
    }
    for (i in 1:loops) {
      
      # Take the first 3 authors, genderize both 1st and 2nd term, see which has higher prob in genderizeR. Assume that one is the first name therefore you know the format
      formattest <- afn.testfirstfew(authorvec, i, confthreshold)
      format <- formattest[[1]]
      genderedauthor <- formattest[[2]]
      
      # Add to outputs
      formats <- c(formats, format)
      genderedauthors <- rbind(genderedauthors, genderedauthor)
    }
    
    # Based on first three, does it seem like format is firstlast or lastfirst?
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
  }
  
  return(list(genderedauthors, authorformat, numauthors, authorvec)) # return the genderized ones so we don't have to do them again, plus the format
  
}

## This function reads the first three authors, genderizes first and second terms, and based on the probability of each term, determines which is most likely a first name, returning the format and the genderized first three authors
afn.testfirstfew <- function(authors, i, confthresh) {
  
  # Extract author i
  author <- authors[i]
  author <- gsub("^[A-Z] ","",author) # remove initials
  author <- gsub(" [A-Z]$","",author)
  names <- strsplit(author," ")
  
  if (length(names[[1]])>1) { # if there is only one term, most likely it is a last name so don't bother genderizing
    
    # Extract both terms from author i
    term1 <- textPrepare(names[[1]][1])
    term2 <- textPrepare(names[[1]][2])
    
    if (length(term1)>0) {
      
      # Genderize term1
      genderedterm1 <- findGivenNames(term1,apikey=apikey)[1]
      genderedterm1 <- genderedterm1[count>countthreshold] # rules out terms that are probably not first names
      
      if (dim(genderedterm1)[[1]]==0) { # term1 isn't in the db > countthreshold times
        prob1 <- 0.5 # it could go either way
      } else {
        prob1 <- as.numeric(genderedterm1$probability)
      }
      
    } else { # textPrepare didn't find a name
      genderedterm1 <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
      prob1 <- 0.5
    }
    
    # If term1 is 95% prob male or female, it's very likely a first name, don't bother genderizing next terms
    if (prob1 >= confthresh) { 
      format <- "firstlast"
      genderedauthor <- genderedterm1
    } else { # If uncertain that term1 is firstname, check term2
      
      if (length(term2)>0) {

        # Genderize term2
        genderedterm2 <- findGivenNames(term2,apikey=apikey)[1]
        genderedterm2 <- genderedterm2[count>countthreshold]
        
        if (dim(genderedterm2)[[1]]==0) { # term2 isn't in the db (enough times)
          prob2 <- 0.5 # assuming equally likely either way
        } else {
          prob2 <- as.numeric(genderedterm2$probability)
        }
        
      } else {
        genderedterm2 <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
        prob2 <- 0.5
      }
      
      if (prob2 > prob1) { # whichever one is more confident assume is firstname
        format <- "lastfirst"
        genderedauthor <- genderedterm2
      } else if (prob1 > prob2) {
        format <- "firstlast"
        genderedauthor <- genderedterm1
      } else if (prob1 == prob2) {
        format <- "unknown"
        genderedauthor <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
      } 
    }
  } else {
    format <- "nofirstname"
    genderedauthor <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
  }
  
  return(list(format, genderedauthor))
  
}

## This function reads in a cleaned author vector, checks whether any author name appears more than once, removes any duplicates and returns the non-duplicated authors
afn.checkduplicateauthors <- function(authorvector) {
  
  # If there is only one author, it can't be duplicated
  if (length(authorvector)==1) { 
    newauthorvector <- authorvector
    numnondups <- 1
  } else {
    
    dupdf <- data.frame(authorposition=integer(),
                        term1=character(),
                        term2=character(),
                        term1dup=logical(),
                        term2dup=logical(),
                        authordup=logical())
    
    for (i in 1:(length(authorvector)-1)) { # -1 because if the last one is a dup you would have seen it by the last iteration
      
      # Extract the first author
      author <- authorvector[i]
      terms <- strsplit(author," ")[[1]]
      term1 <- terms[1]
      term2 <- terms[2]
      
      # Compare that first author to all the other authors in the vector to see whether it has been duplicated
      restofauthors <- authorvector[-(1:i)] # remove all authors who have already been checked
      nextauthors <- strsplit(restofauthors," ")
      for (j in 1:length(restofauthors)) {
        
        # Take each subsequent author one at a time
        nextauthor <- nextauthors[[j]]
        index <- which(authorvector==restofauthors[j])
        
        # Check whether term1 from the first author is included in subsequent authors
        term1dup <- term1 %in% nextauthor
        if (!term1dup) {
          
          # Maybe didn't match because term1 is spelled out in author but initial in nextauthor
          term1i <- substr(term1,1,1) # check term1's initial
          term1dup_i <- term1i %in% nextauthor
          if (term1dup_i) { # they matched!
            match1 <- term1 # term1 was spelled out
          }
          
          # Maybe didn't match because term1 is an initial in author but spelled out in nextauthor
          nextauthori <- substr(nextauthor,1,1)
          term1dup_ii <- term1 %in% nextauthori
          if (term1dup_ii) {
            match1 <- nextauthor[which(term1==nextauthori)] # term1 was initial but it was spelled out in nextauthor
          }
          
          # Didn't match because there's no match (if both false, term2dup will be FALSE; if not, will be TRUE)
          term1dup <- term1dup_i | term1dup_ii
        } else {
          match1 <- term1 # they matched; either both spelled out or both initial
        }
        
        # Check whether term2 from the first author is included in subsequent authors
        term2dup <- term2 %in% nextauthor
        if (!term2dup) {
          
          # Maybe didn't match because term2 is spelled out in author but initial in nextauthor
          term2i <- substr(term2,1,1)
          term2dup_i <- term2i %in% nextauthor
          if (term2dup_i) {
            match2 <- term2 # term2 was spelled out in author but not in nextauthor
          }
          
          # Maybe didn't match because term2 is an initial in author but spelled out in nextauthor
          nextauthori <- substr(nextauthor,1,1)
          term2dup_ii <- term2 %in% nextauthori
          if (term2dup_ii) {
            if (length(which(term2==nextauthori))>1) { # this means that both terms start with same letter, and term2 is that letter (e.g. Yang Yul)
              match2 <- nextauthor[which(term1!=nextauthor)] # makes sure to replace the term that is not term1
            } else {
              match2 <- nextauthor[which(term2==nextauthori)] # term2 was spelled out in nextauthor   
            }
          }
          
          # Didn't match because there's no match (if both false, term2dup will still be FALSE; if not, will be TRUE)
          term2dup <- term2dup_i | term2dup_ii
        } else {
          match2 <- term2 # they matched; either both spelled out or both initial
        }
        
        # If both terms from author1 are duplicated in author2, they have the same name and therefore are duplicates
        if (term1dup && term2dup) {
          authordup <- TRUE # tells if duplicated later, but the later duplicate will be FALSE bc restofauthors removes previous authors
          authorvector[index] <- paste(match1,match2)
          break
        } else {
          authordup <- FALSE
        }
      }
      
      row <- data.frame(authorposition=i,
                        term1=term1,
                        term2=term2,
                        term1dup=term1dup,
                        term2dup=term2dup,
                        authordup=authordup)
      dupdf <- rbind(dupdf,row)
    }
    
    # Take the last author
    t <- strsplit(authorvector[length(authorvector)]," ")[[1]]
    t1 <- t[1]
    t2 <- t[2]
    lastrow <- data.frame(authorposition=length(authorvector),
                          term1=t1,
                          term2=t2,
                          term1dup=FALSE,
                          term2dup=FALSE,
                          authordup=FALSE)
    dupdf <- rbind(dupdf,lastrow)
    
    # Remove authors that have been duplicated from the author list
    removedups <- dupdf[which(!dupdf$authordup),]
    numnondups <- dim(removedups)[[1]]
    newauthorvector <- paste(removedups$term1,removedups$term2)
    
  }
  
  return(list(newauthorvector,numnondups))
}


######################
### RUN BY JOURNAL ###
######################

fname_biorxiv <- here("AltmetricData/bioRxiv.tsv")
outname_biorxiv <- here("GenderizedData/GenderizedBioRxiv.tsv")
alldata_biorxiv <- afn.loaddata(fname_biorxiv)
cleandata_biorxiv <- afn.cleandata(alldata_biorxiv)
data_biorxiv <- afn.subsetdata(cleandata_biorxiv, subsetstartyear, subsetendyear)
newdata_biorxiv <- afn.assessonejournal(data_biorxiv,outname_biorxiv)

fname_cell <- here("AltmetricData/Cell.tsv")
outname_cell <- here("GenderizedData/GenderizedCell.tsv")
alldata_cell <- afn.loaddata(fname_cell)
cleandata_cell <- afn.cleandata(alldata_cell)
data_cell <- afn.subsetdata(cleandata_cell, subsetstartyear, subsetendyear)
newdata_cell <- afn.assessonejournal(data_cell,outname_cell)

fname_nature <- here("AltmetricData/Nature.tsv")
outname_nature <- here("GenderizedData/GenderizedNature.tsv")
alldata_nature <- afn.loaddata(fname_nature)
cleandata_nature <- afn.cleandata(alldata_nature)
data_nature <- afn.subsetdata(cleandata_nature, subsetstartyear, subsetendyear)
newdata_nature <- afn.assessonejournal(data_nature,outname_nature)

fname_nejm <- here("AltmetricData/NEJM.tsv")
outname_nejm <- here("GenderizedData/GenderizedNEJM.tsv")
alldata_nejm <- afn.loaddata(fname_nejm)
cleandata_nejm <- afn.cleandata(alldata_nejm)
data_nejm <- afn.subsetdata(cleandata_nejm, subsetstartyear, subsetendyear)
newdata_nejm <- afn.assessonejournal(data_nejm,outname_nejm)

fname_plos <- here("AltmetricData/PLoS ONE.tsv")
outname_plos <- here("GenderizedData/GenderizedPLoSONE.tsv")
alldata_plos <- afn.loaddata(fname_plos)
cleandata_plos <- afn.cleandata(alldata_plos)
data_plos <- afn.subsetdata(cleandata_plos, subsetstartyear, subsetendyear)
newdata_plos <- afn.assessonejournal(data_plos,outname_plos)

fname_pnas <- here("AltmetricData/PNAS.tsv")
outname_pnas <- here("GenderizedData/GenderizedPNAS.tsv")
alldata_pnas <- afn.loaddata(fname_pnas)
cleandata_pnas <- afn.cleandata(alldata_pnas)
data_pnas <- afn.subsetdata(cleandata_pnas, subsetstartyear, subsetendyear)
newdata_pnas <- afn.assessonejournal(data_pnas,outname_pnas)

fname_science <- here("AltmetricData/Science.tsv")
outname_science <- here("GenderizedData/GenderizedScience.tsv")
alldata_science <- afn.loaddata(fname_science)
cleandata_science <- afn.cleandata(alldata_science)
data_science <- afn.subsetdata(cleandata_science, subsetstartyear, subsetendyear)
newdata_science <- afn.assessonejournal(data_science,outname_science)
