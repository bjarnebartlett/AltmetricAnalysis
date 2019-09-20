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
# install.packages("stringr")
library(data.table)
library(genderizeR)
library(dplyr)
library(lubridate)
library(stringr)

subsetyear <- 2011

probabilitythreshold <- 0.9
confthreshold <- 0.95

# use only for testing
# startarticlenum <- 1
# endarticlenum <- 20

printcleaninginfo <- FALSE


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
  subset <- subset[which(subset$pubyear<2019),] # don't take things that aren't published yet
  
  return(subset)
  
}

afn.assessonejournal <- function(journal_data, outfile) {
  
  newjournaldata <- c()
  
  # for (i in 1:dim(journal_data)[[1]]) { # go through all articles
  for (i in 30001:30000) {
  # for (i in startarticlenum:endarticlenum) { { # for testing purposes to keep within 1000 api limit
    # pull data for just one article
    
    print(paste("On article",i,"of journal",journal_data$journal[i]))
    journal_article <- journal_data[i,]
    
    # extract authors
    article_authors <- afn.getauthors(journal_article)
    authors_gendered <- article_authors[[1]] # all authors
    authors_format <- article_authors[[2]]
    authors_order <- article_authors[[3]] # ordered vector of all authors
    authors_num <- article_authors[[4]] # number of authors
    
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
    
    # could also be helpful to add cleaned authors as a variable to see how it was cleaned
    cleaned_authors <- authors_order[1]
    if (length(authors_order)>1) {
      for (j in 2:length(authors_order)) {
        cleaned_authors <- paste(cleaned_authors,authors_order[j],sep=", ")
      }
    }
    
    # now put everything together
    journal_article <- cbind(journal_article, article_stats, authors_format, cleaned_authors)
    newjournaldata <- rbind(newjournaldata, journal_article)
    assign("newjournaldata",newjournaldata,envir = .GlobalEnv)
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
  numauthors <- authorsformat[[3]]
  nondupauthors <- authorsformat[[4]]
  
  allgenderizednames <- firstthree
  
  if (numauthors > 3) {
    for (i in 4:numauthors) {
      
      author <- nondupauthors[i]
      author <- gsub("^[A-z] ","",author) # if there are any legacy initials from afn.checkduplicateauthors
      author <- gsub(" [A-z]$","",author) # get rid of them
      
      names <- strsplit(author," ")
      
      if (length(names[[1]])>1) {
        # there is only one term, most likely it is a last name 
        # don't bother genderizing
        if (format == "firstlast") {
          # genderizedname <- findGivenNames(names[[1]][1])[1] # look only at first - if there are composite names (Wei-Ming) only the first will be looked at
          name1 <- textPrepare(names[[1]][1])
          if (length(name1)>0) {
            genderizedname <- findGivenNames(name1,apikey="d92354e95b4ff49e7944cd9395e4f908")[1]
          } else {
            genderizedname <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
          }
          
        } else if (format == "lastfirst") {
          # genderizedname <- findGivenNames(names[[1]][2])[1]
          name2 <- textPrepare(names[[1]][2])
          if (length(name2)>0) {
            genderizedname <- findGivenNames(name2,apikey="d92354e95b4ff49e7944cd9395e4f908")[1]
          } else {
            genderizedname <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
          }
          
        } else {
          genderizedname <- data.frame(name=names[[1]][1], gender=NA, probability=NA, count=NA, country_id=NA)
        }
        allgenderizednames <- rbind(allgenderizednames, genderizedname)
      }
    }
  }
  
  return(list(allgenderizednames, format, nondupauthors, numauthors))
  
}

afn.getgenderstats <- function(names, numauthors) {
  
  males <- length(which(names$gender=="male"))
  females <- length(which(names$gender=="female"))
  unknown <- numauthors-(males+females)
  propmales <- males/numauthors
  propfemales <- females/numauthors
  propunknown <- unknown/numauthors
  
  genderstats <- data.frame(males,females, unknown, propmales, propfemales, propunknown, numauthors)
  
  return(genderstats)
  
}

afn.cleanauthors <- function(authorsvector) {
  
  authors <- gsub("\\\\xa0"," ",authorsvector)
  authors <- gsub("[^[:alnum:]^\'^-]"," ",authors)
  authors <- gsub("[[:digit:]+]","",authors)
  authors <- gsub("-[[:lower:]] ","- ",authors)
  
  authorsvec <- strsplit(authors,"\' ")[[1]]
  authorsvec <- gsub(" \'", "", authorsvec)
  authorsvec <- gsub("   "," ",authorsvec)
  authorsvec <- gsub("  ", " ", authorsvec)
  authorsvec <- gsub("- "," ",authorsvec)
  authorsvec <- gsub(" -"," ",authorsvec)
  
  authorsvec <- gsub("^von ","von",authorsvec)
  authorsvec <- gsub(" von "," von",authorsvec)
  authorsvec <- gsub("Van de ","VanDe",authorsvec)
  authorsvec <- gsub("^de ","de",authorsvec)
  authorsvec <- gsub(" de "," de",authorsvec)
  authorsvec <- gsub("Jr.","",authorsvec)
  authorsvec <- gsub("Sr.","",authorsvec)
  
  
  for (i in 1:length(authorsvec)) {
    auth <- authorsvec[i]
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
  authorsvec <- gsub(" [A-z] "," ",authorsvec) # get rid of any lone initials (most likely middle initial)
  
  # authorsvec <- gsub("[A-Z] ","",authorsvec) # get rid of beginning/middle initials
  # authorsvec <- gsub(" [A-Z]$","",authorsvec) # get rid of initials at end

  authorsvec <- trimws(authorsvec)
  authorsvec <- authorsvec[which(authorsvec!="")]
  
  numauthors <- length(authorsvec)
  
  return(list(authorsvec, numauthors))
  
}

afn.checkformat <- function(authorvec) {
  
  print("in afn.checkformat; authors before duplicate check:")
  print(authorvec) 
  nonduplicatedauthors <- afn.checkduplicateauthors(authorvec)
  authorvec <- nonduplicatedauthors[[1]]
  numauthors <- nonduplicatedauthors[[2]]
  print("authors after duplicate check:")
  print(authorvec)
  print(paste("total",numauthors,"authors, including authors with only last names which were removed in the above vec"))
  
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
  if (length(authorvec)==0) { # all authors were removed by afn.checkduplicatedauthors, which probably means they were all nofirstname
    genderedauthors <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
    authorformat <- "nofirstname"
  } else {
    
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
  }
  
  return(list(genderedauthors, authorformat, numauthors, authorvec)) # return the genderized ones so we don't have to do them again, plus the format
  
}

afn.testfirstfew <- function(authors, i, confthresh) {
  
  author <- authors[i]
  author <- gsub("^[A-Z] ","",author) # if there are any legacy initials from afn.checkduplicateauthors
  author <- gsub(" [A-Z]$","",author) # get rid of them
  
  names <- strsplit(author," ")
  
  if (length(names[[1]])>1) { # if there is only one term, most likely it is a last name so don't bother genderizing
    
    term1 <- textPrepare(names[[1]][1])
    term2 <- textPrepare(names[[1]][2])
    
    # genderedterm1 <- findGivenNames(term1)[1]
    if (length(term1)>0) {
      genderedterm1 <- findGivenNames(term1,apikey="d92354e95b4ff49e7944cd9395e4f908")[1]
      genderedterm1 <- genderedterm1[count>100] # rules out terms that are probably not first names?
      
      if (dim(genderedterm1)[[1]]==0) { # term1 isn't in the db (enough times)
        prob1 <- 0.5 # it could go either way
      } else {
        prob1 <- as.numeric(genderedterm1$probability)
      }
      
    } else {
      genderedterm1 <- data.frame(name=NA,gender=NA,probability=NA,count=NA,country_id=NA)
      prob1 <- 0.5
    }
    
    if (prob1 >= confthresh) { # if term is 90% prob male or female, it's likely a first name, don't bother genderizing next terms
      format <- "firstlast"
      genderedauthor <- genderedterm1
    } else { # if uncertain that term1 is firstname, check term2
      
      if (length(term2)>0) {
        # genderedterm2 <- findGivenNames(term2)[1]
        genderedterm2 <- findGivenNames(term2,apikey="d92354e95b4ff49e7944cd9395e4f908")[1]
        genderedterm2 <- genderedterm2[count>100]
        
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
  
  print(format)
  print(genderedauthor)
  return(list(format, genderedauthor))
  
}

afn.checkduplicateauthors <- function(authorvector) {
  
  dupdf <- data.frame(authorposition=integer(),
                      term1=character(),
                      term2=character(),
                      term1dup=logical(),
                      term2dup=logical(),
                      authordup=logical())
  
  if (length(authorvector)==1) { # if there is only one author, it can't be duplicated
    newauthorvector <- authorvector
    numnondups <- 1
  } else {
    
    for (i in 1:(length(authorvector)-1)) { # -1 because if the last one is a dup you would have seen it by the last iteration
      
      author <- authorvector[i]
      terms <- strsplit(author," ")[[1]]
      term1 <- terms[1]
      term2 <- terms[2]
      
      restofauthors <- authorvector[-(1:i)] # remove all authors who have already been checked
      nextauthors <- strsplit(restofauthors," ")
      
      for (j in 1:length(restofauthors)) {
        
        nextauthor <- nextauthors[[j]]
        index <- which(authorvector==restofauthors[j])
        
        term1dup <- term1 %in% nextauthor # is term1 duplicated in this author
        if (!term1dup) {
          # maybe didn't match because term1 is spelled out in author but initial in nextauthor
          term1i <- substr(term1,1,1)
          term1dup_i <- term1i %in% nextauthor
          if (term1dup_i) { # they matched!
            match1 <- term1 # term1 was spelled out
          }
          
          # maybe didn't match because term1 is an initial in author but spelled out in nextauthor
          nextauthori <- substr(nextauthor,1,1)
          term1dup_ii <- term1 %in% nextauthori
          if (term1dup_ii) {
            match1 <- nextauthor[which(term1==nextauthori)] # term1 was initial but it was spelled out in nextauthor
          }
          
          # didn't match because there's no match (if both false, term2dup will be FALSE; if not, will be TRUE)
          term1dup <- term1dup_i | term1dup_ii
        } else {
          match1 <- term1 # they matched; either both spelled out or both initial
        }
          
        term2dup <- term2 %in% nextauthor # is term2 duplicated in this author
        if (!term2dup) {
          
          # maybe didn't match because term2 is spelled out in author but initial in nextauthor
          term2i <- substr(term2,1,1)
          term2dup_i <- term2i %in% nextauthor
          if (term2dup_i) {
            match2 <- term2 # term2 was spelled out in author but not in nextauthor
          }
          
          # maybe didn't match because term2 is an initial in author but spelled out in nextauthor
          nextauthori <- substr(nextauthor,1,1)
          term2dup_ii <- term2 %in% nextauthori
          if (term2dup_ii) {
            if (length(which(term2==nextauthori))>1) { # this means that both terms start with same letter, and term2 is that letter (e.g. Yang Yul)
              match2 <- nextauthor[which(term1!=nextauthor)] # makes sure to replace the term that is not term1
            } else {
              match2 <- nextauthor[which(term2==nextauthori)] # term2 was spelled out in nextauthor   
            }
          }
          
          # didn't match because there's no match (if both false, term2dup will still be FALSE; if not, will be TRUE)
          term2dup <- term2dup_i | term2dup_ii
        } else {
          match2 <- term2 # they matched; either both spelled out or both initial
        }
        
        if (term1dup && term2dup) { # if both true
          authordup <- TRUE # tells if duplicated later, but the later duplicate will be FALSE bc restofauthors removes previous authors
          print(paste("i",i,"j",j))
          print(paste("index",index))
          print(paste("authorvector[index]",authorvector[index]))
          print(paste("match1",match1))
          print(paste("match2",match2))
          print(paste("paste(match1,match2)",paste(match1,match2)))
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
    
    removedups <- dupdf[which(!dupdf$authordup),]
    numnondups <- dim(removedups)[[1]]
    newauthorvector <- paste(removedups$term1,removedups$term2)
    
  }
  
  
  return(list(newauthorvector,numnondups))
}


######################
### RUN BY JOURNAL ###
######################

fname_biorxiv <- "./Data091019/bioRxiv.tsv"
outname_biorxiv <- "DataAnalysis/GenderizedByJournal/NewBioRxiv.tsv"
alldata_biorxiv <- afn.loaddata(fname_biorxiv)
cleandata_biorxiv <- afn.cleandata(alldata_biorxiv)
data_biorxiv <- afn.subsetdata(cleandata_biorxiv,subsetyear)
newdata_biorxiv <- afn.assessonejournal(data_biorxiv,outname_biorxiv)

fname_cell <- "./Data091019/Cell.tsv"
outname_cell    <- "DataAnalysis/GenderizedByJournal/NewCell.tsv"
alldata_cell <- afn.loaddata(fname_cell)
cleandata_cell <- afn.cleandata(alldata_cell)
data_cell <- afn.subsetdata(cleandata_cell,subsetyear)
newdata_cell <- afn.assessonejournal(data_cell,outname_cell)

fname_nature <- "./Data091019/Nature.tsv"
outname_nature  <- "DataAnalysis/GenderizedByJournal/NewNature.tsv"
alldata_nature <- afn.loaddata(fname_nature)
cleandata_nature <- afn.cleandata(alldata_nature)
data_nature <- afn.subsetdata(cleandata_nature,subsetyear)
newdata_nature <- afn.assessonejournal(data_nature,outname_nature)

fname_nejm <- "./Data091019/NEJM.tsv"
outname_nejm    <- "DataAnalysis/GenderizedByJournal/NewNEJM.tsv"
alldata_nejm <- afn.loaddata(fname_nejm)
cleandata_nejm <- afn.cleandata(alldata_nejm)
data_nejm <- afn.subsetdata(cleandata_nejm,subsetyear)
newdata_nejm <- afn.assessonejournal(data_nejm,outname_nejm)

fname_plos <- "./Data091019/PLoS ONE.tsv"
outname_plos    <- "DataAnalysis/GenderizedByJournal/NewPLoSONE.tsv"
alldata_plos <- afn.loaddata(fname_plos)
cleandata_plos <- afn.cleandata(alldata_plos)
data_plos <- afn.subsetdata(cleandata_plos,subsetyear)
newdata_plos <- afn.assessonejournal(data_plos,outname_plos)

fname_pnas <- "./Data091019/PNAS.tsv"
outname_pnas    <- "DataAnalysis/GenderizedByJournal/NewPNAS.tsv"
alldata_pnas <- afn.loaddata(fname_pnas)
cleandata_pnas <- afn.cleandata(alldata_pnas)
data_pnas <- afn.subsetdata(cleandata_pnas,subsetyear)
newdata_pnas <- afn.assessonejournal(data_pnas,outname_pnas)

fname_science <- "./Data091019/Science.tsv"
outname_science <- "DataAnalysis/GenderizedByJournal/NewScience.tsv"
alldata_science <- afn.loaddata(fname_science)
cleandata_science <- afn.cleandata(alldata_science)
data_science <- afn.subsetdata(cleandata_science,subsetyear)
newdata_science <- afn.assessonejournal(data_science,outname_science)


##################
### PRINT INFO ###
##################

if (printcleaninginfo) {
  sink("./DataAnalysis/JournalLevelCleaning.txt")
  
  print(paste("There are",dim(alldata_biorxiv)[[1]],"articles in BioRxiv"))
  print(paste("There are",dim(cleandata_biorxiv)[[1]],"articles in BioRxiv after cleaning (i.e. removing articles with no date/no author)"))
  print(paste("There are",dim(data_biorxiv)[[1]],"articles in BioRxiv between",subsetyear,"and end of 2019"))
  print("***")
  print(paste("There are",dim(alldata_cell)[[1]],"articles in Cell"))
  print(paste("There are",dim(cleandata_cell)[[1]],"articles in Cell after cleaning (i.e. removing articles with no date/no author)"))
  print(paste("There are",dim(data_cell)[[1]],"articles in Cell between",subsetyear,"and end of 2019"))
  print("***")
  print(paste("There are",dim(alldata_nature)[[1]],"articles in Nature"))
  print(paste("There are",dim(cleandata_nature)[[1]],"articles in Nature after cleaning (i.e. removing articles with no date/no author)"))
  print(paste("There are",dim(data_nature)[[1]],"articles in Nature between",subsetyear,"and end of 2019"))
  print("***")
  print(paste("There are",dim(alldata_nejm)[[1]],"articles in NEJM"))
  print(paste("There are",dim(cleandata_nejm)[[1]],"articles in NEJM after cleaning (i.e. removing articles with no date/no author)"))
  print(paste("There are",dim(data_nejm)[[1]],"articles in NEJM between",subsetyear,"and end of 2019"))
  print("***")
  print(paste("There are",dim(alldata_pnas)[[1]],"articles in PNAS"))
  print(paste("There are",dim(cleandata_pnas)[[1]],"articles in PNAS after cleaning (i.e. removing articles with no date/no author)"))
  print(paste("There are",dim(data_pnas)[[1]],"articles in PNAS between",subsetyear,"and end of 2019"))
  print("***")
  print(paste("There are",dim(alldata_science)[[1]],"articles in Science"))
  print(paste("There are",dim(cleandata_science)[[1]],"articles in Science after cleaning (i.e. removing articles with no date/no author)"))
  print(paste("There are",dim(data_science)[[1]],"articles in Science between",subsetyear,"and end of 2019"))
  print("***")
  print(paste("There are",dim(alldata_plos)[[1]],"articles in PLoS ONE"))
  print(paste("There are",dim(cleandata_plos)[[1]],"articles in PLoS ONE after cleaning (i.e. removing articles with no date/no author)"))
  print(paste("There are",dim(data_plos)[[1]],"articles in PLoS ONE between",subsetyear,"and end of 2019"))
  print("***")
  
  sink()
}



