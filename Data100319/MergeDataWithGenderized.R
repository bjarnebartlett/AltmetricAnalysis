###############################################################
###############################################################
########## MERGE DATED AND GENDERIZED ALTMETRIC DATA ##########
###############################################################
###############################################################

# 2019.10.03 Merging

###############
### SUMMARY ###
###############

# I genderized data
# Bjarne updated data to have 1D 2D 3D 5D 1W 1M 1Y altmetric scores
# Need to join genderized dataset with this new altmetric data


##############
### SET UP ###
##############

setwd("/Users/juliefortin/Documents/UBC/Projects/Altmetrics/AltmetricAnalysis/")

# install.packages("data.table")
# install.packages("dplyr")
library(data.table)
library(dplyr)


#################
### FUNCTIONS ###
#################

afn.joindata <- function(dated_filename, genderized_filename) {
  
  # Load the new data that has altmetric scores at all time steps
  dated_data <- fread(dated_filename,sep="\t",header=FALSE,skip=1,col.names = c("id",
                                                            "altmetric_id",
                                                            "authors",
                                                            "journal",
                                                            "type",
                                                            "doi",
                                                            "altmetric_score",
                                                            "date",
                                                            "1day",
                                                            "2day",
                                                            "3day",
                                                            "4day",
                                                            "5day",
                                                            "6day",
                                                            "1week",
                                                            "1month",
                                                            "3month",
                                                            "6month",
                                                            "1year",
                                                            "alltime"))
  # colnames(dated_data) <- c("id",
  #                           "altmetric_id",
  #                           "authors",
  #                           "journal",
  #                           "type",
  #                           "doi",
  #                           "altmetric_score",
  #                           "date",
  #                           "1day",
  #                           "2day",
  #                           "3day",
  #                           "4day",
  #                           "5day",
  #                           "6day",
  #                           "1week",
  #                           "1month",
  #                           "3month",
  #                           "6month",
  #                           "1year",
  #                           "alltime")
  
  # Load the genderized altmetric data
  genderized_data <- fread(genderized_filename,sep="\t")
  genderized_data <- genderized_data[,c("altmetric_id",
                                        "pubyear",
                                        "pubmonth",
                                        "males",
                                        "females",
                                        "unknown",
                                        "propmales",
                                        "propfemales",
                                        "propunknown",
                                        "numauthors",
                                        "firstgender",
                                        "firstgenderprob",
                                        "lastgender",
                                        "lastgenderprob",
                                        "authors_format",
                                        "cleaned_authors")]
  
  # Join the two datasets
  joined_data <- right_join(dated_data,genderized_data,by="altmetric_id")
  
  return(joined_data)
}


#################
### JOIN DATA ###
#################

dated_biorxiv <- "Data092719/bioRxiv.tsv"
genderized_biorxiv <- "DataAnalysis/GenderizedByJournal/NewBioRxiv.tsv"
joined_biorxiv <- afn.joindata(dated_biorxiv,genderized_biorxiv)
fwrite(joined_biorxiv,"Data100319/JoinedBioRxiv.tsv",sep="\t")

dated_cell <- "Data092719/Cell.tsv"
genderized_cell <- "DataAnalysis/GenderizedByJournal/NewCell.tsv"
joined_cell <- afn.joindata(dated_cell,genderized_cell)
fwrite(joined_cell,"Data100319/JoinedCell.tsv",sep="\t")

dated_nejm <- "Data092719/NEJM.tsv"
genderized_nejm <- "DataAnalysis/GenderizedByJournal/NewNEJM.tsv"
joined_nejm <- afn.joindata(dated_nejm,genderized_nejm)
fwrite(joined_nejm,"Data100319/JoinedNEJM.tsv",sep="\t")

dated_nature <- "Data092719/Nature.tsv"
genderized_nature <- "DataAnalysis/GenderizedByJournal/NewNature.tsv"
joined_nature <- afn.joindata(dated_nature,genderized_nature)
fwrite(joined_nature,"Data100319/JoinedNature.tsv",sep="\t")

dated_plosone <- "Data092719/PLoS ONE.tsv"
genderized_plosone <- "DataAnalysis/GenderizedByJournal/NewPLoSONE.tsv"
joined_plosone <- afn.joindata(dated_plosone,genderized_plosone)
fwrite(joined_plosone,"Data100319/JoinedPLoSONE.tsv",sep="\t")

dated_pnas <- "Data092719/PNAS.tsv"
genderized_pnas <- "DataAnalysis/GenderizedByJournal/NewPNAS.tsv"
joined_pnas <- afn.joindata(dated_pnas,genderized_pnas)
fwrite(joined_pnas,"Data100319/JoinedPNAS.tsv",sep="\t")

dated_science <- "Data092719/Science.tsv"
genderized_science <- "DataAnalysis/GenderizedByJournal/NewScience_lines15879_15890fixed.tsv"
joined_science <- afn.joindata(dated_science,genderized_science)
fwrite(joined_science,"Data100319/JoinedScience.tsv",sep="\t")

