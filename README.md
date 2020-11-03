# Altmetric Analysis

[TOC]

## Summary

We have data on Altmetric attention scores for over 200,000 articles from 7 idiotypic journals.

- bioRxiv
- Cell
- NEJM
- Nature
- PLOS ONE
- PNAS
- Science

We are curious whether there are any interesting patterns in Altmetric scores relating to the genders of article's authors.

This analysis genderizes authors for those 200,000 articles, then models Altmetric scores as a function of author gender, year, month, journal and number of authors.



## Folder structure

### Scripts

- [AltmetricExtractor_092719BB.py](./Scripts/AltmetricExtractor_092719BB.py)
  - Reads the raw data from Altmetric.com.
  - Extracts articles from the 7 journals of interest (listed above)
  - Extracts fields of interest: article ID, authors, journal, type of article, DOI, publication date, Altmetric score at the following intervals post-publication: 1-day, 2-day, 3-day, 4-day, 5-day, 6-day, 1-week, 1-month, 3-months, 6-months, 1-year, all-time
  - Writes a tsv file for each journal in [AltmetricData/](./AltmetricData/)
- [GenderizeAuthors_020420JF.R](./Scripts/GenderizeAuthors_020420JF.R)
  - Reads the extracted data from [AltmetricData/](./AltmetricData/)
  - Cleans the data (removes articles with missing data)
  - Subsets the data to years 2011-2018 (inclusive)
    - 2011 is when Altmetric started keeping score, and many social media platforms on which the score is based were widespread
    - Analysis was done at the end of 2019; subsetting years < 2019 allowed us to use 1-year scores from articles published in 2018
  - Cleans the author variable for each article, identifies which is the first name and genderizes author names using [genderizeR](https://kalimu.github.io/project/genderizer/)
  - Creates new variables with gender statistics: number of female/male/unknown gender authors, proportion of female/male/unknown gender authors, gender & probability of first author, gender & probability of last author
  - Merges gender variables with original data
  - Writes a tsv file for each journal in [GenderizedData/](./GenderizedData/)
- [AltmetricScoreModeling_040720JF.R](./Scripts/AltmetricScoreModeling_040720JF.R)
  - Reads the genderized data from [GenderizedData/](./GenderizedData/)
  - Does some basic data exploration
  - Separates data into: 
    - Binary: articles with a 1-year Altmetric score of 0 and articles with a 1-year Altmetric score > 0. This is because so many articles have scores of 0 after 1 year (66% based on data exploration)
    - Linear: subset of all articles with a 1-year Altmetric score > 0
    - Also, bioRxiv was only started in 2015, so its time series is shorter than for other journals, therefore it was coded separately (binary_bioRixv, linear_bioRxiv, binary_allOtherJournals, linear_allOtherJournals)
  - Runs models:
    - Logistic model: whether or not an article obtains a score > 0 ~ interaction between publication year, journal and first/last author gender, publication month, proportion of female authors and total number of authors
    - Linear model: log(score) ~  interaction between publication year, journal and first/last author gender, publication month, proportion of female authors and total number of authors
    - Saved in [Models.](./Models)
  - Does standard model checking via plots
  - Does post-hoc testing via multiple comparisons
  - Creates plots for paper in [Figures](./Figures/)



###AltmetricData

This is folder contains the raw data as extracted in the script [AltmetricExtractor_092719BB.py](./Scripts/AltmetricExtractor_092719BB.py). There is one tsv file for each journal. We cannot share this data; please contact Altmetric.com.



### GenderizedData

This folder contains the Altmetric data as processed in the script [GenderizeAuthors_020420JF.R](./Scripts/GenderizeAuthors_020420JF.R). It contains the original data, plus additional variables describing the authors' genders. There is one tsv file for each journal. We cannot share this data; please contact Altmetric.com.



###Models

This folder contains the R objects containing the models as run in [AltmetricScoreModeling_040720JF.R](./Scripts/AltmetricScoreModeling_040720JF.R). There is one RDS file for the logistic model of bioRxiv, one for the logistic model of all journals except bioRxiv, one for the linear model of bioRxiv and one for the linear model of all journal except bioRxiv.



###Figures

This folder contains the figures generated in [AltmetricScoreModeling_040720JF.R](./Scripts/AltmetricScoreModeling_040720JF.R). 



## Sensitivity analysis

We did a [sensitivity analysis](https://docs.google.com/spreadsheets/d/1qlP01Z7bltWei0CynnoGvjzkl3JQ-FKRe_sl3dKf7As/edit?usp=sharing) to see the impact of genderizing accuracy on our results.

