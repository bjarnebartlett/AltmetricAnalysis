#!/usr/bin/env python
# coding: utf-8

# In[1]:


#Altmetric Analysis
#Designed by Bjarne Bartlett 2019
#bjarne@hawaii.edu

import json #for parsing the files
from collections import defaultdict #dictionary
import csv #storing/manipulating the data
import pandas #parsing csv data
import os, sys #for iterating through files
import glob, shutil #copy files
print("Hello, stuff imported.")
rootdir = "./json/"
fieldnames = ['altmetric_id', 'authors', 'journal', 'altmetric_score']
new_dict = defaultdict(list)
with open('PLoS ONE.tsv','w+') as p:
    print(fieldnames, file=p)
with open('bioRxiv.tsv','w+') as b:
    print(fieldnames, file=b)
with open('Science.tsv','w+') as s:
    print(fieldnames, file=s)
with open('Nature.tsv','w+') as n:
    print(fieldnames, file=n)
with open('NEJM.tsv','w+') as nejm:
    print(fieldnames, file=nejm)
with open('Cell.tsv','w+') as c:
    print(fieldnames, file=c)
with open('PNAS.tsv','w+') as pnas:
    print(fieldnames, file=pnas)
lineNo = 1
for root, dirs, files in os.walk(rootdir, topdown=False):
    for file in files:
        if file.endswith(".txt.json"):
            file_path = rootdir + file
            with open(file_path,'r') as f, open('PLoS ONE.tsv','a') as p, open('bioRxiv.tsv','a') as b, open('Science.tsv','a') as s, open('Nature.tsv','a') as n, open('NEJM.tsv','a') as nejm, open('Cell.tsv','a') as c, open('PNAS.tsv','a') as pnas:
                for line in f:
                    lineNo = lineNo + 1
                    data = json.loads(line)
                    altmetric_id = data.get('altmetric_id')
                    citation = data.get('citation')
                    authors = citation.get('authors')
                    attribution = citation.get('attribution')
                    journal = citation.get('journal')
                    pubdate = citation.get('pubdate')
                    type = citation.get('type')
                    altmetric_score = data.get('altmetric_score')
                    score = altmetric_score.get('score')
                    score_history = altmetric_score.get('score_history')
                    week = altmetric_score.get('1w')
                    month = altmetric_score.get('1m')
                    threemonths = altmetric_score.get('3m')
                    year = altmetric_score.get('1y')
                    at = altmetric_score.get('at')

                    if journal == 'PLoS ONE':

                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(week) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=p) #print the altmetric ids for every entry
                    if journal == 'bioRxiv':

                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(week) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=b) #print the altmetric ids for every entry
                    if journal == 'Science':

                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(week) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=s) #print the altmetric ids for every entry
                    if journal == 'Nature':

                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(week) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=n) #print the altmetric ids for every entry
                    if journal == 'New England Journal of Medicine':

                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(week) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=nejm) #print the altmetric ids for every entry
                    if journal == 'Cell':

                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(week) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=c) #print the altmetric ids for every entry
                    if journal == 'Proceedings of the National Academy of Sciences of the United States of America':

                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(week) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=pnas) #print the altmetric ids for every entry
            f.close()
            p.close()
            b.close()
            s.close()
            n.close()
            nejm.close()
            c.close()
            pnas.close()
print("Done.")
