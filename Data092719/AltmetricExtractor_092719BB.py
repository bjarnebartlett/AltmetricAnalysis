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
fieldnames = ['line', 'id', 'authors', 'journal','type','doi', 'score', 'pubdate', 'week','month','threemonth','year','at']
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
                    altmetric_id = data.get('altmetric_id') or None
                    citation = data.get('citation') or {}
                    authors = citation.get('authors') or None
                    attribution = citation.get('attribution')
                    journal = citation.get('journal') or None
                    pubdate = citation.get('pubdate') or None
                    type = citation.get('type') or None
                    doi = citation.get('doi') or None
                    title = citation.get('title') or None
                    altmetric_score = data.get('altmetric_score') or {}
                    score = altmetric_score.get('score') or None
                    score_history = altmetric_score.get('score_history') or {}
                    sixdays = score_history.get('6d') or None
                    week = score_history.get('1w') or None
                    sixmonths = score_history.get('6m') or None
                    twodays = score_history.get('2d') or None
                    oneday = score_history.get('1d') or None
                    fivedays = score_history.get('5d') or None
                    fourdays = score_history.get('4d') or None
                    threedays = score_history.get('3d') or None
                    month = score_history.get('1m') or None
                    threemonths = score_history.get('3m') or None
                    year = score_history.get('1y') or None
                    at = score_history.get('at') or None

                    if journal == 'PLoS ONE':
                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(doi) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(sixdays) + '\t' + str(week) + '\t' + str(sixmonths) + '\t' + str(twodays) + '\t' + str(oneday) + '\t' + str(fivedays) + '\t' + str(fourdays) + '\t' + str(threedays) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=p)
                            #print the altmetric ids for every entry, file=p
                    if journal == 'bioRxiv':
                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(doi) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(sixdays) + '\t' + str(week) + '\t' + str(sixmonths) + '\t' + str(twodays) + '\t' + str(oneday) + '\t' + str(fivedays) + '\t' + str(fourdays) + '\t' + str(threedays) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=b)
                            #print the altmetric ids for every entry, file=b
                    if journal == 'Science':
                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(doi) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(sixdays) + '\t' + str(week) + '\t' + str(sixmonths) + '\t' + str(twodays) + '\t' + str(oneday) + '\t' + str(fivedays) + '\t' + str(fourdays) + '\t' + str(threedays) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=s)
                            #print the altmetric ids for every entry, file=s
                    if journal == 'Nature':
                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(doi) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(sixdays) + '\t' + str(week) + '\t' + str(sixmonths) + '\t' + str(twodays) + '\t' + str(oneday) + '\t' + str(fivedays) + '\t' + str(fourdays) + '\t' + str(threedays) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=n)
                            #print the altmetric ids for every entry, file=n
                    if journal == 'New England Journal of Medicine':
                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(doi) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(sixdays) + '\t' + str(week) + '\t' + str(sixmonths) + '\t' + str(twodays) + '\t' + str(oneday) + '\t' + str(fivedays) + '\t' + str(fourdays) + '\t' + str(threedays) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=nejm)
                            #print the altmetric ids for every entry, file=nejm
                    if journal == 'Cell':
                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(doi) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(sixdays) + '\t' + str(week) + '\t' + str(sixmonths) + '\t' + str(twodays) + '\t' + str(oneday) + '\t' + str(fivedays) + '\t' + str(fourdays) + '\t' + str(threedays) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=c)
                            #print the altmetric ids for every entry, file=c
                    if journal == 'Proceedings of the National Academy of Sciences of the United States of America':
                            print(str(lineNo) + '\t' + str(altmetric_id) + '\t' + str(authors) + '\t' + str(journal) + '\t' + str(type) + '\t' + str(doi) + '\t' + str(score) + '\t' + str(pubdate) + '\t' + str(sixdays) + '\t' + str(week) + '\t' + str(sixmonths) + '\t' + str(twodays) + '\t' + str(oneday) + '\t' + str(fivedays) + '\t' + str(fourdays) + '\t' + str(threedays) + '\t' + str(month) + '\t' + str(threemonths) + '\t' + str(year) + '\t' + str(at), file=pnas)
                            #print the altmetric ids for every entry, file=pnas
            f.close()
            p.close()
            b.close()
            s.close()
            n.close()
            nejm.close()
            c.close()
            pnas.close()
print("Done.")
