## This script creates a database with the words in narrative lifechart.
## The DB contains of the words, count, valence, and subjectivity score
import numpy
import csv
import pattern
from pattern.en     import Sentence
from pattern.en     import parse
from pattern.en     import sentiment
from pattern.en     import Lexicon
from pattern.search import search
import operator ## to unlist the list of lists
import string
from collections import Counter # to make unique words

def split_nar_words(row_sen):
    terms = row_sen.split()
    return terms

words = list()
valences = list()
with open('E:/LIBR/Data/T500_Narratives_ThreeGroups.csv') as csvfile:
    narr_notes = csv.reader(csvfile)
    for row in narr_notes:
        row_sen = (row)[1]
        ## punctuation removal
        row_sen_clean = row_sen.translate(None, string.punctuation)
        ## parse the narratives
        row_parse = Sentence(parse(row_sen_clean, tags = True))
        ## filter the adjective words
        row_parse = search('JJ', row_parse)
        s = [match[0].string for match in row_parse]
        ## list of all the words
        words.append(s)

## unllist the list of list words
allWords = reduce(operator.concat, words)
## count the unique words
allWordsUnique = Counter(allWords)
## extract words
allPureWords = allWordsUnique.keys()
## compute scores
allValencesScore = list()
for term in allPureWords:
    allValencesScore.append(sentiment(term))

## write the words and count in a file
with open("adjWords_database.csv", "wb") as csvfile:
    writer = csv.writer(csvfile)
    for key, value in allWordsUnique.items():
        writer.writerow([key.encode('utf-8'), value])
csvfile.close()

## write all scores in a file
with open("adjValences_database.csv", "wb") as csvfile2:
    writer = csv.writer(csvfile2)
    writer.writerows(allValencesScore)
csvfile2.close()
