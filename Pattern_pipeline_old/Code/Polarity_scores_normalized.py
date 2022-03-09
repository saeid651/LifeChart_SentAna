## This script computes adjectives that have a polarity
## score (negative/positive, -1.0 to +1.0) for each narrative note
import numpy as np
import csv
import string
import pattern
from pattern.en     import Sentence
from pattern.en     import parse
from pattern.en     import sentiment
from pattern.en     import Lexicon
from pattern.search import search
from ast import literal_eval


def valence_measure(sen_vec):
    pos_valence = list()
    neg_valence = list()
    pos_words = list()
    neg_words = list()
    pos_counter = 0
    neg_counter = 0
    counter = 0
    for word in sen_vec:
        counter = counter+1
        if sentiment(word)[0]>0:
            pos_words.append(str(word))
            pos_words.append(sentiment(word)[0])
            pos_counter = pos_counter+1
            pos_valence.append(sentiment(word)[0])            
        elif sentiment(word)[0]<0:
            neg_words.append(str(word))
            neg_words.append(sentiment(word)[0])
            neg_counter = neg_counter+1
            neg_valence.append(sentiment(word)[0])
    if len(pos_valence)!=0 and len(neg_valence)==0:
        return sum(pos_valence)/len(pos_valence), sum(pos_valence)/sentence_length(row_sen),sum(pos_valence), \
               0, sum(neg_valence)/sentence_length(row_sen),sum(neg_valence), \
               pos_counter, pos_words, neg_counter, neg_words
    elif len(pos_valence)==0 and len(neg_valence)!=0:
        return 0, sum(pos_valence)/sentence_length(row_sen),sum(pos_valence), \
               sum(neg_valence)/len(neg_valence), sum(neg_valence)/sentence_length(row_sen),sum(neg_valence), \
               pos_counter, pos_words, neg_counter, neg_words
    elif len(pos_valence)==0 and len(neg_valence)==0:
        return 0, sum(pos_valence)/sentence_length(row_sen),sum(pos_valence), \
               0, sum(neg_valence)/sentence_length(row_sen),sum(neg_valence), \
               pos_counter, pos_words, neg_counter, neg_words
    else:
        return sum(pos_valence)/len(pos_valence), sum(pos_valence)/sentence_length(row_sen),sum(pos_valence), \
               sum(neg_valence)/len(neg_valence), sum(neg_valence)/sentence_length(row_sen),sum(neg_valence), \
               pos_counter, pos_words, neg_counter, neg_words

def sentence_length(sen):
    sen_len = len(sen.split())
    return sen_len
scores = list()    
with open('E:/LIBR/Data/T500_Lifechart_Narratives_Scores.csv') as csvfile:
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
        scores.append(valence_measure(s))

scores.pop(0)
with open("Polarity_scores.csv", "wb") as myfile:
    writer = csv.writer(myfile)
    writer.writerows(scores)
myfile.close()
