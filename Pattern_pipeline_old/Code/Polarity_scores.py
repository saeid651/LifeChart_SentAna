## This script computes adjectives that have a polarity
## scores (negative/positive, -1.0 to +1.0) for each narrative note
import numpy
import csv
import pattern
from pattern.en     import Sentence
from pattern.en     import parse
from pattern.en     import sentiment
from pattern.en     import Lexicon


def valence_measure(sen_vec):
    pos_valence = list()
    neg_valence = list()
    counter = 0
    for word in sen_vec:
        if sentiment(word)[0]>=0:
            counter = counter+1
##            print counter
            pos_valence.append(sentiment(word)[0])
        else:
            neg_valence.append(sentiment(word)[0])
    return sum(pos_valence), sum(neg_valence)

scores = list()    
with open('E:/LIBR/Data/T1000_LifechartNarratives_Scores.csv') as csvfile:
    narr_notes = csv.reader(csvfile)
    for row in narr_notes:
        row_sen = (row)[1]
        row_split = row_sen.split()
        scores.append(valence_measure(row_split))

scores.pop(0)
with open("Polarity_scores.csv", "wb") as myfile:
    writer = csv.writer(myfile)
    writer.writerows(scores)
myfile.close()
