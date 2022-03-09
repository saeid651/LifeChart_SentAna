## This script compute a subjectivity score(objective/subjective, +0.0 to +1.0)
## for each narrative note
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
from collections import Counter # to unique the words


def narr_score(sen_vec):
    score = list()
    counter = 0
    adj_words = list()
    for word in sen_vec:
        counter = counter+1
        adj_words.append(str(word.encode('utf-8')))
        if word in ("definitely", "especially", \
            "real", "really", "seriously"):
            score.append((sentiment(word)[1])*2)
            adj_words.append(sentiment(word)[1]*2)
        else:
            score.append(sentiment(word)[1])
            adj_words.append(sentiment(word)[1])
    if len(score) == 0:
        return 0, counter, sentence_length(row_sen)
    else:
        return sum(score)/len(score),sum(score)/sentence_length(row_sen), sum(score), counter, \
               adj_words, sentence_length(row_sen)

def sentence_length(sen):
    sen_len = len(sen.split())
    return sen_len

scores = list()
words = list()
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
        scores.append(narr_score(s))

        
scores.pop(0)
with open("Subjectivity_score.csv", "wb") as myfile:
    writer = csv.writer(myfile)
    writer.writerows(scores)
myfile.close()



    

    
    
    

        
        
