#load packages

import string
import re
import numpy as np
from pandas import DataFrame
import matplotlib.pyplot as plt

#%load txt files
filename = 'train_msgs.txt'
data_file = open(filename)
email_txt = data_file.read()
#Replace uppercase by lowercase
for word in string.ascii_lowercase:
	re.sub(word.upper(),word,email_txt)

#get words
word_list = email_txt.split()
#get unique wods
unique_word = set(word_list)
#get stop words
stop_word = ["a","able","about","across","after","all","almost","also","am","among","an","and","any","are","as","at","be","because","been","but","by","can","cannot","could","dear","did","do","does","either","else","ever","every","for","from","get","got","had","has","have","he","her","hers","him","his","how","however","i","if","in","into","is","it","its","just","least","let","like","likely","may","me","might","most","must","my","neither","no","nor","not","of","off","often","on","only","or","other","our","own","rather","said","say","says","she","should","since","so","some","than","that","the","their","them","then","there","these","they","this","tis","to","too","twas","us","wants","was","we","were","what","when","where","which","while","who","whom","why","will","with","would","yet","you","your"]
stop_word = set(stop_word)
#exclude stop words
unique_word = unique_word.difference(stop_word)

#get rows
word_row = email_txt.splitlines()
#create empty word matrix
word_features = np.zeros((len(word_row)-1,len(unique_word)))
print word_features.shape()
