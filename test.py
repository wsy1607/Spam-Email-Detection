import re
import numpy as np
from pandas import DataFrame
import matplotlib.pyplot as plt

filename = 'train_msgs.txt'

data_file = open(filename)
email_txt = data_file.read()
word_list = email_txt.split()
unique_word = set(word_list)

print len(unique_word)
