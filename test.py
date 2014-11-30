import re
import numpy as np
from pandas import DataFrame
import matplotlib.pyplot as plt

filename = 'train_msgs.txt'
txt = open(filename)
data1 = txt.read()
data1 = DataFrame(data1)
data1.head