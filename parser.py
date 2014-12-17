'''
@author: Miles Pathmanathan Dismond
'''

import pandas, string, csv, collections

def remove_punctuation_list(l):
    out = list()
    for x in l:
        out.append(remove_punctuation(x))
    return out

def remove_punctuation(s):
    if s !=  "&lt;#&gt":
        out = ''
        n = len(s)
        for i in range(0,n):
            if s[i] not in string.punctuation:
                out+=s[i]
        return ' '.join(out.split())
    else:
        return s

def contained(s1,s2):
    return s1.lower() in s2.lower()


def contained_list(s, l):
    for x in l:
        if contained(s,x):
            return True
    return False

def equals_ignore_case(s1,s2):
    return s1.lower() == s2.lower()


def equals_ignore_case_list(s, l):
    for x in l:
        if equals_ignore_case(s, x):
            return True
    return False

def equals_list(s, l):
    for x in l:
        if s==x:
            return True
    return False


def get_progress(completed, total):
    print("Percentage Completed: " + str(100*float(completed)/total) + "%")
    

def split_punctuation(s):
    add_space=True
    out = ''
    for i in range(len(s)):
        try:
            if not s[i] in string.punctuation or s[i] == "'":
                add_space=True
                out += s[i]
            else:
                if add_space:
                    out += ' '
                out +=s[i]
                if not s[i+1] in string.punctuation and s[i+1]!="'":
                    out += ' '
                else:
                    add_space=False
        except:
            return out
    return out

def process(messages):
    print("Processing...")
    new_messages = list()
    for message in messages:
        new_message = list()
        for word in message.split():
            temp = split_punctuation(word)
            new_message.append(temp)
        new_messages.append(' '.join(new_message))
    print("Processing complete")
    return new_messages
        
data = pandas.read_csv("messages.txt", header=None, sep=r"\n")
labeled_messages = list()
for i in range(0,len(data)):
    labeled_messages.append(data[0][i])
messages = list()
labels = list()

for i in range(0, len(labeled_messages)):
    item = labeled_messages[i].split('\t')
    labels.append(item[0])
    messages.append(item[1])

classifiers = list()
for i in range(0, len(labels)):
    if equals_ignore_case(labels[i], 'ham'):
        classifiers.append(1)
    else:
        classifiers.append(0)

word_features = collections.OrderedDict()
common = pandas.read_csv("commonwords.txt",header=None,sep=",")
common_words = list()
for i in range(0, len(common.columns)):
    common_words.append(str(common[i]))

messages =  process(messages)
is_word_feature = list()

for i in range(0, len(messages)):
    words = messages[i].split()
    is_word_feature.append(list())
    for word in words:
        added = False
        if not (equals_ignore_case_list(str(word), word_features.keys())) and not (equals_ignore_case_list(str(word), common_words)):
            word_features[str(word).lower()]=0
            is_word_feature[i].append(1)
            added = True
        if equals_ignore_case_list(str(word), word_features.keys()) and not added:
            word_features[str(word).lower()]+=1
            is_word_feature[i].append(1)
            added = True
        if not added:
            is_word_feature[i].append(0)
        
    get_progress(i, len(messages));
word_feature_matrix = [[0 for x in range(len(word_features.keys())+1)] for x in range(len(messages))]
specific_word_features = [[0 for x in range(len(word_features.keys()))] for x in range(len(messages))]


position_dict = dict()
for i in range(len(word_features.keys())):
    position_dict[word_features.keys()[i]] = i
    
for i in range(len(messages)):
    a = messages[i].split()
    for j in range(len(is_word_feature[i])):
        if is_word_feature[i][j] == 1:
            specific_word_features[i][position_dict[a[j].lower()]]+=1
            
    get_progress(i, len(messages))

for i in range(len(messages)):
    for j in range(len(word_features.keys())):
        s = sum(is_word_feature[i])
        if s != 0:
            word_feature_matrix[i][j]=(specific_word_features[i][j]/float(s))
        else:
            word_feature_matrix[i][j] = 0
    get_progress(i,len(messages))
        
for i in range(len(labels)):
    word_feature_matrix[i][len(word_features.keys())] = classifiers[i]
 

print("Writing output to file...")
with open('output.csv', 'w') as csvoutput:
    writer = csv.writer(csvoutput, lineterminator ='\n');
    row = word_features.keys()
    row.append('ham?')
    writer.writerow(row)
    for l in word_feature_matrix:
        writer.writerow(l)
print('Write complete');

