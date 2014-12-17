data <- read.csv('output.csv')

for (i in 1:ncol(word.feature)){
  as.numeric(levels(word.feature[,i]))[word.feature[,i]]
}