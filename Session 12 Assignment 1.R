library(tidyverse)

yeast <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data', stringsAsFactors = FALSE)

l <- readLines('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.names')
l <- l[(grep('^7', l) + 1):(grep('^8', l) - 1)]
l <- l[grep('\\d\\..*:', l)]

names(yeast) <- make.names(c(sub('.*\\d\\.\\s+(.*):.*', '\\1', l), 'class'))
str(yeast)

# a. Perform ANOVA test on the discriminant analysis scores of nuclear localization signals of both nuclear 
# and non-nuclear proteins by class variables (Target).

res.aov <- aov(nuc~class, data = yeast)
summary(res.aov)
print(res.aov)

## As the p value is very low our null hypothesis is rejected.

## b. Which class is significantly different from others?

tab = data.frame(yeast$class, yeast$nuc)
class = as.vector(tab$yeast.class) 
grouped_df(tab, class, drop = TRUE)