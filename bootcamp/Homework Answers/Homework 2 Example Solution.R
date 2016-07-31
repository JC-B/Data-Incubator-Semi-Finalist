#################################################################
## This code is part of Data Science Dojo's bootcamp
## Copyright (C) 2016
#################################################################

# Homework Day 2: Example, Building a Predictive Model,
#    on the Adult Census Income dataset

#################################################################
# Reading in the Data                                           #
#################################################################
# Read in the training set by URL
# There is no headers in this file, so header=FALSE
train.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
adult.train <- read.csv(train.url, header=FALSE)

# Read in the test set by URL.
# However the first row is invalid for a CSV. We must
#   skip this line.
# read.table(): We must use read.table instead because 
#    it allows you to skip lines to be read in.
# header=FALSE: There is also no header in this file
# sep=",": tells it to parse the table by commas
test.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
adult.test <- read.table(test.url, skip=1, header=FALSE, sep=",")

# Combine both train and test into one data frame
adult.full <- rbind(adult.train, adult.test)

#################################################################
# Reading in the Column Names                                   #
#################################################################
# Read in the columns names from the adult.names file
#   on the UCI repository
# However the file is not really a valid format. You may want to
#   create a vector of column names via just copy and paste
#   to derive.... 
#   c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country")
#   However you can also write custom code to derive the column
#   names through pseudo web scraping techniques, the code
#   below shows this.

# The adult.name file URL
meta.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names"

# Read in the adult.name file as a table.
# skip=96: ignore the first 96 lines
# fill=TRUE: ignore invalid rows
# sep=":": separate by colon
# header=FALSE: ignore header
meta<-read.table(meta.url, fill=TRUE, header=FALSE, sep=":", skip=96)

# Grab the first 14 rows of the first column, which is
#    where the string values of our headers are.
col.names<-meta[1:14, 1]

# To work with it easier, we can convert the 1 column
#    data frame into a vector.
col.names<-as.vector(col.names)

# Currently there are only 14 column names. One seems to be
#    missing. We find that "income", the response class,
#    should be added. We will perform an append() to add it
col.names<-append(col.names, "income")

# R does not like '-' in column names because it means
#    subtraction. So we will replace all "-" with ".".
#    To do this we will use a package called "stringr"
#    which allows for string manipulation
library(stringr)
col.names<-str_replace_all(col.names, "-", ".")

# Adds the column names to both datasets
names(adult.full) <- col.names


#################################################################
# Pre-processing 1: Conforming and Cleaning Values              #
#################################################################
str(adult.full)

### Finding Problems ###
table(adult.full$income)
# <=50K    >50K  <=50K.   >50K. 
# 24720    7841   12435    3846 
# For some reason it looks like one of the datasets added a 
#    period and one did not. Let's conform these values by 
#    removing the period. However it gets a little bit tricker 
#    because $income is currently a factor and not a string or 
#    text.

levels(adult.full$income)
# [1] " <=50K"  " >50K"   " <=50K." " >50K." 
# The issue is further complicated because it seems that the data
#    loaded in with a bunch of white spaces before

### Trimming White Space ###
# We will use a function called trimws() which will remove 
#    trailing and following white space.
adult.full$income <- trimws(adult.full$income)

levels(adult.full$income)
# [1] "<=50K"  "<=50K." ">50K"   ">50K." 
# The begining white spaces are now gone. However there's still
#    an issue with the extra period in 2 of the factor levels.

### Removing the period ###
# We will call upon a string manipulation library called stringr.
#    We will use a function called str_replace() within the
#    the library itself. We will feed it the data, then character
#    we are trying to replace, with the character we wish to
#    substitute it with. \\ is an escape character because
#    normally the "." within this context uses a sub-language
#    called regex (regular expression), and "." means wild 
#    character. We have to escape the dot with a double slash to
#    mean a literal dot. We are replacing the dots with an empty
#    string, nothing.
library(stringr)
adult.full$income <- str_replace(adult.full$income,"\\.","")
adult.full$income <- as.factor(adult.full$income)
# Doing a str_replace converted the string to a character data
#    type. We must convert it back to a factor for
#    classification.

# Let's also remove whitespace on all the other string/text based
#    columns as well.
adult.full$workclass <- trimws(adult.full$workclass)
adult.full$education <- trimws(adult.full$education)
adult.full$marital.status <- trimws(adult.full$marital.status)
adult.full$occupation <- trimws(adult.full$occupation)
adult.full$relationship <- trimws(adult.full$relationship)
adult.full$race <- trimws(adult.full$race)
adult.full$sex <- trimws(adult.full$sex)
adult.full$native.country <- trimws(adult.full$native.country)

adult.full$workclass <- as.factor(adult.full$workclass)
adult.full$education <- as.factor(adult.full$education)
adult.full$marital.status <- as.factor(adult.full$marital.status)
adult.full$occupation <- as.factor(adult.full$occupation)
adult.full$relationship <- as.factor(adult.full$relationship)
adult.full$race <- as.factor(adult.full$race)
adult.full$sex <- as.factor(adult.full$sex)
adult.full$native.country <- as.factor(adult.full$native.country)

#################################################################
# Data Visualization and Feature Engineering                    #
#################################################################
library(lattice)
### Income ###
pie(table(adult.full$income))
prop.table(table(adult.full$income))
# 24% of our response class is over 50k income.
#    This will be our baseline. Any model that
#    we build must exceed 76% accuracy.
#    This will also guide our thresholds to
#    consider whether or not a feature is good.

### Age ###
boxplot(age~income, data=adult.full)
# The rich tend to be older

densityplot(~age, data=adult.full)

# Density plots by income bracket
densityplot(
  ~age,
  data=adult.full, 
  group=income,
  auto.key=TRUE  # legend
)
# We notice that poor are skewed to the left
#    and the rich look like a normal distribution.
# Below around ~33 years of age, people tend to
#    not make over 50k. While between ~33 years of
#    age until retirement, making over 50k is much
#    more likely.

# We will eye ball where the convergence line of
#    the two density plots are. It's around ~33.
#    Remember that this is an estimate of an
#    estimate since the density curve is
#    smoothened and not an actual pinpoint line.
#    There seems to be another change at age 65.
densityplot(
  ~age,
  data=adult.full, 
  group=income,
  panel=function(x, ...){
    panel.densityplot(x, ...)
    panel.abline(v=33)
    panel.abline(v=65)
  }
)

### Age Brackets ###
# We noticed from the density plots of age grouped by income
#    showed there was a bracket between 0~33 years of age and 
#    33~65 years of age.

# Index filters
young.index <- adult.full$age < 33
adult.index <- adult.full$age >=33 & adult.full$age <65
retired.index <- adult.full$age >= 65

# Creating new feature by using the filters
adult.full[young.index,"age.bracket"] <- "young"
adult.full[adult.index,"age.bracket"] <- "adult"
adult.full[retired.index,"age.bracket"] <- "retired"

# Casting new feature to an ordinal categorical feature
# where the ordering of "young < adult < retired" is retained
age.ordering <- c("young","adult", "retired")
adult.full$age.bracket <- ordered(adult.full$age.bracket, age.ordering)

# Confirm that the levels are in the correct ordering.
adult.full$age.bracket[1]
# Levels: young < adult < retired

table(adult.full$age.bracket)

### workclass ###
pie(table(adult.full$workclass))
table(adult.full$workclass)
# Overwhelmingly people work in private. This might
#    create bias.
# "Neverworked" and "without-pay" see almost no
#    samples. We may want to bucket those two
#    together.

# Turn "?" into "unknown"
levels(adult.full$workclass)[1] <- "Unknown"

# Merge without pay and never worked together
levels(adult.full$workclass)[4] <- "NeverWorked.NoPay"
levels(adult.full$workclass)[9] <- "NeverWorked.NoPay"

### flnwgt ###
boxplot(fnlwgt~income, data=adult.full)
densityplot(~fnlwgt, data=adult.full, group=income)
# There does not appear to be anything
#    to see here in-terms of patterns.
# We also don't really understand what fnlwgt even
#    means. Let's refer back to the adult.names.
# Upon research, we find out that its a statistcal
#    weight measure to figure out how many people
#    each particular row is representing since
#    they cannot get a total census.
# This number is important for statistical analysis
#    of the population of the US, however for
#    predictive modeling, it is not too useful
#    since building a model will assume
#    that fnlwgt must be taken in as a future
#    value for all future people. We also do not
#    have a readily available way to derive this
#    number. It would end up being null for each
#    new person that we wanted to predict.
# Conclusion: we'll ignore or drop this feature

### education ###
pie(table(adult.full$education))
table(adult.full$education)
# Seems like a fine feature. However preschool
#    may have to be binned with 1st grade since
#    there is not enough representation in this
#    dataset.

### education ###
pie(table(adult.full$education.num))
table(adult.full$education.num)
# This pie chart looks suspiciously similar to education.
# In particular education.num of 1 and preschool
#    both have 51 samples. Which tells us know
#    that its probably a mapping and not represented
#    in terms of education as years.
# Upon further investigation we find that it is
#    a one-to-one mapping. These two columns
#    represent the same exact data. We should
#    consider dropping one of them. We should
#    probably drop education because it is
#    represented as strings vs education.num which
#    is represented as an integer. Integers are much
#    more versatile for machine learning since you
#    can apply operations such as greater than or less
#    than, median, max, min, etc.
# Conclusion: drop education, leave education.num

### martial.status ###
pie(table(adult.full$marital.status))
table(adult.full$marital.status)
# There does not seem to be enough samples for
#    "Married-AF-spouse"

### occupation ###
pie(table(adult.full$occupation))
table(adult.full$occupation)
# Not enough "Priv-house-serv". Should bin with "?"

### relationship ###
pie(table(adult.full$relationship))
table(adult.full$relationship)

### race ###
pie(table(adult.full$race))
table(adult.full$race)
# This feature is overwhelmingly biased towards
#    white. This actually may throw into question
#    the quality or integrity of this dataset.
# We can still use this data, we just need to be
#    aware that our model may be biased towards
#    whites.

# Create a feature where if white or not
adult.full$white.race <- adult.full$race=="White"
adult.full$white.race <- as.factor(adult.full$white.race)
pie(table(adult.full$white.race))

### sex ###
# setting up a filter to find males
isMale <- adult.full$sex=="Male"

# creating a side by side comparison of male vs
#    female distribution.
par(mfrow=c(1, 2))
pie(table(adult.full[isMale,"income"]), main="Income of Male")
pie(table(adult.full[!isMale,"income"]), main="Income of Female")
# Men seem to generally make more than women

### capital.gain ###
densityplot(
  ~capital.gain, 
  data=adult.full, 
  group=income,
  auto.key=TRUE
)
# Overwhelmingly people do not have any capital 
#    gains. However the rich seem to have way more
#    capital gains than not. Let's segment this
#    into capital gain and not.

# Segment by those who have and do not have capital
#    gain.
hasCapGain <- adult.full$capital.gain>0
par(mfrow=c(1, 2))
pie(
    table(adult.full[hasCapGain, "income"]),
    main="Has capital gain."
)
pie(
    table(adult.full[!hasCapGain, "income"]),
    main="Does not have capital gain."
)
# Capital gain seems like an extremely strong
#    indicator of wealth.

### capital.loss ###
# Perform the same analysis for capital loss as 
#    the analysis done on capital gain.
hasCapLoss <- adult.full$capital.loss>0
par(mfrow=c(1, 2))
pie(
    table(adult.full[hasCapLoss, "income"]), 
    main="Has capital loss"
)
pie(
    table(adult.full[!hasCapLoss, "income"]),
    main="Does not have capital loss"
)
# Capital loss also seems like a strong
#    indicator of wealth.
investment <- adult.full$capital.loss + adult.full$capital.gain

# Turn investment into a boolean value, those who have
#   investment and those who do not.
adult.full$has.investments <- investment > 0
adult.full$has.investments <- as.factor(adult.full$has.investments)

### hours.per.week ###
densityplot(
  ~hours.per.week,
  data=adult.full,
  auto.key=TRUE,
  group=income
)
# After about 36+ hours, it seems that people
#    end up being more than >50k income.
densityplot(
  ~hours.per.week,
  data=adult.full,
  auto.key=TRUE,
  group=income,
  panel=function(x, ...){
    panel.densityplot(x, ...)
    panel.abline(v=36)
  }
)

# Build a full time vs not full time column
adult.full$is.fulltime <- adult.full$hours.per.week > 36
adult.full$is.fulltime <- as.factor(adult.full$is.fulltime)

### native.country ###
pie(table(adult.full$native.country))
# This feature does not have any representation
#    among its categories other than United States.
#    It may be best to bin everything except US
#    together into an "other" category.

isUS <- adult.full$native.country=="United-States"
par(mfrow=c(1, 2))
pie(table(adult.full[isUS,"income"]), main="US Origin")
pie(table(adult.full[!isUS,"income"]), main="Foreign Origin")
# US origin tends to be slightly more likely to make
#    make more money.

# Group us vs not us
adult.full$is.us <- isUS
adult.full$is.us <- as.factor(adult.full$is.us)

#################################################################
# Predictive Modeling                                           #
#################################################################

### 70/30 split
max <- nrow(adult.full)
train.index <- sample(1:max,0.7*max)
adult.train <- adult.full[train.index,]
adult.test <- adult.full[-train.index,]

library(randomForest)
rf.model <- randomForest(
  income~.-fnlwgt-education-native.country-white.race-is.us-race,
  data=adult.train,
  importance=TRUE,
  ntree=500,
  mtry=3,
  nodesize=0.01*nrow(adult.train)
)
print(rf.model)
varImpPlot(rf.model)
