#################################################################
## This code is part of Data Science Dojo's bootcamp
## Copyright (C) 2015
#################################################################

# Homework Day 1: Example

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
names(adult.train) <- col.names
names(adult.test) <- col.names

#################################################################
# Data Visualization                                            #
#################################################################
library(lattice)
### Income ###
pie(table(adult.train$income))
prop.table(table(adult.train$income))
# 24% of our response class is over 50k income.
#    This will be our baseline. Any model that
#    we build must exceed 76% accuracy.
#    This will also guide our thresholds to
#    consider whether or not a feature is good.

### Age ###
boxplot(age~income, data=adult.train)
# The rich tend to be older

densityplot(~age, data=adult.train)

# Density plots by income bracket
densityplot(
  ~age,
  data=adult.train, 
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
#    the two density plots are. It's around ~33.2.
#    Remember that this is an estimate of an
#    estimate since the density curve is
#    smoothened and not an actual pinpoint line.
densityplot(
  ~age,
  data=adult.train, 
  group=income,
  panel=function(x, ...){
    panel.densityplot(x, ...)
    panel.abline(v=33.2)
  }
)

### workclass ###
pie(table(adult.train$workclass))
table(adult.train$workclass)
# Overwhelmingly people work in private. This might
#    create bias.
# "Neverworked" and "without-pay" see almost no
#    samples. We may want to bucket those two
#    together with the missing values of "?".

### flnwgt ###
boxplot(fnlwgt~income, data=adult.train)
densityplot(~fnlwgt, data=adult.train, group=income)
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
pie(table(adult.train$education))
table(adult.train$education)
# Seems like a fine feature. However preschool
#    may have to be binned with 1st grade since
#    there is not enough representation in this
#    dataset.

### education ###
pie(table(adult.train$education.num))
table(adult.train$education.num)
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
#    Also, we should group education.num==1 with 2.

### martial.status ###
pie(table(adult.train$marital.status))
table(adult.train$marital.status)
# There does not seem to be enough samples for
#    "Married-AF-spouse"

### occupation ###
pie(table(adult.train$occupation))
table(adult.train$occupation)
# Not enough "Priv-house-serv". Should bin with "?"

### relationship ###
pie(table(adult.train$relationship))
table(adult.train$relationship)

### race ###
pie(table(adult.train$race))
table(adult.train$race)
# This feature is overwhelmingly biased towards
#    white. This actually may throw into question
#    the quality or integrity of this dataset.
# We can still use this data, we just need to be
#    aware that our model may be biased towards
#    whites.

### sex ###
# setting up a filter to find males, notice the
#    invisible space infront and the capital M
isMale <- adult.train$sex==" Male"
# creating a side by side comparison of male vs
#    female distribution.
par(mfrow=c(1, 2))
pie(table(adult.train[isMale,"income"]), main="Income of Male")
pie(table(adult.train[!isMale,"income"]), main="Income of Female")
# Men seem to generally make more than women

### capital.gain ###
densityplot(
  ~capital.gain, 
  data=adult.train, 
  group=income,
  auto.key=TRUE
)
# Overwhelmingly people do not have any capital 
#    gains. However the rich seem to have way more
#    capital gains than not. Let's segment this
#    into capital gain and not.

# Segment by those who have and do not have capital
#    gain.
hasCapGain <- adult.train$capital.gain>0
par(mfrow=c(1, 2))
pie(
    table(adult.train[hasCapGain, "income"]),
    main="Has capital gain."
)
pie(
    table(adult.train[!hasCapGain, "income"]),
    main="Does not have capital gain."
)
# Capital gain seems like an extremely strong
#    indicator of wealth.

### capital.loss ###
# Perform the same analysis for capital loss as 
#    the analysis done on capital gain.
hasCapLoss <- adult.train$capital.loss>0
par(mfrow=c(1, 2))
pie(
    table(adult.train[hasCapLoss, "income"]), 
    main="Has capital loss"
)
pie(
    table(adult.train[!hasCapLoss, "income"]),
    main="Does not have capital loss"
)
# Capital loss also seems like a strong
#    indicator of wealth.

### hours.per.week ###
densityplot(
  ~hours.per.week,
  data=adult.train,
  auto.key=TRUE,
  group=income
)
# After about 45+ hours, it seems that people
#    end up being more than >50k income.

### native.country ###
pie(table(adult.train$native.country))
# This feature does not have any representation
#    among its categories other than United States.
#    It may be best to bin everything except US
#    together into an "other" category.

isUS <- adult.train$native.country==" United-States"
par(mfrow=c(1, 2))
pie(table(adult.train[isUS,"income"]), main="US Origin")
pie(table(adult.train[!isUS,"income"]), main="Foreign Origin")
# US origin tends to be slightly more likely to make
#    make more money.