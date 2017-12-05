

### Main ###


# Importing Data
#---------------

# setting up new working directory using the
setwd("D:\\Work\\R\\R Projects\\Titanic\\dataset")

# putting the data frame into an object called stats
test.d <- read.csv("test.csv", stringsAsFactors = F, na.strings = c('') )
train.d <- read.csv("train.csv", stringsAsFactors = F, na.strings = c(''))

# --------------------------
getwd()

# Load packages
# -----------------------

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# -----------------

# Summary of data

totalsummary <- function(data.df){
  st <- str(data.df)
  hd <- head(data.df,n=6)
  sm <- summary(data.df)
  output <- list(Structure=st , Head=hd , Summary=sm)
  return(output)
}

totalsummary(train.d)
totalsummary(test.d)

# bind training & test data
full.d  <- bind_rows(train.d, test.d)
?bind_rows()

totalsummary(full.d)

# Change survived from integer to boolean

full.d$Survived <- as.logical(full.d$Survived)
levels(full.d$Survived) <- c("Not survived", "Survived")


### Feature Engineering ###
#=========================#

### TITLES

# Getting title names.
head(full.d$Name)
# Example Title,
# "Braund, Mr. Owen 0#######"
# Title starts form ', ' and end with "."
# #######, @@@@@@@. ##### is the title pattern
# subtitute everything before the coma and after the full stop.
gsub("(.*, )|(\\..*)", '', full.d$Name)
# Here .* is for all the values before , OR \\. indicating actual full stop and .* all the values after that.
?gsub()
# Extrcting Titles
full.d$Title <- gsub("(.*, )|(\\..*)", '', full.d$Name)
View(full.d)


# Finding rare Titles #
#---------------------#

# Title count
# Using the table() function for counting frequency
t(table(full.d$Title))
# Title count by sex.
titlefreqbysex <- table(full.d$Sex, full.d$Title)
# Sex wise proportion.
prop.table(titlefreqbysex,1)
# prop.table function gives the propotion of a table.
# 1 here denotes row wise proportion.

# Total proportion of each title
# Column wise sum
colSums(prop.table(titlefreqbysex,1))
margin.table(titlefreqbysex,2)
# margin function returns sum of the table
# 2 here is for column wise sum

# Rare Titles are those which has less than 5% or .05 occurance
# Filtering rare titles
margin.table(prop.table(titlefreqbysex,1),2)<.05
raretitle <- names(which(margin.table(prop.table(titlefreqbysex,1),2)<.05))
raretitle

#-----------------------------------------------#

### Human Check

# Checking for typo errors
# Correcting for typo errors
full.d$Title[full.d$Title == 'Mlle'] <- 'Miss'
full.d$Title[full.d$Title == 'Ms'] <- 'Miss'
full.d$Title[full.d$Title == 'Mme'] <- 'Mrs' 
###

# Treating rare title #
#---------------------#

# Refreshing raretitle
titlefreqbysex <- table(full.d$Sex, full.d$Title)
raretitle <- names(which(margin.table(prop.table(titlefreqbysex,1),2)<.05))
raretitle

# Assiging rare title as 'Rare Title' in Title column
full.d$Title[full.d$Title %in% raretitle] <- 'Rare Title' 

#-----------------------------------------#

# Refreshing Title by sex table
titlefreqbysex <- table(full.d$Sex, full.d$Title)
titlefreqbysex

### SURNAMES
# Getting title names.
head(full.d$Name)

# Using the strsplit function with sapply
?strsplit
sapply(full.d$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
# OR
gsub('(,.*)','',full.d$Name)

# Are both Same?
Aa <- sapply(full.d$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
Bb <- gsub('(,.*)','',full.d$Name)
all(Aa==Bb)
# Yes they are
rm(Aa,Bb)


full.d$Surname <- sapply(full.d$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
# Or
#full.d$Surname <- sapply(full.d$Name, function(x) gsub('(,.*)','',x))

# Different Surnames
# Number of different unique surname
length(unique(full.d$Surname))
# OR
nlevels(factor(full.d$Surname))
# OR
length(names(table(full.d$Surname)))



### Families

# IDENTIFYING FAMILIES
# Families can be identifies using the surname and the size of the family.
# Both variable can compine to form a family variable.
# The family variable groups in the families.


# Family size variable including the passenger themselves
full.d$Fsize <- full.d$SibSp + full.d$Parch + 1
head(full.d)

# Creating a family variable 
full.d$Family <- paste(full.d$Surname, full.d$Fsize, sep='_')
head(full.d)

# Number of families
nlevels(factor(full.d$Family))

# Checking repeats
if(FALSE){
Aa <- table(full.d$Family,full.d$Fsize)
head(Aa)
# ----
Aa <- table(full.d$Family,full.d$Fsize)
head(Aa)
rownames(Aa[,]==Aa)
sapply(Aa[drop=FALSE],function(x)which(x==as.integer(colnames(x))))
col.names(Aa[5,5])
colnames(Aa[5,5,drop=FALSE])
colnames(Aa)

nlevels(factor(full.d$Family==x))
Bb <- table(full.d$Family)

Bb[[full.d$Family]]==full.d$Fsize
  
aggrigate()
Aa

apply(Aa,1,max)
Aa[5,5]


?apply()
rm(Aa,Bb)
# ----
Bb <- table(full.d$Family)
Cc <- levels(factor(full.d$Family))
Bb[[Cc[15]]]
sapply(Cc,2,function(x)Aa[[x,Bb[[x]]]]!=0)
ftable(Aa)
rm(Bb,Cc)
# ----
fuckingchecker <- full.d
Bb <- as.data.frame(table(full.d$Family))
colnames(Bb)<- c('Family','Freq')
head(Bb)
fuckingchecker<-merge(full.d,Bb,by.x='Family',by.y='Family')
fuckingchecker$check <- fuckingchecker$Freq == fuckingchecker$Fsize
fuckingchecker$Family[fuckingchecker$check==FALSE]
View(fuckingchecker)


merge(full.d,as.data.frame(table(full.d$Family)),by.x='Family',by.y='Var1')$Family[merge(full.d,as.data.frame(table(full.d$Family)),by.x='Family',by.y='Var1')$Freq != merge(full.d,as.data.frame(table(full.d$Family)),by.x='Family',by.y='Var1')$Fsize]
# wrf it actuall worked


rm(fuckingchecker,Bb,Aa)

# ----
}


# Relationship between family size & survival
# Using ggplot2 to visualize the relationship between family size & survival
ggplot(full.d[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# There is a relation between family size and survival

# Discretizing family size
full.d$FsizeD[full.d$Fsize == 1] <- 'singleton'
full.d$FsizeD[full.d$Fsize < 5 & full.d$Fsize > 1] <- 'small'
full.d$FsizeD[full.d$Fsize > 4] <- 'large'
View(full.d)

# Discreet Family size and survival.
# Using mosaicplot() function.
# Showing family size by survival using a mosaic plot
mosaicplot(table(full.d$FsizeD, full.d$Survived), main='Family Size by Survival', shade=TRUE)



?mice()
?set.seed()
?legend()
#


### Cabin

# The cabin variable has alot of missing value
head(full.d$Cabin)

# The rows with missing cabin information
head(full.d[is.na(full.d$Cabin),])

# Creating a Deck column variable
# Passenger deck A - F:
full.d$Deck<-factor(sapply(full.d$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
?strsplit()


### MISSING DATA ###
#==================#


# Exploring the missing data
full.d[!complete.cases(full.d),]
# Number of incomplete Rows
nrow(full.d[!complete.cases(full.d),])


# Rows that are missing data of Embarked column #
#-----------------------------------------------#
head(full.d[is.na(full.d$Embarked),])
# Number of  rows missing data of Embarked
nrow(full.d[is.na(full.d$Embarked),])
# Two rows that are missing data fo Embarkment

# Enbarekment can be infered based on passenger class and fare.

# A data set with out the missing Enbarkment.
Aa <- full.d[full.d$PassengerId != 62 & full.d$PassengerId != 830,]
# or
Bb <- full.d %>% filter(PassengerId != 62 & PassengerId != 830)
# Checking both are same or not
table(Aa == Bb, useNA = 'ifany')
rm(Aa,Bb)

embark_fare <- full.d %>% filter(PassengerId != 62 & PassengerId != 830)
head(embark_fare)
?filter()

# visualizing embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# Their fare was $80 for 1st class.
# The median fare for a first class passenger departing from Charbourg (‘C’) coincides nicely with the $80 paid by our embarkment-deficient passengers.
# They most likely embarked from 'C', so it is safe to replace the NA values with ‘C’.
full.d$Embarked[c(62, 830)] <- 'C'
# OR
full.d$Embarked[is.na(full.d$Embarked)] <- 'C'

# Checking
nrow(full.d[is.na(full.d$Embarked),])==0

# ------------------------------------ #

##
# Backup
full.d.b1 <- full.d
##

# Rows that are missing data of Fare column #
#-------------------------------------------#
head(full.d[is.na(full.d$Fare),])
# Number of rows missing data of Fare
nrow(full.d[is.na(full.d$Fare),])
# only one row

# Third class passenger who departed from Southampton (‘S’)
# Others data of same class and embarkment
full.d[full.d$Pclass == '3' & full.d$Embarked == 'S', ]
# Number of data points
nrow(full.d[full.d$Pclass == '3' & full.d$Embarked == 'S', ])

# Median for fare of same class and embarkment
median(full.d$Fare[full.d$Pclass == '3' & full.d$Embarked == 'S'], na.rm = TRUE)

# Visualizing Fares among all others sharing their class and embarkment
ggplot(full.d[full.d$Pclass == '3' & full.d$Embarked == 'S', ], aes(x=Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)), colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) + theme_few()

# Replace missing fare value with median fare for class/embarkment
full.d$Fare[is.na(full.d$Fare)] <- median(full.d$Fare[full.d$Pclass == '3' & full.d$Embarked == 'S'], na.rm = TRUE)

# Checking
nrow(full.d[is.na(full.d$Fare),])==0

#-----------------------------------------------#


# Rows missing age values #
#-------------------------#

head(full.d[is.na(full.d$Age),])
# Number of rows missing data of Fare
nrow(full.d[is.na(full.d$Age),])
# OR
sum(is.na(full.d$Age))
# Missing in lot of rows


# Imputing missing age values by predictive imputation
# Predictive imputation
# Create a predicting model based on other variables.

# Creating a model predicting ages based on other variables.

# Using 'mice' package. (Multivariate Imputation by Chained Equations)
# 'rpart' (recursive partitioning for regression) can also be used.
# Steps for mice imputation
# 1. Factorize the factor variables.
# 2. Perform mice imputation.

# Variable 
colnames(full.d)
# Making variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','Surname','Family','FsizeD')
full.d[factor_vars] <- lapply(full.d[factor_vars], function(x) as.factor(x))

# Setting a random Seed
set.seed(129)

# Performing mice imputation
?mice()
# Excluding certain less-than-useful variables:
mice_mod <- mice(full.d[, !names(full.d) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
# The methord here is Random Forest 

# Saving the complete output
mice_output <- complete(mice_mod)
?complete()

# comparing the results, with the original distribution of passenger ages to ensure that nothing has gone completely awry.
par(mfrow=c(1,2))
hist(full.d$Age, freq=FALSE, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=FALSE, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Pretty similer
# It seems safe to replace age vector in the original data with the output from the mice model.

##
# Backup
full.d.b2 <- full.d
##

# Replacing Age variable from the mice model.
full.d$Age <- mice_output$Age

# Finished imputing values
# Checking
sum(is.na(full.d$Age))==0

#---------------------------------------#

# Relationship between age & survival
ggplot(full.d[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + theme_few()

# Including sex
# As sex is a significant pridictor
ggplot(full.d[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + facet_grid(.~Sex) + theme_few()

### Child and Mother ###

# Mother and child are given priority in an event of accident.
# Mother and child variables are age dependent

# Child is passenger under 18
# Mother is passanger who is
# 1. Female
# 2. Over 18
# 3. has more than 0 childreen
# 4. does not have the title 'Miss'

# Create the column child
# Column indicate whether child or adult
full.d$Child[full.d$Age < 18] <- 'Child'
full.d$Child[full.d$Age >= 18] <- 'Adult'

# Showing counts
table(full.d$Child, full.d$Survived)
# The probability of survival is
prop.table(table(full.d$Child, full.d$Survived),1)
# Probability of survival for child is more than ptobability of not surviving

# Creating Mother variable.
# Adding Mother variable
full.d$Mother <- 'Not Mother'
full.d$Mother[full.d$Sex == 'female' & full.d$Parch > 0 & full.d$Age > 18 & full.d$Title != 'Miss'] <- 'Mother'

# Showing counts
table(full.d$Mother, full.d$Survived)
# The probability of survival is
prop.table(table(full.d$Mother, full.d$Survived),1)
# For Mother probability of survival is significantly more than probability of not surviving

# Factorizing the two new factor variables
full.d$Child <- factor(full.d$Child)
full.d$Mother <- factor(full.d$Mother)

# Checking for missings data
full.d[!complete.cases(full.d),]
# Number of incomplete Rows
nrow(full.d[!complete.cases(full.d),])
# Missing data patterns
?md.pattern()
md.pattern(full.d)
# All relevent missing data treated

#=====================================================================#


### Prediction ###
#================#

### Splitting the data back into the original test and training sets.
trainA<- full.d[1:891,]
testA <- full.d[892:1309,]
# OR
trainB <- full.d[full.d$PassengerId %in% train.d$PassengerId,]
testB <- full.d[full.d$PassengerId %in% test.d$PassengerId,]
#Cheching are both same
all(trainA[complete.cases(trainA),]==trainA[complete.cases(trainA),])
rm(trainA,trainB,testA,testB)
# All good

train.d <- full.d[full.d$PassengerId %in% train.d$PassengerId,]
test.d <- full.d[full.d$PassengerId %in% test.d$PassengerId,]


### Building the model ###
#------------------------#

# Setting a random seed
set.seed(754)

# Building the model
# Using Random Forest
?randomForest()
# Note: not all possible variables are used
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + FsizeD + Child + Mother,
                         data = train.d)

# Showing model error
par(mfrow =c(1,1))
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# The black line shows the overall error rate which falls below 20%
# The red and green lines show the error rate for ‘died’ and ‘survived’ respectively.


#--------------------------------------------------------------------------#

### Variable importance ###
#-------------------------#

# Reviewing relative variable importance by plotting the mean decrease in Gini calculated across all trees.

# Getting importance
?importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
varImportance

# Creating a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))


?mutate()
?paste0
?dense_rank
?desc()

# Visualizing the relative importance of variables
# Using ggplot2
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') + coord_flip() + theme_few()

# Title variable has the highest importance out of all the predictor variables


#-------------------------------------------------------------#


### Prediction ###
#----------------#

# Predicting using the test set
?predict
prediction <- predict(rf_model, test.d)
head(prediction)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test.d$PassengerId, Survived = prediction)
solution

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

#---------------------------------------------------------#
#=========================================================#


# Age, Sex, and Pclass best predictors for survival
ggplot(full.d, aes(x=Age, y=Pclass, color=Survived)) + 
  geom_jitter(position = position_jitter(height = .1)) +
  scale_color_manual(values=c("red", "blue")) + facet_grid(Sex ~ .) +
  ggtitle("Age, Sex, and Class as Survival Factors") + ylab("Pclass")


# If two people are assigned to the same cabin and single familysize then bump up the family size to 2.

n_occur <- data.frame(table(Var1=full.d$Cabin))
# Removing missing cabins and jusing the cabin letter code.
n_occur <- subset(n_occur, nchar(as.character(Var1)) > 1)
sharedCabins <- n_occur$Var1[n_occur$Freq > 1]
sharedCabins

# Makeing a new column called FsizeAdj
# The column is for adjusting family size.
full.d$FsizeAdj <- full.d$Fsize
print(table(full.d$Fsize))


# Indicator
sharedInd <- full.d$FsizeAdj == 1 & full.d$Cabin %in% sharedCabins
full.d$FsizeAdj[sharedInd] <- 2
rowCount <- sum(sharedInd)
print(c("adjusted rows", rowCount))
# Table of family sizes
print(table(full.d$FsizeAdj))

# Clearing
rm(sharedInd,rowCount,sharedCabins)

# Spliting training set into subset training and test set
inTrainingSet <- createDataPartition(full.d$Survived, p = 0.5, list=FALSE)
train.d <- full.d[inTrainingSet,]
test.d <- full.d[-inTrainingSet,]

# Loading library
library(caret)

# Setting up seed
set.seed(820)


# Checking if adding more variables changes the prediction
# Function that returns model accuracy
modelaccuracy <- function(test, rpred) {
  result_1 <- test$Survived == rpred
  sum(result_1) / length(rpred)
}
# Function that returns check accuracy
checkaccuracy <- function(accuracy) {
  if (accuracy > bestaccuracy) {
    bestaccuracy <- accuracy
    assign("bestaccuracy", accuracy, envir = .GlobalEnv)
    label <- 'better'
  } else if (accuracy < bestaccuracy) {
    label <- 'worse'
  } else {
    label <- 'no change'
  }
  label
}

# Building a model
# Using CART
library(rpart)
# starting with Age and Sex as indicators
?formula()
fol <- formula(Survived ~ Age + Sex)
?rpart
rmodel <- rpart(fol, data=train.d, method="class")
rpred <- predict(rmodel, newdata=test.d, type="class")
accuracy <- modelaccuracy(test.d, rpred)
# initiating base accuracy
bestaccuracy <- accuracy 
print(c("accuracy 1", accuracy))     








# Data partition
?createDataPartition




### END ###
rm(varImportance,train.d,test.d,solution,rankImportance,mice_output,importance,full.d.b2,
   full.d.b1,full.d,embark_fare,factor_vars,mice_mod,prediction,raretitle,rf_model,titlefreqbysex,totalsummary)
