# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

train <- read.csv('Data/train.csv')
test  <- read.csv('Data/test.csv')

#The function bind_row is from the dplyr package. We cannot use the rbind function here because the test and train files have different numbers of columns.

full <- bind_rows(train,test)

#Check data. We are using the str() function here. Its an alternate funtion to summary().
str(full) 

# FEATURE ENGINEERING.
# We will not extract the title from the "Name" Column of full and create a new column called "Title".

full$Title <- gsub("(.*, |\\..*)","",full$Name)

# There are some rare titles also. So we'll replace them with rare title if the frequency for them is below a threshold.

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms']   <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)
table(full$Survived,full$Sex)

#Grab Surname from Passenger Name.
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

#Get a summary on different numbers of Surnames.
cat(paste('we have <b>', nlevels(factor(full$Surname)),'</b> unique Surnames.'))


#DO FAMILIES SINK OR SWIM TOGEATHER ?

full$Fsize <- full$SibSp + full$Parch + 1

#Create a family Variable.

full$Family <- paste(paste(full$Surname,"\'s", sep = ''),full$Fsize, sep = '_')


#What does the Fsize variable look like against the survial rate ?

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()