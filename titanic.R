# install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("mice")
install.packages("randomForest")

# Load packages
library('ggplot2') # visualization
library('dplyr') # data manipulation
library("mice") # imputation
library('randomForest') # classification algorithm

# set working directory
setwd("~/PPTs/data analytics")
train <- read.csv('train.csv')
test  <- read.csv('test.csv')
full  <- bind_rows(train, test)
# exploratory analysis
# transfer from integer to factor
full$Survived <- factor(full$Survived, levels=c(1,0))
levels(full$Survived) <- c("Survived", "Died")
full$Pclass <- as.factor(full$Pclass)
levels(full$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

mosaicplot(full[1:891,]$Pclass ~ full[1:891,]$Survived, # specify the x-axis and y-axis
           main="Passenger Survival by Class", # give your ploat a title
           color=c("#8dd3c7", "#fb8072"), # specify color
           shade=FALSE,  xlab="", ylab="", # specify x/y axis labels
           cex.axis=1.4) # specify font size
dev.off() # close the plot

mosaicplot(full[1:891,]$Sex ~ full[1:891,]$Survived, 
           main="Passenger Survival by Gender",
           color=c("#8dd3c7", "#fb8072"), 
           shade=FALSE,  xlab="", ylab="",
           cex.axis=1.4)
dev.off()

# Does family sink together?
# 1. get surname: 875 unique surname out of 1309 people
full$Surname <- sapply(as.character(full$Name),  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
nlevels(factor(full$Surname))
# 2. create a variable of family size:
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1
# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')
# 3. plot histogram based on family size
ggplot(full[1:891,], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  ggtitle("Family size and survival")
# for singleton and family size > 4, they have a survival penalty
# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# missing values
# age is missing
sum(is.na(full$Age))
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex',
                 'Surname','Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
# Set a random seed
set.seed(129)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)
# Plot age distributions, now let's compare before and after
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
dev.off()
# Replace Age variable from the mice model.
full$Age <- mice_output$Age
# Show new number of missing Age values
sum(is.na(full$Age))

# prediction
# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]
# Set a random seed
set.seed(754)
# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare  + FsizeD ,
                         data = train)
# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# Get importance
importance    <- importance(rf_model)
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 