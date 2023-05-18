#################################################
###            German Credit Card             ###
### Author: Jose Alberto Valencia Carrillo    ###
### Email:  albertline.vc@gmail.com           ###
### Date:   2023-03-27                        ###
### Design for class analysis                 ###
###                                           ###
#################################################


#Importing Required libraries
library(readxl)

#Importing data sources
my_germ <- read_excel("Datasets/german credit card.xls")

View(my_germ)

#Objects and information gain process
my_cust <- nrow(my_germ)
my_var  <- ncol(my_germ)

my_dim <- c(my_cust,my_var)

var_1 <- my_germ$purpose
var_2 <- my_germ$good_bad

my_matrix <- matrix(c(var_1,var_2), nrow = my_cust, ncol = 2 )

my_data_frame <- as.data.frame(my_matrix)

my_lst <- list(my_data_frame, my_matrix, var_1, var_2, my_cust)



### SESSION 3 ###
#Information loss Process

var_1[990:1000] #It is a vector, has only one dimension. You don't need coma.
var_2[990:1000] #It is a vector, has only one dimension. You don't need coma.

my_germ[1:5,1:3] #It is a matrix, it has 2 dimensions. you need come for Rows and Columns 

table(var_1) #We crate a frequency table of the values inside Var_1 

table(var_2) #We crate a frequency table of the values inside Var_2


##Trying to fix data types fro purpose and good_bad

is.character(var_1) #true
is.numeric(var_1)   #false

as.numeric(var_1) #It introduced NA because R didn't know what to do with the X's converting to numbers 
as.numeric(var_2) #all values are now NA (R doesn't know what to do with "Good" or "Bad")


#Sub-setting with which() Function

which(my_germ$good_bad == "good" & my_germ$history > 2) #We find all the observations that match the criteria 

length( which(my_germ$good_bad == "good" & my_germ$history > 2)) #We Count the result to see the total using length()



which (my_germ$purpose == 'X') #identifying clients with X in purpose  

my_germ[ which(my_germ$purpose == 'X'), ] #Sub-setting with the previous business logic



which(my_germ$good_bad == 'good') #identifying clients with "good" in purpose 

my_good <- my_germ[ which(my_germ$good_bad == 'good'), ] # Saving it

summary(my_good)

which(my_germ$good_bad == 'bad')  #identifying clients with "bad" in purpose (similar to where in SQL)

my_bad <- my_germ[ which(my_germ$good_bad == 'bad'), ]   # saving it

summary(my_bad)


#Time to replace Values and fix the problem
gsub("X", "10", my_germ$purpose) # what we want to replace, what will be replacing and from where. It is still a character type.

my_germ$purpose_fixed <- as.numeric(gsub("X", "10", my_germ$purpose)) #we convert things from annoying character to Numeric type.
                                                                      #stored the values in a new column 


### SESSION 4 ###

#Solving errors in good bad
my_germ$binary <- gsub("good", "1", my_germ$good_bad) # we keep it as character type because the original data was character.
my_germ$binary <- gsub("bad", "0", my_germ$binary)

my_germ$binary <- as.numeric(my_germ$binary) # we change the type from character to numeric so we can utilize it better.
                                             # we have a table with the original values and the new variable fixed.

#Creating a horizontal loop to check descriptive stats (variable)
my_germ <- as.data.frame(my_germ)#refreshing my_germ as a data frame

for(i in 1:ncol(my_germ)){
  print(min(my_germ[ ,i], na.rm= TRUE)) # we want the variables so i goes after the coma
  print(mean(my_germ[,i], na.rm= TRUE)) 
  print(max(my_germ[ ,i], na.rm= TRUE)) 
}#closing loop

#Implementing Score card for costumers (rows)
my_germ$risk_score <- c()

for (i in 1:nrow(my_germ)){
  my_germ$risk_score[i]<- 0.5*my_germ$duration[i] +
                          0.1*my_germ$age[i] +
                          0.1*my_germ$amount[i] +
                          0.3*my_germ$installp[i]
}#closing loop

#Implementing Score card for costumers with team measurements (rows)
my_germ$risk_score_13 <- c()

for (i in 1:nrow(my_germ)){
  my_germ$risk_score_13[i]<- 0.2*my_germ$duration[i] +
                             0.2*my_germ$age[i] +
                             0.3*my_germ$job[i] +
                             0.3*my_germ$amount[i]
}#closing loop

#Introducing Business Logic to help DB be conservative 

my_germ$label <- c()

for (i in 1:nrow(my_germ)){
  if(my_germ$binary[i]== 1 & my_germ$risk_score[i]< 250){
    my_germ$label[i]<- "Outstanding"
  }else{my_germ$label[i]<- "Not Outstanding"}
}

table(my_germ$label)



### SESSION 5 ###

#creating UDF
risk_score <- function(var1,w1,var2,w2,var3,w3,var4,w4){
  my_score <- var1*w1 +var2*w2 +var3*w3 +var4*w4
  return(my_score)
}#closing the function

#we want to see wide ranges
my_germ$team13_score <- risk_score(var1 = my_germ$duration, w1= 0.2,
                                   var2 = my_germ$age,      w2= 0.2,
                                   var3 = my_germ$amount,  w3= 0.2,
                                   var4 = my_germ$job,      w4= 0.2)

my_germ$team12_score <- risk_score(var1 = my_germ$checking, w1= 0.85,
                                   var2 = my_germ$duration, w2= -0.04,
                                   var3 = my_germ$savings,  w3= 0.22,
                                   var4 = my_germ$employed, w4= 0.2)

#Designing a 4loop to look at desc stats of each variable

for(i in 1:ncol(my_germ)){
  my_min<- try(min(my_germ[,i], na.rm = TRUE))
  my_max<- try(max(my_germ[,i], na.rm = TRUE))
  my_mean<- try(mean(my_germ[,i], na.rm = TRUE))
  my_std<- try(sd(my_germ[,i], na.rm = TRUE))
  print(c(my_min,my_mean,my_std,my_max))
}#closing loop


#######Practice Sampling#######

#game roll dice(practice)
dice <- sample(c(1,2,3,4,5,6), size= 15000, replace = TRUE)
mean(dice)
hist(dice)

#game flip a coin(practice)
coin <- sample(c(0,1), size = 46000, replace = TRUE)
mean(coin)
hist(coin)

###############################



### SESSION 6 ###
#Exponential

train<- rexp(10000, rate = .5)
mean(train)
hist(train)

#looping through horizontal to see shape of vars
for(i in 1:ncol(my_germ)){
  try(hist(my_germ[,i]))
}#closing loop

#removing units using normalization

 #we create a function to normalize variables
normalized<- function(x){#x is your variable 
  min_max<- (x-min(x))/(max(x)-min(x))
  return(min_max)
}#closing

#adding normalized to my_germ
my_germ$checking_norm <- normalized(my_germ$checking)
my_germ$duration_norm <- normalized(my_germ$duration)
my_germ$employed_norm <- normalized(my_germ$employed)
my_germ$installp_norm <- normalized(my_germ$installp)
my_germ$history_norm  <- normalized(my_germ$history)
my_germ$savings_norm  <- normalized(my_germ$savings)
my_germ$amount_norm   <- normalized(my_germ$amount)
my_germ$coapp_norm    <- normalized(my_germ$coapp)
my_germ$age_norm      <- normalized(my_germ$age)


# checking correlations
cor.test(my_germ$age, my_germ$amount)

##Moving into predictive modeling

#train      
training_idx<- sample(1:1000, size= 0.8*nrow(my_germ)) #1000 is the num of rows| size is percentage for train
my_germ_train<- my_germ[training_idx,]


#test
my_germ_test<- my_germ[-training_idx,] #simply remove those in the train index



### SESSION 7 ###
#Building the logistic model

my_logit<- glm(binary ~ age+amount+checking+coapp+savings+amount, data = my_germ_train, family = "binomial")

#Viewing the information 
summary(my_logit)


#logistic with normalized data
my_logit_norm<- glm(binary ~ age_norm+duration_norm+checking_norm+coapp_norm+savings_norm+amount_norm, data = my_germ_train,family = "binomial")

#Viewing the information (data has no unit so we can compare variables)
summary(my_logit_norm)


#Testing performance of our model

#install.packages("caret")
library(caret)

my_prediction<- predict(my_logit, my_germ_test, type= "response") 

confusionMatrix(data = as.factor(as.numeric(my_prediction>0.5)),
                reference = as.factor(as.numeric(my_germ_test$binary)))#the actual data

#Challenge model with a GINI model
library(rpart)
library(rpart.plot)

my_tree<- rpart(binary ~ age+amount+checking+coapp+savings+amount, data = my_germ_train, method = "class", cp = 0.023) #******** HW, play with cp

#ploting the tree
rpart.plot(my_tree, type=1, extra=1)

#predicting the tree to compare
my_germ_tree_predict<- predict(my_tree, my_germ_test, type= "prob")

confusionMatrix(data= as.factor(as.numeric(my_germ_tree_predict[,2]>0.5)),
                reference=as.factor(as.numeric(my_germ_test$binary)))





#######Visualizing Age In Different Ways#######

##basic and simple
hist(my_germ$age)

##Histogram With ggplot2
library(ggplot2)
age_freq<- ggplot(data=my_germ, aes(x=age))+
           geom_histogram(binwidth = 4)

##Histogram with plotly

#install.packages('plotly')
library(plotly)
ggplotly(age_freq) #converting ggplot object to dynamic histogram 






