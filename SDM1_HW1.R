library(ggplot2)
library(DataExplorer)
library(lattice)
library(data.table)
library(mltools)
library(tidyverse)
library(caret)
library(reshape2)

#reading the file and the column names

df = read.table('~/Documents/UB Grad stuff/SDM 1/cereal.csv', sep = ',',
                header = TRUE, stringsAsFactors = FALSE)
cnames = colnames(df)

#adding the rows which are hidden in the df row 20

df1 = as.data.frame(strsplit(df[20,1],","))
n = (nrow(df1)-1)/(ncol(df)-1) 
seq1 = seq(1:n) #n number of additional rows that are present in df[20,1]
s = colsplit(colsplit(df[20,1],"\n", seq1),",",cnames)
df = rbind(df,s)
df = df[complete.cases(df), ] #removing row 20 which has NA values

#duplicating df
data = df

colnames(df)
summary(df)

#Check distribution of rating.
hist(df$rating)
#Rating is normally distributed

#Plotting correlation to identify the main variables which are correlated to rating
quartz()
plot_correlation(df[,c(4:16)])
quartz()
# calories and sugars have the highest correlation with rating. 
# Other variables with significant correlation are fiber, protein, fat and sodium

#######################################################################################
#Checking for the distributions and behaviour of all variables
#######################################################################################

df$mfr = as.factor(df$mfr)
ggplot(df, aes(x=df$mfr, y=df$rating)) + geom_boxplot()
# Nabisco has higher average rating than other manufacturers.
# Kellog has an outlier with >90 rating.
df %>% count(mfr)
# Kellog and General Mills have high number of products

df$type = as.factor(df$type)
ggplot(df, aes(x=df$type, y=df$rating)) + geom_boxplot()
df %>% count(type)
df %>% count(mfr,type)
# Hot has higher mean rating compared to cold but most of the cereals are Cold so shouldn't really be considered

ggplot(df, aes(x = df$calories, y = df$rating)) + geom_point() + stat_smooth()
hist(df$calories)
summary(df$calories)
# calories and rating are not linearly related. Needs transformation or bucketing
# bucketing calories
df$calories_cat = ifelse(df$calories<=80,"a<=80",ifelse(df$calories<=100,"b80-100","c>100"))
ggplot(df, aes(x=df$calories_cat, y=df$rating)) + geom_boxplot()
#mean rating deosn't change much after increasing calories beyond 100

## Looking at the variable protein
ggplot(df, aes(x = df$protein, y = df$rating)) + geom_point() + stat_smooth()
# protein and rating seem to realted as log but protien seems to be better as a categorical var
df %>% count(protein)
df$protein = ifelse(df$protein>4,4,df$protein)
df$protein_cat = as.factor(df$protein)
ggplot(df, aes(x=df$protein_cat, y=df$rating)) + geom_boxplot()
# higher value of protein seems to have higher value of rating

## Looking at the variable fat
ggplot(df, aes(x = df$fat, y = df$rating)) + geom_point() + stat_smooth()
# fat and rating seem to be correlated but it also seems to be better as a categorical variable
df$fat = ifelse(df$fat>3,3,df$fat)
#ggplot(df, aes(x=df$fat_cat, y=df$rating)) + geom_boxplot()
df$fat_cat = as.factor(df$fat)
ggplot(df, aes(x=df$fat_cat, y=df$rating)) + geom_boxplot()
# If fat is not 0 then then increasing it doesn't impact rating much

## Looking at the variable Sodium 
ggplot(df, aes(x=df$sodium, y=df$rating)) + geom_point() + stat_smooth()
# There are 3 different segments in which value of sodium is varying, so will make it categorical
summary(df$sodium)
summary(df$rating)
df$sodium_cat = ifelse(df$sodium<130,"a<=130",ifelse(df$sodium<=210,"b130-210","c>210"))
ggplot(df, aes(x=df$sodium_cat, y=df$rating)) + geom_boxplot()

#Looking at the variable fiber
ggplot(df, aes(x=df$fiber, y=df$rating)) + geom_point() + stat_smooth()
#capping higher values of the variables
df$fiber = ifelse(df$fiber>6,6,df$fiber)
#plotting
ggplot(df, aes(x=df$fiber, y=df$rating)) + geom_point() + stat_smooth()
summary(df$fiber)
#making 4 different values of fiber to make it categorical
df$fiber_cat = ifelse(df$fiber<=1,"a<=1",ifelse(df$fiber<=2,"b1-2",ifelse(df$fiber<=3,"c2-3","d>3")))
ggplot(df, aes(x=df$fiber_cat, y=df$rating)) + geom_boxplot()

#Looking at the variable carbo
ggplot(df, aes(x=df$carbo, y=df$rating)) + geom_point() + stat_smooth()
# carbo is not linearly related. Need to bucket it appropriately
df$carbo = ifelse(df$carbo<0,median(df$carbo),df$carbo)
hist(df$carbo)
#plotting
ggplot(df, aes(x=df$carbo, y=df$rating)) + geom_point() + stat_smooth()
summary(df$carbo)
#making 4 different values of fiber to make it categorical
df$carbo_cat = ifelse(df$carbo<=12,"a<=12",ifelse(df$carbo<=14,"b12-14",
                                                  ifelse(df$fiber<=17,"c14-17","d>17")))
ggplot(df, aes(x=df$carbo_cat, y=df$rating)) + geom_boxplot()

#Looking at the varible sugars
ggplot(df, aes(x=(df$sugars), y=df$rating)) + geom_point() + stat_smooth()
# sugar seems to be related as y = 1/x. Or take intervals such that it is piecewise linear
summary(df$sugars)
df$sugars = ifelse(df$sugars<0,median(df$sugars),df$sugars)
#plotting
ggplot(df, aes(x=1/(df$sugars), y=df$rating)) + geom_point() + stat_smooth()
# the behavoiur of 1/x is linear only for value of sugar less than 3. so have to bucket it
ggplot(df, aes(x=(df$sugars), y=df$rating)) + geom_point() + stat_smooth()
summary(df$sugars)
hist(df$sugars)
#making 3 different values of fiber to make it categorical
df$sugars_cat = ifelse(df$sugars<=5,"a<=5",ifelse(df$sugars<=10,"b5-10","c>10"))
ggplot(df, aes(x=df$sugars_cat, y=df$rating)) + geom_boxplot()

#Looking at the variable potass
ggplot(df, aes(x=(df$potass), y=df$rating)) + geom_point() + stat_smooth()
# potass is not linearly related. Will make it categorical such that it is piecewise linear
summary(df$potass)
#missing values
df$potass = ifelse(df$potass<0,median(df$potass),df$potass)
df$potass = ifelse(df$potass>200,200,df$potass)
#plotting
ggplot(df, aes(x=(df$potass), y=df$rating)) + geom_point() + stat_smooth()
hist(df$potass)
#making 4 different values of fiber to make it categorical
df$potass_cat = ifelse(df$potass<=45,"a<=45",ifelse(df$potass<=90,"b45-90",
                                                    ifelse(df$potass<=120,"c90-120","d>120")))
ggplot(df, aes(x=df$potass_cat, y=df$rating)) + geom_boxplot()

#looking at the variable vitamins
df$vitamins = as.factor(df$vitamins)
ggplot(df, aes(x=df$vitamins, y=df$rating)) + geom_boxplot()
# cereals with 0 vitamins have higher mean rating than others

#looking at the variable shelf
df$shelf = as.factor(df$shelf)
ggplot(df, aes(x=df$shelf, y=df$rating)) + geom_boxplot()
# shelf doesn't seem to effect rating a lot. We can probably remove this variable

#looking at the variable shelf
summary(df$weight)
hist(df$weight)
# mode of weight is one. Should bucket it into 3 as <1,1,>1
df$weight_cat = ifelse(df$weight<1,"a<1",ifelse(df$weight==1,"b=1","c>1"))
ggplot(df, aes(x=df$weight_cat, y=df$rating)) + geom_boxplot()
#mean rating doesn't seem to matter for weight greater than 1 though spread is more for weight = 1

#looking at the variable cups
ggplot(df, aes(x=(df$cups), y=df$rating)) + geom_point() + stat_smooth()
#seems to be almost categorical with most values scattered in 3 points. Values beyond 1 are too less
summary(df$cups)
df$cups = ifelse(df$cups>1,1,df$cups)
df$cups_cat = ifelse(df$cups<=0.67,"a<=0.67",ifelse(df$cups<=0.75,"b0.67-0.75","c>0.75"))
ggplot(df, aes(x=df$cups_cat, y=df$rating)) + geom_boxplot()

#plotting for important variables
#df$protein = as.factor(df$protein)
ggplot(df, aes(x = df$calories, y = df$rating, fill=protein_cat, size = df$sodium)) + geom_point(shape=21)
#df$fiber = as.factor(df$fiber)
ggplot(df, aes(x = df$sugars, y = df$rating, fill=df$fiber_cat, size=df$sodium)) + geom_point(shape=21)

#start modelling

df_final = df[,c(2:16)]
#plot_correlation(df_final)
names(df_final)
#df_final <- as.data.frame(lapply(df_final[,c(3:10,13,14)],
#                                                function(x) ((x - mean(x))/(max(x)-min(x)))))
#cols = c("mfr","type","vitamins","shelf","rating")
#df_final = cbind(df_final,df[,cols])
#df_final[c("mfr","type","vitamins","shelf")] <- lapply(df_final[,c("mfr","type","vitamins","shelf")], factor)
df_final_1h <- one_hot(as.data.table(df_final))

names(df_final_1h)

#final model build
set.seed(42)
training_samples_cont = df_final_1h$rating %>% createDataPartition(p = 0.8, list = FALSE)
train_data_cont = df_final_1h[training_samples_cont, ]
test_data_cont = df_final_1h[-training_samples_cont, ]

#traning using lm function
model_1 = lm(rating ~(.-(potass+mfr_A+type_H+vitamins_100+shelf_3)), data = train_data_cont)
summary(model_1)
predictions_cont = model_1 %>% predict(test_data_cont)
RMSE_cont = RMSE(predictions_cont, test_data_cont$rating)
R2_cont = R2(predictions_cont, test_data_cont$rating)

#plotting residuals
model_1_df = as.data.frame(residuals(model_1))
names(model_1_df) = c("residuals")
ggplot(model_1_df, aes(model_1_df$residuals)) + geom_density()

#Extracting significant variables
x = as.data.frame(summary(model_1)[["coefficients"]])
x$Estimate
names(x) = c("Estimate", "Std. Error", "t value", "p-value")
x_mod = x[x$`p-value`<=0.05,]
x_mod = x_mod[x_mod$`Std. Error` <= 1,]
#x_mod = x_mod[-1,]
column_names = rownames(x_mod)
#column_names <- gsub("'", '', column_names, fixed = TRUE)
f = paste(column_names,collapse = "+")
f

#building model on significant variables and looking at test error
model_mod = lm(rating~(calories+protein+fat+sodium+fiber+sugars),
               data = train_data_cont)
summary(model_mod)
predictions_cont = model_mod %>% predict(test_data_cont)
RMSE_cont_mod = RMSE(predictions_cont, test_data_cont$rating)
R2_cont_mod = R2(predictions_cont, test_data_cont$rating)

#plotting residuals
model_mod_df = as.data.frame(residuals(model_mod))
names(model_mod_df) = c("residuals")
ggplot(model_mod_df, aes(model_mod_df$residuals)) + geom_density()

R2_cont_mod - R2_cont
RMSE_cont_mod - RMSE_cont

#looking for interactions among significant variables
model_sq = lm(rating ~(calories+protein+fat+sodium+fiber+sugars)^2,
              data = train_data_cont)
summary(model_sq)
predictions_cont = model_sq %>% predict(test_data_cont)
RMSE_cont_sq = RMSE(predictions_cont, test_data_cont$rating)
R2_cont_sq = R2(predictions_cont, test_data_cont$rating)

#plotting residuals


#extarcting significant variables from interaction effects
x = as.data.frame(summary(model_sq)[["coefficients"]])
names(x) = c("Estimate", "Std. Error", "t value", "p-value")
x_mod = x[x$`p-value`<=0.1,]
x_mod = x_mod[x_mod$`Std. Error` <= 3,]
#x_mod = x_mod[-1,]

column_names = rownames(x_mod)
#column_names <- gsub("'", '', column_names, fixed = TRUE)
f = paste(column_names,collapse = "+")
f

#Final model using significant variables conidering interaction effects
model_sq_mod = lm(rating ~(fiber+sugars+calories:fiber+protein:sugars+fat:sodium+fat:fiber+sodium:sugars),
              data = train_data_cont)
summary(model_sq_mod)
predictions_cont = model_sq_mod %>% predict(test_data_cont)
RMSE_cont_sq_mod = RMSE(predictions_cont, test_data_cont$rating)
R2_cont_sq_mod = R2(predictions_cont, test_data_cont$rating)

#plotting residuals
model_sq_mod_df = as.data.frame(residuals(model_sq_mod))
names(model_sq_mod_df) = c("residuals")
ggplot(model_sq_mod_df, aes(model_sq_mod_df$residuals)) + geom_density()

################################################################################################
######################## beyond the scope of the current problem ###############################
################################################################################################


#check for collinearity among variables
library(car)
df_vif = as.data.frame(car::vif(model_mod))
# As we can see sugars is highly collinear. So removing it from the model set

#model 2 training
model2 <- lm(rating ~(calories+protein+fat+sodium+fiber+carbo+weight+mfr_A+mfr_G+vitamins_25),
             data = train_data_cont)
#summary
summary(model2)
#predicting on test data
predictions_m2 <- model2 %>% predict(test_data_cont)
#Residual error and R-squared of test data
RMSE_m2 = RMSE(predictions_m2, test_data_cont$rating)
R2_m2 = R2(predictions_m2, test_data_cont$rating)

R2_m2 - R2_cont_mod
#Slight improvement in test R-squared 


#check for correlation before transformations
colnames(df)
df_final = df[,c(1,2,3,12,13,17:26,16)]
summary(df_final)

#one hot encoding
colnames(df_final)
cols = c("mfr","type","calories_cat","protein_cat","fat_cat","sodium_cat",
         "fiber_cat","carbo_cat","sugars_cat","potass_cat","weight_cat","cups_cat")

df_final[cols] <- lapply(df_final[cols], factor)
df_final_1h <- one_hot(as.data.table(df_final))
ncol(df_final_1h)
df_cor = as.data.frame(cor(df_final_1h[,c(2:42)]))
write.csv(df_cor,file = "df_cor.csv")
getwd()
quartz()
plot_correlation(df_final_1h[,c(2:42)])
#potass and fiber are highly correlated but they are different type of food components so
# it might be good to keep them in the model

#remove excess variables in the model as all categorical variables will lead to collinear variables
#removing variables which are least correlated with rating in a group of categorical variables
names(df_final_1h)
drop_cols = c("mfr_Q","type_C","vitamins_100","shelf_1","calories_cat_b80-100","protein_cat_a<=1"    
              ,"fat_cat_b>0","sodium_cat_c>210","fiber_cat_b1-2","carbo_cat_b12-14" ,"sugars_cat_b5-10",
              "potass_cat_b45-90","weight_cat_b=1","cups_cat_c>0.75")

df_final_1h = as.data.frame(df_final_1h)
df_final_1h_model = df_final_1h[, !(colnames(df_final_1h) %in% c(drop_cols,"name"))]

#final model build
set.seed(42)
training_samples = df_final_1h_model$rating %>% createDataPartition(p = 0.8, list = FALSE)
train_data = df_final_1h_model[training_samples, ]
test_data = df_final_1h_model[-training_samples, ]

#traning using lm function
model_1 = lm(rating ~., data = train_data)
summary(model_1)
x = as.data.frame(summary(model_1)[["coefficients"]])
names(x) = c("Estimate", "Std. Error", "t value", "p-value")
x_mod = x[x$`p-value`<=0.1,]

#traning using lm function
model1 = lm(rating ~(`calories_cat_a<=80`+`fat_cat_a=0`+`sodium_cat_a<=130`+`sugars_cat_a<=5`
                     +`sugars_cat_c>10`+`weight_cat_a<1`+`sodium_cat_b130-210`
                     +`fiber_cat_d>3`+`potass_cat_a<=45`), data = train_data)
#summary
summary(model1)
x = as.data.frame(summary(model1)[["coefficients"]])
names(x) = c("Estimate", "Std. Error", "t value", "p-value")
x_mod = x[x$`p-value`<=0.01,]
x_mod = x_mod[-1,]
column_names = rownames(x_mod)
column_names <- gsub("'", '', column_names, fixed = TRUE)
train_data = train_data[, (colnames(train_data) %in% c(column_names))]


#predicting on test data
predictions_m_1 = model_1 %>% predict(test_data)
predictions_m1 = model1 %>% predict(test_data)
#Residual error and R-squared of test data
RMSE_m1 = RMSE(predictions_m1, test_data$rating)
R2_m1 = R2(predictions_m1, test_data$rating)
R2_m_1 = R2(predictions_m_1, test_data$rating)

#check for collinearity among variables
library(car)
df_vif = as.data.frame(car::vif(model1))
# As we can see vitamins is highly collinear

#model 2 training
model2 <- lm(rating ~. -vitamins_0, data = train_data)
#summary
summary(model2)
#predicting on test data
predictions_m2 <- model2 %>% predict(test_data)
#Residual error and R-squared of test data
RMSE_m2 = RMSE(predictions_m2, test_data$rating)
R2_m2 = R2(predictions_m2, test_data$rating)

#R2_m2 is lower than R2_m1, so collinearity is not having much of an effect on the dataset.
#Also there was barely any improvement in Adj-R squared in the training dataset
