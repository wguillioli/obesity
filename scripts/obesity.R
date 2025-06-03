
rm(list = ls())

require(tidyverse)
require(corrplot)
require(naniar) #visualize missing values


# load_data}

project_dir <- "C:/GitHub/obesity/"

train <- read_csv(paste0(project_dir, "data/train.csv"))
dim(train) # 20758    18
glimpse(train)

test <- read_csv(paste0(project_dir, "data/test.csv"))
dim(test) # 13840    17
glimpse(test)

# create_dataset}

train$source <- "train"
test$source <- "test"

test$NObeyesdad <- NA

d <- bind_rows(train, test)
dim(d) #34598    19

# missing_values} 
vis_miss(d[,1:17])

sum(is.na(d[, 1:17]))

#eating habits
#Frequent consumption of high caloric food (FAVC), 
#Frequency of consumption of vegetables (FCVC), 
#Number of main meals (NCP), 
#Consumption of food between meals (CAEC), 
#Consumption of water daily (CH20), 
#Consumption of alcohol (CALC). 

#physical condition are: 
#Calories consumption monitoring (SCC), 
#Physical activity frequency (FAF), 
#Time using technology devices (TUE), 
#Transportation used (MTRANS)

#variables obtained :
#Gender, 
#Age, 
#Height and 
#Weight.

#target_var}

d %>%
  filter(source == "train") %>%
  group_by(NObeyesdad) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n/sum(n),2))
#coerce to factor

# eda_predictors}

is_numeric <- sapply(d, is.numeric)
glimpse(d[,is_numeric])

#numeric_vars <- c("Age", "Height", "Weight", "FCVC", 
#                  "NCP", "CH2O", "FAF", "TUE")

#glimpse(d[,numeric_vars])

is_character <- sapply(d, is.character)
glimpse(d[, is_character])

#character_vars <- c("Gender", "family_history_with_overweight", "FAVC", "CAEC", 
 #                   "SMOKE", "SCC", "CALC", "MTRANS")

#glimpse(d[,character_vars])

#gender





#EDA num vars

summary()
quantiles 
hist

lapply(d[,is_numeric], summary)



#VOY


#univ prep chr y pot pred

#univ prep num y pot pred

#multicolinearity

#corrplot(round(cor(d[,numeric_vars]),2))


#REFERENCES
#https://www.kaggle.com/competitions/playground-series-s4e2/data
#https://www.kaggle.com/datasets/aravindpcoder/obesity-or-cvd-risk-classifyregressorcluster/data
