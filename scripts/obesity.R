
# project setup

rm(list = ls())

require(tidyverse)
require(corrplot)
require(naniar) #visualize missing values
require(moments)

# function to calculate percentiles from a data frame column
percentiles <- function(x){
  
  return(round(quantile(x, 
                        probs = c(0, 0.01, 0.05, 0.1, .25, .5, .75, .9, .95, .99, 1)), 2))
  
}

# function to print normality test stas
normality_test <- function(x){
  
  print(shapiro.test(sample(x, size = 5000)))
  
  print(skewness(x))
  
  print(kurtosis(x))

}

# load_data

project_dir <- "C:/GitHub/obesity/"

train <- read_csv(paste0(project_dir, "data/train.csv"))
dim(train) # 20758    18
glimpse(train)

test <- read_csv(paste0(project_dir, "data/test.csv"))
dim(test) # 13840    17
glimpse(test)


# create working dataset

train$source <- "train"
test$source <- "test"

test$NObeyesdad <- NA

d <- bind_rows(train, test)
dim(d) #34598    19

# missing_values} 
vis_miss(d[,1:17])

sum(is.na(d[, 1:17])) #no NAs


# 1x1 eda
glimpse(d)

#$ source                         <chr> "train", "train", "train", "train", "train", "train", "train", "train…
# nothing to do

#$ NObeyesdad                     <chr> "Overweight_Level_II", "Normal_Weight", "Insufficient_Weight", "Obesi…
d %>%
  filter(source == "train") %>%
  group_by(NObeyesdad) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n/sum(n),2))

#NObesity values are:
#•Underweight Less than 18.5
#•Normal 18.5 to 24.9
#•Overweight 25.0 to 29.9
#•Obesity I 30.0 to 34.9
#•Obesity II 35.0 to 39.9
#•Obesity III Higher than 40

d$NObeyesdad_fct <- factor(d$NObeyesdad, 
                           ordered = TRUE,
                           levels = c("Insufficient_Weight",
                                      "Normal_Weight",
                                      "Overweight_Level_I",
                                      "Overweight_Level_II",
                                      "Obesity_Type_I",
                                      "Obesity_Type_II",
                                      "Obesity_Type_III"))

#$ id                             <dbl> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,…
#just an id

#$ Age                            <dbl> 24.44301, 18.00000, 18.00000, 20.95274, 31.64108, 18.12825, 29.88302,…
summary(d$Age)

percentiles(d$Age)

ggplot(d, aes(x = Age)) +
  geom_histogram()

normality_test(d$Age) #tiny p value so can't assume normal, sk 1.5 to right tail, 
#k 6.6 so more outliers

d %>%
  group_by(NObeyesdad_fct) %>%
  summarise(median(Age))

ggplot(d, aes(Age, NObeyesdad_fct)) +
  geom_boxplot() #Obesity tends to increase with Age

# cap at p99
d$Age <- ifelse(d$Age <= quantile(d$Age, 0.99), d$Age, quantile(d$Age, .99))


#$ Height                         <dbl> 1.699998, 1.560000, 1.711460, 1.710730, 1.914186, 1.748524, 1.754711,…
summary(d$Height) #assume mts, from 1.45 to 1.98 with medi of 1.7

percentiles(d$Height)

ggplot(d, aes(x = Height)) +
  geom_histogram() #looks normal

normality_test(d$Height) #tiny p value so can't assume normal, sk 0.03 to right tail, 
#k 2.4 so not much outliers

d %>%
  group_by(NObeyesdad_fct) %>%
  summarise(median(Height)) #no power

ggplot(d, aes(Height, NObeyesdad_fct)) +
  geom_boxplot() #low power but will keep


#$ Weight                         <dbl> 81.66995, 57.00000, 50.16575, 131.27485, 93.79806, 51.55259, 112.7250…
summary(d$Weight) #assume kg from 39 to 165

percentiles(d$Weight) #seems long rith tail

ggplot(d, aes(x = Weight)) +
  geom_histogram() #normalish

normality_test(d$Weight) #tiny p value so can't assume normal, sk 0.09 to right tail, 
#k 1.9 so not much outliers

d %>%
  group_by(NObeyesdad_fct) %>%
  summarise(median(Weight)) #has power ovbiously

ggplot(d, aes(Weight, NObeyesdad_fct)) +
  geom_boxplot() #strong power ovbiously


#$ Gender                         <chr> "Male", "Female", "Female", "Female", "Male", "Male", "Male", "Male",…
table(d$Gender) #50/50 aprox

(tbl_Gender <- table(d$NObeyesdad_fct, d$Gender))

(tblp_Gender <- round(prop.table(tbl_Gender,2),2))

barplot(tblp_Gender) # some power, Males get more OW II, Ob II

d <- d %>%
  mutate(Gender_fct = factor(Gender)) %>%
  select(-Gender)


#$ family_history_with_overweight <chr> "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "no", "yes", …
table(d$family_history_with_overweight) #28.4k yes, 6.2k no

(tbl_family_history_with_overweight <- table(d$NObeyesdad_fct, 
                                             d$family_history_with_overweight))

(tblp_family_history_with_overweight <- round(prop.table(tbl_family_history_with_overweight,2),2))
# power, yes way more obesity and OWII, no way more insuf, normal o a bit ow

d <- d %>%
  mutate(family_history_with_overweight_fct = factor(family_history_with_overweight)) %>%
  select(-family_history_with_overweight)


#$ FAVC                           <chr> "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes",…
#Frequent consumption of high caloric food (FAVC), 
table(d$FAVC) #31k yes vs 3k no

(tbl_FAVC <- table(d$NObeyesdad_fct, d$FAVC))

(tblp_FAVC <- round(prop.table(tbl_FAVC,2),2))
# power, yes tend to be more obese, while no tend to be insuf o nromal o ow II

d <- d %>%
  mutate(FAVC_fct = factor(FAVC)) %>%
  select(-FAVC)


#$ SMOKE                          <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no…
table(d$SMOKE) #425yes

(tbl_SMOKE <- table(d$NObeyesdad_fct, d$SMOKE))

(tblp_SMOKE <- round(prop.table(tbl_SMOKE,2),2))
# unsure due to low n, but smokers tend to be more ob type ii but normal, so unclear

d <- d %>%
  mutate(SMOKE_fct = factor(SMOKE)) %>%
  select(-SMOKE)


#$ SCC                            <chr> "no", "no", "no", "no", "no", "no", "no", "no", "yes", "no", "no", "n…
#Calories consumption monitoring (SCC), 
table(d$SCC) # 1.1k yes

(tbl_SCC <- table(d$NObeyesdad_fct, d$SCC))

(tblp_SCC <- round(prop.table(tbl_SCC,2),2)) #those that monitor are less obese and viceversa

d <- d %>%
  mutate(SCC_fct = factor(SCC)) %>%
  select(-SCC)


#$ MTRANS                         <chr> "Public_Transportation", "Automobile", "Public_Transportation", "Publ…
#Transportation used (MTRANS)
table(d$MTRANS) # bike and motorbike have low n; mostly public and auto

# bin due to low n and similar prop
d$MTRANS_bin <- ifelse(d$MTRANS %in% c("Bike", "Motorbike", "Walking"),
                       "Bike_Moto_Walk", d$MTRANS)

(tbl_MTRANS <- table(d$NObeyesdad_fct, d$MTRANS_bin))

(tblp_MTRANS <- round(prop.table(tbl_MTRANS,2),2)) 
# some power, obese mostly public y auto; normal mostly bike...

d <- d %>%
  mutate(MTRANS_bin = factor(MTRANS_bin)) %>%
  select(-MTRANS)


#$ CALC                           <chr> "Sometimes", "no", "no", "Sometimes", "Sometimes", "Sometimes", "Some…
#Consumption of alcohol (CALC). 
table(d$CALC) 

# n=2 always so fix
d$CALC_bin <- ifelse(d$CALC %in% c("Always", "Frequently"),
                       "Frequently", d$CALC)

(tbl_CALC <- table(d$NObeyesdad_fct, d$CALC_bin))

(tblp_CALC <- round(prop.table(tbl_CALC,2),2)) 
# some power; frequest mostly ow, no either normal ow2 or ob; all over

d <- d %>%
  mutate(CALC_bin = factor(CALC_bin,
                           ordered = TRUE,
                           levels = c("no", "Sometimes", "Frequently"))) %>%
  select(-CALC)


#$ CAEC                           <chr> "Sometimes", "Frequently", "Sometimes", "Sometimes", "Sometimes", "So…
#Consumption of food between meals (CAEC), 
table(d$CAEC) 

(tbl_CAEC <- table(d$NObeyesdad_fct, d$CAEC))

(tblp_CAEC <- round(prop.table(tbl_CAEC,2),2)) 
# some power, always are normal, freq mostly insuf, no mostly ow1, some all over
# but concern due to some low n but will leave

d <- d %>%
  mutate(CAEC_fct = factor(CAEC,
                           ordered = TRUE,
                           levels = c("no", "Sometimes",
                                      "Frequently", "Always"))) %>%
  select(-CAEC)


#$ TUE                            <dbl> 0.976473, 1.000000, 1.673584, 0.780199, 0.931721, 1.000000, 0.696948,…
#Time using technology devices (TUE), 
summary(d$TUE) #0-2

percentiles(d$TUE)

ggplot(d, aes(x = TUE)) +
  geom_histogram() #mostly 0 and 1, some 2 and a few in between

ggplot(d, aes(TUE, NObeyesdad_fct)) +
  geom_boxplot() #there are some differences but what to do?

# cut to see props
quantile(d$TUE)

d$TUE_cut <- case_when(
  d$TUE == 0 ~ "0",
  d$TUE > 0 & d$TUE <= 1 ~ "1",
  d$TUE > 1 & d$TUE <= 2 ~ "2",
  .default = "error"
)

d$TUE_cut <- factor(d$TUE_cut,
                    ordered = TRUE,
                    levels = c("0", "1", "2"))

(tbl_TUE_cut <- table(d$NObeyesdad_fct, d$TUE_cut))

(tblp_TUE_cut <- round(prop.table(tbl_TUE_cut,2),2)) 
# some potential but confusing and low n
# leave for now number and this and try diff algs


#$ FAF                            <dbl> 0.000000, 1.000000, 0.866045, 1.467863, 1.967973, 1.930033, 0.000000,…
#Physical activity frequency (FAF), 
summary(d$FAF) #0-3

ggplot(d, aes(x = FAF)) +
  geom_histogram() #mostly 0 and 1, some 2 and then 3.. and some in between

ggplot(d, aes(FAF, NObeyesdad_fct)) +
  geom_boxplot() #there are some differences but what to do?

#cut
quantile(d$FAF)

d$FAF_cut <- case_when(
  d$FAF == 0 ~ "0",
  d$FAF > 0 & d$FAF <= 1 ~ "1",
  d$FAF > 1 & d$FAF <= 2 ~ "2",
  d$FAF > 2 & d$FAF <= 3 ~ "3",
  .default = "error"
)

d$FAF_cut <- factor(d$FAF_cut,
                    ordered = TRUE,
                    levels = c("0", "1", "2", "3"))

(tbl_FAF_cut <- table(d$NObeyesdad_fct, d$FAF_cut))

(tblp_FAF_cut <- round(prop.table(tbl_FAF_cut,2),2)) 
# OBiii WITH 0, then more spread


#$ NCP                            <dbl> 2.983297, 3.000000, 1.411685, 3.000000, 1.971472, 3.000000, 3.000000,…
#Number of main meals (NCP), 
summary(d$NCP) #1-4

ggplot(d, aes(x = NCP)) +
  geom_histogram() #mostly 3

# cut based on quantiles to care for n
quantile(d$NCP)

d$NCP_cut <- case_when(
  d$NCP < 3 ~ "<3",
  d$NCP == 3 ~ "3",
  d$NCP > 3 ~ ">3",
  .default = "error"
)

d$NCP_cut <- factor(d$NCP_cut,
                    ordered = TRUE,
                    levels = c("<3", "3", ">3"))

(tbl_NCP_cut <- table(d$NObeyesdad_fct, d$NCP_cut))

(tblp_NCP_cut <- round(prop.table(tbl_NCP_cut,2),2)) 
# >3 insuf, 3 ow, but all over


#$ CH2O                           <dbl> 2.763573, 2.000000, 1.910378, 1.674061, 1.979848, 2.137550, 2.000000,…
#Consumption of water daily (CH20), 
summary(d$CH2O) #1-3

ggplot(d, aes(x = CH2O)) +
  geom_histogram() #mostly 2,1,3 and in between

quantile(d$CH2O) #1,2,3 cut

d$CH2O_cut <- case_when(
  d$CH2O == 1 ~ "1",
  d$CH2O > 1 & d$CH2O <= 2 ~ "2",
  d$CH2O > 2 ~ "3",
  .default = "error"
)

d$CH2O_cut <- factor(d$CH2O_cut,
                    ordered = TRUE,
                    levels = c("1", "2", "3"))

(tbl_CH2O_cut <- table(d$NObeyesdad_fct, d$CH2O_cut))

(tblp_CH2O_cut <- round(prop.table(tbl_CH2O_cut,2),2)) 

# 3 tend to be more obese, normal 1. all over but some power


#$ FCVC                           <dbl> 2.000000, 2.000000, 1.880534, 3.000000, 2.679664, 2.919751, 1.991240,…
#Frequency of consumption of vegetables (FCVC), 
summary(d$FCVC) #1-3

ggplot(d, aes(x = FCVC)) +
  geom_histogram() #mostly 3,2 and a bit of 1 and then spread...

quantile(d$FCVC) #1,2,3 cut

d$FCVC_cut <- case_when(
  d$FCVC == 1 ~ "1",
  d$FCVC > 1 & d$FCVC <= 2 ~ "2",
  d$FCVC > 2 ~ "3",
  .default = "error"
)

d$FCVC_cut <- factor(d$FCVC_cut,
                     ordered = TRUE,
                     levels = c("1", "2", "3"))

(tbl_FCVC_cut <- table(d$NObeyesdad_fct, d$FCVC_cut))

(tblp_FCVC_cut <- round(prop.table(tbl_FCVC_cut,2),2)) 

# 3 ob 2/3, 1 norm, rest all over but some power

# sort by column name
d <- d %>%
  select(order(colnames(d)))

# valid no Nas
# valid data formats of all columsn are ok
# try a tree

#feature eng
#bmi = Metric Units: Weight (kg) / Height (m)² 

#multicolinearity en num y otras...

#corrplot(round(cor(d[,numeric_vars]),2))
# read https://www.google.com/search?q=detect+collinearity+in+categorical+variables&rlz=1C1RXQR_enUS1134US1138&oq=detect+collinearity+in+categorical&gs_lcrp=EgZjaHJvbWUqBwgBECEYoAEyBggAEEUYOTIHCAEQIRigATIHCAIQIRigATIHCAMQIRigATIHCAQQIRigATIHCAUQIRirAjIHCAYQIRifBTIHCAcQIRiPAtIBCDg2MDZqMGo0qAIAsAIB&sourceid=chrome&ie=UTF-8

#REFERENCES
#https://www.kaggle.com/competitions/playground-series-s4e2/data
#https://www.kaggle.com/datasets/aravindpcoder/obesity-or-cvd-risk-classifyregressorcluster/data
