library(tidyverse) #data tidying
library(leaps) #data tidying
library(ggplot2) #visualizing data
library(corrplot) #visualizing data
library(plyr) #data tidying
library(dplyr) 
library(glmnet) #for modeling
library(caret) #for modeling
library(xgboost) #for modeling
library(randomForest) #for modeling
library(pander) #data collation
library(mice) #check missing values
library(ggalluvial)
library(haven)
library(MCM)
library(ggsci)
library(reshape2)
library(nnet)

setwd('/Users/xiangxiang/Desktop/social frailty data')
data_h <- read_dta('H_SHARE_f.dta')

subdat_h <- subset(data_h, select = c(
                        inw8, pn, hhid, # wave 8 IDs
                        r8iwstat,       # w8 r interview status
                        r8wtresp,       # w8 individual weight
                        rabyear,        # birth year
                        
                        # demographics
                        r8agey,         # age at interview
                        ragender,       # gender
                       
                        # education / exposure
                        raedyrs,        # years of education
                        raeducl,        # education level
                        # outcomes / general resources
                        hh8atotb,       # total wealth/assets
                        h8rural,        # rural/urban indicator
                        
                        # outcomes / social resources
                        hh8hhres,       # household size
                        h8kcnt,         # contact with children
                        
                        # social behavior / activity
                        r8socyr,        # social activities participation
                        
                        # social needs fulfillment
                        r8lnlys3,       # felt lonely (CESD item)
                        
                        country,        # country variable
                        rabcountry,      #migration
                        racsevent_s,  #childhood stress
                        rachshlt,   #childhood health
                        r8height
                      ))
                                      

names(subdat_h) <- c("inw8","ID","hhid",
                     "r8iwstat",
                     "r8wtresp", "birth_year",
                     "age", "gender", 
                     "years_edu","H_edu",
                     "household_wealth", "urbanicity",
                     "people_living_with","week_contact",
                     "soc_activity",
                     "loneliness", "country","migration",
                     "racsevent_s",
                     "rachshlt",
                     "height"
                     
)

write.csv(subdat_h,'share.csv')


library(dplyr)
library(mice)


####02 data cleaning####
df <- read.csv("share.csv")

####preparing the df####
##define sample size; n = 46733
sum(df$inw8)
df <- df[df$inw8 == 1,]

##exclude age >= 50; n = 46498
df <- df[which(df$age >= 50), ]

##delete na in social fraity index; n = 41027

vars <- c("household_wealth", "people_living_with", "week_contact", "soc_activity", "loneliness")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)

df <- subset(df, !(is.na(household_wealth)))
df <- subset(df, !(is.na(people_living_with)))
df <- subset(df, !(is.na(week_contact)))
df <- subset(df, !(is.na(soc_activity)))
df <- subset(df, !(is.na(loneliness)))


##delete na in education; n = 41027
df <- subset(df, !(is.na(years_edu)))
df <- subset(df, !(is.na(H_edu)))

# group the respondent by birth year
summary(df$birth_year)
df$decade <- ifelse(df$birth_year >= 1910 & df$birth_year <= 1929, 1920,
                    ifelse(df$birth_year>= 1930 & df$birth_year <= 1939, 1930,
                           ifelse(df$birth_year >= 1940 & df$birth_year <= 1949, 1940,
                                  ifelse(df$birth_year >= 1950 & df$birth_year <= 1959, 1950,
                                         1960))))
table(df$decade)

## percentile ranking of respondents' education level
df <- df %>%
  group_by(decade) %>%
  mutate(percentile_rank = rank(years_edu,ties.method = c("max"))/length(years_edu))
summary(df$percentile_rank)
table(df$percentile_rank)

df$education_quan <- cut(
  df$percentile_rank,
  breaks = quantile(df$percentile_rank, probs = c(0, 0.25, 0.75, 1)),
  labels = c(1, 2, 3),
  include.lowest = TRUE
)
table(df$education_quan)

####construct the outcome: social fraity####
#1. household_wealth
#equivalized_wealth
df$people_living_with <- as.numeric(as.character(df$people_living_with))
df$equivalized_wealth <- df$household_wealth/sqrt(df$people_living_with)
df$equivalized_wealth <- round(df$equivalized_wealth/1000, 2)
summary(df$equivalized_wealth)
median_value <- median(df$equivalized_wealth, na.rm = TRUE)
df$wealth_quan <- cut(
  df$equivalized_wealth,
  breaks = quantile(df$equivalized_wealth, probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("1", "2", "3", "4")
)
df$wealth_quartile1 <- ifelse(df$wealth_quan == "1", 1, 0)

table(df$wealth_quartile)
table(df$wealth_quartile1, useNA = "ifany")
table(df$wealth_quan, useNA = "ifany")


#2. people_living_with
df <- df %>%
  mutate(people_living_with = case_when(
    people_living_with == 1 ~ "1",
    people_living_with >= 2 ~ "0",
    TRUE ~ NA_character_
  ))
table(df$people_living_with)

#3. week_contact 0.no, 1.yes
df <- df %>%
  mutate(week_contact = case_when(
    week_contact == 0 ~ "1",
    week_contact == 1 ~ "0",
    TRUE ~ NA_character_
  ))
table(df$week_contact)

#4. soc_activity #1=Yes, 0=none
df <- df %>%
  mutate(soc_activity = case_when(
    soc_activity == 0 ~ "1",
    soc_activity == 1 ~ "0",
    TRUE ~ NA_character_
  ))
table(df$soc_activity)

#5.loneliness 
#0-3
df <- df %>%
  mutate(loneliness = case_when(
    as.numeric(loneliness) >= 2 ~ "1",
    as.numeric(loneliness) < 2 ~ "0",
    TRUE ~ NA_character_
  ))

table(df$loneliness, exclude = NULL)


vars <- c("wealth_quartile1","wealth_quartile2", "people_living_with", "week_contact", "soc_activity", "loneliness")

df[vars] <- lapply(df[vars], function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))  
  } else {
    as.numeric(x) 
  }
})

str(df[, vars])
df <- df %>%
  mutate(across(all_of(vars), ~as.numeric(as.character(.))))

df$frailty_score <- rowSums(df[, c("wealth_quartile1", "people_living_with", "week_contact", "soc_activity", "loneliness")], na.rm = FALSE)



df$frailty_status <- ifelse(df$frailty_score == 0, "non-frailty",
                            ifelse(df$frailty_score %in% c(1, 2), "pre-frailty",
                                   ifelse(df$frailty_score >= 3, "frailty", NA)))
df$frailty_status2 <- ifelse(df$frailty_score == 0, "non-frailty",
                             ifelse(df$frailty_score ==1, "pre-frailty",
                                    ifelse(df$frailty_score >= 2, "frailty", NA)))


table(df$frailty_status, exclude = NULL)
table(df$frailty_score, exclude = NULL)
table(df$frailty_status2, exclude = NULL)

####covariates####
#recode variabels
#gender: 1 is men; 2 is women
table(df$gender, exclude = NULL)
df$gender <- ifelse(df$gender==1, "men", "women")
df$gender <- factor(df$gender)

age_groups <- cut(df$age, breaks = c(49, 64, 74, 84, Inf), 
                  labels = c("50-64", "65-74", "75-84", "≥85"))
age_groups <- factor(age_groups, levels = c("50-64", "65-74", "75-84", "≥85"), ordered = TRUE)
df$age_groups <- age_groups

#urbanicity
df <- df %>%
  mutate(urbanicity = case_when(
    urbanicity == 0 ~ "urban",
    urbanicity == 1 ~ "rural",
    TRUE ~ NA_character_
  ))
table(df$urbanicity)
df$urbanicity <- factor(df$urbanicity)

table(df$migration, useNA = "ifany")

df <- df %>%
  mutate(chil_health = case_when(
    rachshlt %in% c(1, 2, 3) ~ "healthy",
    rachshlt %in% c(4, 5) ~ "less_healthy", 
    TRUE ~ NA_character_
  ))

df <- df %>%
  mutate(chil_Stress = case_when(
    racsevent_s >= 1~ "stress",
    racsevent_s == 0~ "non-stress",
    TRUE ~ NA_character_
  ))

variables <- c("age", "gender","race","urbanicity","height","chil_Stress","chil_health","migration")

for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("Variable:", var, 
      "- Missing:", missing_count, 
      "- Missing %:", round(missing_percentage, 2), "%\n")
}


cat_vars <- c("gender","urbanicity","chil_Stress","chil_health","migration")
cont_vars <- c("age","height")

all_vars <- c(cont_vars, cat_vars)
df[cat_vars] <- lapply(df[cat_vars], function(x) as.factor(x))

###mice
df_subset <- df[all_vars]
ini <- mice(df_subset, maxit = 0)  
ini$method
method <- ini$method
imp <- mice(df_subset, method = method, m = 5, seed = 123)
df_imputed <- complete(imp, 1)
df[all_vars] <- df_imputed

write.csv(df,"share_cleaning.csv")


df <- read.csv("/Users/xiangxiang/Desktop/social frailty data/share_cleaning.csv")


####descriptive table
cat_vars <- c("gender","race","urbanicity", "education_quan","H_edu", 
              "wealth_quartile1", "people_living_with", "week_contact", 
              "soc_activity", "loneliness",
              "frailty_status","frailty_status2","frailty_score",
              "migration","chil_Stress","chil_health")

# 连续变量
cont_vars <- c("age","height")

all_vars <- c(cont_vars, cat_vars)
df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty", 
                                                          "pre-frailty", 
                                                          "frailty"))

df$frailty_status2 <- factor(df$frailty_status2, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))

df$education_quan <- factor(df$education_quan)
df$H_edu <- factor(df$H_edu)

ftable( df$education_quan,df$frailty_status)
ftable(df$education_quan,df$frailty_status2)
ftable(df$H_edu,df$frailty_status)
ftable(df$frailty_status, df$education_quan, df$gender)


t1 <- CreateTableOne(vars = all_vars, data = df, factorVars = cat_vars)

html_output1 <- print(t1, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE, html = TRUE)
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

t2 <- CreateTableOne(vars = all_vars, 
                     data = df, 
                     factorVars = cat_vars,
                     strata = "gender")

html_output2 <- print(t2, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE, html = TRUE)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by gender",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")
summary(as.numeric(df$frailty_score), na.rm = TRUE)
summary(as.numeric(df$frailty_scoreb), na.rm = TRUE)

t3 <- CreateTableOne(vars = all_vars, 
                     data = df, 
                     factorVars = cat_vars,
                     strata = "age_groups")

html_output3 <- print(t3, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE, html = TRUE)
print(t3, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by age",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

####Mlogit model####
#demographic vari
fit_1 <- multinom(
  frailty_status ~  education_quan + gender+ age+migration + urbanicity,
  data = df,
  Hess = TRUE 
)

fit_1_2 <- multinom(
  frailty_status2 ~  education_quan + gender+ age +migration + urbanicity,
  data = df,
  Hess = TRUE 
)

fit_2 <- multinom(
  frailty_status ~ H_edu+ gender + age+migration  + urbanicity,
  data = df,
  Hess = TRUE
)

fit_2_2 <- multinom(
  frailty_status2 ~ H_edu+ gender + age+migration  + urbanicity,
  data = df,
  Hess = TRUE
)

summary(fit_1)
summary(fit_2)
summary(fit_1_2)
summary(fit_2_2)
tab_model(fit_1)
tab_model(fit_1_2)
tab_model(fit_2)
tab_model(fit_2_2)

# education + confounding
fit_3 <- multinom(
  frailty_status ~  education_quan+ gender + age +migration + urbanicity  + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE 
)
fit_3_2 <- multinom(
  frailty_status2 ~  education_quan+ gender + age +migration + urbanicity  + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE 
)
fit_4 <- multinom(
  frailty_status ~ H_edu  + gender + age +migration +  urbanicity + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE
)
fit_4_2 <- multinom(
  frailty_status2 ~ H_edu  + gender + age +migration +  urbanicity + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE
)
summary(fit_3)
summary(fit_3_2)
summary(fit_4)
summary(fit_4_2)
tab_model(fit_3)
tab_model(fit_3_2)
tab_model(fit_4)
tab_model(fit_4_2)

###subgroup analysis
#women
fit_w1 <- multinom(
  frailty_status ~ education_quan + age +migration + urbanicity,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m1 <- multinom(
  frailty_status ~ education_quan + age+migration  + urbanicity,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w1)
summary(fit_m1)
tab_model(fit_w1)
tab_model(fit_m1)

fit_w2 <- multinom(
  frailty_status ~ education_quan + + age +migration + urbanicity  + height + chil_Stress + chil_health,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m2<- multinom(
  frailty_status ~ education_quan + age +migration + urbanicity  + height + chil_Stress + chil_health,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w2)
summary(fit_m2)
tab_model(fit_w2)
tab_model(fit_m2)

###weight
df_sub <- subset(df, !is.na(r8wtresp))
summary(df_sub$r8wtresp)
df_sub$WEIGHT <- df_sub$r8wtresp / sum(df_sub$r8wtresp) * 41027
summary(df_sub$WEIGHT)

fit_weight <- multinom(
  frailty_status ~ education_quan + gender + age +urbanicity +
    height + chil_Stress + chil_health,
  data = df_sub,
  weights = WEIGHT,   
  Hess = TRUE
)


summary(fit_weight)
tab_model(fit_weight)

