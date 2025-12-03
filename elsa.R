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
data_h <- read_dta('H_ELSA_g3.dta')

subdat_h <- subset(data_h, select = c(inw9,idauniq,hh9hhid,
                                      r9iwstat, #w4 r interview status
                                      r9cwtresp, 
                                      rabyear,# birth year
                                      #demographics
                                      r9agey,#age at interview
                                      ragender,#gender
                                      raracem, #race
                                      #childhoood
                                      ralsevent_e,  # r's summary count of lifetime stressful events
                                      rachshlt, # r childhood health status
                                      r6mheight,
                                      #exposre
                                      raeduc_e,raeducl,#edu
                                      #outcome, General Resources
                                      h9atotb, #Asset: r+s total wealth
                                      #h4rural, #live in rural/urban
                                      #outcome, Social Resources
                                      h9hhres, #number of people living in this household
                                      r9kcnt, #w4 any weekly contact w/ children in person/phone/ema
                                      #outcome, Social Behaviors or Activities
                                      r9socyr, # w4 r participate in social activities
                                      #outcome, Fulfillment of Basic Social Needs
                                      r9flone #w9 r CESD: Felt lonely
                                      

))

names(subdat_h) <- c("inw9","ID","hhid",
                     "r9iwstat",
                     "r9wtrespb", "birth_year",
                     "age", "gender", "race", 
                     "count_stress_events",
                     "child_health",
                     "height"
                     "years_edu","H_edu",
                     "household_wealth",
                     "people_living_with","week_contact",
                     "soc_activity",
                     "loneliness",
           
)
write.csv(subdat_h,'elsa.csv')

df <- read.csv("elsa.csv")
data_h <- read_dta('wave_9_ifs_derived_variables.dta')
data_h_subset <- data_h %>%
  select(ID = idauniq, findiff) %>%
  mutate(
    findiff = ifelse(findiff %in% c(-8, -9, -1), NA, findiff),
    financial_unsat1 = case_when(
      findiff %in% c(4, 5, 6) ~ 1,
      findiff %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    )
  )
table(data_h_subset$findiff)
table(data_h_subset$financial_unsat1)

df <- df %>%
  mutate(ID = as.character(ID))
data_h_subset <- data_h_subset %>%
  mutate(ID = as.character(ID))
df_merged <- df %>%
  left_join(data_h_subset, by = "ID")
table(df_merged$financial_unsat1, useNA = "ifany")

write.csv(df_merged,'elsa.csv')


####02 data cleaning####
df <- read.csv("elsa.csv")

####preparing the df####
##define sample size; n = 8736
sum(df$inw9)
df <- df[df$inw9 == 1,]

##exclude age >= 50; n = 8557 
df <- df[which(df$age >= 50), ]

##delete na in social fraity index; n = 5599
vars <- c("financial_unsat1", "people_living_with", "week_contact", "soc_activity", "loneliness")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)

df <- subset(df, !(is.na(financial_unsat1)))
df <- subset(df, !(is.na(people_living_with)))
df <- subset(df, !(is.na(week_contact)))
df <- subset(df, !(is.na(soc_activity)))
df <- subset(df, !(is.na(loneliness)))

##delete na in education; n = 5109
vars <- c("years_edu", "H_edu")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)
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

df$people_living_with <- as.numeric(as.character(df$people_living_with))

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
# 1.Rarely or none of the time < 1 day;2.Some or a little of the time 1-2 days;3.Occasionally or a moderate amount of 3; 4.Most or all of the time 5-7 days
df <- df %>%
  mutate( loneliness= case_when(
    loneliness == 1 ~ "1",
    loneliness == 0 ~ "0",
    TRUE ~ NA_character_
  ))
table(df$loneliness)

vars <- c("financial_unsat1", "people_living_with", "week_contact", "soc_activity", "loneliness")

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

df$frailty_score <- rowSums(df[, c("financial_unsat1", "people_living_with", "week_contact", "soc_activity", "loneliness")], na.rm = FALSE)


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


table(df$race, exclude = NULL)
df <- df %>%
  mutate(
    race_binary = case_when(
      race == 1 ~ 1,
      !is.na(race) ~ 0,
      TRUE ~ NA_real_
    )
  )
#age
age_groups <- cut(df$age, breaks = c(49, 64, 74, 84, Inf), 
                  labels = c("50-64", "65-74", "75-84", "≥85"))
age_groups <- factor(age_groups, levels = c("50-64", "65-74", "75-84", "≥85"), ordered = TRUE)
df$age_groups <- age_groups

##childhood
df <- df %>%
  mutate(chil_Stress = case_when(
    count_stress_events == 0 ~ "No",
    count_stress_events >= 1 ~ "Yes",
    TRUE ~ NA_character_  
  ))
table(df$chil_Stress)

#childhood, chil_health
df <- df %>%
  mutate(chil_health = case_when(
    child_health %in% c(1, 2, 3) ~ "healthier",
    child_health %in% c(4, 5,6) ~ "less_healthy",
    TRUE ~ NA_character_  
  ))


variables <- c("age", "gender","race","height","chil_Stress","chil_health")
for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("Variable:", var, 
      "- Missing:", missing_count, 
      "- Missing %:", round(missing_percentage, 2), "%\n")
}
cat_vars <- c("gender","race",
              "chil_Stress","chil_health")

# 连续变量
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

write.csv(df,"elsa_cleaning.csv")

df <- read.csv("elsa_cleaning.csv")

####descriptive table
cat_vars <- c("gender","race", "education_quan","H_edu", 
              "findiff","financial_unsat1", "people_living_with", "week_contact", 
              "soc_activity", "loneliness",
              "frailty_status","frailty_status2","frailty_score",
              "chil_Stress","chil_health")

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
ftable(df$gender,df$frailty_status)

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

t3 <- CreateTableOne(vars = all_vars, 
                     data = df, 
                     factorVars = cat_vars,
                     strata = "age_groups")

html_output3 <- print(t3, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE, html = TRUE)
print(t3, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by age",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")


fit_1 <- multinom(
  frailty_status ~  education_quan + gender+ age  + race,
  data = df,
  Hess = TRUE 
)

fit_1_2 <- multinom(
  frailty_status2 ~  education_quan + gender+ age  + race,
  data = df,
  Hess = TRUE 
)

fit_2 <- multinom(
  frailty_status ~ H_edu+ gender + age  +race,
  data = df,
  Hess = TRUE
)

fit_2_2 <- multinom(
  frailty_status2 ~ H_edu+ gender + age  +race,
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
  frailty_status ~  education_quan+ gender + age  +race + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE 
)
fit_3_2 <- multinom(
  frailty_status2 ~  education_quan+ gender + age  +race + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE 
)
fit_4 <- multinom(
  frailty_status ~ H_edu  + gender + age  +race + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE
)
fit_4_2 <- multinom(
  frailty_status2 ~ H_edu  + gender + age  +race + height + chil_Stress + chil_health,
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
  frailty_status ~ education_quan + age + race,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m1 <- multinom(
  frailty_status ~ education_quan + age + race ,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w1)
summary(fit_m1)
tab_model(fit_w1)
tab_model(fit_m1)

fit_w2 <- multinom(
  frailty_status ~ education_quan + + age  +race  +height + chil_Stress + chil_health,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m2<- multinom(
  frailty_status ~ education_quan + age  +race + height + chil_Stress + chil_health,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w2)
summary(fit_m2)
tab_model(fit_w2)
tab_model(fit_m2)
table(df$gender)
###weight
df$WEIGHT <- df$r9wtrespb/sum(df[, 'r9wtrespb'])*5109
fit_weight <- multinom(
  frailty_status ~ education_quan + gender + age +race+
    height + chil_Stress + chil_health,
  data = df,
  weights = WEIGHT,   
  Hess = TRUE
)

summary(fit_weight)
tab_model(fit_weight)

