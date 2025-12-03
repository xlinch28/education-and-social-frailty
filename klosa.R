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
data_h <- read_dta('/Users/xiangxiang/Desktop/social frailty data/H_KLoSA_e2.dta')

subdat_h <- subset(data_h, select = c(
  inw7, pid, hh7hhid,
  r7iwstat, r7wtresp,
  rabyear,
  r7agey, ragender,
  raeduc_k, raeducl,
  r7atotb, h7hhres, r7kcnt, r7socwk, r7flonel,
  r7satfnncl_k,
  r7rural,
  r7relgwk,
  r7height
))

names(subdat_h) <- c("inw7","ID","hhid",
                     "r7iwstat",
                     "r7wtrespb", 
                     "birth_year",
                     "age", "gender",
                     "SHLT",
                     "years_edu","H_edu",
                     "household_wealth",
                     "people_living_with","week_contact",
                     "soc_activity",
                     "loneliness",
                     "financial_sat","urbanicity","rel_activity","height"
)
write.csv(subdat_h,'klosa.csv')


df <- read.csv("klosa.csv")


####preparing the df####
##define sample size; n = 6940
sum(df$inw7)
df <- df[df$inw7 == 1,]

##exclude age >= 50; n = 6940
df <- df[which(df$age >= 50), ]

df <- df %>%
  mutate(
    # 社会工作 participation
    no_participate_socwk = case_when(
      soc_activity == 0 ~ 1,
      soc_activity == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 宗教活动 participation
    no_participate_rel = case_when(
      rel_activity== 0 ~ 1,
      rel_activity == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # soc_activity：表示“无社会活动”
    soc_activity = case_when(
      is.na(no_participate_socwk) & is.na(no_participate_rel) ~ NA_real_,    # 两个都缺失  NA
      no_participate_socwk == 1 & no_participate_rel == 1 ~ 1,               # 都不参与  1
      (no_participate_socwk == 1 & is.na(no_participate_rel)) ~ 1,           # 一个不参与，一个缺失  1
      (no_participate_rel == 1 & is.na(no_participate_socwk)) ~ 1,           # 一个不参与，一个缺失  1
      TRUE ~ 0                                                               # 其他情况  0
    )
  )

##delete na in social fraity index; n = 6706
vars <- c("people_living_with", "week_contact", "soc_activity", "loneliness","financial_sat")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)

#df <- subset(df, !(is.na(household_wealth)))
df <- subset(df, !(is.na(people_living_with)))
df <- subset(df, !(is.na(week_contact)))
df <- subset(df, !(is.na(soc_activity)))
df <- subset(df, !(is.na(loneliness)))
df <- subset(df, !(is.na(financial_sat)))

##delete na in education; n = 6706
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

median_value <- median(df$financial_sat, na.rm = TRUE)

# 根据中位数创建新变量：financial_unsat
df$financial_unsat1 <- ifelse(
  df$financial_sat < 40, 1, 0
)

table(df$financial_unsat1)


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

#5.loneliness 
df <- df %>%
  mutate(loneliness = case_when(
    loneliness %in% c(3, 4) ~ "1",
    loneliness %in% c(1, 2) ~ "0",
    TRUE ~ NA_character_
  ))

table(df$loneliness, exclude = NULL)

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
table(df$frailty_status2, exclude = NULL)
table(df$frailty_score, exclude = NULL)

####covariates####
#recode variabels
#gender: 1 is men; 2 is women
table(df$gender, exclude = NULL)
df$gender <- ifelse(df$gender==1, "men", "women")
df$gender <- factor(df$gender)

#urbanicity
df <- df %>%
  mutate(urbanicity = case_when(
    urbanicity == 0 ~ "urban",
    urbanicity == 1 ~ "rural",
    TRUE ~ NA_character_
  ))
table(df$urbanicity)
df$urbanicity <- factor(df$urbanicity)


variables <- c("age", "gender","urbanicity","height")

for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("Variable:", var, 
      "- Missing:", missing_count, 
      "- Missing %:", round(missing_percentage, 2), "%\n")
}

cat_vars <- c("gender","urbanicity")

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

write.csv(df,"klosa_cleaning.csv")


df <- read.csv("/Users/xiangxiang/Desktop/social frailty data/klosa_cleaning.csv")

age_groups <- cut(df$age, breaks = c(49, 64, 74, 84, Inf), 
                  labels = c("50-64", "65-74", "75-84", "≥85"))
age_groups <- factor(age_groups, levels = c("50-64", "65-74", "75-84", "≥85"), ordered = TRUE)
df$age_groups <- age_groups


####descriptive table
cat_vars <- c("gender","urbanicity", "education_quan","H_edu", 
              "financial_sat", "financial_unsat1","people_living_with", "week_contact", 
              "soc_activity", "loneliness",
              "frailty_status","frailty_score")

# 连续变量
cont_vars <- c("age","height")

all_vars <- c(cont_vars, cat_vars)
df[cat_vars] <- lapply(df[cat_vars], function(x) as.factor(x))

df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty", 
                                                          "pre-frailty", 
                                                          "frailty"))

df$frailty_status2 <- factor(df$frailtye_status2, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))


df$education_quan <- factor(df$education_quan)
df$H_edu <- factor(df$H_edu)
table(df$education_quan)
table(df$H_edu)

ftable( df$education_quan,df$frailty_status)
ftable(df$education_quan,df$frailty_status2)
ftable(df$H_edu,df$frailty_status)
ftable(df$frailty_status, df$education_quan, df$gender)
ftable(df$frailty_status, df$age_groups)


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

####Mlogit model####

fit_1 <- multinom(
  frailty_status ~  education_quan + gender+ age + urbanicity,
  data = df,
  Hess = TRUE 
)

fit_1_2 <- multinom(
  frailty_status2 ~  education_quan + gender+ age + urbanicity,
  data = df,
  Hess = TRUE 
)

fit_2 <- multinom(
  frailty_status ~ H_edu+ gender + age + urbanicity,
  data = df,
  Hess = TRUE
)

fit_2_2 <- multinom(
  frailty_status2 ~ H_edu+ gender + age + urbanicity,
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
  frailty_status ~  education_quan+ gender + age + urbanicity  + height ,
  data = df,
  Hess = TRUE 
)
fit_3_2 <- multinom(
  frailty_status2 ~  education_quan+ gender + age+ urbanicity  + height ,
  data = df,
  Hess = TRUE 
)
fit_4 <- multinom(
  frailty_status ~ H_edu  + gender + age +  urbanicity + height ,
  data = df,
  Hess = TRUE
)
fit_4_2 <- multinom(
  frailty_status2 ~ H_edu  + gender + age+  urbanicity + height ,
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
  frailty_status ~ education_quan + age + urbanicity,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m1 <- multinom(
  frailty_status ~ education_quan + age + urbanicity,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w1)
summary(fit_m1)
tab_model(fit_w1)
tab_model(fit_m1)

fit_w2 <- multinom(
  frailty_status ~ education_quan + + age + urbanicity  + height ,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m2<- multinom(
  frailty_status ~ education_quan + age + urbanicity  + height ,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w2)
summary(fit_m2)
tab_model(fit_w2)
tab_model(fit_m2)
table(df$gender)

###weight
df$WEIGHT <- df$r7wtrespb/sum(df[, 'r7wtrespb'])*6706
fit_weight <- multinom(
  frailty_status ~ education_quan + gender + age +urbanicity + height ,
  data = df,
  weights = WEIGHT,   
  Hess = TRUE
)

summary(fit_weight)
tab_model(fit_weight)
