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
library(sjPlot)
library(ggplot2) 
library(dplyr) 
library(ggalluvial)
library(ggsci)
library(tableone)
library(kableExtra)
library(sjPlot)
library(geepack)
library(tidyr)
library(DT)
library(nnet)
setwd('/Users/xiangxiang/Desktop/social frailty data')
data_h <- read_dta('/Users/xiangxiang/Desktop/social frailty data/H_LASI_a3.dta')

subdat_h <- subset(data_h, select =c(
  inw1,             # wave 1 interview wave
  prim_key,         # respondent ID
  hhid,             # household ID
  
  r1iwstat,         # interview status
  r1wtresp,         # individual weight
  rabyear,          # birth year
  
  # demographics
  r1agey,           # age at interview
  ragender,         # gender
  r1lbrf_l,         # labor force status
  r1mstat,          # marital status
  
  # health-related
  r1shlt,           # self-reported health
  r1tr20,           # word recall
  r1orient,         # orientation
  r1arthre,         # arthritis
  r1cancre,         # cancer
  r1hibpe,          # high blood pressure
  r1diabe,          # diabetes
  r1lunge,          # lung disease
  r1hearte,         # heart problem
  r1stroke,         # stroke
  r1psyche,         # psychological problem
  
  r1vgactx,         # days/week vigorous physical activity
  r1mdactx,         # days/week moderate physical activity
  
  r1drink3m,        # ever drank alcohol in past 3 months
  r1smoken,         # currently smoking
  r1smokef,         # number of cigarettes per day
  
  r1adltot6,        # ADL (Activities of Daily Living) 6-item total score
  r1iadlza,         # IADL (Instrumental Activities of Daily Living) summary
  
  # education / parental background
  raedyrs,          # years of education
  raeducl,          # education level
  ramomeducl,       # mother's education level
  radadeducl,       # father's education level
  
  # general resources
  hh1atotb,         # total household wealth/assets
  hh1rural,         # rural or urban location
  
  # social resources
  hh1hhres,         # number of people living in household
  r1fcntpm,         # frequency of contact with children via phone/mail
  r1socyr,          # participation in social activities
  r1relgwk,
  
  # fulfillment of social needs
  r1flonel,         # CESD: felt lonely
  
  r1caste           # caste/social class (country-specific)
))


names(subdat_h) <- c("inw1","ID","hhid",
                     "r1iwstat",
                     "r1wtrespb", "birth_year",
                     "age", "gender", "lb_status","marital_status",
                     "SHLT","r1tr20","orient",
                     "r1arthre","r1cancre","r1hibpe","r1diabe","r1lunge","r1hearte","r1stroke","r1psyche",
                     "vigor_act", "moder_act", "drink_now", "smoke_now","num_smoke","ADL", "IADL", 
                     "years_edu","H_edu","H_edu_m","H_edu_f",
                     "household_wealth", "urbanicity",
                     "people_living_with","week_contact",
                     "soc_activity","rel_activity",
                     "loneliness", "caste"
)

write.csv(subdat_h,'lasi.csv')

library(dplyr)
library(mice)

data_h <- read_dta('/Users/xiangxiang/Desktop/social frailty data/lasi_w1b_hh.dta')
data_h_subset <- data_h %>%
  select(hhid, in904) %>%
  mutate(
    financial_unsat1 = ifelse(in904 %in% c(1, 2), 1, 
                             ifelse(in904 %in% c(3, 4, 5), 0, NA)),
    financial_unsat2 = ifelse(in904 %in% c(1, 2,3), 1, 
                             ifelse(in904 %in% c(4, 5), 0, NA))
    
  )


table(data_h_subset$financial_unsat, useNA = "ifany")

####02 data cleaning####
df <- read.csv("lasi.csv")
df <- df %>%
  mutate(hhid = as.character(hhid))

df_merged <- df %>%
  left_join(data_h_subset, by = "hhid")
table(df_merged$financial_unsat1, useNA = "ifany")

write.csv(df_merged,'lasi.csv')

df <- read.csv("lasi.csv")

####preparing the df####
##define sample size; n = 73408
sum(df$inw1)
df <- df[df$inw1 == 1,]

##exclude age >= 50; n = 53166
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

##delete na in social fraity index; n = 50310

vars <- c("financial_unsat1", "people_living_with", "week_contact", "soc_activity","no_participate_socwk","no_participate_rel", "loneliness")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)

df <- subset(df, !(is.na(financial_unsat1)))
df <- subset(df, !(is.na(people_living_with)))
df <- subset(df, !(is.na(week_contact)))
df <- subset(df, !(is.na(soc_activity)))
df <- subset(df, !(is.na(loneliness)))


##delete na in education; n = 50310
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
  mutate(
    education_quan = case_when(
      !is.na(years_edu) & years_edu == 0     ~ 1,
      years_edu >= 1 & years_edu <= 7        ~ 2,
      years_edu >= 8                         ~ 3,
      TRUE                                   ~ NA_real_
    )
  )

table(df$education_quan, useNA = "ifany")


####construct the outcome: social fraity####
#1. household_wealth
# #equivalized_wealth
# df$people_living_with <- as.numeric(as.character(df$people_living_with))
# df$equivalized_wealth <- df$household_wealth/sqrt(df$people_living_with)
# df$equivalized_wealth <- round(df$equivalized_wealth/1000, 2)
# summary(df$equivalized_wealth)
# df$wealth_quartile <- cut(
#   df$equivalized_wealth,
#   breaks = quantile(df$equivalized_wealth, probs = c(0, 0.5, 1), na.rm = TRUE),
#   include.lowest = TRUE,
#   labels = c("1","0")
# )
# table(df$wealth_quartile)
# table(df$wealth_quartile, useNA = "ifany")


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
#
df <- df %>%
  mutate(loneliness = case_when(
    loneliness %in% c(3, 4) ~ "1",
    loneliness %in% c(1, 2) ~ "0",
    TRUE ~ NA_character_
  ))

table(df$loneliness, exclude = NULL)

vars <- c("financial_unsat1", "financial_unsat2", "people_living_with", "week_contact", "soc_activity", "loneliness")

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

df$frailty_scoreb <- rowSums(df[, c("financial_unsat2", "people_living_with", "week_contact", "soc_activity", "loneliness")], na.rm = FALSE)


df$frailty_status <- ifelse(df$frailty_score == 0, "non-frailty",
                            ifelse(df$frailty_score %in% c(1, 2), "pre-frailty",
                                   ifelse(df$frailty_score >= 3, "frailty", NA)))
df$frailty_status2 <- ifelse(df$frailty_score == 0, "non-frailty",
                             ifelse(df$frailty_score ==1, "pre-frailty",
                                    ifelse(df$frailty_score >= 2, "frailty", NA)))

df$frailty_statusb <- ifelse(df$frailty_scoreb == 0, "non-frailty",
                             ifelse(df$frailty_scoreb %in% c(1, 2), "pre-frailty",
                                    ifelse(df$frailty_scoreb >= 3, "frailty", NA)))
df$frailty_statusb2 <- ifelse(df$frailty_scoreb == 0, "non-frailty",
                              ifelse(df$frailty_scoreb ==1, "pre-frailty",
                                     ifelse(df$frailty_scoreb >= 2, "frailty", NA)))

table(df$frailty_status, exclude = NULL)
table(df$frailty_score, exclude = NULL)
table(df$frailty_status2, exclude = NULL)
table(df$frailty_scoreb, exclude = NULL)


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


# #hukou
# table(df$hukou)
# #1.Agricultual hukou, 2.Non-agricultural hukou, 3.Unified residence hukou, 4.Do not have hukou
# #combine 1 and 4
# df$hukou <- ifelse(df$hukou==1|df$hukou==4, 1, 0)
# table(df$hukou)


#labour force status
table(df$lb_status)
#1.Agri employed, 2.Agri self-employed, 3.Non-agri employed, 4.Non-agri self-employed
#5.Non-agri family business, 6.Unemployed, 7.Retired, 8.Never work
df$lb_status <- ifelse(df$lb_status==1|df$lb_status==2|df$lb_status==3|df$lb_status==4|df$lb_status==5,"employed", "other")

#orientation
summary(df$orient)

#word recall
summary(df$r1tr20)

df <- df %>%
  mutate(
    cognitive_impairment = ifelse(
      orient < mean(orient, na.rm = TRUE) - 1.5 * sd(orient, na.rm = TRUE) &
        r1tr20 < mean(r1tr20, na.rm = TRUE) - 1.5 * sd(r1tr20, na.rm = TRUE),
      "Impaired", 
      "Normal"
    )
  ) %>%
  ungroup() 



#chronic diseases
disease <- subset(df, select = c(r1arthre, #  r ever had arthritis
                                 r1cancre, #  r ever had cancer
                                 r1hibpe,  #  r Ever had high blood pressure
                                 r1diabe,  #  r ever had diabetes
                                 r1lunge,  #  r ever had lung disease
                                 r1hearte, #  r ever had heart problem
                                 r1stroke, #  r ever had stroke
                                 r1psyche  #  r ever had psych problem
))
disease <- data.frame(disease) 
disease$n_chronic <- rowSums(disease) 

disease_numeric <- disease %>%
  mutate(n_chronic_category = case_when(
    n_chronic == 0 | n_chronic == 1~ "0 or 1 chronic",
    n_chronic >= 2 & n_chronic <= 8 ~ "more than 2 chronic",
    TRUE ~ NA_character_  
  ))
df <- cbind(df, n_chronic_category = disease_numeric$n_chronic_category)

#drink
df <- df %>%
  mutate(drink_now = case_when(
    as.numeric(drink_now) == 1 ~ "Yes",
    as.numeric(drink_now) == 0 ~ "No",
    TRUE ~ NA_character_
  ))

table(df$drink_now, exclude = NULL)

#smoke status
table(df$smoke_now, exclude = NULL) #1=Yes, 0=none


#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
df$marital_status<- ifelse(df$marital_status==1|df$marital_status==3, "married/partnered", "other")
table(df$marital_status, exclude = NULL)

#ADL
table(df$ADL)
table(df$IADL)

#days of vigorous/moderate activity
table(df$vigor_act, exclude = NULL)
table(df$moder_act, exclude = NULL)

#caste
df$caste <- as.numeric(df$caste)
df$caste_group <- ifelse(df$caste %in% c(1, 2), 1, 0)
df$caste_group <- factor(df$caste_group,
                         levels = c(1, 0),
                         labels = c("Scheduled caste or scheduled tribe", "Others"))
df <- df %>%
  mutate(any_vigoral_activities = case_when(
    vigor_act %in% 1:4 ~ 1,
    vigor_act == 5 ~ 0,
    TRUE ~ NA_real_
  ))
df <- df %>%
  mutate(any_vigoral_activities = as.factor(any_vigoral_activities))
                         
variables <- c("age", "gender","race","urbanicity","drink_now","smoke_now",
               "any_vigoral_activities","n_chronic_category","caste_group"
)
for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("Variable:", var, 
      "- Missing:", missing_count, 
      "- Missing %:", round(missing_percentage, 2), "%\n")
}



cat_vars <- c("gender","caste","urbanicity", "education_quan","H_edu", 
              "financial_unsat1","financial_unsat2", "people_living_with", "week_contact", 
              "soc_activity", "loneliness",
              "frailty_status","frailty_status2","frailty_score",
              "frailty_statusb","frailty_statusb2","frailty_scoreb",
              "marital_status","n_chronic_category","drink_now","smoke_now",
              "any_vigoral_activities")


# 连续变量
cont_vars <- c("age")
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

write.csv(df,"lasi_cleaning.csv")

df <- read.csv("/Users/xiangxiang/Desktop/social frailty data/lasi_cleaning.csv")

# 把分类变量转换成 factor 类型
cat_vars <- c("gender","caste","urbanicity", "education_quan","H_edu", 
              "in904","financial_unsat1","financial_unsat2", "people_living_with", "week_contact", 
              "soc_activity", "loneliness",
              "frailty_status","frailty_status2","frailty_score",
              "frailty_statusb","frailty_statusb2","frailty_scoreb",
              "marital_status","n_chronic_category","drink_now","smoke_now",
              "any_vigoral_activities","caste_group")


# 连续变量
cont_vars <- c("age")

all_vars <- c(cont_vars, cat_vars)

df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty", 
                                                          "pre-frailty", 
                                                          "frailty"))

df$frailty_status2 <- factor(df$frailty_status2, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))
df$frailty_statusb <- factor(df$frailty_statusb, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))

df$frailty_statusb2 <- factor(df$frailty_statusb2, levels = c("non-frailty", 
                                                              "pre-frailty", 
                                                              "frailty"))
df$education_quan <- factor(df$education_quan)
df$H_edu <- factor(df$H_edu)

ftable( df$education_quan,df$frailty_status)
ftable(df$education_quan,df$frailty_status2)
ftable(df$H_edu,df$frailty_status)

ftable(df$frailty_status, df$education_quan, df$gender)
####descriptive table
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

library(nnet)

fit_1 <- multinom(
  frailty_status ~  education_quan + gender+ age+caste_group + urbanicity,
  data = df,
  Hess = TRUE 
)

fit_1_2 <- multinom(
  frailty_status2 ~  education_quan + gender+ age+caste_group + urbanicity,
  data = df,
  Hess = TRUE 
)

fit_2 <- multinom(
  frailty_status ~ H_edu+ gender + age +caste_group+ urbanicity,
  data = df,
  Hess = TRUE
)

fit_2_2 <- multinom(
  frailty_status2 ~ H_edu+ gender + age+caste_group + urbanicity,
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
  frailty_status ~  education_quan+ gender + age+caste_group + urbanicity  + n_chronic_category + drink_now + smoke_now + any_vigoral_activities,
  data = df,
  Hess = TRUE 
)
fit_3_2 <- multinom(
  frailty_status2 ~  education_quan+ gender + age +caste_group+ urbanicity  + n_chronic_category + drink_now + smoke_now + any_vigoral_activities,
  data = df,
  Hess = TRUE 
)
fit_4 <- multinom(
  frailty_status ~ H_edu  + gender + age +caste_group+  urbanicity + n_chronic_category + drink_now + smoke_now+any_vigoral_activities,
  data = df,
  Hess = TRUE
)
fit_4_2 <- multinom(
  frailty_status2 ~ H_edu  + gender + age+caste_group+urbanicity + n_chronic_category + drink_now + smoke_now+any_vigoral_activities,
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
  frailty_status ~ education_quan + age +caste_group + urbanicity,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m1 <- multinom(
  frailty_status ~ education_quan + age +caste_group + urbanicity,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w1)
summary(fit_m1)
tab_model(fit_w1)
tab_model(fit_m1)

fit_w2 <- multinom(
  frailty_status ~ education_quan + + age +caste_group + urbanicity  + n_chronic_category + drink_now + smoke_now + any_vigoral_activities,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m2<- multinom(
  frailty_status ~ education_quan + age +caste_group + urbanicity  + n_chronic_category + drink_now + smoke_now + any_vigoral_activities,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w2)
summary(fit_m2)
tab_model(fit_w2)
tab_model(fit_m2)
table(df$gender)
#weight
df$WEIGHT <- df$r1iwstat/sum(df[, 'r1iwstat'])*50310
fit_weight <- multinom(
  frailty_status ~ education_quan + gender + age + caste_group +urbanicity +
    n_chronic_category + drink_now + smoke_now + any_vigoral_activities,
  data = df,
  weights = WEIGHT,   
  Hess = TRUE
)

summary(fit_weight)
tab_model(fit_weight)

