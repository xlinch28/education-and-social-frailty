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
data_h2 <- read_dta('H_HRS_c.dta')

subdat_h <- subset(data_h2, select = c(hhidpn, hhid,
                                      h13kcnt,
                                      r13socwk,
                                      r13relgwk,
                                      h13rural,
                                      raeducl,
                                      r13mheight

))

names(subdat_h) <- c("ID","hhid",
                     "week_contact",
                     "soc_activity",
                     "rel_activity",
                     "urbanicity",
                     "H_edu",
                     "height"
                     
)

write.csv(subdat_h,'hrs1.csv')


data_h <- read_dta('/Users/xiangxiang/Desktop/social frailty data/randhrs1992_2022v1.dta')

subdat_h2 <- subset(data_h, select = c(inw13,hhidpn, hhid,
                                      r13lbsatfin,
                                      h13hhres,
                                      r13flone,
                                      ragender,
                                      r13agey_m,
                                      raracem,
                                      raedyrs,
                                      rabyear,
                                      rahispan,
                                      r13wtresp,
                                      r13wtcrnh
                                      
                                      
                                      
))

names(subdat_h2) <- c("inw13","ID", "hhid",
                      "financial_unsat",
                      "people_living_with",
                      "loneliness",
                      "gender",
                      "age",
                      "race",
                      "years_edu",
                      "birth_year",
                      "hispanic",
                      "r13wtresp",
                      "r13wtcrnh"
                      
)

merged_data <- merge(subdat_h, subdat_h2, by = "ID", all = FALSE)
write.csv(merged_data,'hrs.csv')

childhood <- read_dta("AGGCHLDFH2016A_R.dta")
childhood <- childhood %>%
  mutate(hhidpn = str_c(HHID, PN, sep = ""))

#childhood stress/financial/warmth
childhood <- childhood %>%
  mutate(
    #  General stress indicator
    chil_Stress = case_when(
      TRPOLICE == 1 ~ "YES",     # Police trouble before 18
      DRKDRUG == 1 ~ "YES",     # Family problems from parental substance abuse
      PHYABUSE == 1 ~ "YES",    # Physical abuse by parents
      SCHLOVER == 1 ~ "YES",    # Repeated a school year before 18
      TRPOLICE == 5 & DRKDRUG == 5 & PHYABUSE == 5 & 
        SCHLOVER == 5 ~ "NO", # No police trouble, no family drug problems, no physical abuse, no school repetition
      TRUE ~ NA_character_
    )
  )

table(childhood$chil_Stress)

#chilhood health
childhood <- childhood %>%
  mutate(chil_health = case_when(
    RTHLTHCH %in% c(1, 2, 3) ~ "healthy",
    RTHLTHCH %in% c(4, 5) ~ "less_healthy", 
    TRUE ~ NA_character_
  ))
childhood$hhidpn <- as.numeric(childhood$hhidpn)
childhood <- dplyr::select(childhood, c("hhidpn","chil_health","chil_Stress"))
childhood <- dplyr::rename(childhood, ID = hhidpn)

df <- read.csv("hrs.csv")
df <- left_join(df,childhood, by = c("ID"))
table(df$chil_health)
write.csv(df,'hrs.csv')

df <- read.csv("hrs.csv")

####preparing the df####
##define sample size; n = 20911
sum(df$inw13)
df <- df[df$inw13 == 1,]

##exclude age >= 50; n = 20146
df <- df[which(df$age >= 50), ]

##delete na in social fraity index; n = 11969
df <- df %>%
  mutate(
    # social  participation
    no_participate_socwk = case_when(
      soc_activity == 0 ~ 1,
      soc_activity == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # rel participation
    no_participate_rel = case_when(
      rel_activity== 0 ~ 1,
      rel_activity == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    soc_activity = case_when(
      is.na(no_participate_socwk) & is.na(no_participate_rel) ~ NA_real_, 
      no_participate_socwk == 1 & no_participate_rel == 1 ~ 1,               
      (no_participate_socwk == 1 & is.na(no_participate_rel)) ~ 1,          
      (no_participate_rel == 1 & is.na(no_participate_socwk)) ~ 1,          
      TRUE ~ 0                                                               
    )
)

##delete na in social fraity index; n = 5412
vars <- c("financial_unsat", "people_living_with", "week_contact","soc_activity","loneliness")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)

df <- subset(df, !(is.na(financial_unsat)))
df <- subset(df, !(is.na(people_living_with)))
df <- subset(df, !(is.na(week_contact)))
df <- subset(df, !(is.na(soc_activity)))
df <- subset(df, !(is.na(loneliness)))

##delete na in education; n = 5390
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

df <- df %>%
  mutate(
    financial_unsat1 = case_when(
      financial_unsat %in% c(1, 2) ~ 1,
      financial_unsat %in% c(3, 4, 5) ~ 0,
      TRUE ~ NA_real_
    )
  )

df$financial_unsat1 <- labelled(
  df$financial_unsat1,
  labels = c("Satisfied" = 0, "Unsatisfied" = 1)
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
table(df$frailty_score, exclude = NULL)
table(df$frailty_status2, exclude = NULL)

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
age_groups <- cut(df$age, breaks = c(49, 64, 74, 84, Inf), 
                  labels = c("50-64", "65-74", "75-84", "≥85"))
age_groups <- factor(age_groups, levels = c("50-64", "65-74", "75-84", "≥85"), ordered = TRUE)
df$age_groups <- age_groups

#race and hispanic
#Generate a combined race: 0 non-hisp white, 1 non-hisp black, 2 hispanic, 3 other 
df$Race <- ifelse(df$race == 1 & 
                    df$hispanic == 0, 0,
                  ifelse(df$race == 2 & 
                           df$hispanic == 0, 1,
                         ifelse(df$hispanic == 1, 2, 3)))

df$Race <- ifelse(df$Race == 0, "Non-Hispanic White" , 
                  ifelse(df$Race == 1, "Non-Hispanic Black",
                         ifelse(df$Race == 2, "Hispanic","Other")))
table(df$Race)


variables <- c("age", "gender","urbanicity" ,"Race","height","chil_Stress","chil_health"
)
for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("Variable:", var, 
      "- Missing:", missing_count, 
      "- Missing %:", round(missing_percentage, 2), "%\n")
}
cat_vars <- c("gender","Race","urbanicity","chil_Stress","chil_health")


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

write.csv(df,"hrs_cleaning.csv")


####03 data analysis####

df <- read.csv("hrs_cleaning.csv")

cat_vars <- c("gender","Race","urbanicity", "education_quan","H_edu", 
              "financial_unsat","financial_unsat1","people_living_with", "week_contact", 
              "soc_activity", "loneliness",
              "frailty_status","frailty_status2","frailty_score",
              "chil_Stress","chil_health")

# 连续变量
cont_vars <- c("age","height")

all_vars <- c(cont_vars, cat_vars)
df[cat_vars] <- lapply(df[cat_vars], function(x) as.factor(x))

df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty", 
                                                          "pre-frailty", 
                                                          "frailty"))

df$frailty_status2 <- factor(df$frailty_status2, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))

ftable( df$education_quan,df$frailty_status)
ftable(df$education_quan,df$frailty_status2)
ftable(df$H_edu,df$frailty_status)
ftable(df$gender,df$frailty_status)

ftable(df$frailty_status, df$education_quan, df$gender)



####TABLE####
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
#demographic vari
fit_1 <- multinom(
  frailty_status ~  education_quan + gender+ age  + Race + urbanicity,
  data = df,
  Hess = TRUE 
)

fit_1_2 <- multinom(
  frailty_status2 ~  education_quan + gender+ age  + Race + urbanicity,
  data = df,
  Hess = TRUE 
)

fit_2 <- multinom(
  frailty_status ~ H_edu+ gender + age  +Race + urbanicity,
  data = df,
  Hess = TRUE
)

fit_2_2 <- multinom(
  frailty_status2 ~ H_edu+ gender + age  +Race + urbanicity,
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
  frailty_status ~  education_quan+ gender + age  +Race + urbanicity  + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE 
)
fit_3_2 <- multinom(
  frailty_status2 ~  education_quan+ gender + age  +Race + urbanicity  + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE 
)
fit_4 <- multinom(
  frailty_status ~ H_edu  + gender + age  +Race +  urbanicity + height + chil_Stress + chil_health,
  data = df,
  Hess = TRUE
)
fit_4_2 <- multinom(
  frailty_status2 ~ H_edu  + gender + age  +Race +  urbanicity + height + chil_Stress + chil_health,
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
  frailty_status ~ education_quan + age + Race + urbanicity,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m1 <- multinom(
  frailty_status ~ education_quan + age + Race + urbanicity,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w1)
summary(fit_m1)
tab_model(fit_w1)
tab_model(fit_m1)

fit_w2 <- multinom(
  frailty_status ~ education_quan + + age  +Race + urbanicity  +  height + chil_Stress + chil_health,
  data = subset(df, gender == "women"),
  Hess = TRUE
)

fit_m2<- multinom(
  frailty_status ~ education_quan + age  +Race + urbanicity   + height + chil_Stress + chil_health,
  data = subset(df, gender == "men"),
  Hess = TRUE
)

summary(fit_w2)
summary(fit_m2)
tab_model(fit_w2)
tab_model(fit_m2)

###weight
df$WEIGHT <- df$r13wtresp/sum(df[, 'r13wtresp'])*5390
fit_weight <- multinom(
  frailty_status ~ education_quan + gender + age + Race + urbanicity + height + chil_Stress + chil_health,
  data = df,
  weights = WEIGHT,   
  Hess = TRUE
)
summary(fit_weight)
tab_model(fit_weight)

###
df$WEIGHT <- df$r13wtcrnh/sum(df[, 'r13wtcrnh'])*5390
fit_weight2 <- multinom(
  frailty_status ~ education_quan + gender + age + Race + urbanicity + height + chil_Stress + chil_health,
  data = df,
  weights = WEIGHT,   
  Hess = TRUE
)

summary(fit_weight2)
tab_model(fit_weight2)



