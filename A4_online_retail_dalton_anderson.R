

library(readxl)
library(dplyr)
setwd("C:/Users/abhatt/Desktop/SDM/Data")
df <- read_excel("OnlineRetailPromotions.xlsx", sheet='Data')
str(df)
View(df)

#' Feature engineering/Data Preprocessing
#' (this is SUPER-IMPORTANT, but something that most of you will never do)


View(df)
  
tempdf=df %>%
  mutate(
    level_of_spending = case_when(
      historysegment == "1) $0 - $100" ~ "low",
      historysegment == "2) $100 - $200" ~ "low",
      historysegment == "3) $200 - $350" ~ "low",
      historysegment == "4) $350 - $500" ~ "mid",
      historysegment == "5) $500 - $750" ~ "mid",
      historysegment == "6) $750 - $1,000" ~ "mid",
      historysegment == "7) $1,000 +" ~ "high",
      TRUE ~ "other"
    )
  )

df = tempdf

tempdf=df %>% 
  mutate(email_type = recode(campaign, 
                          "Womens E-Mail" = "targeted",
                          "Mens E-Mail" = "targeted",
                          "No E-Mail" = "non-targeted"))
df = tempdf

tempdf=df %>% 
  mutate(gender = recode(mens, 
                             "1" = "male",
                             "0" = "female"))
df = tempdf


as.factor(df$gender)
as.factor(df$email_type)
as.factor(df$level_of_spending)
as.factor(df$channel)
as.factor(df$zipcode)





#which(! complete.cases(df)) 
#colSums(is.na(df))
# We have 311 NA values in spa; hence, it's better to avoid this variable
#df <- df[, -c(1:3, 5:7, 10, 13:15, 17, 20, 22:24)]
#str(df)

#' Data visualizations

hist(df$spend)
#looks like I need to use a glm model
hist(log(df$spend))




#' High correlations (>0.8) between listprice, sqft, and lotsqft; between 
#' pricesold and listprice (1.00); and between baths, pricesold, listprice, and 
#' sqft. Since sqft is a core predictor of pricesold, we must drop listprice and 
#' lotsqft from our analysis to avoid multicollinearity. We keep baths though 
#' it has a 0.85 correlation with sqft because it is an important variable.

#' Regression models

mbase <- lm(spend~1, data=df)
summary(mbase)

m1 <- lm(spend ~ recency + level_of_spending, data=df)
summary(m1)

m2 <- lm(spend ~ recency + level_of_spending + email_type, data=df)
summary(m2)

poisson1 <- glm(spend ~ 1, family=poisson (link=log), data=df)   # Intercept only model
summary(poisson1)

poisson2 <- glm(spend ~ recency + level_of_spending + email_type
                , family=poisson (link=log), data=df) 
summary(poisson2)

poisson3 <- glm(spend ~ recency + level_of_spending + email_type + newcustomer+zipcode
                , family=poisson (link=log), data=df)   # Intercept only model
summary(poisson3)

library(stargazer)
stargazer(m2, poisson2, poisson3, type='text', single.row=TRUE)



df_male <- df %>% filter(gender == "male")

poissonmale <- glm(spend ~ recency + level_of_spending + email_type + newcustomer+zipcode
                , family=poisson (link=log), data=df_male)   # Intercept only model
summary(poissonmale)

df_female <- df %>% filter(gender == "female")

poissonfemale <- glm(spend ~ recency + level_of_spending + email_type + newcustomer+zipcode
                   , family=poisson (link=log), data=df_female)   # Intercept only model
summary(poissonfemale)

stargazer(poissonmale, poissonfemale, type='text', single.row=TRUE)
#male results in 46 extra dollors in spending per targeted email.
#female results in 50 extra dollors in spending per targeted email

df_men = df
 df %>% select(df$campaign, df$recency, df$level_of_spending, df$channel)

#should we target new customers?
 
 poisson3 <- glm(spend ~ recency + level_of_spending + email_type + newcustomer+zipcode
                 , family=poisson (link=log), data=df)   # Intercept only model
 summary(poisson3)
 
 #new customers spend 50 dollars less than recurring ones

#what is better for this business Web or Phone ads? 
 df_channel <- df %>% filter(channel =="Web" | channel =="Phone")
 
 df_channel <- glm(spend ~ recency + level_of_spending + email_type + newcustomer+zipcode+channel
                      , family=poisson (link=log), data=df_female)   # Intercept only model
 summary(df_channel)
 
 #people on the web spend 15 more dollars than on the phone.
 

 
 
 