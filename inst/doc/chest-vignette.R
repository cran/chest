## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(chest)
names(diab_df)

## ----chest_speedglm_1, fig.height=3.2, fig.width = 6.4------------------------
chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = c("Age", "Sex", "Married", "Smoke", "Cancer", "CVD","Education", "Income"), 
  zero = 1, data = diab_df)

## ---- speedglm_2, fig.height=3.2, fig.width = 6.4-----------------------------
vlist <- c("Age", "Sex", "Married", "Smoke", "Cancer", "CVD","Education", "Income")
chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = vlist,   zero = 1, data = diab_df)

## ---- speedglm_3, fig.height=3.2, fig.width = 6.4-----------------------------
chest_speedglm(
  crude = "Endpoint ~ Diabetes", xlist = vlist, 
  data = diab_df, zero = c(0.98, 1.02),  na_omit = TRUE)

## ---- speedglm_4, fig.height=3.2, fig.width = 6.4-----------------------------
library(tidyverse)
diab_df <- diab_df %>%
mutate(Age_Sex = Age*Sex, Age2 = Age^2)
vlist_1<-c("Age", "Sex", "Age2", "Age_Sex", "Married", "Cancer", "CVD", "Education", "Income")
chest_speedglm(crude = "Endpoint ~ Diabetes", xlist = vlist_1, na_omit=TRUE, data = diab_df)

## ---- chest_glm, eval=FALSE---------------------------------------------------
#  vlist <- c("Age", "Sex", "Married", "Smoke", "Education")
#  chest_glm(crude = "Endpoint ~ Diabetes", xlist = vlist, data = diab_df, indicate = TRUE)

## ---- coxhp, fig.height=3.2, fig.width = 6.4----------------------------------

chest_cox(crude = "Surv(t0, t1, Endpoint) ~ Diabetes", xlist = vlist,
          na_omit = TRUE, data = diab_df, zero = 1)

## ---- clogit, fig.height=3.2, fig.width = 6.4---------------------------------
chest_clogit(crude = "Endpoint ~ Diabetes + strata(mid)", 
             xlist = vlist, data = diab_df, zero = 1)

