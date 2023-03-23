## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("chest")

## ----setup--------------------------------------------------------------------
library(chest)
library(ggplot2)
names(diab_df)

## ---- chest_glm, eval=FALSE---------------------------------------------------
#  vlist <- c("Age", "Sex", "Married", "Smoke", "Education")
#  results <- chest_glm(crude = "Endpoint ~ Diabetes", xlist = vlist,
#            data = diab_df, indicate = TRUE)

## ---- coxhp, fig.height=5, fig.width = 6.4------------------------------------
vlist <- c("Age", "Sex", "Married", "Smoke", "Education")
results <- chest_cox(crude = "Surv(t0, t1, Endpoint) ~ Diabetes", 
                     xlist = vlist, data = diab_df)
chest_plot(results)

chest_forest(results)

## ---- clogit, fig.height=3.2, fig.width = 6.4---------------------------------
results <- chest_clogit(crude = "Endpoint ~ Diabetes + strata(mid)", 
             xlist = vlist, data = diab_df)

## ---- lm, fig.height=6, fig.width = 6.4---------------------------------------
vlist<-c("Age", "Sex", "Married", "Cancer", "CVD","Education", "Income")
results <- chest_lm(crude = "BMI ~ Diabetes", xlist = vlist, data = diab_df)
chest_plot(results)

