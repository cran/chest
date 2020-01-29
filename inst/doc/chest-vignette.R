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

## ----chest_speedglm_1, fig.height=5.5, fig.width = 6--------------------------
results <- chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = c("Age", "Sex", "Married", "Smoke", "Cancer", "CVD","Education", "Income"),
  data = diab_df)
results$data

## ----chest_speedglm_plot, fig.height=5, fig.width = 6-------------------------
results <- chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = c("Age", "Sex", "Married", "Smoke", "Cancer", "CVD","Education", "Income"),
  data = diab_df)
chest_plot(results)

## ----chest_speedglm_plot_2, fig.height=4, fig.width = 6-----------------------
results <- chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = c("Age", "Sex", "Married", "Smoke", "Cancer", "CVD","Education", "Income"),
  data = diab_df)
p <- chest_plot(results, nudge_y = 0, value_position = 5)  
p + scale_x_continuous(breaks = c(0.5, 1:4), limits = c(0.5, 8))

## ----chest_speedglm_plot_3, fig.height=4, fig.width = 6-----------------------
results <- chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = c("Age", "Sex", "Married", "Smoke", "Cancer", "CVD","Education", "Income"),
  data = diab_df)
chest_plot(results, no_values = TRUE)  

## ----chest_speedglm_forest, fig.height=4, fig.width = 6-----------------------
results <- chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = c("Age", "Sex", "Married", "Smoke", "Cancer", "CVD","Education", "Income"),
  data = diab_df)
  chest_forest(results)  

## ---- speedglm_2, fig.height=6.4, fig.width = 5.4-----------------------------
vlist <- c("Age", "Sex", "Married", "Smoke", "Cancer", "CVD","Education", "Income")
results <- chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = vlist, data = diab_df)
  results$data 
  chest_plot(results)

## ---- speedglm_4, fig.height=6, fig.width = 6.4-------------------------------
diab_df$Age_Sex <- diab_df$Age*diab_df$Sex 
diab_df$Age2 = diab_df$Age^2
vlist_1<-c("Age", "Sex", "Age2", "Age_Sex", "Married", "Cancer", "CVD", "Education", "Income")
results <- chest_speedglm(crude = "Endpoint ~ Diabetes", xlist = vlist_1, data = diab_df)

chest_plot(results)

chest_forest(results)

## ---- chest_glm, eval=FALSE---------------------------------------------------
#  vlist <- c("Age", "Sex", "Married", "Smoke", "Education")
#  results <- chest_glm(crude = "Endpoint ~ Diabetes", xlist = vlist,
#            data = diab_df, indicate = TRUE)

## ---- coxhp, fig.height=5, fig.width = 6.4------------------------------------

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

