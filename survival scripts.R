library(survival)
library(dplyr)
library(survminer)
library(readxl)

# importing data
survival_data_all <- read_excel("survival data4.xlsx", sheet = "all"); attach(survival_data_all)

survival_data_control <- read_excel("survival data4.xlsx", sheet = "control"); attach(survival_data_control)
survival_data_ic18 <- read_excel("survival data4.xlsx", sheet = "ic18"); attach(survival_data_ic18)
survival_data_ic20 <- read_excel("survival data4.xlsx", sheet = "ic20"); attach(survival_data_ic20)
survival_data_ic655 <- read_excel("survival data4.xlsx", sheet = "ic655"); attach(survival_data_ic655)


# Survival function: 
survival_function_all <- survfit(Surv(Time, status)~Treatment, data=survival_data_all)
survival_function_control <- survfit(Surv(Time, status)~1, data=survival_data_control)
survival_function_ic18 <- survfit(Surv(Time, status)~1, data=survival_data_ic18)
survival_function_ic20 <- survfit(Surv(Time, status)~1, data=survival_data_ic20)
survival_function_ic655 <- survfit(Surv(Time, status)~1, data=survival_data_ic655)

# summarly survival all
summary(survival_function_all, times=seq(0, 10, 1))

# Kaplan-Meier Plots Plots
ggsurvplot(survival_function_all, data = survival_data_all, pval=TRUE)

ggsurvplot(survival_function_control)
ggsurvplot(survival_function_ic18)
ggsurvplot(survival_function_ic20)
ggsurvplot(survival_function_ic655)



# difference in Survival curves

survdiff(Surv(Time, status)~Treatment, data=survival_data_all, rho = 0)
survdiff(Surv(Time, status)~Treatment, data=survival_data_all, rho = 1)
survdiff(Surv(Time, status)~Treatment, data=survival_data_all, rho = 1/2)

# Cox proportional hazards model
Cox_regression <- coxph(Surv(Time, status)~Treatment, data=survival_data_all)
Cox_regression

ggforest(Cox_regression, data = survival_data_all)


