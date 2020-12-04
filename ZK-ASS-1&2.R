########## ASSIGNMENT-1 ##########

install.packages('tidyverse')
library('tidyverse')
#
install.packages('broom')
library('broom')
#
install.packages('psych')
library('psych')

install.packages('car')
library(car)

install.packages('lmtest')
library(lmtest) 

install.packages('sandwich')
library(sandwich) 

install.packages('boot')
library(boot) 

install.packages ('lmboot')
library(lmboot) 

install.packages('lm.beta')
library(lm.beta)

install.packages('gridExtra')
library(gridExtra) 


data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

View(data_sample_1)

data_sample_1 %>%
  summary()

data_sample_1 %>%
  ggplot() +
  aes(x= pain) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= STAI_trait) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= pain_cat) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= cortisol_serum) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= cortisol_saliva) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= mindfulness) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= weight) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= IQ) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= household_income) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x= age) +
  geom_histogram()

data_sample_1 = data_sample_1 [-c(93), ]


mod_1 <- lm(pain ~ age + sex, data = data_sample_1)

plot_1 = data_sample_1 %>% ggplot() + aes(x = age, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_2 = data_sample_1 %>% ggplot() + aes(x = sex, y = pain) +
  geom_point() + geom_smooth(method = "lm")

grid.arrange(plot_1, plot_2, nrow = 1)

mod_1 %>%
  summary()

apa.reg.table(mod_1)

apa.reg.table(mod_1, filename = "Table1_APA.doc", table.number = 1)

mod_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum +cortisol_saliva , data = data_sample_1)

apa.reg.table(mod_2)

apa.reg.table(mod_2, filename = "Table2_APA.doc", table.number = 2)


plot_3 = data_sample_1 %>% ggplot() + aes(x = STAI_trait, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_4 = data_sample_1 %>% ggplot() + aes(x = pain_cat, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_5 = data_sample_1 %>% ggplot() + aes(x = mindfulness, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_6 = data_sample_1 %>% ggplot() + aes(x = cortisol_serum, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_7 = data_sample_1 %>% ggplot() + aes(x = cortisol_saliva, y = pain) +
  geom_point() + geom_smooth(method = "lm")

grid.arrange(plot_1, nrow = 1)

grid.arrange(plot_2, nrow = 1)

grid.arrange(plot_3, nrow = 1)

grid.arrange(plot_4, nrow = 1)

grid.arrange(plot_5, nrow = 1)

grid.arrange(plot_6, nrow = 1)

grid.arrange(plot_7, nrow = 1)

mod_2 %>%
  summary()

AIC(mod_1)

AIC(mod_2)

confint(mod_1)

lm.beta(mod_1)


confint(mod_2)

lm.beta(mod_2)

mod_1 %>% ggplot() + aes(x = age, y = pain) + geom_point() +
  geom_smooth(method = "lm")

mod_1 %>% plot(which = 5)

mod_1 %>% plot(which = 4)

mod_2 %>% plot(which = 5)

mod_2 %>% plot(which = 4)

mod_1 %>% plot(which = 2)

mod_2 %>% plot(which = 2)

residuals_mod_1 = enframe(residuals(mod_1))

residuals_mod_1 %>% ggplot() + aes(x = value) + geom_histogram()

residuals_mod_2 = enframe(residuals(mod_2))

residuals_mod_2 %>% ggplot() + aes(x = value) + geom_histogram()


describe(residuals(mod_1))

describe(residuals(mod_2))

mod_1 %>% residualPlots()

mod_2 %>% residualPlots()

mod_1 %>% plot(which = 3)

mod_2 %>% plot(which = 3)

mod_1 %>% ncvTest()

mod_2 %>% ncvTest()

mod_1 %>% vif()

mod_2 %>% vif()

########## ASSIGNMENT-2 ##########


initial_model_backward <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income , data = data_sample_1)

apa.reg.table(initial_model_backward)

apa.reg.table(initial_model_backward, filename = "Table3_APA.doc", table.number = 3)


plot_8 = data_sample_1 %>% ggplot() + aes(x = STAI_trait, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_9 = data_sample_1 %>% ggplot() + aes(x = pain_cat, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_10 = data_sample_1 %>% ggplot() + aes(x = mindfulness, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_11 = data_sample_1 %>% ggplot() + aes(x = cortisol_serum, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_12 = data_sample_1 %>% ggplot() + aes(x = weight, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_13 = data_sample_1 %>% ggplot() + aes(x = IQ, y = pain) +
  geom_point() + geom_smooth(method = "lm")

plot_14 = data_sample_1 %>% ggplot() + aes(x = household_income, y = pain) +
  geom_point() + geom_smooth(method = "lm")

grid.arrange(plot_8, nrow = 1)

grid.arrange(plot_9, nrow = 1)

grid.arrange(plot_10, nrow = 1)

grid.arrange(plot_11, nrow = 1)

grid.arrange(plot_12, nrow = 1)

grid.arrange(plot_13, nrow = 1)

grid.arrange(plot_14, nrow = 1)


initial_model_backward %>%
  summary()

AIC(mod_1)

AIC(mod_2)

AIC(initial_model_backward)


confint(mod_1)

lm.beta(mod_1)


confint(mod_2)

lm.beta(mod_2)

confint(initial_model_backward)

lm.beta(initial_model_backward)

initial_model_backward %>% plot(which = 5)

initial_model_backward %>% plot(which = 4)

initial_model_backward %>% plot(which = 2)

residuals_initial_model_backward = enframe(residuals(mod_2))

residuals_initial_model_backward %>% ggplot() + aes(x = value) + geom_histogram()


describe(residuals(initial_model_backward))


initial_model_backward %>% residualPlots()

initial_model_backward %>% plot(which = 3)

initial_model_backward %>% ncvTest()

initial_model_backward %>% vif()


backward_model <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum , data = data_sample_1)

backward_model %>%
  summary()

residuals_backward_model = enframe(residuals(mod_2))

residuals_backward_model %>% ggplot() + aes(x = value) + geom_histogram()


describe(residuals(backward_model))


backward_model %>% residualPlots()

backward_model %>% plot(which = 3)

backward_model %>% ncvTest()

backward_model %>% vif()


theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum +cortisol_saliva , data = data_sample_1)

theory_based_model %>%
  summary()

theory_based_model_wsex <- lm(pain ~ age + STAI_trait + pain_cat + mindfulness + cortisol_serum +cortisol_saliva , data = data_sample_1)

theory_based_model_wsex %>%
  summary() 

AIC(backward_model)

AIC(theory_based_model)

AIC(theory_based_model_wsex)

anova(backward_model, theory_based_model)

anova(backward_model, theory_based_model_wsex)

data_sample_2 = read.csv('https://tinyurl.com/ha-dataset2')
view(data_sample_2)

summary(data_sample_2)

head(data_sample_2)

age = c(40, 31, 41, 44, 45, 33, 52, 29)
sex = c(female, male, male, male, female, male, female, female)
STAI_trait = c(50, 39, 40, 51, 46, 40, 39, 40)
pain_cat = c(37, 36, 34, 23, 28, 28, 26, 30)
mindfulness = c(1.00, 2.14, 2.26, 2.44, 2.57, 3.19, 1.98, 5.02)
cortisol_serum = c(5.68, 5.06, 5.48, 4.97, 4.14, 4.93, 4.59, 5.11)
cortisol_saliva = c(5.75, 5.09, 5.18, 6.30, 4.51, 5.34, 4.67, 5.76)

newdata_to_predict =as.data.frame(cbind(age, pain_cat, mindfulness, cortisol_serum))

predicted_pain =predict(backward_model, newdata = newdata_to_predict)

cbind(newdata_to_predict, predicted_pain)

newdata_to_predict_1 =as.data.frame(cbind(age, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva))

predicted_pain_1 =predict(theory_based_model_wsex, newdata = newdata_to_predict_1)

cbind(newdata_to_predict_1, predicted_pain_1)


RSS =sum((data_sample_2$pain- predict(backward_model))^2)
RSS

backward_model_mean <-lm(pain~1, data = data_sample_2)

TSS =sum((data_sample_2$pain- predict(backward_model_mean))^2)
TSS

R2 = 1-(RSS/TSS)
R2


RSS =sum((data_sample_2$pain- predict(theory_based_model_wsex))^2)
RSS

theory_based_model_wsex_mean <-lm(pain~1, data = data_sample_2)

TSS =sum((data_sample_2$pain- predict(theory_based_model_wsex_mean))^2)
TSS

R2 = 1-(RSS/TSS)
R2


install.packages("apaTables")

library(apaTables)

install.packages('MBESS')
library(MBESS)

apa.cor.table(data_sample_1)
apa.cor.table(data_sample_1, filename = "Tablecor_APA.doc", table.number = 1)

apa.reg.table(backward_model)

apa.reg.table(backward_model, filename = "Table4_APA.doc", table.number = 4)

apa.reg.table(theory_based_model_wsex)

apa.reg.table(theory_based_model_wsex, filename = "Table5_APA.doc", table.number = 5)


########## ASSIGNMENT-3 ##########


