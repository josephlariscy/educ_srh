# Gender-Specific Trends in Educational Attainment and Self-Rated Health, 1972-2018
# This R program updates the AJPH article by Hill and Needham, through 2018

# This program produces the ordinal regression results (Table 1). A separate
# R program produces Figure 1.
# Results are weighted with wtssall.
# Status: This paper received an R&R at the journal Biodemography and Social Biology.

# Author: Joseph Lariscy
# Updated: July 17, 2024


library(remotes)
library(haven)
library(sjmisc)
library(MASS)   # polr() function
library(AER)   # coeftest() function
library(stargazer)  
library(ggplot2)
library(ggeffects)  # ggpredict() function


options(scipen = 10) # turn off scientific notation

install_github("kjhealy/gssr")
library(gssr)
data(gss_all)

gss7218 <- subset(gss_all,
                  year >= 1972 & year <= 2018 & 
                  year != 1978 & year != 1983 & year != 1986 &
                  age >= 25,
                  select = c(year, health, educ, sex, age, race, wtssall, 
                             sample, marital, wrkstat, region))
attach(gss7218)

# Check variables - SHOULD I ADD MARITAL STATUS AND WORK STATUS?
table(year, useNA = "ifany")
frq(health)
frq(sex)
frq(race)
frq(marital)
frq(wrkstat)
summary(educ)
summary(age)
summary(wtssall)
  # The weight variable wtss has 34,687 missing values.
table(year, health, useNA = "ifany")

# Remove black oversample in 1982 and 1987
  # source: https://gss.norc.org/Lists/gssFAQs/DispForm.aspx?ID=11
  # The 1982 and 1987 GSSs included oversamples of black respondents. To adjust 
  # statistical results for this oversampling, one may either exclude cases in 
  # the black oversamples (codes 4, 5, and 7 on variable SAMPLE) or weight 
  # statistical results using weights in variable OVERSAMP.
  # gssr doesn't have the oversamp variable.
frq(sample)
gss7218 <- subset(gss7218,
                  sample != 4 & sample != 5 & sample != 7,
                  select = c(year, health, educ, sex, age, race, wtssall, 
                             marital, wrkstat, region))
# Removed 89 cases with value 4, 216 cases with value 5, and 298 cases with value 7

# Delete cases with missing values
gss.listwise <- na.omit(gss7218)

attach(gss.listwise)

# Variables after removing missing cases
table(year)
frq(health)
frq(sex)   # These will be the analytic sample sizes of women and men
frq(race)
summary(educ)
summary(age)
summary(wtssall)


# Reverse code health so that 1=poor and 4=excellent
health.rev <- 5 - health
health.ord <- as.ordered(health.rev)   # Must be a factor for polr()
frq(health.ord)
table(health, health.ord, useNA = "ifany")


# Recode year so that values range 1 to 29
table(year)
year1 <- NA
year1[year==1972] = 1
year1[year==1973] = 2
year1[year==1974] = 3
year1[year==1975] = 4
year1[year==1976] = 5
year1[year==1977] = 6
year1[year==1980] = 7
year1[year==1982] = 8
year1[year==1984] = 9
year1[year==1985] = 10
year1[year==1987] = 11
year1[year==1988] = 12
year1[year==1989] = 13
year1[year==1990] = 14
year1[year==1991] = 15
year1[year==1993] = 16
year1[year==1994] = 17
year1[year==1996] = 18
year1[year==1998] = 19
year1[year==2000] = 20
year1[year==2002] = 21
year1[year==2004] = 22
year1[year==2006] = 23
year1[year==2008] = 24
year1[year==2010] = 25
year1[year==2012] = 26
year1[year==2014] = 27
year1[year==2016] = 28
year1[year==2018] = 29
frq(year1)

# Mean center year1
year1cen <- year1 - mean(year1)
frq(year1cen)
summary(year1cen)

## Recoding covariates

# Age
age25 <- age - 25
summary(age25)

# Work status - collapse eight categories into two
employed <- wrkstat
employed[wrkstat==1 | wrkstat==2 | wrkstat==3] = 1
employed[wrkstat==4 | wrkstat==5 | wrkstat==6 | wrkstat==7 | wrkstat==8] = 0
table(employed, useNA = "ifany")

# Marital status - collapse five categories into three
marital3 <- marital
marital3[marital == 1] = 1  # Married
marital3[marital == 2 | marital == 3 | marital ==4] = 2  # Formerly married
marital3[marital ==5] = 3  # Never married
table(marital3, useNA = "ifany")

# Region - collapse the nine divisions into four regions
region4 <- region
region4[region == 1 | region == 2] = 1  # Northeast
region4[region == 3 | region == 4] = 2  # Midwest
region4[region == 5 | region == 6 | region == 7] = 3  # South
region4[region == 8 | region == 9] = 4  # West
table(region4, useNA = "ifany")

# Change race, employment, marital status, and region to factor variables
race.f <- factor(race)
employed.f <- factor(employed)
marital3.f <- factor(marital3)
region4.f <- factor(region4)

# Recode continuous education as educational categories for supplemental analysis
frq(educ)
educ_cat <- educ
educ_cat[educ >=  0 & educ <= 11] = 1
educ_cat[educ == 12]              = 2
educ_cat[educ >= 13 & educ <= 15] = 3
educ_cat[educ == 16]              = 4
educ_cat[educ >= 17 & educ <= 20] = 5
frq(educ_cat)
table(educ, educ_cat, useNA = "ifany")
educ_cat_f <- as.factor(educ_cat)

# Bind new variables to listwise-deleted dataset
gss.recode <- cbind(gss.listwise,
                    health.rev, health.ord,
                    educ_cat_f, race.f, 
                    age, age25,
                    year1, year1cen, year1censq,
                    employed.f, marital3.f, region4.f)

gss.male   <- subset(gss.recode, sex == 1)
gss.female <- subset(gss.recode, sex == 2)


# Table 1: Coefficients for ordered logistic regression of self-rated health, 1972-2018

### Women ###

nrow(gss.female)

# Only intercept model (necessary for model chi-square)
nopredF <- polr(health.ord ~ 1, data = gss.female, Hess = T, weights = wtssall)
nopredF$zeta
ilogit(nopredF$zeta)
   # ilogit() gives inverse logit. It is in package faraway.
   # gives cumulative probability of being =< in health group
   # source: https://www.bookdown.org/rwnahhas/RMPH/blr-ordinal.html
frq(health.ord)

# Model 1

model1 <- polr(health.ord ~ year1cen + I(year1cen^2) + age + race.f,
               data = gss.female, Hess = T, weights = wtssall)
  # A year1cen^3 term was not significant
coeftest(model1)
  # coeftest() is in package AER, provides p-values and asterisks
  # summary(model1) does not provide p-values for a polr() model
#nobs(model1)

# Model 2

model2 <- polr(health.ord ~ year1cen + I(year1cen^2) + educ + age + race.f,
               data = gss.female, Hess = T, weights = wtssall)
coeftest(model2)
#nobs(model2)

# Model 3

model3 <- polr(health.ord ~ year1cen + I(year1cen^2) + educ + age + race.f + 
               employed.f + marital3.f + region4.f,
               data = gss.female, Hess = T, weights = wtssall)
coeftest(model3)
#nobs(model3)

# Fit statistics for women

chi.model1 <- anova(nopredF, model1, test = "Chisq")
chi.model2 <- anova(nopredF, model2, test = "Chisq")
chi.model3 <- anova(nopredF, model3, test = "Chisq")
chi.change.1to2.f <- anova(model1,model2, test = "Chisq")
chi.change.2to3.f <- anova(model2,model3, test = "Chisq")

chi.model1[2, 6:7]  # Model 1 chi^2 and p-value
chi.model2[2, 6:7]  # Model 2 chi^2 and p-value
chi.model3[2, 6:7]  # Model 3 chi^2 and p-value
chi.change.1to2.f[2, 6:7]  # Model 1 to 2 chi^2 and p-value
chi.change.2to3.f[2, 6:7]  # Model 2 to 3 chi^2 and p-value


### Men ###

nrow(gss.male)

# Only intercept model (necessary for chi-square)
nopredM <- polr(health.ord ~ 1, data = gss.male, Hess = T, weights = wtssall)

# Model 4

model4 <- polr(health.ord ~ year1cen + I(year1cen^2) + age + race.f, 
               data = gss.male, Hess = T, weights = wtssall)
coeftest(model4)
# nobs(model4)

# Model 5

model5 <- polr(health.ord ~ year1cen + I(year1cen^2) + educ + age + race.f,
               data = gss.male, Hess = T, weights = wtssall)
coeftest(model5)
# nobs(model5)

# Model 6

model6 <- polr(health.ord ~ year1cen + I(year1cen^2) + educ + age + race.f + 
               employed.f + marital3.f + region4.f,
               data = gss.male, Hess = T, weights = wtssall)
coeftest(model6)
#nobs(model6)

# Fit statistics for men

chi.model4   <- anova(nopredM, model4, test = "Chisq")
chi.model5   <- anova(nopredM, model5, test = "Chisq")
chi.model6   <- anova(nopredM, model6, test = "Chisq")
chi.change.4to5.m <- anova(model4, model5, test = "Chisq")
chi.change.5to6.m <- anova(model5, model6, test = "Chisq")

chi.model4[2, 6:7]
chi.model5[2, 6:7]
chi.model6[2, 6:7]
chi.change.4to5.m[2, 6:7]
chi.change.5to6.m[2, 6:7]


stargazer(model1, model2, model3, model4, model5, model6,
          type = "text", 
          dep.var.labels = c("Self-Rated Health"),
          column.labels = c("Women", "Men"), column.separate = c(3, 3),
          order = c("year1cen", "I(year1cen^2)", "educ",
                    "age", "race.f"),
          covariate.labels = c("Survey year", "Survey year squared", 
                               "Education", "Age", "Black", "Other race",
                               "Employed", 
                               "Previously married", "Never married",
                               "Midwest", "South", "West"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          report = "vc*",
          no.space = TRUE,
          notes.align = "l",
          omit.stat = "n")
  # I omit the number of Observations (n) because they are the weighted number.
  # The unweighted sample size is 23,753 women and 18,652 men.

# report option:
  # "vc*" reports variable names, coefficients, and significance stars and
  # removes standard errors



## Supplemental Analyses:

#What if education was categorical rather than continuous?

gss.female_educ_cat <- cbind (gss.female, educ_cat_f[sex==2])

model3_educ_cat <- polr(health.ord ~ year1cen + I(year1cen^2) + educ_cat_f + 
                        age + race.f + employed.f + marital3.f + region4.f,
                        data = gss.female, Hess = T, weights = wtssall)
coeftest(model3_educ_cat)


gss.male_educ_cat <- cbind (gss.male, educ_cat_f[sex==1])

model6_educ_cat <- polr(health.ord ~ year1cen + I(year1cen^2) + educ_cat_f +  
                        age + race.f + employed.f + marital3.f + region4.f,
                        data = gss.male, Hess = T, weights = wtssall)
coeftest(model6_educ_cat)



# Plots with ggpredict

# Women: Models 1 and 2
#ggpredict(model1, terms = "year1cen[all]") |> plot()
#ggpredict(model2, terms = "year1cen[all]") |> plot()

ggp1 <- ggpredict(model1, terms = "year1cen[all]")
ggp2 <- ggpredict(model2, terms = "year1cen[all]")

# Change groups from 1 and 2 to descriptive labels
ggp1$group <- "Model 1: without educ control"
ggp2$group = "Model 2: with educ control"

# Add a column of consecutive numbers
# Otherwise, the x-axis values in ggplot will range from -15 to 14
ggp1$year <- rep(1:29, each = 4)
ggp2$year <- rep(1:29, each = 4)

ggp_female <- rbind(ggp1, ggp2)

# SRH category labels
srh_labels <- list('1' = "Poor health", '2' = "Fair health",
                   '3' = "Good health", '4' = "Excellent health")

health_labeller <- function(variable, value){
  return(srh_labels[value])
}

# ggplot plots models 1 and 2 for each of the four health categories
ggplot(ggp_female, aes(year, predicted, group = interaction(response.level, group))) + 
  geom_point() + 
  stat_smooth(aes(col=group)) +
  facet_wrap(response.level ~ ., labeller = health_labeller) +
  ylab("Predicted probabilities") +
  xlab("Survey year") +
  guides(color = guide_legend(title = "Women"))

ggsave("C:/Users/jlariscy/educ_srh/analysis/memo_figure_women.png")


# Men: Models 4 and 5
#ggpredict(model4, terms = "year1cen[all]") |> plot()
#ggpredict(model5, terms = "year1cen[all]") |> plot()

ggp4 <- ggpredict(model4, terms = "year1cen[all]")
ggp5 <- ggpredict(model5, terms = "year1cen[all]")

# Change groups from 4 and 5 to descriptive labels
ggp4$group <- "Model 4: without educ control"
ggp5$group = "Model 5: with educ control"

# Add a column of consecutive numbers
# Otherwise, the x-axis values in ggplot will range from -15 to 14
ggp4$year <- rep(1:29, each = 4)
ggp5$year <- rep(1:29, each = 4)

ggp_male <- rbind(ggp4, ggp5)


# ggplot plots models 4 and 5 for each of the four health categories
ggplot(ggp_male, aes(year, predicted, group = interaction(response.level, group))) + 
  geom_point() + 
  stat_smooth(aes(col = group)) +
  facet_wrap(response.level ~ ., labeller = health_labeller) +
  ylab("Predicted probabilities") +
  xlab("Survey year") +
  guides(color = guide_legend(title = "Men"))

ggsave("C:/Users/jlariscy/educ_srh/analysis/memo_figure_men.png")



# Scrap

# Change names of ggp2 variables before column binding the datasets
ggp2$x2 <- ggp2$x
ggp2$predicted2 <- ggp2$predicted
ggp2$std.error2 <- ggp2$std.error
ggp2$conf.low2 <- ggp2$conf.low
ggp2$conf.high2 <- ggp2$conf.high
ggp2$response.level2 <- ggp2$response.level
ggp2$group2 <- ggp2$group

ggp2 <- subset(ggp2, select = c(x2, predicted2, std.error2, conf.low2, conf.high2, response.level2, group2))
ggp2$group2 = 2

ggp_female <- cbind(ggp1, ggp2)


ggplot(data = ggp_female, aes(y = predicted, x = x, group = response.level)) +
  geom_point() +
  stat_smooth(aes(col = response.level)) 



ggplot(data = ggp_female) +
  geom_point(aes(y = predicted,   x = x, group = response.level, col = response.level)) +
  geom_point(aes(y = predicted2, x = x, group = response.level, col = response.level2)) +
  stat_smooth(aes(y = predicted, x = x, col = response.level)) +
  stat_smooth(aes(y = predicted2, x = x, col = response.level2))    

# With facet_wrap
ggplot(data = ggp_female) +
  geom_point(aes(y = predicted,   x = x, col = response.level)) +
  geom_point(aes(y = predicted2,  x = x, col = response.level2)) +
  stat_smooth(aes(y = predicted,  x = x, col = response.level)) +
  stat_smooth(aes(y = predicted2, x = x, col = response.level2)) +
  facet_wrap(~response.level)


plot(ggpredict(model1, terms = "year1cen[all]"))
plot(ggpredict(model2, terms = "year1cen[all]"), add = TRUE)


ggpredict(model1, terms = "year1cen[all]") |> plot()
ggpredict(model2, terms = "year1cen[all]") |> plot()




e <- ggpredict(model1, terms = "year1cen[all]")
f <- ggpredict(model2, terms = "year1cen[all]")

plot(rbind(e,f))
g


df1=model1
df2=model2

df1$model <- "A"
df2$model <- "B"

dfc <- rbind(df1, df2)

ggplot(dfc, aes(year1cen, health.ord, group = model)) + 
  geom_point() + 
  stat_smooth(aes(col=model))

df1$model <- model1
df2$model <- model2

dfc <- rbind(df1, df2)

ggplot(dfc, aes(year1cen, health.ord, group = model)) + 
  geom_point() + 
  stat_smooth(aes(col=model))
