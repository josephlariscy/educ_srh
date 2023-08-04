# Replicating AJPH article by Hill and Burdette
# Gender-Specific Trends in Educational Attainment and Self-Rated Health

library(remotes)
library(haven)
library(sjmisc)
library(MASS)   # polr() function
library(AER)   # coeftest() function
library(stargazer)

options(scipen = 10) # turn off scientific notation

install_github("kjhealy/gssr")
library(gssr)
data(gss_all)

gss7218 <- subset(gss_all,
                  year >= 1972 & year <= 2018 & 
                  year != 1978 & year != 1983 & year != 1986 &
                  age >= 25,
                  select = c(year, health, educ, sex, age, race, wtss, wtssall))
attach(gss7218)

# Check variables
frq(year)
frq(health)
frq(sex)
frq(race)
summary(educ)
summary(age)

table(year, health)

# Delete cases with missing values
gss.listwise <- na.omit(gss7218)

attach(gss.listwise)

frq(year)
frq(health)
frq(sex)   # These will be the analytic sample sizes of women and men
frq(race)
summary(educ)
summary(age)


# Reverse code health so that 1=poor and 4=excellent
health.rev <- 5 - health
health.ord <- as.ordered(health.rev)   # Must be a factor for polr()
table(health, health.ord)

# Recode year so that values range 1 to 21
frq(year)
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
year1censq <- year1cen^2
year1cencub <- year1cen^3

age25 <- age - 25

# Recode race as factor
race.f <- factor(race)
frq(race.f)


gss.recode <- cbind(gss.listwise,
                    health.rev, health.ord,
                    race.f, 
                    age, age25,
                    year1, year1cen, year1censq, year1cencub)

gss.male   <- subset(gss.recode, sex == 1)
gss.female <- subset(gss.recode, sex == 2)


# Table 1: Coefficients for ordered logistic regression of subjective health

### Women ###

# Only intercept model (necessary for model chi-square)
nopredF <- polr(health.ord ~ 1, data = gss.female, Hess = T)

# Model 1

model1 <- polr(health.ord ~ year1cen + I(year1cen^2) + age + race.f,
               data = gss.female, Hess = T)
coeftest(model1)
   # coeftest() is in package AER, provides p-values and asterisks
   # summary(model1) does not provide p-values for a polr() model
nobs(model1)

# Model 2

model2 <- polr(health.ord ~ year1cen + I(year1cen^2) + educ + age + race.f,
               data = gss.female, Hess = T)
coeftest(model2)
nobs(model2)

# Fit statistics for women

chi.model1   <- anova(nopredF, model1, test = "Chisq")
chi.model2   <- anova(nopredF, model2, test = "Chisq")
chi.change.f <- anova(model1,  model2, test = "Chisq")

chi.model1[2, 6:7]   # Model 1 chi^2 and p-value
chi.model2[2, 6:7]   # Model 2 chi^2 and p-value
chi.change.f[2, 6:7]   # Model 1 to 2 chi^2 and p-value


### Men ###

# Only intercept model (necessary for chi-square)
nopredM <- polr(health.ord ~ 1, data = gss.male, Hess = T)

# Model 3

model3 <- polr(health.ord ~ year1cen + I(year1cen^2) + I(year1cen^3) + age + race.f, 
               data = gss.male, Hess = T)
coeftest(model3)
nobs(model3)

# Model 4

model4 <- polr(health.ord ~ year1cen + I(year1cen^2) + I(year1cen^3) + educ + age + race.f,
               data = gss.male, Hess = T)
coeftest(model4)
nobs(model4)

# Fit statistics for men

chi.model3   <- anova(nopredM, model3, test = "Chisq")
chi.model4   <- anova(nopredM, model4, test = "Chisq")
chi.change.m <- anova(model3,  model4, test = "Chisq")

chi.model3[2, 6:7]
#print(chi.model3$`LR stat.`)
#print(chi.model3$`Pr(Chi)`)
chi.model4[2, 6:7]
chi.change.m[2, 6:7]

stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Self-Rated Health"),
          column.labels = c("Women", "Men"), column.separate = c(2, 2),
          order = c("year1cen", "I(year1cen^2)", "I(year1cen^3)", "educ",
                    "age", "race.f"),
          covariate.labels = c("Survey year", "Survey year squared", 
                               "Survey year cubed", "Education", "Age", 
                               "Black", "Other race"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          report = "vc*",
          no.space = TRUE,
          notes.align = "l")

# "vc*" reports variable names, coefficients, and significance stars and
# removes standard errors