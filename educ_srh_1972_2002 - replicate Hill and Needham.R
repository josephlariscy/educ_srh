# Replicating AJPH article by Hill and Burdette
# Gender-Specific Trends in Educational Attainment and Self-Rated Health

# This program produces the ordinal regression results (Table 1). A separate 
# R program produces Figure 1.


# Updated June 16, 2024


library(remotes)
library(haven)
library(sjmisc)
library(MASS)   # contains polr() function
library(ordinal)   # contains clm() function
library(AER)   # contains coeftest() function
library(stargazer)
library(ggplot2)

options(scipen = 10) # turn off scientific notation

install_github("kjhealy/gssr")
library(gssr)
data(gss_all)

gss7202 <- subset(gss_all,
                  year >= 1972 & year <= 2002 & 
                  year != 1978 & year != 1983 & year != 1986 &
                  age >= 25,
                  select = c(year, health, educ, sex, age, race))
attach(gss7202)

# Check variables
table(year)
frq(health)
frq(sex)
frq(race)
summary(educ)
summary(age)

table(year, health, useNA = "ifany")

# Delete cases with missing values
gss.listwise <- na.omit(gss7202)

attach(gss.listwise)

table(year)
frq(health)
frq(sex)   # 12,969 men and 16,658 women (match Table 1 sample sizes)
frq(race)
summary(educ)
summary(age)


# Reverse code health so that 1=poor and 4=excellent
health.rev <- 5 - health
health.ord <- as.ordered(health.rev)   # Must be a factor for polr()
table(health, health.ord, useNA = "ifany")

# Recode year so that values range 1 to 21
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
frq(year1)

# Mean center year1
year1cen <- year1 - mean(year1)
frq(year1cen)
summary(year1cen)

age25 <- age - 25

# Recode race as factor
race.f <- factor(race)
frq(race.f)


gss.recode <- cbind(gss.listwise,
                    health.rev, health.ord,
                    race.f, 
                    age, age25,
                    year1, year1cen)

gss.male   <- subset(gss.recode, sex == 1)
gss.female <- subset(gss.recode, sex == 2)


# Table 1: Coefficients for ordered logistic regression of subjective health

### Women ###

# Only intercept model (necessary for model chi-square)
nopredF <- polr(health.ord ~ 1,
                data = gss.female, Hess = T)
coeftest(nopredF)

# Model 1 #

model1 <- polr(health.ord ~ year1cen + age + race.f,
               data = gss.female, Hess = T)
coeftest(model1)
   # coeftest() is in package AER, provides p-values and asterisks
   # summary(model1) does not provide p-values for a polr() model
nobs(model1)

# Model 2 #

model2 <- polr(health.ord ~ year1cen + educ + age + race.f,
               data = gss.female, Hess = T)
coeftest(model2)
nobs(model2)

# Fit statistics for women

chi.model1   <- anova(nopredF, model1, test = "Chisq")
chi.model2   <- anova(nopredF, model2, test = "Chisq")
chi.change.f <- anova(model1,  model2, test = "Chisq")

print(chi.model1$`LR stat.`)
print(chi.model2$`LR stat.`)
print(chi.change.f$`LR stat.`)


### Men ###

# Only intercept model (necessary for chi-square)
nopredM <- polr(health.ord ~ 1,
                data = gss.male, Hess = T)
coeftest(nopredM)

# Model 3 #

model3 <- polr(health.ord ~ year1cen + I(year1cen^2) + I(year1cen^3) + age + race.f, 
               data = gss.male, Hess = T)
coeftest(model3)
nobs(model3)

# Model 4 #

model4 <- polr(health.ord ~ year1cen + I(year1cen^2) + I(year1cen^3) + educ + age + race.f,
               data = gss.male, Hess = T)
coeftest(model4)
nobs(model4)

# Fit statistics for men

chi.model3   <- anova(nopredM, model3, test = "Chisq")
chi.model4   <- anova(nopredM, model4, test = "Chisq")
chi.change.m <- anova(model3,  model4, test = "Chisq")

print(chi.model3$`LR stat.`)
print(chi.model4$`LR stat.`)
print(chi.change.m$`LR stat.`)


# Table in text
stargazer(model1, model2, model3, model4, 
          type = "text", 
          dep.var.labels = c("Self-Rated Health"),
          column.labels = c("Women", "Men"), column.separate = c(2, 2),
          order = c("year1cen", "I(year1cen^2)", "I(year1cen^3)", "educ", "age",
                    "race.f"),
          covariate.labels = c("Survey year", "Survey year squared", 
                               "Survey year cubed", "Education", "Age", "Black",
                               "Other race"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          report = "vc*",
          no.space = TRUE,
          notes.align = "l")

# Table in HTML
# To view table, go to Files of bottom right window, select models.htm, select
# "Open in Editor", select Preview
stargazer(model1, model2, model3, model4, 
          type = "html", 
          dep.var.labels = c("Self-Rated Health"),
          column.labels = c("Women", "Men"), column.separate = c(2, 2),
          order = c("year1cen", "I(year1cen^2)", "I(year1cen^3)", "educ", "age",
                    "race.f"),
          covariate.labels = c("Survey year", "Survey year<sup>2</sup>", 
                               "Survey year<sup>3</sup>", "Education", "Age", "Black",
                               "Other race"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          report = "vc*",
          no.space = TRUE,
          notes.align = "l",
          out = "models.htm")

# report option "vc*": variable names, coefficients, significance asterisks
# Source: https://stackoverflow.com/questions/74415416/how-to-show-t-stat-and-p-value-with-coefficient-with-stargazer
# a character string containing only elements of "v", "c", "s","t", "p", "" that
# determines whether, and in which order, variable names ("v"), coefficients 
# ("c"), standard errors/confidence intervals ("s"), test statistics ("t") and 
# p-values ("p") should be reported in regression tables. If one of the 
# aforementioned letters is followed by an asterisk (""), significance stars 
# will be reported next to the corresponding statistic.



# Figure 1. Mean educational level and self-rated health status, by year, for women
# Figure 2. Mean educational level and self-rated health status, by year, for men


# labeller function to have true minus sign for negative numbers
label_true_minus <- function(x){
  ifelse(sign(x) == -1, paste0("\u2212", abs(x)), x)
}


# Figure 1. Women

# Calculate mean years of education for each year
F.mean.educ <- aggregate(gss.female$educ, list(year), FUN = mean)
F.mean.educ
F.Z.educ <- scale(F.mean.educ$x, center = TRUE, scale = TRUE)
F.Z.educ

Fig1.educ <- cbind(F.mean.educ, F.Z.educ)

# Plot of education trend
ggplot(data = Fig1.educ, aes(x = Group.1, y = F.Z.educ)) +
  geom_line() +
  geom_point(shape = 18, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2002, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Survey Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = 2.3, x = 2000, label = "Education", size = 4)


# Calculate mean SRH for each year
F.mean.srh <- aggregate(gss.female$health.rev, list(year), FUN = mean)
F.mean.srh
F.Z.srh <- scale(F.mean.srh$x, center = TRUE, scale = TRUE)
F.Z.srh

Fig1.srh <- cbind(F.mean.srh, F.Z.srh)

# Plot of self-rated health trend
ggplot(data = Fig1.srh, aes(x = Group.1, y = F.Z.srh)) +
  geom_line() +
  geom_point(shape = 15, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2002, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Survey Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = -0.5, x = 1999, label = "Self-Rated Health", size = 4)


F.educ.srh <- merge(Fig1.educ, Fig1.srh, by = "Group.1")
F.educ.srh <- as.data.frame(F.educ.srh)

# Plot of trends in education and self-rated health among men
ggplot(data = F.educ.srh) +
  geom_line(aes(y = F.Z.educ, x = Group.1)) +
  geom_line(aes(y = F.Z.srh, x = Group.1)) +
  geom_point(aes(y = F.Z.educ, x = Group.1), shape = 18, size = 3) +
  geom_point(aes(y = F.Z.srh,  x = Group.1), shape = 15, size = 3) +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2002, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Survey Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = 2.3, x = 2000, label = "Education", size = 4) +
  annotate("text", y = -0.5, x = 1999, label = "Self-Rated Health", size = 4)



# Figure 2. Men

attach(gss.male)

# Calculate mean years of education for each year
M.mean.educ <- aggregate(gss.male$educ, list(year), FUN = mean)
M.mean.educ
M.Z.educ <- scale(M.mean.educ$x, center = TRUE, scale = TRUE)
M.Z.educ

Fig2.M.educ <- cbind(M.mean.educ, M.Z.educ)

# Plot of education trend for men
ggplot(data = Fig2.M.educ, aes(x = Group.1, y = M.Z.educ)) +
  geom_line() +
  geom_point(shape = 18, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2002, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Survey Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = 2.3, x = 2000, label = "Education", size = 4)


# Calculate mean SRH for each year
M.mean.srh <- aggregate(gss.male$health.rev, list(year), FUN = mean)
M.mean.srh
M.Z.srh <- scale(M.mean.srh$x, center = TRUE, scale = TRUE)
M.Z.srh

Fig2.M.srh <- cbind(M.mean.srh, M.Z.srh)

# Plot of self-rated health trend among men
ggplot(data = Fig2.M.srh, aes(x = Group.1, y = M.Z.srh)) +
  geom_line() +
  geom_point(shape = 15, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2002, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Survey Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = -1, x = 1997, label = "Self-Rated Health", size = 4)


M.educ.srh <- merge(Fig2.M.educ, Fig2.M.srh, by = "Group.1")
M.educ.srh <- as.data.frame(M.educ.srh)

# Plot of trends in education and self-rated health among men
ggplot(data = M.educ.srh) +
  geom_line(aes(y = M.Z.educ, x = Group.1)) +
  geom_line(aes(y = M.Z.srh, x = Group.1)) +
  geom_point(aes(y = M.Z.educ, x = Group.1), shape = 18, size = 3) +
  geom_point(aes(y = M.Z.srh,  x = Group.1), shape = 15, size = 3) +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2002, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Survey Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = 2.3, x = 2000, label = "Education", size = 4) +
  annotate("text", y = -1, x = 1997, label = "Self-Rated Health", size = 4)
