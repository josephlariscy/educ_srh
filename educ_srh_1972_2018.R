# Replicating AJPH article by Hill and Needham (2006)
# Gender-Specific Trends in Educational Attainment and Self-Rated Health

library(remotes)
library(haven)
library(sjmisc)
library(ggplot2)
library(ggpubr)   # Required for ggarrange
library(extrafont)

# Read-in GSS data
install_github("kjhealy/gssr")
library(gssr)
data(gss_all)

gss_1972_2018 <- subset(gss_all,
                  year >= 1972 & year <= 2018 &
                  year != 1978 & year != 1983 & year != 1986 &
                  age >= 25,
                  select = c(year, health, educ, sex, age, wtssall))
   # SRH wasn't measured in 1978, 1983, or 1986
   # Limit age to 25+ so respondents are old enough to complete schooling

attach(gss_1972_2018)

# Check variables
frq(year)
frq(health)
frq(sex)
summary(educ)
summary(age)

table(year, health, useNA = 'ifany')

# Delete cases with missing values
gss.listwise <- na.omit(gss_1972_2018)

attach(gss.listwise)

frq(year)
frq(health)
frq(sex)   # Sample sizes: 18,652 men and 23,753 women
summary(educ)
summary(age)

# Reverse code health so that 1=poor and 4=excellent
health.rev <- 5 - health
table(health, health.rev)


gss.recode <- cbind(gss.listwise, health.rev)
rm(health.rev)

gss.male   <- subset(gss.recode, sex == 1)
gss.female <- subset(gss.recode, sex == 2)


# Women

attach(gss.female)

# factor version of year, for series of dummy variables in regression
F.year.f <- factor(year)
F.regression <- cbind(gss.female, F.year.f)

F.age.mean.cen <- age - mean(age)
F.regression <- cbind(gss.female, F.year.f, F.age.mean.cen)

# Predict mean SRH for each year for women, age adjusted
F.model.srh <- lm(health.rev ~ F.year.f + F.age.mean.cen, 
                  data = F.regression, weight = wtssall)
summary(F.model.srh)

F.pred.srh <- data.frame(F.year.f = c("1972", "1973", "1974", "1975", "1976",
                                      "1977", "1980", "1982", "1984", "1985",
                                      "1987", "1988", "1989", "1990", "1991",
                                      "1993", "1994", "1996", "1998", "2000",
                                      "2002", "2004", "2006", "2008", "2010",
                                      "2012", "2014", "2016", "2018"),
                         F.age.mean.cen = c(mean(age)))

F.mean.srh <- predict(F.model.srh, newdata = F.pred.srh, terms = F.year.f)
F.mean.srh <- cbind(F.pred.srh, F.mean.srh)
F.mean.srh
plot(x = F.mean.srh$F.year.f, y = F.mean.srh$F.mean.srh, type="l")

# Convert mean SRH to Z-scores
F.Z.srh <- scale(F.mean.srh$F.mean.srh, center = TRUE, scale = TRUE)
F.Z.srh


# Predict mean education for each year for women, age adjusted
F.model.educ <- lm(educ ~ F.year.f + F.age.mean.cen, 
                   data = F.regression, weight = wtssall)
summary(F.model.educ)

F.pred.educ <- data.frame(F.year.f = c("1972", "1973", "1974", "1975", "1976",
                                       "1977", "1980", "1982", "1984", "1985",
                                       "1987", "1988", "1989", "1990", "1991",
                                       "1993", "1994", "1996", "1998", "2000",
                                       "2002", "2004", "2006", "2008", "2010",
                                       "2012", "2014", "2016", "2018"),
                          F.age.mean.cen = c(mean(age)))

F.mean.educ <- predict(F.model.educ, newdata = F.pred.educ, terms = F.year.f)
F.mean.educ <- cbind(F.pred.educ, F.mean.educ)
F.mean.educ
plot(x = F.mean.educ$F.year.f, y = F.mean.educ$F.mean.educ, type="l")

# Convert mean education to Z-scores
F.Z.educ <- scale(F.mean.educ$F.mean.educ, center = TRUE, scale = TRUE)
F.Z.educ


# Men

attach(gss.male)

# factor version of year, for series of dummy variables in regression
M.year.f <- factor(year)
M.regression <- cbind(gss.male, M.year.f)

M.age.mean.cen <- age - mean(age)
M.regression <- cbind(gss.male, M.year.f, M.age.mean.cen)

# Predict mean SRH for each year for women, age adjusted
M.model.srh <- lm(health.rev ~ M.year.f + M.age.mean.cen, 
                  data = M.regression, weight = wtssall)
summary(M.model.srh)

M.pred.srh <- data.frame(M.year.f = c("1972", "1973", "1974", "1975", "1976",
                                      "1977", "1980", "1982", "1984", "1985",
                                      "1987", "1988", "1989", "1990", "1991",
                                      "1993", "1994", "1996", "1998", "2000",
                                      "2002", "2004", "2006", "2008", "2010",
                                      "2012", "2014", "2016", "2018"),
                         M.age.mean.cen = c(mean(age)))

M.mean.srh <- predict(M.model.srh, newdata = M.pred.srh, terms = M.year.f)
M.mean.srh <- cbind(M.pred.srh, M.mean.srh)
M.mean.srh
plot(x = M.mean.srh$M.year.f, y = M.mean.srh$M.mean.srh, type = "l")

# Convert mean SRH to Z-scores
M.Z.srh <- scale(M.mean.srh$M.mean.srh, center = TRUE, scale = TRUE)
M.Z.srh


# Predict mean education for each year for men, age adjusted
M.model.educ <- lm(educ ~ M.year.f + M.age.mean.cen, 
                   data = M.regression, weight = wtssall)
summary(M.model.educ)

M.pred.educ <- data.frame(M.year.f = c("1972", "1973", "1974", "1975", "1976",
                                       "1977", "1980", "1982", "1984", "1985",
                                       "1987", "1988", "1989", "1990", "1991",
                                       "1993", "1994", "1996", "1998", "2000",
                                       "2002", "2004", "2006", "2008", "2010",
                                       "2012", "2014", "2016", "2018"),
                          M.age.mean.cen = c(mean(age)))

M.mean.educ <- predict(M.model.educ, newdata = M.pred.educ, terms = M.year.f)
M.mean.educ <- cbind(M.pred.educ, M.mean.educ)
M.mean.educ
plot(x = M.mean.educ$M.year.f, y = M.mean.educ$M.mean.educ, type = "l")

# Convert mean education to Z-scores
M.Z.educ <- scale(M.mean.educ$M.mean.educ, center = TRUE, scale = TRUE)
M.Z.educ


# Put variables together

year <- as.numeric(F.mean.educ$F.year.f)

fig.F.educ <- cbind(F.mean.educ, F.Z.educ, year)
fig.F.srh  <- cbind(F.mean.srh,  F.Z.srh,  year)

fig.M.educ <- cbind(M.mean.educ, M.Z.educ, year)
fig.M.srh  <- cbind(M.mean.srh,  M.Z.srh,  year)


# Function to have true minus sign for negative numbers on y-axis of plots
label_true_minus <- function(x){
  ifelse(sign(x) == -1, paste0("\u2212", abs(x)), x)
}


# Plots for women

# Plot of education trend among women
ggplot(data = fig.F.educ, aes(x = year, y = F.Z.educ)) +
  geom_line() +
  geom_point(shape = 18, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab('Standardized metric units') +
  xlab('Survey year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = 2.3, x = 2000, label = "Education", size = 4)


# Plot of self-rated health trend among women
ggplot(data = fig.F.srh, aes(x = year, y = F.Z.srh)) +
  geom_line() +
  geom_point(shape = 15, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab('Standardized metric units') +
  xlab('Survey year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = -0.5, x = 1999, label = "Self-Rated Health", size = 4)


# Plots for men

attach(gss.male)

# Plot of education trend among men
ggplot(data = fig.M.educ, aes(x = year, y = M.Z.educ)) +
  geom_line() +
  geom_point(shape = 18, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab('Standardized metric units') +
  xlab('Survey year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = 2.3, x = 2000, size = 4, label = "Education")


# Plot of self-rated health trend among men
ggplot(data = fig.M.srh, aes(x = year, y = M.Z.srh)) +
  geom_line() +
  geom_point(shape = 15, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab('Standardized metric units') +
  xlab('Survey year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = -1, x = 1997, size = 4, label = "Self-Rated Health")


# Figure 1: Plots of self-rated health and education by sex

F.educ.srh <- as.data.frame(merge(fig.F.educ, fig.F.srh, by = "year"))
M.educ.srh <- as.data.frame(merge(fig.M.educ, fig.M.srh, by = "year"))


windows(width = 10, height = 10)

# Women
p1 <- ggplot(data = F.educ.srh) +
  geom_vline(xintercept = 2002.5, linetype = "dashed") +
  geom_line(aes(y = F.Z.educ, x = year), size = 1.05, color = "#003087") +
  geom_line(aes(y = F.Z.srh,  x = year), size = 1.05, color = "#898D8D") +
  geom_point(aes(y = F.Z.educ, x = year), shape = 18, color = "#003087", size = 3) +
  geom_point(aes(y = F.Z.srh,  x = year), shape = 15, color = "#898D8D", size = 3) +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab(expression(bold('Standardized units'))) +
  xlab('') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 12,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title = element_text(size = 14),
        text = element_text(family = "Times New Roman"),
        legend.position = "none") +
  annotate("text", y =  2.1, x = 2014, size = 14/.pt, label = "Education", 
           family = "Times New Roman") +
  annotate("text", y = -1.5, x = 2012, size = 14/.pt, label = "Self-Rated Health", 
           family = "Times New Roman") +
  geom_segment(x = 1977, xend = 1972, y = -3, yend = -3,
               arrow = arrow(ends = "last", angle = 90, length=unit(.2,"cm"))) +
  geom_segment(x = 1997, xend = 2002, y = -3, yend = -3,
               arrow = arrow(ends = "last", angle = 90, length=unit(.2,"cm"))) +
  annotate("text", y =-3, x = 1987, size = 12/.pt,
           label = "Period of Hill and Needham (2006) study", 
           family = "Times New Roman")


# Men
p2 <- ggplot(data = M.educ.srh) +
  geom_vline(xintercept = 2002.5, linetype = "dashed") +
  geom_line(aes(y = M.Z.educ, x = year), size = 1.05, color = "#003087") +
  geom_line(aes(y = M.Z.srh,  x = year), size = 1.05, color = "#898D8D") +
  geom_point(aes(y = M.Z.educ, x = year), shape = 18, color = "#003087", size = 3) +
  geom_point(aes(y = M.Z.srh,  x = year), shape = 15, color = "#898D8D", size = 3) +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab(expression(bold('Standardized units'))) +
  xlab(expression(bold('Survey year'))) +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 12,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title = element_text(size = 14),
        text = element_text(family = "Times New Roman"),
        legend.position = "none") +
  annotate("text", y =  2, x = 2014, size = 14/.pt, label = "Education", 
           family = "Times New Roman") +
  annotate("text", y = -2, x = 2010, size = 14/.pt, label = "Self-Rated Health", 
           family = "Times New Roman") +
  annotate("segment", x = 1977, xend = 1972, y = -3, yend = -3,
           arrow = arrow(ends = "last", angle = 90, length = unit(.2,"cm"))) +
  annotate("segment", x = 1997, xend = 2002, y = -3, yend = -3,
           arrow = arrow(ends = "last", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", y =-3, x = 1987, size = 12/.pt,
           label = "Period of Hill and Needham (2006) study", 
           family = "Times New Roman")

figure <- ggarrange(p1, p2,
                    labels = c("Women", "Men"),
                    hjust = c(-1.5, -2.5),
                    vjust = c(3.5, 3.5),
                    ncol = 1, nrow = 2,
                    font.label = list(family = "Times New Roman", size = 16))
figure

ggsave("C:/Users/jtlriscy/educ_srh/figure.png", bg = "white")
   # bg = "white" removed a thin grey line between the two plots