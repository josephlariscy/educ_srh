# Replicating AJPH article by Hill and Burdette
# Gender-Specific Trends in Educational Attainment and Self-Rated Health

library(remotes)
library(haven)
library(sjmisc)
library(ggplot2)
library(ggpubr)   # Required for ggarrange

# Read-in GSS data
install_github("kjhealy/gssr")
library(gssr)
data(gss_all)


gss_1972_2018 <- subset(gss_all,
                  year >= 1972 & year <= 2018 &
                  year != 1978 & year != 1983 & year != 1986 &
                  age >= 25,
                  select = c(year, health, educ, sex, age))
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
frq(sex)   # 18,652 men and 23,753 women
summary(educ)
summary(age)

# Reverse code health so that 1=poor and 4=excellent
health.rev <- 5 - health
table(health, health.rev)


gss.recode <- cbind(gss.listwise, health.rev)
rm(health.rev)

gss.male   <- subset(gss.recode, sex == 1)
gss.female <- subset(gss.recode, sex == 2)


# function to have true minus sign for negative numbers on y-axis of plots
label_true_minus <- function(x){
  ifelse(sign(x) == -1, paste0("\u2212", abs(x)), x)
  }


# Figure 1. Women

attach(gss.female)

# Calculate mean years of education for each year
F.mean.educ <- aggregate(educ, list(year), FUN = mean)
F.mean.educ
F.Z.educ <- scale(F.mean.educ$x, center = TRUE, scale = TRUE)
F.Z.educ

Fig1.F.educ <- cbind(F.mean.educ, F.Z.educ)

# Plot of education trend among women
ggplot(data = Fig1.F.educ, aes(x = Group.1, y = F.Z.educ)) +
  geom_line() +
  geom_point(shape = 18, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = 2.3, x = 2000, label = "Education", size = 4)


# Calculate mean SRH for each year
F.mean.srh <- aggregate(health.rev, list(year), FUN = mean)
F.mean.srh
F.Z.srh <- scale(F.mean.srh$x, center = TRUE, scale = TRUE)
F.Z.srh

Fig1.F.srh <- cbind(F.mean.srh, F.Z.srh)

# Plot of self-rated health trend among women
ggplot(data = Fig1.F.srh, aes(x = Group.1, y = F.Z.srh)) +
  geom_line() +
  geom_point(shape = 15, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = -0.5, x = 1999, label = "Self-Rated Health", size = 4)


# Figure 2. Men

attach(gss.male)

# Calculate mean years of education for each year
M.mean.educ <- aggregate(educ, list(year), FUN = mean)
M.mean.educ
M.Z.educ <- scale(M.mean.educ$x, center = TRUE, scale = TRUE)
M.Z.educ

Fig2.M.educ <- cbind(M.mean.educ, M.Z.educ)

# Plot of education trend among men
ggplot(data = Fig2.M.educ, aes(x = Group.1, y = M.Z.educ)) +
  geom_line() +
  geom_point(shape = 18, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = 2.3, x = 2000, size = 4, label = "Education")


# Calculate mean SRH for each year
attach(gss.male)
M.mean.srh <- aggregate(health.rev, list(year), FUN = mean)
M.mean.srh
M.Z.srh <- scale(M.mean.srh$x, center = TRUE, scale = TRUE)
M.Z.srh

Fig2.M.srh <- cbind(M.mean.srh, M.Z.srh)

# plot of self-rated health trend among men
ggplot(data = Fig2.M.srh, aes(x = Group.1, y = M.Z.srh)) +
  geom_line() +
  geom_point(shape = 15, size = 3) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab('Standardized Metric Units') +
  xlab('Year') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9)) +
  annotate("text", y = -1, x = 1997, size = 4, label = "Self-Rated Health")


# Final figure: Plots of self-rated health and education by sex

F.educ.srh <- as.data.frame(merge(Fig1.F.educ, Fig1.F.srh, by = "Group.1"))
M.educ.srh <- as.data.frame(merge(Fig2.M.educ, Fig2.M.srh, by = "Group.1"))


windows(width = 10, height = 10)

# Women
p1 <- ggplot(data = F.educ.srh) +
  geom_vline(xintercept = 2002.5, linetype = "dashed") +
  geom_line(aes(y = F.Z.educ, x = Group.1), size = 1.05) +
  geom_line(aes(y = F.Z.srh,  x = Group.1), size = 1.05, color = "grey") +
  geom_point(aes(y = F.Z.educ, x = Group.1), shape = 18, size=3) +
  geom_point(aes(y = F.Z.srh,  x = Group.1), shape = 15, size=3, color="grey") +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab(expression(bold('Standardized Metric Units'))) +
  xlab('') +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        legend.position = "none") +
  annotate("text", y =  2.1, x = 2014, size = 4, label = "Education") +
  annotate("text", y = -1.5, x = 2012, size = 4, label = "Self-Rated Health") +
  geom_segment(x = 1977.5, xend = 1972, y = -3, yend = -3,
               arrow = arrow(ends = "last", angle = 90, length=unit(.2,"cm"))) +
  geom_segment(x = 1996.5, xend = 2002, y = -3, yend = -3,
               arrow = arrow(ends = "last", angle = 90, length=unit(.2,"cm"))) +
  annotate("text", y =-3, x = 1987, size = 3,
           label = "Time period of Hill and Needham (2006) study")


# Men
p2 <- ggplot(data = M.educ.srh) +
  geom_vline(xintercept = 2002.5, linetype = "dashed") +
  geom_line(aes(y = M.Z.educ, x = Group.1), size = 1.05) +
  geom_line(aes(y = M.Z.srh,  x = Group.1), size = 1.05, color = "grey") +
  geom_point(aes(y = M.Z.educ, x = Group.1), shape = 18, size=3) +
  geom_point(aes(y = M.Z.srh,  x = Group.1), shape = 15, size=3, color="grey") +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), labels = label_true_minus) +
  scale_x_continuous(breaks = seq(1972, 2018, by = 2)) +
  ylab(expression(bold('Standardized Metric Units'))) +
  xlab(expression(bold('Year'))) +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "black", size = 9,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        legend.position = "none") +
  annotate("text", y =  2, x = 2014, size = 4, label = "Education") +
  annotate("text", y = -2, x = 2010, size = 4, label = "Self-Rated Health") +
  annotate("segment", x = 1977.5, xend = 1972, y = -3, yend = -3,
           arrow = arrow(ends = "last", angle = 90, length = unit(.2,"cm"))) +
  annotate("segment", x = 1996.5, xend = 2002, y = -3, yend = -3,
           arrow = arrow(ends = "last", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", y =-3, x = 1987, size = 3,
           label = "Time period of Hill and Needham (2006) study")

figure <- ggarrange(p1, p2,
                    labels = c("Women", "Men"),
                    hjust = c(-1.5, -2.5),
                    vjust = c(3.5, 3.5),
                    ncol = 1, nrow = 2)
figure

ggsave("C:/Users/jtlriscy/educ_srh/figure_wo_age_adj.png")



# Supplemental analysis: I want to adjust for age. First, I will make sure that
# predicted means from a linear model match the calculated means used in the
# tables above. Then, in a separate R script, I will run linear models that
# adjust for age.


attach(gss.female)
F.mean.age <- aggregate(age, list(year), FUN = mean)
F.mean.age
plot(x=F.mean.age$Group.1, y=F.mean.age$x, type="l")


# Women - predict values of mean SRH from linear model (not age adjusted yet)

F.year.f <- factor(year)
frq(F.year.f)
frq(year)
F.regression.data <- cbind(gss.female, F.year.f)

F.model.srh <- lm(health.rev ~ F.year.f, data = F.regression.data)
summary(F.model.srh)

F.pred.srh <- data.frame(F.year.f = c("1972","1973","1974","1975","1976",
                                      "1977","1980","1982","1984","1985",
                                      "1987","1988","1989","1990","1991",
                                      "1993","1994","1996","1998","2000",
                                      "2002","2004","2006","2008","2010",
                                      "2012","2014","2016","2018"))
F.mean.predsrh <- predict(F.model.srh, newdata = F.pred.srh)
F.mean.predsrh <- cbind(F.pred.srh, F.mean.predsrh)

# Confirm that mean SRH predicted from the linear model match the calculated
# mean SRH

F.mean.predsrh
F.mean.srh


# Women - predict values of mean educ from linear model (not age adjusted yet)

F.model.educ <- lm(educ ~ F.year.f, data = F.regression.data)
summary(F.model.educ)

F.pred.educ <- data.frame(F.year.f = c("1972","1973","1974","1975","1976",
                                       "1977","1980","1982","1984","1985",
                                       "1987","1988","1989","1990","1991",
                                       "1993","1994","1996","1998","2000",
                                       "2002","2004","2006","2008","2010",
                                       "2012","2014","2016","2018"))
F.mean.prededuc <- predict(F.model.educ, newdata = F.pred.educ)
F.mean.prededuc <- cbind(F.pred.srh, F.mean.prededuc)

# Confirm that mean education predicted from the linear model matches the
# calculated mean education 

F.mean.prededuc
F.mean.educ



# Men

attach(gss.male)
M.mean.age <- aggregate(age, list(year), FUN = mean)
M.mean.age

# Men - predict values of mean SRH from linear model (not age adjusted yet)

M.year.f <- factor(year)
frq(M.year.f)
frq(year)
M.regression.data <- cbind(gss.male, M.year.f)

M.model.srh <- lm(health.rev ~ M.year.f, data = M.regression.data)
summary(M.model.srh)

M.pred.srh <- data.frame(M.year.f = c("1972","1973","1974","1975","1976",
                                      "1977","1980","1982","1984","1985",
                                      "1987","1988","1989","1990","1991",
                                      "1993","1994","1996","1998","2000",
                                      "2002","2004","2006","2008","2010",
                                      "2012","2014","2016","2018"))
M.mean.predsrh <- predict(M.model.srh, newdata = M.pred.srh)
M.mean.predsrh <- cbind(M.pred.srh, M.mean.predsrh)

# Confirm that mean SRH predicted from the linear model match the calculated
# mean SRH

M.mean.predsrh
M.mean.srh


# Women - predict values of mean educ from linear model (not age adjusted yet)

M.model.educ <- lm(educ ~ M.year.f, data = M.regression.data)
summary(M.model.educ)

M.pred.educ <- data.frame(M.year.f = c("1972","1973","1974","1975","1976",
                                       "1977","1980","1982","1984","1985",
                                       "1987","1988","1989","1990","1991",
                                       "1993","1994","1996","1998","2000",
                                       "2002","2004","2006","2008","2010",
                                       "2012","2014","2016","2018"))
M.mean.prededuc <- predict(M.model.educ, newdata = M.pred.educ)
M.mean.prededuc <- cbind(M.pred.srh, M.mean.prededuc)

# Confirm that mean education predicted from the linear model matches the
# calculated mean education 

M.mean.prededuc
M.mean.educ
