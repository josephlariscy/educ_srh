# Replicating AJPH article by Hill and Burdette
# Gender-Specific Trends in Educational Attainment and Self-Rated Health

library(remotes)
library(haven)
library(sjmisc)
library(ggplot2)


install_github("kjhealy/gssr")
library(gssr)
data(gss_all)


gss7202 <- subset(gss_all,
                  year >= 1972 & year <= 2002 & 
                  year != 1978 & year != 1983 & year != 1986 &
                  age >= 25,
                  select=c(year, health, educ, sex, age))

attach(gss7202)

# Check variables
frq(year)
frq(health)
frq(sex)
summary(educ)
summary(age)

table(year, health, useNA = 'ifany')


# Delete cases with missing values
gss.listwise <- na.omit(gss7202)

attach(gss.listwise)

frq(year)
frq(health)
frq(sex)   # 12,969 men and 16,658 women (match Table 1 sample sizes)
summary(educ)
summary(age)


# Reverse code health so that 1=poor and 4=excellent
health.rev <- 5 - health
table(health, health.rev)


gss.recode <- cbind(gss.listwise, health.rev)
rm(health.rev)

gss.male <- subset(gss.recode, sex == 1)
gss.female <- subset(gss.recode, sex == 2)


# labeller function to have true minus sign for negative numbers
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
F.mean.srh <- aggregate(health.rev, list(year), FUN = mean)
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


F.educ.srh <- merge(Fig1.educ, Fig1.srh, by="Group.1")
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
M.mean.educ <- aggregate(educ, list(year), FUN = mean)
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
M.mean.srh <- aggregate(health.rev, list(year), FUN = mean)
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
