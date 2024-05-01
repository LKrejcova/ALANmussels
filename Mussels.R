################################################################################
# SET UP

### load data
# light "shock" 
library(readr)
shock_l <- read_csv("https://raw.githubusercontent.com/LKrejcova/ALANmussels/main/Light_shock.csv", show_col_types = FALSE) # long format
shock_w <- read_csv("https://raw.githubusercontent.com/LKrejcova/ALANmussels/main/Light_shock_table.csv", show_col_types = FALSE) # wide format

# acclimatization 1
acc_l <- read_csv("https://raw.githubusercontent.com/LKrejcova/ALANmussels/main/Light_acc.csv", show_col_types = FALSE) # long format
acc_w <- read_csv("https://raw.githubusercontent.com/LKrejcova/ALANmussels/main/Light_acc_table.csv", show_col_types = FALSE) # wide format

# acclimatization 2
acc2_l <- read_csv("https://raw.githubusercontent.com/LKrejcova/ALANmussels/main/light_acc_2_long.csv", show_col_types = FALSE) # long format
acc2_w <- read_csv("https://raw.githubusercontent.com/LKrejcova/ALANmussels/main/light_acc_2.csv", show_col_types = FALSE) # wide format

# transpose rows and columns
shock_wt <- data.frame(t(shock_w[-1]))
acc_wt <- data.frame(t(acc_w[-1]))
acc2_wt <- data.frame(t(acc2_w[-1]))

# add headers
colnames(shock_wt) <- c("Treatment", "Volume", "T0", "T10", "T20", "T30", "Length")
colnames(acc_wt) <- c("Treatment", "Volume", "T0", "T10", "T20", "T30", "Length")
colnames(acc2_wt) <- c("Treatment", "Volume", "T0", "T10", "T20", "T30", "Length")

# make variables factors
shock_wt$Treatment <- as.factor(shock_wt$Treatment)
acc_wt$Treatment <- as.factor(acc_wt$Treatment)
acc2_wt$Treatment <- as.factor(acc2_wt$Treatment)

# make variables numeric
shock_wt$Volume <- as.numeric(shock_wt$Volume)
shock_wt$T0 <- as.numeric(shock_wt$T0)
shock_wt$T10 <- as.numeric(shock_wt$T10)
shock_wt$T20 <- as.numeric(shock_wt$T20)
shock_wt$T30 <- as.numeric(shock_wt$T30)
shock_wt$Length <- as.numeric(shock_wt$Length)

acc_wt$Volume <- as.numeric(acc_wt$Volume)
acc_wt$T0 <- as.numeric(acc_wt$T0)
acc_wt$T10 <- as.numeric(acc_wt$T10)
acc_wt$T20 <- as.numeric(acc_wt$T20)
acc_wt$T30 <- as.numeric(acc_wt$T30)
acc_wt$Length <- as.numeric(acc_wt$Length)

acc2_wt$Volume <- as.numeric(acc2_wt$Volume)
acc2_wt$T0 <- as.numeric(acc2_wt$T0)
acc2_wt$T10 <- as.numeric(acc2_wt$T10)
acc2_wt$T20 <- as.numeric(acc2_wt$T20)
acc2_wt$T30 <- as.numeric(acc2_wt$T30)
acc2_wt$Length <- as.numeric(acc2_wt$Length)

#' Since we lost the particle count for A1 at T0 in the acc2 dataset, we replace it with an average 
#' of the particle values at T0 in the same treatment

acc2_wt[1,3] <- mean(acc2_wt[2:4,3])

# calculate grazing rate for light shock for T30
shock_wt$grazing <- - log(shock_wt$T30/shock_wt$T0)/0.5
shock_wt$filtration <- shock_wt$grazing * shock_wt$Volume

# calculate grazing rate for acclimatized for T30
acc_wt$grazing <- - log(acc_wt$T30/acc_wt$T0)/0.5
acc_wt$filtration <- acc_wt$grazing * acc_wt$Volume

# calculate grazing rate for acclimatized for T30
acc2_wt$grazing <- - log(acc2_wt$T30/acc2_wt$T0)/0.5
acc2_wt$filtration <- acc2_wt$grazing * acc2_wt$Volume

################################################################################
# plot the results
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(shock_l, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(shock_l, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(shock_l, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(shock_l, Color == "White"),
     main = "White light")

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = shock_wt,
        las = 1,
        main = "Filtration rate for light shock",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

#' We see that there are negative filtration rates, which show that there is something
#' wrong. Potential explanation: resuspension of particles when taking the measurements

# ANOVA with filtration rate as response variable
filter_fit <- aov(filtration ~ Treatment, data = shock_wt)
summary(filter_fit)

#' The p-value is far from significant

# look at TA-Plot and QQ-Plot
par(mfrow = 1:2)
plot(filter_fit, which = 1:2)


################################################################################
# new data frames excluding T30 for the shock treatment
shock_l_20 <- subset(shock_l, Time <= 20)
shock_wt_20 <- subset(shock_wt, select = -T30)


# plot the results
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(shock_l_20, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(shock_l_20, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(shock_l_20, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(shock_l_20, Color == "White"),
     main = "White light")

# calculate grazing rate
shock_wt_20$grazing <- - log(shock_wt_20$T20/shock_wt_20$T0)/0.3
shock_wt_20$filtration <- shock_wt_20$grazing * shock_wt_20$Volume

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = shock_wt_20,
        las = 1,
        main = "Light shock",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
filter_fit_shock_20 <- aov(filtration ~ Treatment, data = shock_wt_20)
summary(filter_fit_shock_20)

# TA-Plot und QQ-Plot anschauen
par(mfrow = 1:2)
plot(filter_fit_shock_20, which = 1:2)


# remove negative values from filtration rates
shock_pos <- subset(shock_wt_20, filtration > 0)

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = shock_pos,
        las = 1,
        main = "Filtration rate, only positive filtration",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
filter_fit_pos <- aov(filtration ~ Treatment, data = shock_pos[1:9,])
summary(filter_fit_pos)

################################################################################

# ACCLIMATIZED MUSSELS, DAY 7

################################################################################
# plot the results for acclimatized 
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(acc_l, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(acc_l, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(acc_l, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(acc_l, Color == "White"),
     main = "White light")


# Boxplot for filtration rate at T30
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = acc_wt,
        las = 1,
        main = "Filtration rate, acclimatized",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
fit_acc <- aov(filtration ~ Treatment, data = acc_wt)
summary(fit_acc)

# look at TA-Plot and QQ-Plot
par(mfrow = 1:2)
plot(fit_acc, which = 1:2)

################################################################################
# new data frames excluding T30
acc_l_20 <- subset(acc_l, Time <= 20)
acc_w_20 <- subset(acc_wt, select = -T30)


# plot the results
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(acc_l_20, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(acc_l_20, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(acc_l_20, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(acc_l_20, Color == "White"),
     main = "White light")

# calculate grazing rate
acc_w_20$grazing <- - log(acc_w_20$T20/acc_w_20$T0)/0.3
acc_w_20$filtration <- acc_w_20$grazing * acc_w_20$Volume

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = acc_w_20,
        las = 1,
        main = "Filtration rate",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
fit_acc_20 <- aov(filtration ~ Treatment, data = acc_w_20)
summary(fit_acc_20)

# look at TA-Plot and QQ-Plot 
par(mfrow = 1:2)
plot(fit_acc_20, which = 1:2)

################################################################################

# ACCLIMATIZED MUSSELS, DAY 9

################################################################################

# plot the results for acclimatized 
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(acc2_l, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(acc2_l, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(acc2_l, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(acc2_l, Color == "White"),
     main = "White light")


# Boxplot for filtration rate at T30
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = acc2_wt,
        las = 1,
        main = "Filtration rate, acclimatized",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
fit_acc2 <- aov(filtration ~ Treatment, data = acc2_wt)
summary(fit_acc2)

# look at TA-Plot and QQ-Plot
par(mfrow = 1:2)
plot(fit_acc, which = 1:2)

################################################################################
# new data frames excluding T30
acc2_l_20 <- subset(acc2_l, Time <= 20)
acc2_w_20 <- subset(acc2_wt, select = -T30)


# plot the results
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(acc2_l_20, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(acc2_l_20, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(acc2_l_20, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(acc2_l_20, Color == "White"),
     main = "White light")

# calculate grazing rate
acc2_w_20$grazing <- - log(acc2_w_20$T20/acc2_w_20$T0)/0.3
acc2_w_20$filtration <- acc2_w_20$grazing * acc2_w_20$Volume

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = acc2_w_20,
        las = 1,
        main = "Acclimatized (9 days)",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
fit_acc2_20 <- aov(filtration ~ Treatment, data = acc2_w_20)
summary(fit_acc2_20)

# look at TA-Plot and QQ-Plot 
par(mfrow = 1:2)
plot(fit_acc_20, which = 1:2)

################################################################################

# COMPARE EXPOSURE TIMES

################################################################################
shock_wt_20$exposure <- as.factor(rep("shock", 16))
acc_w_20$exposure <- as.factor(rep("acc", 16))
acc2_w_20$exposure <- as.factor(rep("acc2", 16))

all_20 <- rbind(shock_wt_20, acc_w_20, acc2_w_20)

fit_exp <- aov(filtration ~ Treatment + exposure, data = all_20)
summary(fit_exp)

################################################################################

# LOOK AT EFFECT OF LENGTH

################################################################################
fit_length_shock <- aov(filtration ~ Treatment + Length, data = shock_wt_20)
summary(fit_length_shock)

fit_length_acc <- aov(filtration ~ Treatment + Length, data = acc_w_20)
summary(fit_length_acc)

fit_length_acc2 <- aov(filtration ~ Treatment * Length, data = acc2_w_20)
summary(fit_length_acc2)

fit_length_all <- aov(filtration ~ Treatment + Length, data = all_20)
summary(fit_length_all)

################################################################################

# LOOK AT EFFECT OF INDIVIDUAL MUSSELS

################################################################################
all_20$ind <- as.factor(rep(1:16,3))

fit_ind <- aov(filtration ~ Treatment * ind, data = all_20)
summary(fit_ind)
