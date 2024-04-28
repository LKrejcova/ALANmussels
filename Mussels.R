################################################################################
# SET UP
# set working directory
setwd("/Users/lea/Documents/Documents – Lea's MacBook Air (2)/ETHZ/DTU/Environmental Effects of Maritime Activities")

# load data
light <- read.csv("Light_shock.csv", header = T)
light_table <- read.csv("Light_shock_table.csv", header = T)
light_table <- (light_table)[1:6,]
light_acc <- read.csv("Light_acc.csv", header = T)
light_acc_table <- read.csv("Light_acc_table.csv", header = T)

# transpose rows and columns
light_table_t <- data.frame(t(light_table[-1]))
light_acc_t <- data.frame(t(light_acc_table[-1]))

# add headers
colnames(light_table_t) <- c("Treatment", "Volume", "T0", "T10", "T20", "T30")
colnames(light_acc_t) <- c("Treatment", "Volume", "T0", "T10", "T20", "T30")

# make variables factors
light$Color <- as.factor(light$Color)
light$Block <- as.factor(light$Block)
light$Treatment <- as.factor(light$Treatment)
light_table_t$Treatment <- as.factor(light_table_t$Treatment)
light_acc$Color <- as.factor(light_acc$Color)
light_acc$Block <- as.factor(light_acc$Block)
light_acc$Treatment <- as.factor(light_acc$Treatment)
light_acc_t$Treatment <- as.factor(light_acc_t$Treatment)

# make variables numeric
light_table_t$Volume <- as.numeric(light_table_t$Volume)
light_table_t$T0 <- as.numeric(light_table_t$T0)
light_table_t$T10 <- as.numeric(light_table_t$T10)
light_table_t$T20 <- as.numeric(light_table_t$T20)
light_table_t$T30 <- as.numeric(light_table_t$T30)

light_acc_t$Volume <- as.numeric(light_acc_t$Volume)
light_acc_t$T0 <- as.numeric(light_acc_t$T0)
light_acc_t$T10 <- as.numeric(light_acc_t$T10)
light_acc_t$T20 <- as.numeric(light_acc_t$T20)
light_acc_t$T30 <- as.numeric(light_acc_t$T30)

# calculate grazing rate for light shock
light_table_t$grazing <- - log(light_table_t$T20/light_table_t$T0)/0.5
light_table_t$filtration <- light_table_t$grazing * light_table_t$Volume

# calculate grazing rate for acclimatized 
light_acc_t$grazing <- - log(light_acc_t$T20/light_acc_t$T0)/0.5
light_acc_t$filtration <- light_acc_t$grazing * light_acc_t$Volume

################################################################################
# plot the results
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(light, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(light, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(light, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(light, Color == "White"),
     main = "White light")

# linear fit of the filtration rate (?)
lm


# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = light_table_t,
        las = 1,
        main = "Filtration rate",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
filter_fit <- aov(filtration ~ Treatment, data = light_table_t)
summary(filter_fit)

# TA-Plot und QQ-Plot anschauen
par(mfrow = 1:2)
plot(filter_fit, which = 1:2)


################################################################################
# new data frames excluding T30
light_20 <- subset(light, Time <= 20)
light_table_20 <- subset(light_table_t, select = -T30)


# plot the results
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(light_20, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(light_20, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(light_20, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(light_20, Color == "White"),
     main = "White light")

# calculate grazing rate
light_table_20$grazing <- - log(light_table_20$T20/light_table_20$T0)/0.3
light_table_20$filtration <- light_table_20$grazing * light_table_20$Volume

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = light_table_20,
        las = 1,
        main = "Filtration rate",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
filter_fit <- aov(filtration ~ Treatment, data = light_table_t)
summary(filter_fit)

# TA-Plot und QQ-Plot anschauen
par(mfrow = 1:2)
plot(filter_fit, which = 1:2)


# remove negative values from filtration rates
light_pos <- subset(light_table_20, filtration > 0)

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = light_pos,
        las = 1,
        main = "Filtration rate",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
filter_fit_pos <- aov(filtration ~ Treatment, data = light_pos[1:9,])
summary(filter_fit_pos)

################################################################################

# ACCLIMATIZED MUSSELS

################################################################################
# plot the results for acclimatized 
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(light_acc, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(light_acc, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(light_acc, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(light_acc, Color == "White"),
     main = "White light")


# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = light_acc_t,
        las = 1,
        main = "Filtration rate, acclimatized",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
filter_fit_acc <- aov(filtration ~ Treatment, data = light_acc_t)
summary(filter_fit_acc)

# TA-Plot und QQ-Plot anschauen
par(mfrow = 1:2)
plot(filter_fit_acc, which = 1:2)

################################################################################
# new data frames excluding T30
light_acc_20 <- subset(light_acc, Time <= 20)
light_acc_t_20 <- subset(light_acc_t, select = -T30)


# plot the results
par(mfrow = c(2,2))
plot(Particles ~ Time, data = subset(light_acc_20, Color == "Dark"),
     main = "Control", pch = 19)
plot(Particles ~ Time, data = subset(light_acc_20, Color == "Red"),
     main = "Red light", col = "red")
plot(Particles ~ Time, data = subset(light_acc_20, Color == "Green"),
     main = "Green light", col = "green")
plot(Particles ~ Time, data = subset(light_acc_20, Color == "White"),
     main = "White light")

# calculate grazing rate
light_acc_t_20$grazing <- - log(light_acc_t_20$T20/light_acc_t_20$T0)/0.3
light_acc_t_20$filtration <- light_acc_t_20$grazing * light_acc_t_20$Volume

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = light_acc_t_20,
        las = 1,
        main = "Filtration rate",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
filter_fit_acc_20 <- aov(filtration ~ Treatment, data = light_acc_t_20)
summary(filter_fit_acc_20)

# TA-Plot und QQ-Plot anschauen
par(mfrow = 1:2)
plot(filter_fit_acc_20, which = 1:2)


# remove negative values from filtration rates
light_pos_acc <- subset(light_acc_t_20, filtration > 0)

# Boxplot for filtration rate
par(mfrow = c(1,1))
boxplot(filtration ~ Treatment, data = light_pos,
        las = 1,
        main = "Filtration rate",
        ylab = "Filtration rate [L/individual/hour]",
        xlab = "",
        col = c("grey", "green", "red", "yellow"))

# ANOVA with filtration rate as response variable
filter_fit_pos <- aov(filtration ~ Treatment, data = light_pos[1:9,])
summary(filter_fit_pos)
