# load the libraries and read in the raw data
library(ggplot2)
library(datasets)

head(ToothGrowth)
unique(ToothGrowth$dose)
unique(ToothGrowth$supp)
summary(ToothGrowth)

# inital plot of the data
#tooth_plot <- ggplot(data = ToothGrowth, aes(x = supp, y = len)) +
#              geom_boxplot(aes(fill = supp)) +
#              facet_wrap(~dose)

tooth_plot_01 <- ggplot(data = ToothGrowth, aes(x = supp, y = len)) +
                  geom_boxplot(aes(fill = supp))
print(tooth_plot_01)

tooth_plot_02 <- ggplot(data = ToothGrowth, aes(x = dose, y = len)) +
                  geom_boxplot(aes(fill = factor(dose)))
print(tooth_plot_02)

test01 <- t.test(len ~ supp, data = ToothGrowth, paired = FALSE)

growth_dose_A <- subset(ToothGrowth, dose %in% c(0.5, 1.0))
growth_dose_B <- subset(ToothGrowth, dose %in% c(1.0, 2.0))
growth_dose_C <- subset(ToothGrowth, dose %in% c(0.5, 2.0))

t.test(len ~ dose, data = growth_dose_A, paired = FALSE)
t.test(len ~ dose, data = growth_dose_B, paired = FALSE)
t.test(len ~ dose, data = growth_dose_C, paired = FALSE)

