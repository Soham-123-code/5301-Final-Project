library(WRS2)
library(patchwork)
library(car)
library(outliers)

head(diet)
View(diet)

str(diet)

#descriptive statistics
summary(diet$gender)
summary(diet$age)
summary(diet$height)
summary(diet$diet.type)
summary(diet$initial.weight)
summary(diet$final.weight)
summary(diet$weight.loss)


#groupwise analysis
#Gender-age
diet %>%
  group_by(diet$gender) %>%
  summarise(
    Mean = mean(diet$age),
    Median = median(diet$age),
    SD = sd(diet$age),
    Min = min(diet$age),
    Max = max(diet$age)
  )

#Gender-height
diet %>%
  group_by(diet$gender) %>%
  summarise(
    Mean = mean(diet$height),
    Median = median(diet$height),
    SD = sd(diet$height),
    Min = min(diet$height),
    Max = max(diet$height)
  )

#Gender-Initial Weight
diet %>%
  group_by(diet$gender) %>%
  summarise(
    Mean = mean(diet$initial.weight),
    Median = median(diet$initial.weight),
    SD = sd(diet$initial.weight),
    Min = min(diet$initial.weight),
    Max = max(diet$initial.weight)
  )

#Gender-Final Weight
diet %>%
  group_by(diet$gender) %>%
  summarise(
    Mean = mean(diet$final.weight),
    Median = median(diet$final.weight),
    SD = sd(diet$final.weight),
    Min = min(diet$final.weight),
    Max = max(diet$final.weight)
  )

#Gender-Weight  Loss
diet %>%
  group_by(diet$gender) %>%
  summarise(
    Mean = mean(diet$weight.loss),
    Median = median(diet$weight.loss),
    SD = sd(diet$weight.loss),
    Min = min(diet$weight.loss),
    Max = max(diet$weight.loss)
  )


#Diet Type-age
diet %>%
  group_by(diet$diet.type) %>%
  summarise(
    Mean = mean(diet$age),
    Median = median(diet$age),
    SD = sd(diet$age),
    Min = min(diet$age),
    Max = max(diet$age)
  )

#Diet Type-height
diet %>%
  group_by(diet$diet.type) %>%
  summarise(
    Mean = mean(diet$height),
    Median = median(diet$height),
    SD = sd(diet$height),
    Min = min(diet$height),
    Max = max(diet$height)
  )

#Diet Type-Initial weight
diet %>%
  group_by(diet$diet.type) %>%
  summarise(
    Mean = mean(diet$initial.weight),
    Median = median(diet$initial.weight),
    SD = sd(diet$initial.weight),
    Min = min(diet$initial.weight),
    Max = max(diet$initial.weight)
  )

#Diet Type-Final Weight
diet %>%
  group_by(diet$diet.type) %>%
  summarise(
    Mean = mean(diet$final.weight),
    Median = median(diet$final.weight),
    SD = sd(diet$final.weight),
    Min = min(diet$final.weight),
    Max = max(diet$final.weight)
  )

#Diet Type-Weight Loss
diet %>%
  group_by(diet$diet.type) %>%
  summarise(
    Mean = mean(diet$weight.loss),
    Median = median(diet$weight.loss),
    SD = sd(diet$weight.loss),
    Min = min(diet$weight.loss),
    Max = max(diet$weight.loss)
  )

#Frequency of Categorical Variable
# Frequency table for a gender
table(diet$gender)

# Percentage table for a gender
prop.table(table(diet$gender)) * 100


# Frequency table for a diet.type
table(diet$diet.type)

# Percentage table for a categorical variable
prop.table(table(diet$diet.type)) * 100

#Checking for missing values
colSums(is.na(diet))


#specifying colors for the plot
gender_colors <- c("hotpink", "blue")

#histogram weightloss by gender 
p1 <- ggplot(data = diet, aes(x = weight.loss, fill = gender)) +
  geom_histogram(position = "identity", alpha = 0.65) +
  labs(title = "Weight Loss vs Gender", x = "Weight Loss", y = "Count") +
  scale_fill_manual(values = gender_colors) +
  theme_minimal() 

#density plot for weightloss by diet.type
p2 <- ggplot(data = diet, aes(x=weight.loss, fill = gender)) +
  geom_density(aes(x=weight.loss, color = gender), position  = "identity", alpha = 0.65, linewidth = 1) +
  labs(title = "Weight Loss vs Gender", x= "Weight Loss") +
  scale_fill_manual(values = gender_colors) +
  theme_minimal()

#Gender plots
p1 + p2

#boxplot for weightloss by gender
ggplot(data =diet, aes(x=weight.loss, fill = gender)) +
  geom_boxplot(aes(x = gender, y=weight.loss, group = gender)) +
  labs(title = "Weight Loss grouped by Gender", x="Gender", y = "Weight Loss") + 
  scale_fill_manual(values = gender_colors) +
  theme_minimal()

#diet colors
diet_colors <- c("#b30000", "#0d88e6", "#ebdc78")

#Histogram plots for weight loss by diet type
p3 <- ggplot(data= diet, aes(x = diet$weight.loss, fill = diet$diet.type)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  labs(title = "Weight Loss by Diet Type", x="Weight Loss", y="Count", fill = "Diet Type") +
  scale_fill_manual(values = diet_colors) +
  theme_minimal()

p4 <- ggplot(data = diet, aes(x = diet$weight.loss, fill = diet$diet.type)) + 
  geom_density(aes(x=weight.loss), position = "identity", alpha =0.65, linewidth =1) +
  labs(title = "Weight Loss by Diet Type", x= "Weight Loss", fill = "Diet Type") +
  scale_fill_manual(values = diet_colors) +
  theme_minimal()

#Plots for weight loss by diet type
p3 + p4

#boxplot for weight loss by diet type
ggplot(data = diet, aes(x=weight.los, fill = diet.type)) + 
  geom_boxplot(aes(x=diet.type, y=weight.loss, group = diet.type)) +
  labs(title = "Boxplot for Weightloss by Diet Type", x = "Diet Type", y = "Weight Loss", fill = "Diet Type") +
  scale_fill_manual(values = diet_colors) +
  theme_minimal()

#outlier detection

grubbs.test(diet$weight.loss)

#Normality Test for Gender
shapiro.test(diet$weight.loss[diet$gender == "Female"])
shapiro.test(diet$weight.loss[diet$gender == "Male"])

#Normality Test for Diet Type
shapiro.test(diet$weight.loss[diet$diet.type == "A"])
shapiro.test(diet$weight.loss[diet$diet.type == "B"])
shapiro.test(diet$weight.loss[diet$diet.type == "C"])

#Homogeneity of Variance Test for Gender
leveneTest(data = diet, group = diet$gender, y= diet$weight.loss)

#Homogeneity of Variance Test for Diet Type
leveneTest(data = diet, group = diet$diet.type, y= diet$weight.loss)

#Two-way ANOVA
model <- aov(weight.loss ~ gender * diet.type, data=diet)

summary(model)


#post-hoc analysis
TukeyHSD(model)
