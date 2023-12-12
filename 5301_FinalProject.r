library(WRS2)
library(patchwork)
library(car)
library(outliers)


diet_data <- data("diet")
str(diet_data)
head(diet_data)
head(diet)
View(diet)

str(diet)

diet_table_type <- table(diet$diet.type)
diet_table_gender <- table(diet$gender)

diet_table_counts
diet_table_gender

#scatter plot of all features
plot(diet)

#Count plots
ggplot(data = diet, aes(x= diet$gender, fill = diet$age, group = diet$age)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Gender vs Age", x="Gender", y="count") +
  theme_minimal()

ggplot(data =diet, aes(x=diet$gender, fill = diet$height, group = diet$height)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Gender vs Height", x="Gender", y="count") +
  theme_minimal()

ggplot(data =diet, aes(x=diet$gender, fill = diet$diet.type, group = diet$diet.type)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Gender vs Diet Type", x="Gender", y="count") +
  theme_minimal()

ggplot(data =diet, aes(x=diet$gender, fill = diet$initial.weight, group = diet$initial.weight)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Gender vs Initial Height", x="Gender", y="count") +
  theme_minimal()

ggplot(data =diet, aes(x=diet$gender, fill = diet$final.weight, group = diet$final.weight)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Gender vs Final Weight", x="Gender", y="count") +
  theme_minimal()

ggplot(data =diet, aes(x=diet$gender, fill = diet$weight.loss)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Gender vs Weight Loss", x="Gender", y="count") +
  theme_minimal()

ggplot(data = diet) +
  geom_point(aes(x=diet$age, y=diet$height, color = diet$diet.type)) +
  labs(titel = "Age vs Height", x = "Age", y = "Height") +
  theme_minimal()

#specifying colors for the plot
gender_colors <- c("hotpink", "blue")

#histogram weightloss by gender 
p1 <- ggplot(data = diet, aes(x = weight.loss, fill = gender)) +
  geom_histogram(position = "identity", alpha = 0.65) +
  labs(title = "Superimposed Bar Plot of Weight Loss by Gender", x = "Weight Loss", y = "Count") +
  scale_fill_manual(values = gender_colors) +
  theme_minimal() 

#density plot for weightloss by diet.type
p2 <- ggplot(data = diet, aes(x=weight.loss, fill = gender)) +
  geom_density(aes(x=weight.loss, color = gender), position  = "identity", alpha = 0.65, linewidth = 1) +
  labs(title = "Superimposed Line Plots of Weight Loss by Gender", x= "Weight Loss", y = "Count") +
  scale_fill_manual(values = gender_colors) +
  theme_minimal()

#Gender plots
p1 + p2

#diet colors
diet_colors <- c("#b30000", "#0d88e6", "#ebdc78")

#Histogram plots for weight loss by diet type
p3 <- ggplot(data= diet, aes(x = diet$weight.loss, fill = diet$diet.type)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  labs(title = "Superimposed plot of Weight Loss vs Diet Type", x="Weight Loss", y="Count") +
  scale_fill_manual(values = diet_colors) +
  theme_minimal()

p4 <- ggplot(data = diet, aes(x = diet$weight.loss, fill = diet$diet.type)) + 
  geom_density(aes(x=weight.loss), position = "identity", alpha =0.65, linewidth =1) +
  labs(title = "Superimposed Line plot of Weight Loss by Diet Type", x= "Weight Loss", y="Count") +
  scale_fill_manual(values = diet_colors) +
  theme_minimal()

#Plots for weight loss by diet type
p3 + p4


#boxplot for weightloss by gender
ggplot(data =diet, aes(x=weight.loss, fill = gender)) +
  geom_boxplot(aes(x = gender, y=weight.loss, group = gender)) +
  labs(title = "boxplot for Weightloss by Female") + 
  theme_minimal()

#boxplot for weight loss by diet type
ggplot(data = diet, aes(x=weight.los, fill = diet.type)) + 
  geom_boxplot(aes(x=diet.type, y=weight.loss, group = diet.type)) +
  labs(title = "Boxplot for Weightloss by Diet Type", x = "Diet Type", y = "Weight Loss") + 
  theme_minimal()

#outlier detection
zscores <- scale(diet$weight.loss)
outliers <- abs(zscores) >3
outliers

grubbs.test(diet$weight.loss)

#Normality Test
shapiro.test(diet$age[diet$gender == "Female"])
shapiro.test(diet$age[diet$gender == "Male"])

shapiro.test(diet$height[diet$gender == "Female"])
shapiro.test(diet$height[diet$gender == "Male"])

shapiro.test(diet$initial.weight[diet$gender == "Female"])
shapiro.test(diet$initial.weight[diet$gender == "Male"])

shapiro.test(diet$final.weight[diet$gender == "Female"])
shapiro.test(diet$final.weight[diet$gender == "Male"])

shapiro.test(diet$weight.loss[diet$gender == "Female"])
shapiro.test(diet$weight.loss[diet$gender == "Male"])

leveneTest(data = diet, group = diet$gender, y= diet$weight.loss)

#Two-way ANOVA
model <- aov(weight.loss ~ gender * diet.type, data=diet)

summary(model)

plot(model)

#post-hoc analysis
TukeyHSD(model)
