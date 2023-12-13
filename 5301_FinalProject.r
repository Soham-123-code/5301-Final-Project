library(WRS2)
library(patchwork)
library(car)
library(outliers)

head(diet)
View(diet)

str(diet)



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
  labs(title = "Weight Loss vs Diet Type", x="Weight Loss", y="Count") +
  scale_fill_manual(values = diet_colors) +
  theme_minimal()

p4 <- ggplot(data = diet, aes(x = diet$weight.loss, fill = diet$diet.type)) + 
  geom_density(aes(x=weight.loss), position = "identity", alpha =0.65, linewidth =1) +
  labs(title = "Weight Loss by Diet Type", x= "Weight Loss", y="Count", fill = "Diet Type") +
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

grubbs.test(diet$weight.loss)

#Normality Test
shapiro.test(diet$weight.loss[diet$gender == "Female"])
shapiro.test(diet$weight.loss[diet$gender == "Male"])

leveneTest(data = diet, group = diet$gender, y= diet$weight.loss)
leveneTest(data = diet, group = diet$diet.type, y= diet$weight.loss)

#Two-way ANOVA
model <- aov(weight.loss ~ gender * diet.type, data=diet)

summary(model)


#post-hoc analysis
TukeyHSD(model)
