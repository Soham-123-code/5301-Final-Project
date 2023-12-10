library(WRS2)
diet_data <- data("diet")
str(diet_data)
head(diet_data)
head(diet)

diet_table_counts <- table(diet$diet.type)

diet_table_counts


