R.home("bin")

#1. Create a data frame containing information of 5 students with the following columns
name = c("Aarav", "Priya", "Karan", "Sneha", "Aditya")
ages = c(20, 21, 19, 22, 20)
genders = c(1, 0, 1, 0, 1)  # 1 = Male, 0 = Female
passed = c(TRUE, TRUE, FALSE, TRUE, FALSE)

students = data.frame(name, ages, genders, passed)
students

str(students)
summary(students)

library(readxl)
Book1 = read_excel("C:/Users/vaidy/Documents/clg/sem 5/prob and stats/prob-stats-lab/college_product_sales.xlsx")
View(Book1)

Book1$Total_Sales = Book1$`Units Sold` * Book1$`Price per Unit`

str(mtcars)
cars_more_than_6_cyl = subset(mtcars, cyl > 6)

average_mpg = mean(cars_more_than_6_cyl$mpg)
average_mpg

Book1$Category = as.factor(Book1$Category)

levels(Book1$Category)
table(Book1$Category)

high_price_products = subset(Book1, `Price per Unit` > 100)
table(high_price_products$Category)

plot(mtcars$wt, mtcars$mpg, main = "Scatter Plot of Weight vs MPG", xlab = "Weight (1000 lbs)", ylab = "Miles Per Gallon", pch = 19, col = "blue")

boxplot(mpg ~ cyl, data = mtcars, main = "MPG by Number of Cylinders", xlab = "Number of Cylinders", ylab = "Miles Per Gallon", col = c("orange", "lightblue", "lightgreen"))

gear_counts <- table(mtcars$gear)
pie(gear_counts, main = "Distribution of Cars by Gear", col = c("tomato", "skyblue", "lightgreen"))

avg_mpg_by_cyl <- tapply(mtcars$mpg, mtcars$cyl, mean)

barplot(avg_mpg_by_cyl, main = "Average MPG by Cylinder Count", xlab = "Number of Cylinders", ylab = "Average MPG", col = "purple")
