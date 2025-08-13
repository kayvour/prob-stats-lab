#Q.1 Using the inbuilt mtcars data set in R, import it and consider the variables mpg (dependent), displacement and hp (independent)
#Write code to fit multiple linear reg model, display summary and visualize

data(mtcars)
Regmodel2 = lm(mtcars$mpg ~ (mtcars$disp + mtcars$hp))
print(Regmodel2)

summary(Regmodel2)

library(scatterplot3d)
graph = scatterplot3d(mtcars$disp, mtcars$hp, mtcars$mpg)
graph $ plane3d(Regmodel2)


#Q.2 Create a data frame with sales (dependent), advertising and price (independent) for 8 products
# Fit a multiple linear reg model and interpret the regression coefficients

sales_data = data.frame(sales = c(110, 80, 70, 120, 150, 90, 70, 120), advertising = c(30, 40, 20, 50, 60, 40, 20, 60), price = c(11, 10, 7, 15, 19, 12, 8, 14))
Regmodel3 = lm(sales_data$sales ~ (sales_data$advertising + sales_data$price))
print(Regmodel3)

summary(Regmodel3)

library(scatterplot3d)
graph = scatterplot3d(sales_data$advertising, sales_data$price, sales_data$sales)
graph $ plane3d(Regmodel3)
