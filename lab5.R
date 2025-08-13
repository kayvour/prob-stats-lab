R.home("bin")

xx = c(1, 2, 3, 4, 5, 6 , 7)
yy = c(4, 3, 1, 2, 6, 5, 7)

#cor(xx, yy),  Karl Pearson
#for rank correlation, Spearman

cor.test(xx, yy, method = "spearman")

#multiple correlation regression coefficient
# y = ax1 + bx2 + c

#The sales of a product in lakhs of rupees(Y) is expected to be influenced by two variable, namely the advertising expenditure (X1) and the number of salespersons (X2).
#Sample data of each region has given the following observations
y = c(110, 80, 70, 120, 150, 90, 70, 120) #Sale of a product
x1 = c(30, 40, 20, 50, 60, 40, 20, 60) #Advert. expenditure
x2 = c(11, 10, 7, 15, 19, 12, 8, 14) #No. of sales persons

Regmodel = lm(y ~ x1 + x2)
print(Regmodel)
#intercept is c, x1 is a and x2 is b

summary(Regmodel)

library(scatterplot3d)
graph = scatterplot3d(x1, x2, y)
graph $ plane3d(Regmodel)

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
