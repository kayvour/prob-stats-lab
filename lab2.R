R.home("bin")

1 : 10
seq(1, 10, by = 2)
x = c(1 : 10)
sum(x)
mean(x)

y = c(1, 2, NA)
sum(y, na.rm = TRUE)

rep(5, times = 3)
rep(1 : 10, times = 2)

#generate table
#data.frame(var1, var2, ...)
EmpId = c(101, 102, 103, 104)
Name = c("x", "y", "z", "w")
Age = c(30, 40, 50, 55)
Department = c("Mathematics", "Physics", "Chemistry", "CS")
Salary = c(80000, 85000, 50000, 55000)

Employee_data = data.frame(EmpId, Name, Age, Department, Salary)
print(Employee_data)

Employee_data[1, ]
Employee_data[ , 3]
Employee_data[1, 2]

Employee_data $ Bonus = c(20000, 30000, 10000, 15000)
print(Employee_data)

#Q.1 Create a table with the following attributes:
#Name, RollNo, Age and Marks for 10 students
#Extract the name column from the table
#Find the averages of the marks

#Q.2 Create a table of 5 products with ProductID, ProductName, Price and Quantity
#Calculate Total Cost = Price x Quantity and add it as a new column

StudName = c("Aarav", "Diya", "Rohan", "Ishita", "Kabir", "Anaya", "Vivaan", "Meera", "Aditya", "Sneha")
RollNo = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Age = c(17, 16, 17, 17, 18, 17, 18, 16, 17, 18)
Marks = c(85, 92, 78, 88, 69, 95, 73, 81, 90, 87)
Student_data = data.frame(StudName, RollNo, Age, Marks)

print(Student_data $ StudName)
print(mean(Student_data $ Marks))

ProductId = c("P101", "P102", "P103", "P104", "P105")
ProductName = c("Laptop", "Smartphone", "Headphones", "Keyboard", "Monitor")
Price = c(60000, 25000, 2000, 1500, 12000)
Quantity = c(10, 25, 50, 40, 20)
Product_data = data.frame(ProductId, ProductName, Price, Quantity)

Product_data $ TotalCost = Product_data $ Price * Product_data $ Quantity
print(Product_data)

#matrix
M = matrix(1 : 9, nrow = 3, ncol = 3, byrow = TRUE)
print(M)

#packages
library(readxl)
Book1 <- read_excel("C:/Users/vaidy/Documents/socrates/Philosophy_Observances.xlsx")
View(Book1)

#data
data(mtcars)
View(mtcars)
