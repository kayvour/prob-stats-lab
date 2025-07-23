R.home("bin")

empid = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110)
empid

age = c(25, 30, 28, 35, 40, 26, 32, 29, 31, 27)
age

gender = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1)
gender

status = c(1, 2, 1, 1, 2, 1, 2, 2, 1, 1)
status

empinfo = data.frame(empid, age, gender, status)
empinfo

#factor num to char
empinfo$gender = factor(empinfo$gender, labels = c("male", "female"))
empinfo$gender

empinfo$status = factor(empinfo$status, labels = c("staff", "faculty"))
empinfo$status

#filtering data - subset
#extracting female data
female = subset(empinfo, empinfo$gender == "female")
female

male = subset(empinfo, empinfo$gender == "male")
male

summary(empinfo)

summary(male)
summary(female)
summary(gender)

#frequency table - table - one way
table1 = table(empinfo$gender)
table1

table2 = table(empinfo$status)
table2

#two way
table3 = table(empinfo$gender, empinfo$status)
table3

#scatter plot
plot(empinfo)
plot(empinfo$age, type = "l", main = "Age of Employees", xlab = "empid", ylab = "age", col = "#FF5733") #l = line

#pie chart
pie(table1, col = c("skyblue", "lightpink"), main = "Gender Distribution")

#bar graph
colors = c("blue", "red")
barplot(table3, beside = TRUE, col = colors, xlim = c(1, 15), ylim = c(0, max(table3) + 2), main = "Gender vs Status", xlab = "Status", ylab = "Count")
legend("topright", legend = rownames(table3), fill = colors)

#boxplot
boxplot(age ~ gender, data = empinfo, col = c("skyblue", "lightpink"), main = "Boxplot of Age by Gender", xlab = "Gender", ylab = "Age")

#Q.1 Create vectors for roll no, age, dept and gender for 10 students Combine them into a data frame called studentinfo.
#Convert the numeric gender vector into a factor with labels male and female. Also, convert the dept into a factor with labels like ECE, CSE, etc
#Print the data frame and view how it changes after labeling
#From the data frame, extract all male students and store them in a new object called male_students.
#Extract all students from CSE dept
#Use summary function to get age statistics of all students, only female students, only ECE students
#Create a table to count how many students are there in each dept
#Create a two way table to show the number of male and female students in each dept
#Draw a pie chart to show the gender distribution of the students
#Use a bar plot to show how many male and female students are there in each department. Include a legend.
#Create a box plot comparing the age distribution between male and female students
#Use a line plot to show the ages of students using their rollno as reference

rollno = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110)
age = c(18, 19, 20, 18, 21, 19, 20, 18, 22, 19)
dept <- c(1, 2, 3, 4, 2, 1, 2, 3, 4, 1)  # 1 = CSE, 2 = ECE, 3 = MECH, 4 = CIVIL
gender = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1)    # 1 = Male, 0 = Female

studentinfo = data.frame(rollno, age, dept, gender)
studentinfo

studentinfo$dept = factor(studentinfo$dept, labels = c("CSE", "ECE", "MECH", "CIVIL"))
studentinfo$dept
studentinfo$gender = factor(studentinfo$gender, labels = c("Male", "Female"))
studentinfo$gender
studentinfo

male_student = subset(studentinfo, studentinfo$gender == "Male")
male_student

cse = subset(studentinfo, studentinfo$dept == "CSE")
cse

summary(studentinfo)
summary(subset(studentinfo, studentinfo$gender == "Female"))
summary(subset(studentinfo, studentinfo$dept == "ECE"))

table_dept = table(studentinfo$dept)
print(table_dept)

table_gender_dept = table(studentinfo$dept, studentinfo$gender)
print(table_gender_dept)

pie(table(studentinfo$gender), main = "Gender Distribution", col = c("skyblue", "pink"))

barplot(table(studentinfo$gender), beside = T, col = c("skyblue", "pink"), main = "Gender VS Department", xlab = "Department", ylab = "Count")
legend("topright", legend = colnames(table_gender_dept), fill = c("skyblue", "pink"))

boxplot(age ~ gender, data = studentinfo, main = "Age Distribution by Gender", xlab = "Gender", ylab = "Age", col = c("skyblue", "pink"))

plot(studentinfo$rollno, studentinfo$age, type = "l", main = "Age of Students by Roll Number", xlab = "Roll Number", ylab = "Age", col = "darkgreen", pch = 16)
