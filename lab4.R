R.home("bin")

#covariance
#Cov(x, y) = E(xy) - E(x)E(y)
# = summation(xy)/x - x bar y bar where x bar = sum(x)/x and y bar = sum(y)/y

#Q. Calculate the covariance coefficient for the following types of orders treated as x and y (in inces)
x1 = c(65, 66, 67, 67, 68, 69, 70, 72)
y1 = c(67, 68, 65, 68, 72, 72, 69, 71)

sum((x1 - mean(x1)) * (y1 - mean(y1))) / (length(x1) - 1)
cov(x1, y1)

#correlation coefficient
#r = cov(x, y)/root((mean(x_sq)/x) - x bar sq) x root((mean(y_sq)/y) - y bar sq)
print(sum(x1 * y1))
print(sum(x1 * x1))
print(sum(y1 * y1))

r = cov(x1, y1) / sqrt((sum(x1 * x1) - length(x1) * mean(x1)^2) * (sum(y1 * y1) - length(y1) * mean(y1)^2))
print(r)
cor(x1, y1)

#simple linear regression model
#y = mx + c

#Equation of line of line of regression y on x
#y - y bar - b(yx)(x - x bar)

#x on y
#x - x bar = b(xy)(y - y bar)y) sum(x - x bar)(y - y bar)/
#where b(x)
#Q. From the following data, find the 2 regression equations, correlation coefficient, most likely marks in stats when marks in economics is 30
eco_marks = c(25, 28, 35,  32, 31, 36, 29, 38, 34, 32)
stat_marks = c(43, 46, 49, 41, 36, 32, 31, 30, 33, 39)

x_bar = mean(eco_marks)
y_bar = mean(stat_marks)

xmxb = eco_marks - x_bar
ymyb = stat_marks - y_bar
print(xmxb)
print(ymyb)

xmxb_sq = xmxb * xmxb
ymyb_sq = ymyb * ymyb
print(sum(xmxb_sq))
print(sum(ymyb_sq))

print(sum(eco_marks - x_bar))
print(sum(stat_marks - y_bar))

bxy = sum(xmxb * ymyb)/sum(ymyb_sq)
print(bxy)
byx = sum(xmxb * ymyb)/sum(xmxb_sq)
print(byx)
#Equation: y = -0.664x + 59.248, x = -0.233y + 40.87

linearmodel1 = lm(eco_marks ~ stat_marks)
print(linearmodel1)

linearmodel2 = lm(stat_marks ~ eco_marks)
print(linearmodel2)

cor(stat_marks, eco_marks) #negative correlation, inverse relation
#r = +/- root(b(yx) b(xy))

predict(linearmodel2, data.frame(eco_marks = 30))

plot(eco_marks, stat_marks)
abline(linearmodel2)

cars

a = cars$speed
b = cars$dist

ad = a - mean(a)
bd = b - mean(b)

byx = sum(ad * bd) / sum(ad^2)
bxy = sum(ad * bd) / sum(bd^2)

print(byx)
print(bxy)

car_model = lm(cars$dist ~ cars$speed)
print(car_model)

plot(cars$speed, cars$dist)
abline(car_model)
