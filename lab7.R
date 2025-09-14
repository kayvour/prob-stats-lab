# normal distribution - continuous random variable
# pdf of mean m and sd sigma is given by:
# f(x) = 1/(sigma x root(2 pi)) x e^(x - mu/2 sigma)^2, x and u b/w - infinity and infinity
# standard curve - bell

# standard normal distribution
# mean is 0, sd is 1
# mgf = e^(ut + t^2 sigma^2/2)

# Q.1 A normal distribution has my 20 and sd is sigma. The normal variate z = (x - mu)/sigma. P(15 <= x <= 40) = ?
# when x = 15, z = -0.5
# when x = 40, z = 2

# to find area under the given curve (bell)
# P(-0.5 <= Z <= 2) = P(-0.5 <= z <= 0) + P(0 <= z <= 2) i.e 1.6687

# Q.2 The average seasonal rainfall in a place is 60 inches with sd of 4 inches. What is the probability that in a year the rainfall in that place will be in between 20 and 24 inches?
# P(20 <= z <= 24) 
# x = 20, z = 1
# x = 24, z = 2
# P(1 <= z <= 2) = 0.13591

# Q.3 An electrical firm manufactures light bulbs that have a life before burnout that is normally distributed with mean 800 hours and a standard deviation of 40 hours.
# Find the probability that the bulb burns more than 834 hours. 
# The probability that a bulb burns between 778 and 834 hours.
# P(x > 834)
# x = 834, z = 0.85
# P(z > 0.85) = 

# Q.4 A company finds that the time taken by one of its engineers to complete or repair job as a normal distribution is 20 minutes and sd is 5 minutes.
# What proportion of jobs take, < 15 mins
# b/w 15 to 25 mins
# > 25 mins
# plot the distribution
# create result table
mu = 20
sigma = 5
x = seq(0, 40, 0.1) # random values for symmetry about mean
y = dnorm(x, mean = mu, sd = sigma) # normal distribution, P(x)

plot(x, y, type = "l", lwd = 2, xlab = "sequence", ylab = "distribution")
legend("topright", legend = c("x < 15", "15 ≤ x ≤ 25", "x > 25"), fill = c("pink", "lightblue", "lightgreen"))

# x < 15
x1 = seq(0, 15, 0.1)
y1 = dnorm(x1, mu, sigma)
polygon(c(0, x1, 15), c(0, y1, 0), col = "pink")

# 15 < x < 25
x2 = seq(15, 25, 0.1)
y2 = dnorm(x2, mu, sigma)
polygon(c(15, x2, 25), c(0, y2, 0), col = "lightblue")

# x > 25
x3 = seq(25, 40, 0.1)
y3 = dnorm(x3, mu, sigma)
polygon(c(25, x3, 40), c(0, y3, 0), col = "lightgreen")

data.frame(Interval = c("X < 15", "15 ≤ X ≤ 25", "X > 25"), Probability = c(pnorm(15, mu, sigma), pnorm(25, mu, sigma) - pnorm(15, mu, sigma), 1 - pnorm(25, mu, sigma)))

#Q. The savings bank account of a customer shows an average balance of rs 150 and a sd of rs 50. Assuming that the account balance is normally distributed
# What percentage of account is over rs 200
# Between rs 120 and rs 170
# Less than rs 75
# Plot and show the results in table format
mu = 150
sigma = 50
x = seq(0, 300, 1)
y = dnorm(x, mean = mu, sd = sigma)

plot(x, y, type = "l", lwd = 2, xlab = "Balance (Rs)", ylab = "Density")
legend("topright", legend = c("X > 200", "120 ≤ X ≤ 170", "X < 75"), fill = c("lightgreen", "lightblue", "pink"))

# X > 200
x1 = seq(200, 300, 0.1)
y1 = dnorm(x1, mu, sigma)
polygon(c(200, x1, 300), c(0, y1, 0), col = "lightgreen")

# 120 ≤ X ≤ 170
x2 = seq(120, 170, 0.1)
y2 = dnorm(x2, mu, sigma)
polygon(c(120, x2, 170), c(0, y2, 0), col = "lightblue")

# X < 75
x3 = seq(0, 75, 0.1)
y3 = dnorm(x3, mu, sigma)
polygon(c(0, x3, 75), c(0, y3, 0), col = "pink")

p1 = 1 - pnorm(200, mu, sigma)
p2 = pnorm(170, mu, sigma) - pnorm(120, mu, sigma)
p3 = pnorm(75, mu, sigma)

data.frame(Interval = c("X > 200", "120 ≤ X ≤ 170", "X < 75"), Probability = c(p1, p2, p3))
