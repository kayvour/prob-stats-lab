#Q.1 Find the probability in tossing a fair coin 5 times
n = 5
p = 1/2
q = 1 - p

#a. 3 heads
P1 = dbinom(3, n, p)
print(P1)

#b. 3 tails and 2 heads
P2 = dbinom(3, n, q)
print(P2)

#c. at least 1 head
P3 = 1 - dbinom(0, n, p)
print(P3)

#d. not more than 1 tail
P4 = dbinom(0, n, q) + dbinom(1, n, q)
print(P4)

#Find mean, variance and plot the distribution
Ex = n * p
print(Ex)
Varx = n * p * q
print(Varx)

x = 0:n
Px = dbinom(x, n, p)
plot(x, Px, type = "l", lwd = 2, col = "red", xlab = "Number of Heads", ylab = "Probability")

#Q.2 An insurance company found that only 0.01% of the population is involved in a certain type of accident each year. If a thousand policy holders were randomly selected, what is the probability that not more than 2 of its clients are involved in such an accident next year?
n = 1000
p = 0.0001
lambda = n * p

# not more than 2 accidents
P1 = ppois(2, lambda)
print(P1)

# exactly 2 accidents
P2 = dpois(2, lambda)
print(P2)

# mean and variance
Ex = lambda
print(Ex)
Varx = lambda
print(Varx)

x = 0:5
Px = dpois(x, lambda)
plot(x, Px, type = "l", lwd = 2, col = "blue", xlab = "Number of Accidents", ylab = "Probability")
