R.home("bin")

#n - no. of independent Bernouli trials p - success, q - failure
#binomial expression
#P(X = x) = xc x p^x q^n-x

#4 coins are tossed simultaneously, what is the probability of getting
#a. 2 heads
#b. at least 2 heads
#c. at most 2 heads
#d. expectation of x
#e. variance of x
#f. visualize the probability distribution

n = 4
p = 1/2
q = 1/2

#a. P(X = x) = 4cx (1/2)^4
#P(x = 2) = 0.375
P1 = dbinom(2, n, p)
print(P1)

#b. P(x>=2) = P(x = 2) + P(x = 3) + P(X = 4)
#P(x>=2) = 0.687
P2 = sum(dbinom(2:4, n, p))
print(P2)
1 - pbinom(1, n, p) #cumulative

#c. P(x <= 2) = P(x = 0) + P(x = 1) + P(X = 2)
#P(x<=2) = 0.687
P3 = sum(dbinom(0:2, n, p))
print(P3)
pbinom(2, n, p)

#d. E(x) = summation from - inf to inf xP(x)
x = 0:n
Px = dbinom(x, n, p)
Ex = weighted.mean(x, Px)
print(Ex)

#e. var(x) = E(x^2)^2 - E(x)^2
weighted.mean(x*x, Px) - (Ex*Ex)

#f. visualization
plot(x, Px, type = "l", xlab = "Instances", ylab = "Probability", col = "blue")

#Poisson Distribution
#P(X = x) = e^-lambda lambda^x/x1 0 , x = 0, 1, 2, ...
#lambda = np

#A manufacturer of pins knows that 2 percent of his products are defective. If he sells pins in boxes of 20 in a consignment of a thousand boxes, then find the number of boxes containing
#a. at least 2 defective
#b. exactly 2
#c. at most 2 defective
#d. expectation
#e. variance
#f. visualization

n = 20
p = 0.2
lambda = 0.4

#a.
D1 = dpois(2, lambda)
print(D1)
D1 = round(1000*D1)
print(D1)

#b.
D2 = round(1000*D1)
print(D2)
D2 = round(1000*D2)
print(D2)


#c.
D3 = sum(dpois(2:n, lambda))
print(D3)
1 - ppois(1, lambda)
D3 = round(1000*D3)
print(D3)


#d.
x = 0:n
Dx = dpois(x,lambda)
Ex = weighted.mean(x, Dx)
print(Ex)

#e.
weighted.mean(x*x, Dx) - (Ex*Ex)

#f. 
plot(x, Dx, type = "l", xlab = "Instances", ylab = "Probability", col = "blue")
