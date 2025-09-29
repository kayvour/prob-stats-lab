# 1. A factory claims that the average lifetime of its LED bulbs is 2,000 hours. A random sample of 120 bulbs shows a mean lifetime of 1,950 hours with a standard deviation of 150 hours. At the 1% level of significance, test whether the company’s claim is valid.
mu = 2000      # claimed mean
xbar = 1950    # sample mean
s = 150        # sample standard deviation
n = 120        # sample size
alpha = 0.01

# test statistic
z = (xbar - mu)/(s/sqrt(n))
z

# two-tailed test
z_critical = qnorm(1 - alpha/2)
z_critical

if(abs(z) > z_critical){
  cat("Reject H0: The claim of mean = 2000 hrs is not valid\n")
} else {
  cat("Fail to reject H0: The claim is valid\n")
}

# 2. A random sample of 400 students in college A has a mean score of 55 in an aptitude test with a standard deviation of 10. Another independent sample of 500 students in college B has a mean score of 57 with a standard deviation of 12. Can we conclude at the 5% significance level that students of college B perform better than those of college A?
n1 = 400
xbar1 = 55
s1 = 10

n2 = 500
xbar2 = 57
s2 = 12
alpha = 0.05

# test statistic
z = (xbar1 - xbar2)/sqrt((s1^2/n1) + (s2^2/n2))
z

# one-tailed test (H1: mean B > mean A)
z_critical = qnorm(1 - alpha)
z_critical

if(z < -z_critical){
  cat("Reject H0: Students of college B perform better\n")
} else {
  cat("Fail to reject H0: No evidence that college B students perform better\n")
}

# 3. A manufacturer claims that 95% of the items produced are defect-free. Out of 600 items chosen at random, 570 items are found to be defect-free. Test at the 5% level of significance whether the claim is true.
P = 0.95     # claimed proportion
n = 600
x = 570      # defect-free items
alpha = 0.05

# sample proportion
p = x/n

# test statistic
z = (p - P)/sqrt(P*(1-P)/n)
z

# two-tailed test
z_critical = qnorm(1 - alpha/2)
z_critical

if(abs(z) > z_critical){
  cat("Reject H0: The claim that 95% are defect-free is not true\n")
} else {
  cat("Fail to reject H0: The claim is valid\n")
}

# 4. A survey of 1,000 families in rural areas showed that 400 families own a television. A similar survey of 1,200 families in urban areas showed that 700 families own a television. Test whether there is a significant difference between the proportions of families owning a TV in rural and urban areas at the 1% level of significance.
n1 = 1000
x1 = 400
p1 = x1/n1

n2 = 1200
x2 = 700
p2 = x2/n2
alpha = 0.01

# pooled proportion
p_pool = (x1 + x2)/(n1 + n2)
q_pool = 1 - p_pool

# test statistic
z = (p1 - p2)/sqrt(p_pool*q_pool*(1/n1 + 1/n2))
z

# two-tailed test
z_critical = qnorm(1 - alpha/2)
z_critical

if(abs(z) > z_critical){
  cat("Reject H0: Significant difference in TV ownership between rural and urban\n")
} else {
  cat("Fail to reject H0: No significant difference\n")
}
