R.home("bin")

# large sample test

# Suppose the mean weight of king penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. Assume the population sd is 2.5kg.
# At 0.05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year?
mu = 15.4
xbar = 14.6
sigma = 2.5
n = 35
alpha = 0.05

# test statistic - type 1
z = (xbar - mu)/(sigma/sqrt(n))
z

# two tailed test
z_critical = qnorm(1 - alpha/2) # alpha - los
z_critical

# decision
if (abs(z) > z_critical){
  cat("Reject the null hypothesis : Mean penguin weight differs from last year")
} else{
  cat("Fail to reject the null hypothesis: No significant difference in mean from last year")
}

# The fatality rate of typhoid patients is believed to be 17.26%. In a certain year 640 patients suffering from typhoid were treated in a metropolitan hospital and only 63 patients died.
# Can you consider the hospital efficient?
P = 0.1726 # population proportion
x = 63
n = 640
alpha = 0.05

# sample proportion
p = x/n

# test statistic - single proportion test - type 3
z = (p - P) / sqrt(P * (1 - P)/n)
z

# H1 refers to efficiency ie lower fatality rate
# Critical value - left tailed test
z_critical = qnorm(alpha)
z_critical

if(z < z_critical){
  cat("Reject H0: The hospital is efficient")
} else{
  cat("Fail to reject H0: No evidence of efficiency")
}

# In a random sample of size 500, the mean is found to be 20. In another independent sample of size 400, the mean is 15.
# Could the samples have been drawn from the same population with sd 4?
n1 = 500
n2 = 400
xbar1 = 20
xbar2 = 15
sigma = 4
alpha = 0.05

# type 2 - diff of 2 mean test
z = (xbar1 - xbar2)/sqrt((sigma * sigma/n1) + (sigma * sigma/n2))
z

z_critical = qnorm(1 - alpha/2)
z_critical

if(abs(z) > z_critical){
  cat("Reject H0: No significant difference")
} else{
  cat("Fail to reject H0: There is a significant difference between the samples")
}

# In a large city A, 20% of a random sample of 950 school boys had a slight physical defect. In another large city B, 18.5% of a random sample of 1600 school boys had the same defect.
# Is the difference between the proportions significant?
p1 = 0.20
n1 = 900
p2 = 0.185
n2 = 1600
alpha = 0.05
# P = combined proportions
P = (p1*n1 + p2*n2)/(n1 + n2)
P
Q = 1 - P

# type 4 - diff of 2 proportions
z = (p1 - p2)/sqrt(P*Q*((1/n1) + (1/n2)))
z

# counts
x1 = round(p1 * n1)
x2 = round(p2 * n2)

# pooled proportion - population
p_pool = (x1 + x2)/(n1 + n2)

# test statistic - 2 tailed
z = (p1 - p2)/sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
z

z_critical = qnorm(1 - alpha/2)
z_critical

if(abs(z) > z_critical){
  cat("H0 rejected: Proportions difference is significant")
} else{
  cat("Fail to reject: Proportions difference is not significant")
}
