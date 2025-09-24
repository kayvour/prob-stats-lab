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
