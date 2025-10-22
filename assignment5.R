R.home("bin")

#1
sample1 = c(19,17,15,21,16,18,16,14)
sample2 = c(15,14,15,19,15,18,16,20)

# Two-sample t-test assuming equal variances
t_result <- t.test(sample1, sample2, var.equal = TRUE)
t_result

cv = t_result$statistic
df = t_result$parameter
alpha = 0.05
tv = qt(1 - alpha/2, df)  # critical value at 5% LOS

# Conclusion
if(abs(cv) < tv) {
  conclusion = "The difference between the sample means is not significant."
} else {
  conclusion = "The difference between the sample means is significant."
}

conclusion

#2

test1 = c(19,17,15,21,16,18,16,14,19,20)  # before
test2 = c(15,14,15,19,15,18,16,20,22,19)  # after

# Perform paired t-test
t_result = t.test(test1, test2, paired = TRUE)
t_result

cv = t_result$statistic
df = length(test1) - 1
alpha = 0.05
tv = qt(1 - alpha, df)

if(cv < tv) {
  conclusion = "No significant improvement (coaching failed)."
} else {
  conclusion = "Significant improvement (coaching successful)."
}

conclusion

#3

sample1 = c(19,17,15,21,16,18,16,14)
sample2 = c(15,14,15,19,15,18,16,20)
alpha = 0.05

f_result = var.test(sample1, sample2)
f_result

cv = f_result$statistic
df1 = length(sample1) - 1
df2 = length(sample2) - 1
tv = qf(1 - alpha, df1, df2)

if(cv <= tv) {
  conclusion = "No significant difference in variances."
} else {
  conclusion = "Significant difference in variances."
}
conclusion

#4

n = 5
N = 256
p = 0.5
alpha = 0.05

x = 0:5
observed = c(5,35,75,84,45,12)
expected = dbinom(x, n, p) * N

# Chi-square statistic
cv = sum((observed - expected)^2 / expected)
df = n - 1
tv = qchisq(1 - alpha, df)

if(cv < tv) {
  conclusion = "Coins appear to be unbiased (fit is good)."
} else {
  conclusion = "Coins appear to be biased (fit is poor)."
}
conclusion

#5

data = matrix(c(69,51,81,20,35,44), ncol=2, byrow=TRUE)
alpha = 0.05

# Perform chi-square test
chi_result = chisq.test(data)
chi_result

cv = chi_result$statistic
df = (nrow(data)-1)*(ncol(data)-1)
tv = qchisq(1 - alpha, df)

if(cv < tv) {
  conclusion = "Child’s condition is independent of house condition."
} else {
  conclusion = "Child’s condition is associated with house condition."
}
conclusion
