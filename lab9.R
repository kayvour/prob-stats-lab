#ANOVA - Analysis of variance
#One-way ANOVA - completely randomized design
#Two-way ANOVA - randomized block design
#Latin Square Cost - three-way ANOVA

# 1. A car rental agency, which uses 5 different brands of tires in the process of deciding the brand of tire to purchase as standard equipment for its fleet, finds that each of the 5 tires of each brand last the following number of kms (in thousands)
A = c(36, 37, 42, 38, 47)
B = c(46, 39, 35, 37, 43)
C = c(35, 42, 47, 43, 38)
D = c(45, 36, 39, 35, 32)
E = c(41, 39, 37, 35, 38)

#Test the hypothesis that the five brands have almost the same avg life

#null hypothesis: all brands have the same avg life
#alt hypothesis: at least one brand has a diff mean life

brand = factor(rep(c("A", "B", "C", "D", "E"), each = 5))
life = c(A, B, C, D, E)
data = data.frame(brand, life)

#one-way ANOVA
result = aov(life ~ brand, data = data)
summary(result)

p_value = summary(result)[[1]][["Pr(>F)"]][1]
p_value

if(p_value < 0.05){
  cat("Reject the null hypothesis: there is a significant difference in the mean life of tires.\n")
} else {
  cat("Fail to reject the null hypothesis: there is no significant difference in the mean life of tires.\n")
}

# 2. The following table gives monthly sales (in thousand rs) for a certain firm in the 3 states by its 4 salesmen
sales = c(6, 5, 3, 8, 8, 9, 6, 5,  10, 7, 8, 7)
state = factor(rep(c("A", "B", "C"), each = 4))
salesman = factor(rep(c("I", "II", "III", "IV"), times = 3))

#Setup the analysis of variance table and test whether there is any significant diff b/w
#sales in the salesmen
#between sales in the states

data = data.frame(state, salesman, sales)
data

#two-way ANOVA w/o replication
result = aov(sales ~ state + salesman, data = data)
summary(result)

p_value_1 = summary(result)[[1]][["Pr(>F)"]][1]
p_value_1
p_value_2 = summary(result)[[1]][["Pr(>F)"]][2]
p_value_2

if(p_value_1 < 0.05){
  cat("Reject the null hypothesis: there is a significant difference in the averages sales between states\n")
} else {
  cat("Fail to reject the null hypothesis: there is no significant difference in the average sales between states\n")
}

if(p_value_2 < 0.05){
  cat("Reject the null hypothesis: there is a significant difference in the averages sales between salesmen\n")
} else {
  cat("Fail to reject the null hypothesis: there is no significant difference in the average sales between salesmen\n")
}
