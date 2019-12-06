##### assignment 3 - geoffrey #####


### author: Lisa Holzer ###
### date: 6th Dec 2019 ###

#notes:
#4groups in a one-way fixed effects analysis of variance (ANOVA) design.
#expected standardized means: [-.5, 0, 0, .5], SD = 1
#Cohen's f = 0.35
#determine how many participants you need to find a statistically significant effect 50-90% of the time (i.e., 50-90% power)


library(stats)

nSims<-100
nSub<-50
df1 = 4 - 1

pb <-txtProgressBar(min = 0, max = 100, initial = 0, style = 3)
for (n in 10:nSub) {# loop over the simulations for each sample size of interest
  df2 = 4 * n - 4
  count = 0
  Lambda = 4 * n * 0.35^2
  F_crit = qf(0.95, df1, df2)
  power = 1-pf(F_crit, df1, df2, Lambda)
  print(n)
  print(power)
  
  for (i in 1:nSims) {# for each sample size loop through number of simulations. Simulate the data and do the planned analysis. Then query the p value-if the p value is less than .05 increment success by 1.
    v1 = rnorm(n = n, mean = -0.5,sd = 1) 
    v2 = rnorm(n = n, mean = 0,sd = 1)
    v3 = rnorm(n = n, mean = 0,sd = 1)
    v4 = rnorm(n = n, mean = 0.5,sd = 1)
    mean_v1 = mean(v1)
    mean_v2 = mean(v2)
    mean_v3 = mean(v3)
    mean_v4 = mean(v4)
    group_mean = (mean(v1)+mean(v2)+mean(v3)+mean(v4))/4
    between_group_var = ((mean_v1- group_mean)**2 + (mean_v2-group_mean)**2 + (mean_v3-group_mean)**2 + (mean_v4-group_mean)**2)**0.5
    Lambda = between_group_var*between_group_var*n*4
    F_val = (n * df2/((n - 1) * df1) * Cohens_F^2/(var(v1) + var(v2) + var(v3) + var(v4)))
    
    if (F_val > F_crit) { 
      count = count + 1
      }
    if (i == 100)
    {
      print(count/100)
    }
  }
}
