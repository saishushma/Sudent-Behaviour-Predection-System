x<-read.csv(file.choose())
head(x)

#association between grades and health
aggregate( G1 ~ health, data = x, FUN = mean)

boxplot (x $ G1 ~ x $ health, main = "Distribution of grades (first period)
         for different levels of health status", xlab = "health status",
         ylab = "grades first period", col = "darkorange")

aggregate( G2 ~ health, data = x, FUN = mean)

boxplot (x $ G2 ~ x $ health, main = "Distribution of grades (first period)
         for different levels of health status", xlab = "health status",
         ylab = "grades first period", col = "darkorange")

aggregate( G3 ~ health, data = x, FUN = mean)

boxplot (x $ G3 ~ x $ health, main = "Distribution of grades (first period)
         for different levels of health status", xlab = "health status",
         ylab = "grades first period", col = "darkorange")



#------------------------------------------------------------------------------

#corelation between health and grade

cor.test (x = x $ health, y = x $ G1)

cor.test (x = x $ health, y = x $ G2)

cor.test (x = x $ health, y = x $ G3)


#-----------------------------------------------------------------------------

#differnce in grades of female and male

t.test (x $ G1 ~ x$ sex, alternative = "two.sided")

t.test (x $ G2 ~ x$ sex, alternative = "two.sided")

t.test (x $ G3 ~ x$ sex, alternative = "two.sided")

#-------------------------------------------------------------------------------
#scatter plot between age and grades of period 1

plot (x = x $ age, y = x $ G1,
      xlab = "age",
      ylab = "grades first period",
      main = "Scatterplot showing the association
     between students' age
     and their grades in first period"
)
with (subset(x, age < "17"), 
      points(age, G1,
             pch = 16, col = "lightsalmon1") )
with (subset(x, age >= "17"),
      points (age, G1,
              pch = 16, col = "green"))
legend("topright",
       c("age < 17", "age >= 17"),
       pch = 16,
       col = c("lightsalmon1", "green")
)
abline(a = 17.97725, 
       b = -0.39286)


#---------------------------------------------------------------------------------
#association between absences and why students choose this school

par(mfrow = c(2, 4)) 
x.hist <- unique (x $ reason)
for (reason.i in x.hist) {
  data.temp <- subset(x,
                      reason == reason.i)
  hist(data.temp$absences,
       main = paste ("Amount of Absences 
                     for following
                     reason:",
                     reason.i),
       xlab = "Absences")
}


fit <- aov (formula = x $ absences ~ x $ reason)
summary (fit)


#---------------------------------------------------------------------------------
#association btween study time and grade

hist (x $ studytime,
     main = "Students' studytime",
     xlab = "studytime")
abline(v = median(x $ studytime), 
       lty = 1, col = "red")
abline(v = mean(x $ studytime), 
       lty = 2, col = "blue")
text(mean(x $ studytime, na.rm = T), 300,
     labels = paste("
Mean\n", round(mean(x $ studytime, na.rm = T), 2), sep = ""  ),
     adj = 0,
     pos = 4,
     col = "blue"
)
text(median(x $ studytime, na.rm = T), 250,
     labels = paste("Median\n", round(median(x $ studytime, na.rm = T), 2), sep = ""  ),
     adj = 0,
     pos = 4,
     col = "red"
)


aov.study <- aov (formula = x $ G3 ~ x $ studytime)
summary (aov.study)
