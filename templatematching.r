#install.packages("sos")
library("sos")
#install.packages("RWeka")
library("RWeka")
library("arules")
x<-read.arff(file.choose()) 

#------------------------------------------------------------------------
#association between school and higher(wants to take higher education)

rules <- apriori(x, parameter= list(supp=0.2, conf=0.5),
                 appearance=list
                 (lhs= c("school=GP","school=MS"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "higher=" ) )


#association between school and reason to choose school

rules <- apriori(x, parameter= list(supp=0.2, conf=0.2),
                 appearance=list
                 (lhs= c("school=GP","school=MS"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "reason=" ) )


#association between school and freetime

rules <- apriori(x, parameter= list(supp=0.1, conf=0.2),
                 appearance=list
                 (lhs= c("school=GP","school=MS"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "freetime=" ) )


#association between gender and free time

rules <- apriori(x, parameter= list(supp=0.1, conf=0.2),
                 appearance=list
                 (lhs= c("sex=F","sex=M"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "freetime=" ) )

#association between gender and studytime

rules <- apriori(x, parameter= list(supp=0.1, conf=0.2),
                 appearance=list
                 (lhs= c("sex=F","sex=M"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "studytime=" ) )


#association between gender and paid  for extra classes

rules <- apriori(x, parameter= list(supp=0.1, conf=0.2),
                 appearance=list
                 (lhs= c("sex=F","sex=M"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "paid=" ) )


#association between sex and Walc

rules <- apriori(x, parameter= list(supp=0.1, conf=0.26),
                 appearance=list
                 (lhs= c("sex=F","sex=M"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "Walc=" ) )


#association between sex and Dalc

rules <- apriori(x, parameter= list(supp=0.1, conf=0.26),
                 appearance=list
                 (lhs= c("sex=F","sex=M"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "Dalc=" ) )


#association between famsize and pstatus

rules <- apriori(x, parameter= list(supp=0.1, conf=0.3),
                 appearance=list
                 (lhs= c("famsize=LE3","famsize=GT3"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "Pstatus=" ) )




rules <- apriori(x, parameter= list(supp=0.1, conf=0.1),
                 appearance=list(lhs=c("health=1","health=5","health=2","health=3","health=4"
                                  ), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "G3=" ) )

rules <- apriori(x,parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
rules <- apriori(x, parameter= list(supp=0.2, conf=0.3),appearance=list(lhs=c("Medu=1","Medu=0","Medu=2","Medu=3","Medu=4","Fedu=1","Fedu=0","Fedu=2","Fedu=3","Fedu=4"), default="rhs"))
inspect( subset( rules, subset = rhs %pin% "Dalc=" ) )