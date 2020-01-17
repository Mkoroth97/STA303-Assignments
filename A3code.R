

##Q1 (a)

M <- length(like[sex=="Male"])  #total numbers of males 
F <- length(like[sex == "Female"]) #otal number of females

GamerM<-like[like== "1" & sex=="Male" ] #male gamers
NonGamerM <- like[like=="0" & sex=="Male"] #male non gamers
GamerF<- like[like=="1" & sex=="Female"] #female gamers
NonGamerF <- like[like=="1" & sex=="Female"] #female non gamers

GM <-length(like[like== "1" & sex=="Male"])   #proporion of gamer males 
NGM <-length(like[like== "0" & sex=="Male"])  #proportion of non gamer males
GF <-length(like[like== "1" & sex=="Female"]) #proportaion of female gamer
NGF<-length(like[like== "0" & sex=="Female"]) #proportion of female no gamer

table<- matrix(c(GM, NGM, GF, NGF), nrow=2,byrow=TRUE)
dimnames(table)<- list(c("Male", "Female"), c("Likes", "Does not Like"))
names(dimnames(table))<- c("Sex", "Video Games")
table

chisq.test(table, correct=FALSE)
prop.test(table, correct=FALSE)

##Q1(b)

# A+
GamerMAP<- like[like== "1" & sex=="Male" & grade =="1" ] #gamer males who get A+
NonGamerMAP<- like[like== "0" & sex=="Male" & grade =="1"]  #gamer males who  get A+
GamerFAP<- like[like=="1" & sex=="Female" & grade=="1"]  #gamer females who get A+
NonGamerFAP<- like[like=="0" & sex=="Female" & grade=="1"] #nongamer females who get A+

MAP <- length(GamerMAP) + length(NonGamerMAP)
FAP <- length(GamerFAP) + length(NonGamerFAP)

GMAP<- length(like[like== "1" & sex=="Male" & grade =="1" ])
NGMAP<-length(like[like== "0" & sex=="Male" & grade =="1" ])
GFAP<-length(like[like=="1" & sex=="Female" & grade=="1"])
NGFAP<-length(like[like=="0" & sex=="Female" & grade=="1"])


table1<-matrix(c(GMAP, NGMAP, GFAP, NGFAP), nrow=2,byrow=TRUE)
dimnames(table1)<-list(c("Male", "Female"), c("Likes", "Does not Like"))
names(dimnames(table1)) <-c("Sex", "Video Games")
table1

chisq.test(table1, correct=FALSE)
prop.test(table1, correct=FALSE)

#with no A+
GamerMNA <- like[like== "1" & sex=="Male" & grade =="0" ]   #gamer males who dont get A+
NonGamerMNA <- like[like== "0" & sex=="Male" & grade =="0" ] #non gamer males who dont get A+
GamerFNA<-like[like=="1" & sex=="Female" & grade=="0"] #gamer females who dont get A+
NonGamerFNA<-like[like=="0" & sex=="Female" & grade=="0"] #nongamer females who dont get A+

GMNA<-length(like[like== "1" & sex=="Male" & grade =="0" ])
NGMNA<-length(like[like== "0" & sex=="Male" & grade =="0" ])
GFNA<- length(like[like=="1" & sex=="Female" & grade=="0"])
NGFNA<-length(like[like=="0" & sex=="Female" & grade=="0"])


MNAP <- length(GamerMNAP) + length(NonGamerMNA)
FNAP <- length(GamerFNAP) + length(NonGamerFNA)

table2<-matrix(c(GMNA, NGMNA, GFNA, NGFNA), nrow=2,byrow=TRUE)
dimnames(table2)<-list(c("Male", "Female"), c("Likes", "Does not Like"))
names(dimnames(table2))<-c("Sex", "Video Games")
table2

chisq.test(table2, correct=FALSE)
prop.test(table2, correct=FALSE)


##Q2

#Model 2.1
mod1 <-glm(like~ sex + grade + sex*grade, data= data, family=binomial)
summary(mod1)

#Model 2.2
mod2 <- glm(like ~ sex + grade, data=data, family= binomial)
summary(mod2)

##(a)
##model 1 
## Y = 0.1574 -1.7668B1ISex -0.0185B2IGrade -0.5231B3ISexGrade + ei

#tests: Wald tests and LRT

library(aod)
wald.test(Sigma=vcov(mod1), b=coef(mod1), Terms=1:3) 
wald.test(Sigma=vcov(mod2), b=coef(mod2), Terms=1:2)

#anova(mod1, test="Chisq")
#anova(mod2, test="Chisq")   not sure

anova(mod1, mod2,  test="LRT") #lrt pval=0.3264 which means that the interaction doesnt improve the model in addition to sex and grade



##Q3

###counts




counts <- c(NGFAP, NGFNA, NGMAP, NGMNA, GFAP, GFNA, GMAP, GMNA)
likes <- c("no", "no", "no", "no", "yes", "yes", "yes", "yes")
sexes <- c("female", "female", "male","male", "female", "female", "male", "male")
grades <- c("A+", "not A+", "A+", "not A+", "A+", "not A+", "A+","not A+")

table4<- data.frame(counts, likes, sexes, grades, stringsAsFactors = FALSE) ##table from the A3 sheet

##Q3(a)



#model 3.1 with three way interaction
mod3 <-glm(counts ~ likes + sexes + grades + likes*sexes + likes*grades + sexes*grades + likes*sexes*grades, family= poisson, data=table4)
summary(mod3)

#model 3.2 without three way interaction
mod4<-glm(counts ~ likes + sexes + grades + likes*sexes + likes*grades + sexes*grades, family= poisson, data=table4)
summary(mod4)

##Q3(b)
##i) Deviance

anova(mod3, mod4, test="LRT")

##ii) Wald test
wald.test(Sigma=vcov(mod3), b=coef(mod3), Terms=3:7)
wald.test(Sigma=vcov(mod4), b=coef(mod4), Terms=3:6)




