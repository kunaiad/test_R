
install.packages('DBI')
install.packages('RJDBC')
library(RJDBC)
library(DBI)

driver <- JDBC('oracle.jdbc.driver.OracleDriver', 'ojdbc6.jar')
oracle_db <- dbConnect(driver, 'jdbc:oracle:thin:@//127.0.0.1:1521/orcl', 'sh', 'sh')

sales_query <- 'select * from sales'
sales_data <- dbGetQuery(oracle_db,sales_query)
nrow(sales_data)

emp_query <- 'select * from emp'
emp_data <- dbGetQuery(oracle_db, emp_query)

emp_data





boxplot(emp$sal, horizontal =TRUE)


emp_query <- 'select *
 from (select job, 
       sum_sal,
       avg(sum_sal) over() avg_sal
       from (select job,
             sum(sal) sum_sal
             from emp
             group by job))
where sum_sal > avg_sal'
emp_data <- dbGetQuery(oracle_db, emp_query)

emp_data



x <- boxplot(emp$sal)
x

graphics.off()

plot(emp$sal, col='blue')
par(new=T)
boxplot(emp$sal)


#q168

x<-c(19,rep(20,3),21)
table(x)
mean(x)


kp <-c(rep(19,3),rep(20,6),rep(21,3),145,147)
mean(x)
mean(kp)

install.packages("outliers")
library(outliers)
outlier(kp)

q170 <- boxplot(kp)
q170

median(kp)

co2 <- read.csv("co2.csv", fileEncoding="euc-kr",header = TRUE)
co2
max(co2$에너지.사용량.TJ.)
quantile(emp$sal[order(emp$sal)])

co2 <- na.omit(co2)
co2
#4분위수


boxplot(emp$sal,main='SALARY boxplot')
joy<-c(NA,'JOY',NA,NA,NA,'8000',NA,NA)
emp02<-rbind(emp,joy)
emp02$sal<-as.integer(emp02$sal)
boxplot(emp02$sal,man='SALARY boxplot with outlier')


co2
type_cnt <-aggregate(GHG~co2$에너지.사용량.TJ. ,co2, sum)
barplot(type_cnt$GHG, names.arg= type_cnt$category)

co2$에너지.사용량.TJ.<-as.integer(co2$에너지.사용량.TJ.)
boxplot(co2$에너지.사용량.TJ.,main='energe boxplot')
IQR(co2$GHG, na.rm=T)

colnames(co2[c(6,8)]) <-c('category','energy')


co2$energy<-as.integer(co2$energy)
max(co2$energy)
str(co2$energy)

co2$energy

plot(co2$에너지.사용량.TJ., col='blue')
par(new=T)
boxplot(co2$에너지.사용량.TJ.,main='energe boxplot')

summary(co2[c('energy')])



boxplot(emp$sal, horizontal =TRUE)

usedcars <-read.csv('usedcars.csv',header=T,stringsAsFactors=FALSE)

usedcars
hist(usedcars$price,main="Histogram of Used Car Prices",xlab="Price ($)")

nrow(usedcars)


x <- seq(-3, 3, length=200)
plot(x, dnorm(x, mean=0, sd=1), type='l', main="Normal distribution, X~N(0,1)")


x174 <- c (rep(c(1:8),c(4,6,4,4,3,2,1,1)))
mean(x174)
median(x174)

plot(x174,dnorm(x174,mean=mean(x174),sd=sd(x174)),type='l')
par(new=T)
boxplot(x174,main='정규분포 그래프',horizontal = T)


q175 <- c (rep(c(1,4,6,8,9,10,11,12),c(1,1,2,3,4,4,5,5)))
plot(q175,dnorm(q175,mean=mean(q175),sd=sd(q175)),type='l')
par(new=T)
boxplot(q175,main='정규분포 그래프',horizontal = T)

var(usedcars$price)

sd(usedcars$price)
q176 <- usedcars$price
q176
plot(q176,dnorm(q176,mean=mean(q176),sd=sd(q176)),type='l')

q176
x <- seq(from=5000,to=+15000,length.out = 5000)
y <- dnorm(x)
plot(x,y,type='l',ylab='Density')
abline(h=0)
region.x <- x[5000<= x & x <=15000]
region.y <- y[5000<= x & x <=15000]
region.x <- c(region.x[1],region.x,tail(region.x,1))
region.y <- c(0,region.y,0)
polygon(region.x,region.y,density=-1)


#177

table(emp$deptno)
table(usedcars$color)


color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
color_pct 
round(color_pct,digits=1)


job_tabl <- table(emp$job)
job_pct <- prop.table(job_tabl) * 100
job_pct
round(job_pct,digits = 1)


x<-c(rep(c(1,2,3,31,32,33),c(3,4,2,2,4,3)))
x
table(x)
which.max(x)

which.max(table(match(x)))
which.max(table(x))
mode(table(x))



plot(emp$comm, emp$sal)

plot(usedcars$mileage,usedcars$price)



usedcars$conservative <- usedcars$color %in% c('Black','Silver','White','Gray')
table(usedcars$conservative)

#2/3 이 보수적인 반면 , 1/3 이 보수적인 색이 아니다

install.packages("gmodels")
library(gmodels)
CrossTable(x=usedcars$model,y=usedcars$conservative)


#
CrossTable(x=usedcars$model,y=usedcars$conservative,chisq =TRUE)

library(outliers)

kp <-c(rep(19,3),rep(20,6),rep(21,3),145,147)
outlier(kp)
bkp <- boxplot(kp)
bkp


boxplot(emp$sal, horizontal =TRUE)
