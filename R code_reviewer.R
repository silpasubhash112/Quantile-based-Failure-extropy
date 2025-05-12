est=rep()
set.seed(444444449)
for(i in 1:1000)
{
n=1000
ci=c(((seq(0,n-2))/(n*(n-1))),((2-n)/(2*n)))
x=runif(n,0,1)
xi=sort(x,decreasing=FALSE)
est[i]=sum(ci*xi)
}
qqnorm(est)
qqline(est)
 set.seed(444444449)
m=mean(est)
v=var(est)
 den=function(x){(1/(sqrt(2*pi*v)))*(exp(-((x-m)^2)/(2*v)))}
 xx=seq(-0.18,-0.15,by=0.00001)
 plot(xx,den(xx),type="l",col="red",ylim=c(0,100),ylab = "frequency",xlab = "value")
hist(est,probability = TRUE,add=TRUE, breaks = 20)

#################################3
est1=rep()
set.seed(191119001)
Q=function(u )
{
  u-u^2
}
for(i in 1:1000)
{
  n=1000
  ci=c(((seq(0,n-2))/(n*(n-1))),((2-n)/(2*n)))
  u=runif(n,0,1)
  x=Q(u)
  xi=sort(x,decreasing=FALSE)
  est1[i]=sum(ci*xi)
}
qqnorm(est1)
qqline(est1)
m=mean(est1)
v=var(est1)
curve(den(x),-3,3,col="blue")
x=seq(-0.0235, -0.0179, by=0.0001)
den=function(x){(1/(sqrt(2*pi*v)))*(exp(-((x-m)^2)/(2*v)))}
den(x)
plot(x,den(x),type="l",xlim=c(-0.0235,-0.0179),col="blue",ylab = "frequency",xlab = "value")
hist(est1,probability = TRUE, add=TRUE)


