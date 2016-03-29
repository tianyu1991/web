before_co<-read.csv("before_co.csv",header=TRUE)
library(dplyr)
before_co2<-group_by(before_co,thedevice)
##2016-02-28T23:58:34.738-0500 -2016-02-15T00:02:00.052
before_co3<-summarise(before_co2,create_account_perday=sum(reg_success)/14)

##    device     create_account_perday
##1   DESKTOP             108.57143
##2    MOBILE              14.28571
##3      NULL               0.00000
##4    TABLET              14.35714
sum(before_co3$create_account_perday)
##137.2143

date<-strftime(before_co[,1])
before_co4<-cbind(before_co,date)
mode_list<-c("DESKTOP","MOBILE","TABLET")
for(i in 1:3){
	mode1<-before_co4[before_co4$thedevice==mode_list[i],]
	mode2<-group_by(mode1,date)
	mode3<-summarise(mode2,create_account=sum(reg_success))
	mode<-rep(mode_list[i],nrow(mode3))
	mode4<-cbind(mode3,mode)
	if(i==1){
		num_co<-mode4}else	
	{num_co<-rbind(num_co,mode4)}
}
state<-rep("before",nrow(num_co))
num_co_sum<-cbind(num_co,state)

before_wl<-read.csv("before_wl.csv",header=TRUE)
before_wl2<-before_wl[grepl("C",before_wl$login_or_create),]
before_wl3<-group_by(before_wl2,thedevice)
before_wl4<-summarise(before_wl3,create_account_perday=n()/14)

##    thedevice   create_account_perday
##1   DESKTOP             37.214286
##2    MOBILE             19.571429
##3    TABLET              5.857143
sum(before_wl4$create_account_perday)
## 62.64286

date<-strftime(before_wl2[,1])
before_wl5<-cbind(as.data.frame(as.character(before_wl2[,7])),date)
for(i in 1:3){
	mode1<-before_wl5[before_wl5[,1]==mode_list[i],]
	mode2<-group_by(mode1,date)
	mode3<-summarise(mode2,create_account=n())
	mode<-rep(mode_list[i],nrow(mode3))
	mode4<-cbind(mode3,mode)
	if(i==1){
		num_wl<-mode4}else	
	{num_wl<-rbind(num_wl,mode4)}
}
state<-rep("before",nrow(num_wl))
num_wl_sum<-cbind(num_wl,state)

test_co<-read.csv("test_co.csv",header=TRUE)
##2016-03-13T23:59:38.675-0400-2016-02-29T00:00:16.286-0500 14 days
test_co2<-group_by(test_co,thedevice)
test_co3<-summarise(test_co2,create_account_perday=sum(reg_success)/14)

##  device      create_account_perday
##1   DESKTOP              56.78571
##2    MOBILE               9.50000
##3      NULL               0.00000
##4    TABLET               8.00000
sum(test_co3$create_account_perday)
## 74.28571

date<-strftime(test_co[,1])
test_co4<-cbind(test_co,date)
for(i in 1:3){
	mode1<-test_co4[test_co4$thedevice==mode_list[i],]
	mode2<-group_by(mode1,date)
	mode3<-summarise(mode2,create_account=sum(reg_success))
	mode<-rep(mode_list[i],nrow(mode3))
	mode4<-cbind(mode3,mode)
	if(i==1){
		num_co<-mode4}else	
	{num_co<-rbind(num_co,mode4)}
}
state<-rep("test",nrow(num_co))
num_co_sum<-rbind(num_co_sum,cbind(num_co,state))


summary(test_co$themodule)
##tried social login number
##    0     F     G 
##14236   124    67 

test_co_f<-test_co[test_co$themodule=="F"&test_co$reg_success=="1",]
summary(test_co_f$thedevice)/nrow(test_co_f)

##the distribution of tried account
##  DESKTOP    MOBILE      NULL    TABLET 
##0.6612903 0.2096774 0.0000000 0.1290323 
##the distribution of created account
##  DESKTOP    MOBILE      NULL    TABLET 
##0.6590909 0.1931818 0.0000000 0.1477273 

test_co_g<-test_co[test_co$themodule=="G"&test_co$reg_success=="1",]
summary(test_co_g$thedevice)/nrow(test_co_g)

##the distribution of tried account
##  DESKTOP    MOBILE      NULL    TABLET 
##0.7014925 0.1791045 0.0000000 0.1194030 
##the distribution of created account
##  DESKTOP    MOBILE      NULL    TABLET 
##0.7307692 0.1346154 0.0000000 0.1346154 
##

test_co_fg<-group_by(test_co[test_co$themodule!=0,],themodule)
test_co_fg2<-summarise(test_co_fg,sucees_rate=sum(reg_success)/sum(did_try_sm))

##      module sucees_rate
##1         F   0.7096774
##2         G   0.7761194

##a<-test_co[,c(4,5,7,8,9)]

test_wl<-read.csv("test_wl.csv",header=TRUE)
test_wl2<-test_wl[grepl("C",test_wl$login_or_create),]
test_wl3<-group_by(test_wl2,thedevice)
test_wl4<-summarise(test_wl3,create_account_perday=n()/14)

##  thedevice create_account_perday
##1   DESKTOP             33.571429
##2    MOBILE             18.928571
##3    TABLET              6.928571
sum(test_wl4$create_account_perday)
## 59.42857

date<-strftime(test_wl2[,1])
test_wl5<-cbind(as.data.frame(as.character(test_wl2[,7])),date)
for(i in 1:3){
	mode1<-test_wl5[test_wl5[,1]==mode_list[i],]
	mode2<-group_by(mode1,date)
	mode3<-summarise(mode2,create_account=n())
	mode<-rep(mode_list[i],nrow(mode3))
	mode4<-cbind(mode3,mode)
	if(i==1){
		num_wl<-mode4}else	
	{num_wl<-rbind(num_wl,mode4)}
}
state<-rep("test",nrow(num_wl))
num_wl_sum<-rbind(num_wl_sum,cbind(num_wl,state))



summary(test_wl$themodule)
##   0    F    G 
##2724  329  134 

test_wl_f<-test_wl2[test_wl2$themodule=="F",]
summary(test_wl_f$thedevice)/nrow(test_wl_f)
##the distribution of tried account
##    DESKTOP    MOBILE      NULL    TABLET 
##   0.5015198 0.3890578 0.0000000 0.1094225 
##the distribution of created account
##   DESKTOP  MOBILE    NULL  TABLET 
##   0.59375 0.30625 0.00000 0.10000  

test_wl_g<-test_wl2[test_wl2$themodule=="G",]
summary(test_wl_g$thedevice)/nrow(test_wl_g)
##  DESKTOP    MOBILE      NULL    TABLET 
##0.5597015 0.2686567 0.0000000 0.1716418 
##the distribution of created account
##  DESKTOP    MOBILE      NULL    TABLET 
##0.6129032 0.2043011 0.0000000 0.1827957 
test_wl_fg<-group_by(test_wl[test_wl$did_try_sm!=0,],themodule)
test_wl_fg2<-summarise(test_wl_fg,sucees_rate=length(grep("C",login_or_create))/n())
##  themodule sucees_rate
##1         F   0.4863222
##2         G   0.6940299

after_co<-read.csv("after_co.csv",header=TRUE)
after_co2<-group_by(after_co,thedevice)
##2016-03-18T15:33:23-2016-03-15T00:00:00
days<-(15+33/60)/24+3
after_co3<-summarise(after_co2,create_account_perday=sum(reg_success)/days)
##  thedevice create_account_perday
##1   DESKTOP              98.96059
##2    MOBILE              12.33581
##3      NULL               0.00000
##4    TABLET              10.96516
sum(after_co3$create_account_perday)
##122.2616

date<-strftime(after_co[,1])
after_co4<-cbind(after_co,date)
for(i in 1:3){
	mode1<-after_co4[after_co4$thedevice==mode_list[i],]
	mode2<-group_by(mode1,date)
	mode3<-summarise(mode2,create_account=sum(reg_success))
	mode<-rep(mode_list[i],nrow(mode3))
	mode4<-cbind(mode3,mode)
	if(i==1){
		num_co<-mode4}else	
	{num_co<-rbind(num_co,mode4)}
}
num_co[c(4,8,12),2]<-num_co[c(4,8,12),2]/(33/60)
state<-rep("after",nrow(num_co))
num_co_sum<-rbind(num_co_sum,cbind(num_co,state))



after_wl<-read.csv("after_wl.csv",header=TRUE)
after_wl2<-after_wl[grepl("C",after_wl$login_or_create),]
after_wl3<-group_by(after_wl2,thedevice)
after_wl4<-summarise(after_wl3,create_account_perday=n()/days)
##    thedevice create_account_perday
##1   DESKTOP              28.78355
##2    MOBILE              13.70645
##3    TABLET               6.03084

sum(after_wl4$create_account_perday)
##48.52085

date<-strftime(after_wl2[,1])
after_wl5<-cbind(as.data.frame(as.character(after_wl2[,7])),date)
for(i in 1:3){
	mode1<-after_wl5[after_wl5[,1]==mode_list[i],]
	mode2<-group_by(mode1,date)
	mode3<-summarise(mode2,create_account=n())
	mode<-rep(mode_list[i],nrow(mode3))
	mode4<-cbind(mode3,mode)
	if(i==1){
		num_wl<-mode4}else	
	{num_wl<-rbind(num_wl,mode4)}
}
num_wl[c(4,8,12),2]<-num_wl[c(4,8,12),2]/(33/60)
state<-rep("after",nrow(num_wl))
num_wl_sum<-rbind(num_wl_sum,cbind(num_wl,state))



###picture
library(ggplot2)
sum<-c(137.2143,74.28571,122.2616,62.64286,59.42857,48.52085)
rnames<-c("Created at checkout","Created at wish list")
cnames<-c("befroe","test","after")
sum2<-matrix(sum,nrow=2,ncol=3,byrow=TRUE,dimnames=list(rnames, cnames))

png(file = "overall.png", width = 480, height = 480)
barplot(sum2,beside = TRUE,col=c("lightblue", "mistyrose"),
	legend = rownames(sum2),names.arg=colnames(sum2),ylim=c(0,160),
	main="social login impact on  the number of account creations")
dev.off()

sum_co<-merge(before_co3,test_co3,by="thedevice")
sum_co2<-merge(sum_co,after_co3,by="thedevice")
sum_co2<-sum_co2[c(1,2,4),]
time<-c(rep("before",3),rep("test",3),rep("after",3))
mode<-rep(c("desktop","mobile","tablet"),3)
nums<-c(sum_co2[,2],sum_co2[,3],sum_co2[,4])
sum_co3<-data.frame(time,mode,nums)

png(file = "change with mode.png", width = 480, height = 480)
ggplot(sum_co3,aes(x=factor(time,levels=c("before","test","after")),y=nums,colour=mode,group=mode))+
	theme(plot.title = element_text(face="bold",colour="black",size=15))+
	ggtitle("Social Login Impact on Account Creation at Checkout")+xlab("Stage")+ylab("the Average Number of Account Creations per Day")+
	geom_line(size=1)
dev.off()

num_co_sum$date<-substr(num_co_sum$date,6,10)
png(file = "change with mode2.png", width = 600, height = 600)
ggplot(num_co_sum,aes(x=date,y=create_account,colour=state,group=mode))+
	theme(axis.text.x=element_text(angle=90), plot.title = element_text(face="bold",colour="black",size=15)) +
	scale_size_area()+ggtitle("Social Login Impact on Account Creation at Checkout")+xlab("Date")+ylab("the Number of Account Creations per Day")+
	geom_line(size=1)+facet_grid(mode~ .,scales="free", space="free")
dev.off()

sum_wl<-merge(before_wl4,test_wl4,by="thedevice")
sum_wl2<-merge(sum_wl,after_wl4,by="thedevice")
time<-c(rep("before",3),rep("test",3),rep("after",3))
mode<-rep(c("desktop","mobile","tablet"),3)
nums<-c(sum_wl2[,2],sum_wl2[,3],sum_wl2[,4])
sum_wl3<-data.frame(time,mode,nums)

png(file = "change with mode wl.png", width = 480, height = 480)
ggplot(sum_wl3,aes(x=factor(time,levels=c("before","test","after")),y=nums,colour=mode,group=mode))+
	theme(plot.title = element_text(face="bold",colour="black",size=15))+
	ggtitle("Social Login Impact on Account Creation at Wishlist Page")+xlab("Stage")+ylab("the Average Number of Account Creations per Day")+
	geom_line(size=1)
dev.off()

num_wl_sum$date<-substr(num_wl_sum$date,6,10)
png(file = "change with mode2 wl.png", width = 600, height = 600)
ggplot(num_wl_sum,aes(x=date,y=create_account,colour=state,group=mode))+
	theme(axis.text.x=element_text(angle=90), plot.title = element_text(face="bold",colour="black",size=15)) +
	scale_size_area()+ggtitle("Social Login Impact on Account Creation at Checkout")+xlab("Date")+ylab("the Number of Account Creations per Day")+
	geom_line(size=1)+facet_grid(mode~ .,scales="free", space="free")
dev.off()

theme(axis.text.x=element_text(angle=90), plot.title = element_text(face="bold",colour="black",size=15))+
ggtitle("the Performance of Scoial Login")+ylab("the Number of Account")
rnames<-c("Created at checkout","Created at wish list")
cnames<-c("befroe","test","after")
sum2<-matrix(sum,nrow=2,ncol=3,byrow=TRUE,dimnames=list(rnames, cnames))

png(file = "overall.png", width = 480, height = 480)
barplot(sum2,beside = TRUE,col=c("lightblue", "mistyrose"),
	legend = rownames(sum2),names.arg=colnames(sum2),ylim=c(0,160),
	main="social login impact on  the number of account creations")
dev.off()

nums<-rep(1,1047)
type<-c(rep("facebook",701),rep("google",346))
result<-c(rep("tried",124),rep("success",88),rep("tried",329),rep("success",160),rep("tried",67),rep("success",52),rep("tried",134),rep("success",93))
page<-c(rep("check out",212),rep("wish list",489),rep("check out",119),rep("wish list",227))
gf<-data.frame(nums, type,result,page)

png(file = "fg.png", width = 480, height = 480)
ggplot(data=gf,aes(x=type,fill=factor(result)))+geom_bar(position="dodge")+
theme(plot.title = element_text(face="bold",colour="black",size=15))+
ggtitle("the Performance of Scoial Login")+ylab("the Number of Account")+xlab("Account Type")+
facet_grid(page~.,scales="free", space="free")
dev.off()

type<-c(rep("facebook",160),rep("google",93))
method<-c(rep("desktop",95),rep("mobile",49),rep("tablet",16),rep("desktop",57),rep("mobile",19),rep("tablet",17))
gf_d<-data.frame(type,method)

png(file = "fg2.png", width = 480, height = 480)
ggplot(data=gf_d,aes(x=type,fill=method))+geom_bar(position="dodge")+
theme(plot.title = element_text(face="bold",colour="black",size=15))+
ggtitle("the Performance of Scoial Login at Wishlist Page")+ylab("the Number of Account")+xlab("Account Type")
dev.off()


type<-c(rep("facebook",88),rep("google",52))
method<-c(rep("desktop",58),rep("mobile",17),rep("tablet",13),rep("desktop",38),rep("mobile",7),rep("tablet",7))
gf_d_co<-data.frame(type,method)

png(file = "fg3.png", width = 480, height = 480)
ggplot(data=gf_d_co,aes(x=type,fill=method))+geom_bar(position="dodge")+
theme(plot.title = element_text(face="bold",colour="black",size=15))+
ggtitle("the Performance of Scoial Login at Checkout Page")+ylab("the Number of Account")+xlab("Account Type")
dev.off()

##t-test
t.test(brfore_co$reg_success,test_co$reg_success)
t.test(after_co$reg_success,test_co$reg_success)
t.test(after_co$reg_success,before_co$reg_success)

t.test(grepl("C",before_wl$login_or_create),grepl("C",test_wl$login_or_create))
t.test(grepl("C",after_wl$login_or_create),grepl("C",test_wl$login_or_create))
t.test(grepl("C",before_wl$login_or_create),grepl("C",after_wl$login_or_create))
