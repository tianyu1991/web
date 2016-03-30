##dataset:
type <-c(rep("facebook",88),rep("google",52))
method<-c(rep("desktop",58),rep("mobile",17),rep("tablet",13),rep("desktop",38),rep("mobile",7),rep("tablet",7))
gf_d_co<-data.frame(type,method)

##plot 
library(ggplot2)
g<-ggplot(data=gf_d_co,aes(x=type,fill=method))+
##set datset, x axis and fill and plot method

theme(plot.title = element_text(face="bold",colour="black",size=15))+
#set the font format ot title, or set the font format or x axis title by add axis.text.x=element_text(angle=90,face="bold",colour="black",size=10) to theme ()
#font face ("plain", "italic", "bold", "bold.italic")

ggtitle("the Performance of Scoial Login at Checkout Page")+ylab("the Number of Account")+xlab("Account Type")
##set xlab, ylab, or title

g+geom_bar(position="dodge")

g+geom_bar(position="stack")

g+geom_bar(position="identity",alpha=0.5)

geom_bar(position="fill")

g+geom_bar(position="dodge")+
facet_grid(method~.)

g+geom_bar(position="dodge")+
facet_grid(method~.,scales="free", space="free")


##how to save a plot
png(file = "test.png", width = 480, height = 480)
g+geom_bar(position="dodge")+
facet_grid(method~.)
dev.off()


##compare with barplot
##to plot a bar chart using barplot,normally, you need to covert your dataset to a matrix 
nums<-c(137,74,122,62,59,48)
rnames<-c("A","B")
cnames<-c("befroe","test","after")
data<-matrix(nums,nrow=2,ncol=3,byrow=TRUE,dimnames=list(rnames, cnames))
data
##  befroe test after
##A    137   74   122
##B     62   59    48

png(file = "test2.png", width = 480, height = 480)
barplot(data,beside = TRUE,col=c("lightblue", "mistyrose"),
	legend = rownames(data),names.arg=colnames(data),ylim=c(0,160),
	main="title of the plot")
dev.off()

