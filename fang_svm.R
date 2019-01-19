# SVM task
install.packages("e1071")
library(e1071)
library(MASS)
library(plotly)
folder = "/Users/xfang7/Google\ Drive/Courses/CSC591-004/hw/SVM/"
df =  read.csv(paste(folder,"xfang7.csv", sep=''),header = FALSE)

names(df) = c("x1", "x2","l")
plot(df$x1,df$x2,col=df$l,,xlab = "x1",ylab="x2")


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
df[1] = range01(df[1])
df[2] = range01(df[2])


set.seed(123)
sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
df.train = df[sample,]
df.test = df[-sample,]


svmfit = svm(l~.,data = df.train,type ="C-classification")
plot(svmfit,data = df.train)

pred_train = predict(svmfit,df.train)
mean(pred_train== df.train$l)

pred_test = predict(svmfit,df.test[,-3],type = "class")
plot(pred_test)
tab = table(pred_test,df.test[,3])
mean(pred_test == df.test[,3])

set.seed(123)
# factor(l) set the type C-classification 
tunedResult = tune.svm(factor(l)~.,data = df.train,gamma =2^(seq(-15,3,2)), cost = 2^(seq(-5,15,2)),tunecontrol=tune.control(cross=5))


svmBest = svm(factor(l)~.,data = df.train,gamma=0.125,cost =8)
pred_train_best = predict(svmBest,df.train)
mean(pred_train_best== df.train$l)

pred_test_best = predict(svmBest,df.test[,-3],type = "class")
tab = table(pred_test_best,df.test[,3])
mean(pred_test_best == df.test[,3])

plot(tunedResult, transform.x = log2, transform.y = log2)

plot(tunedResult, type = "perspective", theta = 120, phi = 45)



x = log2(tunedResult$performances$gamma)
y = log2(tunedResult$performances$cost)
z = (1 - tunedResult$performances$error)*100
z= round(z,3)
tunedResult$performances$gamma = x
tunedResult$performances$cost = y
tunedResult$performances = cbind(tunedResult$performances,z)

map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

co = map2color(tunedResult$performances$z,rainbow(200))
volcano = as.matrix(tunedResult$performances$z)

p <- plot_ly(type = 'mesh3d') 
gamma_power2 = tunedResult$performances$gamma
cost_power2 =tunedResult$performances$cost
accuracy_percent = tunedResult$performances$z
p <- plot_ly(x = ~gamma_power2, y = ~cost_power2, z = ~accuracy_percent, type = 'mesh3d',xlab = "gammar",ylab="cost")

svmperformance = tunedResult$performances
uni = unique(svmperformance$z)
n = length(uni)
for(i in 1:n){
  sub1 = svmperformance[svmperformance$z == uni[i],]
  p = p %>% add_trace(x = ~sub1$gamma, y = ~sub1$cost,  z=~sub1$z)
}
print(p)

# sub1 = svmperformance[svmperformance$z == uni[1],]
# sub2 = svmperformance[svmperformance$z == uni[2],]
# sub3 = svmperformance[svmperformance$z == uni[3],]
# sub4 = svmperformance[svmperformance$z == uni[4],]
# sub5 = svmperformance[svmperformance$z == uni[5],]
# sub6 = svmperformance[svmperformance$z == uni[6],]
# sub7 = svmperformance[svmperformance$z == uni[7],]
# sub8 = svmperformance[svmperformance$z == uni[8],]
# sub9 = svmperformance[svmperformance$z == uni[9],]
# sub10 = svmperformance[svmperformance$z == uni[10],]
# sub11 = svmperformance[svmperformance$z == uni[11],]
# sub12 = svmperformance[svmperformance$z == uni[12],]
# sub13 = svmperformance[svmperformance$z == uni[13],]
# sub14 = svmperformance[svmperformance$z == uni[14],]
# sub15 = svmperformance[svmperformance$z == uni[15],]
# sub16 = svmperformance[svmperformance$z == uni[16],]
# sub17 = svmperformance[svmperformance$z == uni[17],]
# sub18 = svmperformance[svmperformance$z == uni[18],]
# 
# p = p %>% add_trace(x = ~sub1$gamma, y = ~sub1$cost,  z=~sub1$z)  %>% add_trace(x = ~sub2$gamma, y = ~sub2$cost,  z=~sub2$z) %>% add_trace(x = ~sub3$gamma, y = ~sub3$cost,  z=~sub3$z) %>% add_trace(x = ~sub4$gamma, y = ~sub4$cost,  z=~sub4$z)%>% add_trace(x = ~sub5$gamma, y = ~sub2$cost,  z=~sub5$z)  %>% add_trace(x = ~sub6$gamma, y = ~sub6$cost,  z=~sub6$z) %>% add_trace(x = ~sub7$gamma, y = ~sub7$cost,  z=~sub7$z) %>% add_trace(x = ~sub8$gamma, y = ~sub8$cost,  z=~sub8$z)%>% add_trace(x = ~sub9$gamma, y = ~sub9$cost,  z=~sub9$z) %>% add_trace(x = ~sub10$gamma, y = ~sub10$cost,  z=~sub10$z) %>% add_trace(x = ~sub11$gamma, y = ~sub11$cost,  z=~sub11$z) %>% add_trace(x = ~sub12$gamma, y = ~sub12$cost,  z=~sub12$z)%>% add_trace(x = ~sub13$gamma, y = ~sub13$cost,  z=~sub13$z) %>% add_trace(x = ~sub14$gamma, y = ~sub14$cost,  z=~sub14$z) %>% add_trace(x = ~sub15$gamma, y = ~sub15$cost,  z=~sub15$z)%>% add_trace(x = ~sub16$gamma, y = ~sub16$cost,  z=~sub16$z) %>% add_trace(x = ~sub17$gamma, y = ~sub17$cost,  z=~sub17$z) %>% add_trace(x = ~sub18$gamma, y = ~sub18$cost,  z=~sub18$z) %>% layout(showlegend = T)
# print(p)