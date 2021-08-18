height = read.table("/Users/joseizammontt/Desktop/Privado/GitHub/joigmz/PredictiveStadistic/Heights.txt", header = TRUE)
summary(height)
plot(height[,1], height[,2], pch = 20)

dim = dim(height) 
dim[2]

miX = cbind(rep(1,dim[1]), height[,2])
miY = height[,1]

# x transpuesto por x a la menos 1
solve(t(miX)%*%miX)
# x transpuesto por y
t(miX)%*%miY

#ahora todo junto seria
betas = solve(t(miX)%*%miX)%*%t(miX)%*%miY

xmin = min(miX[,2])-1
xmax = max(miX[,2])+1
ymin = min(miY)-1
ymax = max(miY)+1

eq = function(x){betas[1]+betas[2]*x}
plot(height[,1], height[,2], pch=20, xlim=c(xmin ,xmax), ylim=c(ymin,ymax), xlab = "Altura mama",ylab = "Altura hija")
par(new=TRUE)
plot(eq(1:100), type='l',  xlim=c(xmin ,xmax), ylim=c(ymin,ymax), xlab = "Altura mama",ylab = "Altura hija")

#Ahora con haremos lo mismo pero con la funcion de LM

mod1 = lm(daughter_height~mother_height, data = height)
summary(mod1)
