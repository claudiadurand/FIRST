x=c(-0.1, +0.1)

plot.new()
plot.window(xlim=c(-1,1), ylim= c(-1,1))
axis(1)
axis(2)
box()


for(i in 1:1000){
  x= runif(2, min=-1, max=1)
  
  if(sign(x[1]*x[2])==+1) {
    #signo positivo 
    
    points(x[1], x[2], col="blue", pch=19)
  } else {
    #signo negativo
    points(x[1], x[2], col="red", pch=19)
  }
}

 #7
  
coef= c(1,3,-5) #(a,b,c)

a=coef[1]
b=coef[2]
c=coef[3]
deter=b^2- 4*a*c

if(deter<0) {
  #determinante negativo
  
  deter= -1* deter
  raices=(-b+ c(+1,-1)* sqrt(deter)*1i)/(2*a)
} else {
  #determinante positivo 
  raices=(-b+ c(+1,-1)* sqrt(deter))/(2*a)
}
print(raices)

#ejemplo 1

coef= c(16,-5,6) #(a,b,c)

a=coef[1]
b=coef[2]
c=coef[3]
deter=b^2- 4*a*c

if(deter<0) {
  #determinante negativo
  
  deter= -1* deter
  raices=(-b+ c(+1,-1)* sqrt(deter)*1i)/(2*a)
} else {
  #determinante positivo 
  raices=(-b+ c(+1,-1)* sqrt(deter))/(2*a)
}
print(raices)

#ejemplo 2

coef= c(-9,1,-12) #(a,b,c)

a=coef[1]
b=coef[2]
c=coef[3]
deter=b^2- 4*a*c

if(deter<0) {
  #determinante negativo
  
  deter= -1* deter
  raices=(-b+ c(+1,-1)* sqrt(deter)*1i)/(2*a)
} else {
  #determinante positivo 
  raices=(-b+ c(+1,-1)* sqrt(deter))/(2*a)
}
print(raices)

   # crear función 
   
calcularRaices= function(coef) {
  
  a=coef[1]
  b=coef[2]
  c=coef[3]
  deter=b^2- 4*a*c
  
  if(deter<0) {
    #determinante negativo
    
    deter= -1* deter
    raices=(-b+ c(+1,-1)* sqrt(deter)*1i)/(2*a)
  } else {
    #determinante positivo 
    raices=(-b+ c(+1,-1)* sqrt(deter))/(2*a)
  }
  
  return(raices)
}


graficarNumeros= function(n, min, max, col, pch){
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n) {
    x=runif(2,min=min,max=max)
    if(sign(x[1]*x[2])==+1) {
      #signo positivo
      points(x[1],x[2],col=col[1],pch=pch)
    } else {
      #signo negativo 
      points(x[1], x[2], col=col[2], pch=pch)
    }
  }
  return(invisible())
}


#aplicar la nueva función 

calcularRaices(coef=c(-9,1,-12))


#aplicar otra función de  ejemplo  


graficarNumeros(n=100, min=-1, max=1, col=c("blue","red"), pch=19)

graficarNumeros(n=1e4, min=-1, max=1, col=c("blue","red"), pch=19)




graficarNumeros2= function(n, min=-1, max=+1, col=c("blue", "red"), pch=19){
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n) {
    x=runif(2,min=min,max=max)
    if(sign(x[1]*x[2])==+1) {
      #signo positivo
      points(x[1],x[2],col=col[1],pch=pch)
    } else {
      #signo negativo 
      points(x[1], x[2], col=col[2], pch=pch)
    }
  }
  return(invisible())
}

graficarNumeros2(n=100)
graficarNumeros2(n=1000)

graficarNumeros3= function(n, min=-1, max=+1, col=c("blue", "red"), pch=19, cex){
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n) {
    x=runif(2,min=min,max=max)
    if(sign(x[1]*x[2])==+1) {
      #signo positivo
      points(x[1],x[2],col=col[1],pch=pch)
    } else {
      #signo negativo 
      points(x[1], x[2], col=col[2], pch=pch)
    }
  }
  return(invisible())
}
graficarNumeros4= function(n, min=-1, max=+1, col=c("blue", "red"),...){
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n) {
    x=runif(2,min=min,max=max)
    if(sign(x[1]*x[2])==+1) {
      #signo positivo
      points(x[1],x[2],col=col[1], ...)
    } else {
      #signo negativo 
      points(x[1], x[2], col=col[2], ...)
    }
  }
  return(invisible())
}

graficarNumeros4(n=1000, cex=1.5)
graficarNumeros4(n=1000, pch=14)

nuevaGraficarNUmeros= function(n, col=c("green", "orange"),  ...){
  graficarNumeros4(n=n, col=col, ...)
   return(invisible())
}
      # ejercicio 12 
# calcular n!
n=-2

if(n<0) stop("Factorial no definido para numeros negativos.")
if(n==0){
factorial=1
} else {
  factorial=1
for(i in 1:n) {
  factorial= factorial*i
}
}
print(factorial)

 factorial=function(n){

  if(n<0) stop("Factorial no definido para numeros negativos.")
  if(n%%1 !=0) warning("Factorial definido solo para numeros enteros, usando parte entera.")
  if(n==0){
      factorial=1
  } else {
      factorial=1
       for(i in 1:n) {
            factorial= factorial*i
          }
      }
return(factorial)
 }
 
 factorial(3)
 factorial(6)
 factorial(0)
 factorial(-5)