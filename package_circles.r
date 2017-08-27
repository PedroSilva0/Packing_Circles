library(pso)
library(plotrix)


package=function(){
  
  #Verificar se os círculos estão dentro do círculo grande
  outside=function(centros){
    pen=0
    distancias=t(sapply(1:nrow(centros), function(i) (sqrt(sum(centros[i,]**2)) + i) - 18))
    for(i in distancias){
      if(i > 0){
        pen=pen+i
      }
    }
    pen
  }
  
  #Avaliar se os circulos interiores estão corretos
  inside=function(centros){
    pen=0
    dist=function(i,j){
      sqrt(sum((centros[i,]-centros[j,])**2))
    }
    
    in_dist=outer(1:9,1:9,Vectorize(dist))
    
    for(i in 1:8){
      for(j in (i+1):9){
        if(in_dist[i,j]<(i+j)){
          pen=pen+abs(in_dist[i,j]-(i+j))
        }
        
      }
    }
    pen
  }
  
  
  #Função de avaliação
  evaluation=function(coords){
    centros=matrix(coords,ncol=2,byrow=TRUE)
    pen_total=0 
    pen1=inside(centros)
    pen2=outside(centros)
    pen_total=(pen1*0.45)+(pen2*0.55)
    pen_total
  }
  list(dims=18,min=-18,max=18,fun=evaluation)
}




#Função de teste
test = function() {
  f = package()
  res = psoptim(rep(NA, f$dims),f$fun,lower=-18,upper=18, control = list(abstol=1e-8, maxit=10000))
  list(f = f, res = res)
  res
}


#Fazer teste
resultado=test()
#Imprimir resultado
cat(resultado$par)
centros=matrix(resultado$par,ncol=2,byrow=TRUE)

#Imprimir círculos
par(pty="s")
plot(c(-20:20),c(-20:20),type="n",main="Test draw.circle",asp=1)
draw.circle(0,0,18,border="red",lty=1,lwd=1)
for(i in 1:9){
  draw.circle(centros[i,1],centros[i,2],i,border="purple",lty=1,lwd=1)
}



