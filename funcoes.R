library(pso)

sphere = function(dims) {
  anchor = runif(dims, min = -10, max = 10)
  list(dims = dims, min = -100, max = 100, fun = function(x) sum((x - anchor)** 2))
}

create_ann = function(ninputs, ninner, nouter) {
  list(
    nweights = ninputs * ninner + 2 * ninner + nouter,
    ann = function(weights) {
      mid = ninputs * ninner
      
      wlayer1 = matrix(weights[1 : mid], nrow = ninner, ncol = ninputs)
      bias1 = weights[(mid + 1) : (mid + ninner)]
      
      #print(wlayer1)
      #print(bias1)
      
      mid = mid + ninner
      
      wlayer2 = weights[(mid + 1) : (mid + ninner)]
      mid = mid + ninner
      bias2 = weights[(mid + 1) : (mid + nouter)]
      
      #print(wlayer2)
      #print(bias2)
      
      ann = function(input) {
        inner = wlayer1 %*% input + bias1
        inner = sapply(inner, tanh)
        outer = inner %*% wlayer2 + bias2
        tanh(outer)
      }
      ann
    })
}

annxor = function() {
  ann = create_ann(2, 2, 1)
  eval = function(weights) {
    pred = ann$ann(weights)
    sum = 0
    for(i1 in 0:1) {
      for(i2 in 0:1) {
        exp = 2 * as.integer(xor(i1, i2)) - 1
        obt = pred(2 * c(i1, i2) - 1)
        sum = sum + (exp - obt) ** 2
      }
    }
    sum
  }
  list(dims = ann$nweights, min = -100, max = 100, fun = eval)
}

bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

annmultiplex = function(size) {
  inputs = 2**size + size
  inner = round(inputs * 2)
  ann = create_ann(inputs, inner, 1)
  eval = function(weights) {
    pred = ann$ann(weights)
    sum = 0
    for(s in 0 : (2**inputs - 1)) {
     inp = sapply(intToBits(s), as.integer)[1 : inputs]
     norm = inp[1 : (2 ** size)]
     choi = inp[2 ** size + (1 : size)]
     exp = 2 * norm [1 + bitsToInt(choi)] - 1
     obt = pred(2 * inp - 1)
     sum = sum + (exp - obt) ** 2
    }
    sum
  }
  mostrar = function(weights) {
    pred = ann$ann(weights)
    sum = 0
    for(s in 0 : (2**inputs - 1)) {
      inp = sapply(intToBits(s), as.integer)[1 : inputs]
      norm = inp[1 : (2 ** size)]
      choi = inp[2 ** size + (1 : size)]
      exp = 2 * norm [1 + bitsToInt(choi)] - 1
      obt = pred(2 * inp - 1)
      if(exp != obt) {
        cat(norm, "\t", choi, "\t", exp, "\t", obt, "\n")
      }
    }
    sum
  }
  list(dims = ann$nweights, min = -100, max = 100, fun = eval, mostrar = mostrar)
}

taylorfit = function(size) {
	taylor = function(weights) {
		function(x) {
			sum(sapply(1 : size, function(n) x ** (n - 1)) * weights)
		}
	}
	eval = function(weights) {
		vals = seq(-pi/2, pi/2, 0.1)
		exp = sapply(vals, sin)
		obt = sapply(vals, taylor(weights))
		sum((exp - obt) ** 2)
	}
	list(dims = size, min = -100, max = 100, fun = eval,  taylor = taylor)
}

test = function(size) {
	f = taylorfit(size)
	res = psoptim(rep(NA, f$dims), f$fun, lower=f$min, upper=f$max, control = list(abstol=1e-8))
	vals = seq(-pi/2, pi/2, 0.1)
	plot(vals, sapply(vals, f$taylor(res$par)), type = "l", lty = 1)
	lines(vals, sapply(vals, sin), lty = 4)
	list(f = f, res = res)
}

# package=function(coords){
#   centros=matrix(coords,ncol=2,byrow=TRUE)
#   pen=0
#   outside=function(linha){
#     pen =pen + sqrt(sum(centros[linha,])**2) - (18 - linha)
#   }
#   inside=function(centros){
#     
#     dist=sqrt(sum(centros[1:9,]-centros[1:9,])**2)
#     pen=pen+sum(outer(1:9,1:9,vectorize(dist)))
#   }
#   
# }

test(3)


# coords=c(1:18)
# coords
# centros=matrix(coords,ncol=2,byrow=TRUE)
# centros
# dis=sqrt(sum(centros[1,]-centros[2,])**2)
# dis
# 
# dist=function(i,j){
#   sqrt(sum(centros[i,]-centros[j,])**2)
# }
# 
# #dist=sqrt(sum(centros[1:9,]-centros[1:9,])**2)
# sum(outer(1:9,1:9,Vectorize(dist)))
# 
# pen =sqrt(sum(centros[1,])**2) - (18 - 1)
# pen
# 
# 
