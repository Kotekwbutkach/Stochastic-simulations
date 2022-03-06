# funkcja do cwiczen 1.1, 1.2, 1.3

cauchy_elimination <- function(n = 1000, graphics = TRUE)
{
  # parametr graphics odpowiada za rysowanie wykresow
    
    
  # losujemy zmienne X i Y z odpowiednich rozkladow jednostajnych
  X = runif(n, -1, 1)
  Y = runif(n, 0, 1)
  
  # stosujemy eliminacje - wybieramy punkty znajdujace sie w polkolu
  inCircle = X^2+Y^2 <= 1
  n1 = sum(inCircle)
  X1 = X[inCircle]
  Y1 = Y[inCircle]
  
  Z = X1/Y1  # tworzymy zmienna losowa Z o rozkladzie X/Y
  
  if (graphics)
  {
    
    print(n1)  # ile tak naprawde powtorzen zmiennych losowych uzyskalismy
    
    par(mfrow = c(4,2))  # ustawienie miejsca na 8 wykresow (4x2)
    
    plot (X,Y, main= "(X,Y) original pairs")
    plot (X1, Y1, main= "(X,Y) pairs after elimination")
    
    
    colour = "darkblue"
    
    hist(Z, prob = TRUE, main = "X/Y density", breaks = "FD")
    hist(Z, prob = TRUE, main = "X/Y density on [-25, 25]", breaks = "FD",
         xlim=c(-25,25)) 
    curve(dcauchy(x), col = colour, add = TRUE)
    hist(Z, prob = TRUE, main = "X/Y density on [-10,10]", breaks = "FD",
         xlim=c(-10,10)) 
    curve(dcauchy(x), col = colour, add = TRUE)
    hist(Z, prob = TRUE, main = "X/Y density on [-5,5]", breaks = "FD",
         xlim=c(-5,5)) 
    curve(dcauchy(x), col = colour, add = TRUE)
    
    colour = "deepskyblue"
    
    plot(ecdf(Z), main = "X/Y distribution on [-100,100]", xlim=c(-100,100))
    curve(pcauchy(x), col = colour, add = TRUE)
    
    plot(ecdf(Z), main = "X/Y distribution on [-5,5]", xlim=c(-5,5))
    curve(pcauchy(x), col = colour, add = TRUE)
  }
  
  test_results <- ks.test(Z, pcauchy)
  return (test_results$p.value)
}

cauchy_elimination(n = 10000, graphics = TRUE)

m = 1000

p_values = NULL

for (i in 1:m)
{
  p <- cauchy_elimination(n = 100, graphics = FALSE)
  p_values <- append(p_values, p)
}

par (mfrow=c(1,1))
print(p_values)
hist(p_values)

ks.test(p_values, pnorm)

