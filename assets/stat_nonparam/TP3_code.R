#EXO1
## (a)
# construire l'estimateur régressograme comme une fonction de N, de l'échantillon (X_i, Y_i) et de x
# ou N est le nombre de classes / intervalles / bins
# et x est un point dans [0,1]
estimateur_reg <- function(x, N, echX, echY){
  #assertation
  stopifnot(length(echX)==length(echY))
  n <- length(echX)
  # découper [0, 1] en N classes de même taille
  breakpoints <- seq(0, 1, length.out = N + 1)
  # calculer la valeur de l'éstimateur régressogramme en point x: <<m_chap_x>>
  for (i in 1:N){
    # réperer la bonne classe <<i>> ou se trouve x 
    if(x >= breakpoints[i] & x < breakpoints[i+1]){
      # calculer la moyenne des Y_i dont X_i tombent dans la classe <<j>>
      
      
      is_echX_in_classj <- #à complèter
        echY_in_classj <- echY[is_echX_in_classj]
      m_chap_x <- #à complèter
        
        
        break
    }
  }
  return(m_chap_x)
}

## (b)
# le modèle de regression est vrai pour les données simulées
set.seed(1)
echX_ <- runif(1000)
echEpsilon <- rnorm(1000, 0, 0.5)
echY_ <- sin(2*pi*echX_^2)^2 + echEpsilon

# appliquer et tracer l'estimateur sur les données simulées
x_list <- seq(0,0.999,length.out = 1000)
estimation_reg <- sapply(x_list, function(x_){
  return(estimateur_reg(x_,50,echX_,echY_))
}
)
plot(echX_, echY_, cex = 0.5)
lines(x_list,sin(2*pi*x_list^2)^2, type="s", col = "red", lwd=2)
lines(x_list,estimation_reg, type="s", col = "blue", lwd=2)

#EXO2
## (a)
#construire l'estimateur à noyau Gaussien comme une fonction de h, de l'échantillon (X_i, Y_i) et de x
estimateur_noyau_Gaussien <- function(x, h, echX, echY){
  #assertation
  stopifnot(length(echX)==length(echY))
  # calculer la valeur de l'éstimateur à noyau en point x: <<m_chap_x>>
  
  #à complèter
  
  return(m_chap_x)
}

## (c)
h_star <- 
  
# le package utlise une autre déf du bandwidth, en fonction de quartiles de la Gaussian, 
# qui est égale à 2.67*h (avec h de notre définition) 
# plus d'explication: 
# https://stats.stackexchange.com/questions/396324/what-does-bandwidth-in-kernel-regression-mean
estimation_ksmooth <- ksmooth(echX_, echY_, kernel = "normal", bandwidth = 2.67*h_star,n.points= 1000, range.x = c(0,1))

plot(echX_, echY_, cex = 0.5)
lines(x_list, estimation_noyau_Gaussien,col="green", lwd=3)
lines(estimation_ksmooth,col="blue",type='s', lwd=1)

# la méthode d'estimation de Nadaraya-Watson est un cas particulier de la technique des 
# polynômes locaux qui est réalisée dans R avec la fonction locpoly. L'estimateur de 
# Nadaraya-Watson=l'estimateur est lié au degré zéro et s'obtient ici :
library(KernSmooth)
estimation_locpoly <- locpoly(echX_, echY_,degree=0,bandwidth=h_star,gridsize=1000,range.x=c(0,1))

plot(echX_, echY_, cex = 0.5)
lines(x_list, estimation_noyau_Gaussien,col="green", lwd=3)
lines(estimation_locpoly,col="blue",type='s', lwd=1)

#EXO3
## (a)
R_chap <- function(h, echX, echY){
  #calculer le vecteur numérateur: (Y1-m_chap_h(X1), Y2-m_chap_h(X2),..., Yn-m_chap_h(Xn))
  
  vect_numérateur <- #à complèter
    
    #calculer le vecteur dénominateur: (1-w1(X1), 1-w2(X2),..., 1-wn(Xn))
    wi_Xi <- function(Xi){
      int_K <- (echX - Xi)/h
      val <- dnorm(0)/sum(dnorm(int_K))
      return(val)
    }
  vect_dénominateur <- 1-sapply(echX, wi_Xi) 
  
  return( mean( vect_numérateur^2 / vect_dénominateur^2 ) )
}

## (b)
h_list <- seq(0.01, 0.1, 0.001)

## à complèter


