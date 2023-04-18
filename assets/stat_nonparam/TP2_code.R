#EXO1
## a)
# construire l'estimateur histogramme comme une fonction de N, de l'échantillon et de x
# où N est le nombre de classes / intervalles / bins
# et x est un point dans [0,2]
estimateur_hist <- function(x, N, ech){
  n <- length(ech)
  # découper [0, 2] en N classes de même taille, la fenêtre h ainsi = 1/N
  breakpoints <- seq(0, 2, length.out = N + 1)
  h = 2/N # la fenêtre 
  # calculer la valeur de l'éstimateur histogramme en point x: <<f_chap_x>>
  for (i in 1:N){
    # réperer la bonne classe <<i>> ou se trouve x 
    if(x >= breakpoints[i] & x < breakpoints[i+1]){
      # calculer la fréquence du nb de obs qui tombent dans la classe <<i>>
      
      
      
      f_chap_x = # à complèter 
        
        
        
        break
    }
  }
  return(f_chap_x)
}

## b)
library(truncnorm)
set.seed(1)
ech_mgt <- c(rtruncnorm(100,a = 0,b = 2,mean=0.5,sd = 0.2),
             rtruncnorm(100,a = 0,b = 2,mean=1.5,sd = 0.2))
x_list <- seq(0,1.999,length.out = 2000)
estimation_hist <- sapply(x_list, function(x_){
  return(estimateur_hist(x_,20,ech_mgt))
}
)
plot(x_list,estimation_hist, type="s")


##c)
hist(ech_mgt, breaks =, freq =)

#EXO2
##a)
#construire l'estimateur à noyau Gassien comme une fonction de h, de l'échantillon et de x
#NB: la fenêtre h n'est pas le même hyper-paramètre qu'avant, qui au lieu contrôle les poids des obs autour de x dans le calculs de f_chap(x)
estimateur_noyau_Gassien <- function(x, h, ech){
  n <- length(ech)
  # calculer la valeur de l'éstimateur à noyau en point x: <<f_chap_x>>
  int_K <- (ech - x)/h
  
  
  f_chap_x <- # à complèter 
    
    
    return(f_chap_x)
}

##c)
install.packages("kdensity")
estimation_density <- density(ech_mgt, kernel = , bw =, n = 2000, from = 0, to = 2)
plot(estimation_density,col="black")

#EXO3
##a)
U_n <- function(h, ech){
  n <- length(ech)
  # 1ère terme 
  
  sum_ <- # à compléter
    
    sum_ <- sum_/(n-1)
  # 2ème terme 
  
  K0 <- # à compléter
    
    K0 <- K0/(n-1)/h
  return(sum_ +  K0)
}

##b)
V_n <- function(h, ech){
  n <- length(ech)
  # Fonction wrapper: 
  # convertir la rentrée et la sortie de la estimateur_noyau_Gassien(x_,h,ech) en vecteur
  # fixer les 2eme 3eme arguments
  fsq <- function(x){
    return(sapply(x, function(x_) return(estimateur_noyau_Gassien(x_,h,ech)^2)))
  }
  integral_fsq <- integrate(fsq, lower=0, upper =2)$value
  return(integral_fsq)
}

##c)
J_n <- function(h, ech){
  return(## à complèter 
  )
}

h_list <- seq(0.01, 2, 0.01)
val_J_n <- ## à complèter
  plot(h_list, val_J_n, type = "s")

# h optimal

## à complèter







