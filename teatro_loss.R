if(!require(rgl)){
  install.packages("rgl")
  library(rgl)
}

if(!require(GA)){
  install.packages("GA")
  library(GA)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}


### FUNZIONI ##############################################################################

## generazione sample #################

teatro<-function(x,y){
  
  # set.seed(123)
  
  mic1<-c(0,0)#coordinate del primo microfono fisso
  mic2<-c(20,0)#coordinate del secondo microfono fisso
  
  #simulo movimenti attori, con rumore casuale. 
  #Gli attori si muovono in 5 posizioni principali durante lo spettacolo
  mov_attoreA<-matrix(c(2,2,9,3,12,5,8,7,10,7),2,5)
  mov_attoreB<-matrix(c(18,2,15,4,12,5,10,4,15,3),2,5)
  noise1<-runif(5,0,2)
  noise2<-runif(5,0,2)
  noise<-matrix(c(noise1,noise2),2,5)
  mov_attoreA<-mov_attoreA+noise
  noise1<-runif(5,0,2)
  noise2<-runif(5,0,2)
  noise<-matrix(c(noise1,noise2),2,5)
  mov_attoreB<-mov_attoreB+noise
  
  #calcolo intensita' suono ricevuto ad ogni mossa dai tre microfoni
  M<-matrix(mic1,2,5)
  suono_mic1_A<-6*log(3000/colSums((mov_attoreA-M)^2))
  suono_mic1_B<-6*log(3000/colSums((mov_attoreB-M)^2))
  M<-matrix(mic2,2,5)
  suono_mic2_A<-6*log(3000/colSums((mov_attoreA-M)^2))
  suono_mic2_B<-6*log(3000/colSums((mov_attoreB-M)^2))
  M<-matrix(c(x,y),2,5)
  suono_mic3_A<-6*log(3000/colSums((mov_attoreA-M)^2))
  suono_mic3_B<-6*log(3000/colSums((mov_attoreB-M)^2))
  #la voce e' catturata dal microfono che rileva l'intensita' maggiore
  intensityA<-pmax(suono_mic1_A,suono_mic2_A,suono_mic3_A)
  intensityB<-pmax(suono_mic1_B,suono_mic2_B,suono_mic3_B)
  
  out<-rbind(intensityA,intensityB)
  return(out)
}


## loss function ###########
loss_function <- function(x, y){
  
  intensity <- teatro(x, y)
  
  loss <- -1*(mean( (intensity[1, ] - intensity[2, ])^2 ) )
  
  return(loss)
  
}


## calcolo loss media ###############
loss_media <- function(x, y, niter = 100){
  
  loss <-matrix(0, niter, 1)
  
  for (k in c(1:niter)){
    
    loss[k] <- loss_function(x, y)
  }
  
  return(mean(loss))
}


### calcolo coordinate #############
calcolo_coordinate <- function(niter){
  
  coordinata <- matrix(0, 2, niter)
  
  for( i in 1:niter){
    
    mic3_x_low <- 0
    mic3_x_high <- 20
    mic3_y_low <- 0
    mic3_y_high <- 10
    
    mic3_x_0<-runif(1,mic3_x_low,mic3_x_high)
    mic3_y_0<-runif(1,mic3_y_low,mic3_y_high)
    
    coordinata[, i] = rbind(mic3_x_0, mic3_y_0)
    
  }
  
  return(coordinata)
  
}

### calcolo sd loss ###
calcolo_sd <- function(x, y, nitermax=1000){

  niter<- 0
  L <- c()
  
  while (niter<nitermax){
    
    niter<-niter+1

    L[niter] <- loss_function(x, y)
    
  }
  
  stdev = sd(L, na.rm = TRUE)
  return(stdev)

}

## funzione per richiamare i risultati

results <- function() {
  results_positions <- results_positions[-1,]
  print("LOSS VALUES")
  print(results_loss)
  print("BEST MIC3 POSITIONS")
  print(results_positions)
}


### ALGORITMI ##############################################################################

## inizializzo matrice vuota per salvare i risultati
results_loss <- data.frame(row.names = "loss value")
results_positions <- matrix(ncol = 3)

### 3d plot + countour plot loss ######

plot_loss <- function(){
  
  x <- seq(0, 20, by = 1)
  y <- seq(0, 10, by = 1)
  
  f <- matrix(0,length(x),length(y))
  
  for (i in c(1:length(x))){
    
    for (j in c(1:length(y))){
      
      f[i,j] <- loss_function(x[i],y[j])
      
    }
  }
  
  #persp3d(x, y, f, col = "yellow",
    #     polygon_offset = 1)
  persp3D(x, y, f, theta = 50, phi = 20, color.palette = bl2gr.colors)
  filled.contour(x, y, f, color.palette = topo.colors)
  #persp3d(x, y, f, front = "lines", back = "lines", 
  #        lit = FALSE, add = TRUE)
  highlevel() 
  #quartz()
  #windows()
  #filled.contour(x, y, f, color.palette = topo.colors)
  
}


plot_loss()


### random search #############<

random_search <- function(nitermax = 1000){
  
  #coordinate iniziali
  posizioni_0 <- calcolo_coordinate(1)
  
  L_0 <- loss_function(posizioni_0[1, ],posizioni_0[2, ])
  
  parametri <- c(posizioni_0,L_0)
  
  sd <- calcolo_sd(posizioni_0[1, ], posizioni_0[2, ])
  
  niter <- 0
  
  nomi_col <- c("x","y","L")
  
  write.table(t(parametri), file = "iterazioni_teatro.csv", append = FALSE,sep = ";", eol = "\n", na = "NA", dec = ",", row.names = FALSE,col.names = nomi_col)
  
  #Start optimization
  while (niter<nitermax){
    
    niter<-niter+1
    
    posizioni_1 <- calcolo_coordinate(1)
    
    L_1<-loss_function(posizioni_1[1, ], posizioni_1[2, ])
    
    if (L_1 < L_0-2*sd){
      
      #se costo inferiore aggiorno i parametri
      L_0<-L_1
      
      posizioni_0<-posizioni_1
      
    }
    
    parametri<-c(posizioni_0, L_0)
    
    write.table(t(parametri), file = "iterazioni_teatro.csv", append = TRUE,sep = ";", eol = "\n", na = "NA", dec = ".", row.names = FALSE,col.names = FALSE)
    
  }
  
  
  print("valori ottimi trovati")
  print(posizioni_0)
  print("intensita' max")
  print(teatro(posizioni_0[1, ], posizioni_0[2, ]))
  print("valore della corrispondente funzione costo")
  print(L_0)
  print("Numero iterazioni")
  print(nitermax)
  results_loss <<- mutate(results_loss, random_search = L_0)
  results_positions <<- rbind(results_positions, c("random", t(posizioni_0)))
  
}

random_search()


### genetic algorithm #####

genetic_algorithm <- function(){
  
  #estremi del range 
  max_x<-20
  max_y<-10
  
  #applico GA per cercare il minimo costo: cambio segno e le passo un vettore
  #con i parametri da ottimizzare
  f2 <- function(x)  - loss_function(x[1], x[2])
  
  GA <- ga(type = "real-valued", 
           fitness = f2,
           lower = c(0, 0), upper = c(max_x, max_y), 
           popSize = 200, maxiter = 100, run = 1000)
  
  #grafico di evoluzione fitness
  #windows()
  plot(GA) 
  ga_sum <<- summary(GA)
  results_loss <<- mutate(results_loss, genetic = ga_sum$fitness)
  results_positions <<- rbind(results_positions, c("genetic", unlist(ga_sum$solution)))
  rm(ga_sum)
  return(summary(GA)) 
}

genetic_algorithm()

## bat

microbat <- function (){
  #estremi del range del primo parametro
  range1x<-0
  range2x<-20
  #estremi del range del secondo parametro
  range1y<-0
  range2y<-10
  #applico bat algorithm: cerca il minimo della funzione
  f2<- function(x1,x2) loss_function(x1,x2)
  bat <- bat_optim(D = 2,NP = 40,N_Gen = 1000,alpha = 0.4,gam=0.8, ro = 0.01, 
                 Qmin = 0, Qmax = 2, Lower = c(range1x, range1y),
                 Upper = c(range2x,range2y), FUN =f2)
  results_loss <<- mutate(results_loss, bats = bat$min_fitness)
  results_positions <<- rbind(results_positions, c("bats", unlist(bat$best_solution)))
  # soluzione 
  # bat$best_solution
  # fitness media nell'ultima iterazione
  # mean(bat$fitness)
  # best fitness
  # bat$min_fitness
  print(bat)
  }

microbat()

## show results

results()
