library("igraph")

generuj <- function(ile){
  # random tree with root not leaf
  T <- make_tree(0)
  T <- add_vertices(T, ile, color = "white", sta = "", n22 = 0,  leaf = 0, sw = 0, n2 = 0, n0 = 0, n1 = 0, add = NULL)
  T <- add.edges(T, c(1,2))
  T <- add.edges(T, c(1,3))
  for (i in c(4:ile)){
    T <- add.edges(T, c( sample(1:(i-1),1) ,i))
  }
  return(T)
}


phase0 <- function(T){
  for (v in vcount(T):1){
    father <- neighbors(T, v, "in")
    if (ego_size(T,1,v)-1 == 1){   # leaf
      V(T)[v]$sta <- "L"
      V(T)[father]$leaf <- V(T)[father]$leaf + 1
      V(T)[father]$add <- v
    }
    if (V(T)[v]$leaf == 1)   V(T)[v]$sta <- "SW"
    if (V(T)[v]$leaf > 1)    V(T)[v]$sta <- "S"
  }
  return(T)
}


phase1 <- function(T){
  for (v in vcount(T):1){
    father <- neighbors(T, v, "in")    # who is your father?
    
    if(V(T)[v]$sta != "L"){ # not leaf
      if (V(T)[v]$leaf > 0 ){    
          if(V(T)[v]$n0 + V(T)[v]$n1 == 0 && V(T)[v]$n22 == 0 && V(T)[v]$leaf == 1){ # true weak support
            V(T)[father]$sw <- V(T)[father]$sw + 1  
          }
        else V(T)[v]$sta <- "S"
        V(T)[father]$n2 <- V(T)[father]$n2 + 1
      }
      else{    # not support
        if (V(T)[v]$n0 == 1 &&  V(T)[v]$n1 + V(T)[v]$sw == 0){ # not support
          V(T)[v]$sta <- 22
          V(T)[father]$n22 <- V(T)[father]$n22 + 1
          V(T)[father]$n2 <- V(T)[father]$n2 + 1
        }
        
        if(V(T)[v]$n0 == 0 && V(T)[v]$n2 > 0){
          V(T)[v]$sta <- 1
          V(T)[father]$n1 <- V(T)[father]$n1 + 1
        }
        if(V(T)[v]$n0 + V(T)[v]$sw + V(T)[v]$n1 > 1){
          if(length(father) && V(T)[father]$sta == "SW") f <- 1 else f <- 0 
          if(V(T)[v]$n0 > V(T)[v]$sw + f){ # add leaves
            V(T)[v]$sta <- 2
            V(T)[father]$n2 <- V(T)[father]$n2 + 1
          }
          else {   # move down to n0
            V(T)[v]$sta <- 11
            V(T)[father]$n1 <- V(T)[father]$n1 + 1
          }
        }
        if(V(T)[v]$n0 == 0 && V(T)[v]$n2 == 0){
          V(T)[v]$sta <- 0
          V(T)[father]$n0 <- V(T)[father]$n0 + 1
        }
        
      }
    }
  }
  if (V(T)[1]$n2 == 0)
    V(T)[1]$sta <- 2 
  return(T)
}


phase22 <- function(T){
  for (v in vcount(T):1){
    father <- neighbors(T, v, "in")    # who is your father?
    if(length(father)) {
      grandfather <- neighbors(T, father, "in") 
    }
    
    if(v != 1){
      if(V(T)[v]$sta == "SW" && V(T)[father]$sta %in% c(2, "SW", "S")){
        child <- V(T)[v]$add
        V(T)[child]$sta <- 2
      }
        
      if(V(T)[v]$sta == 0 && V(T)[father]$sta == 11){
        V(T)[v]$sta <- 2
      }
      
      if(length(grandfather)){
        if(V(T)[v]$sta == 0 && V(T)[father]$sta == 22 && V(T)[grandfather]$sta %in% c(2, 22, "S", "SW")){
          V(T)[v]$sta <- 2
          V(T)[father]$sta <- 1
          if (V(T)[grandfather]$sta == "SW")
            V(T)[grandfather]$sta <- "S"
        }
      }
    } # not root
  }
  if(V(T)[1]$sta == "SW" && (V(T)[1]$n0 + V(T)[1]$n1 == 0)){
    child <- V(T)[1]$add
    V(T)[child]$sta <- 2
  }
  
  return(T)
}

linear_Trees <- function(n){
  T <- generuj(n)
  T <- phase0(T)
  T <- phase1(T)
  T <- phase22(T)
  licznik = 0
  for (v in V(T)){
    if (V(T)[v]$sta %in% c(2, 22, "S", "SW")) licznik = licznik + 1
  }
  return(licznik)
}


T <- generuj()
T <- phase0(T)
T <- phase1(T)
T <- phase22(T)

for (v in V(T)){
  if (V(T)[v]$sta %in% c(2, 22, "S", "SW")) V(T)[v]$color = "#EE767B"
  else V(T)[v]$color = "#599959"
}


#tkplot(T, canvas.width = 1500, canvas.height = 700, degree = 180, layout = layout_as_tree, edge.arrow.mode = "-")
tkplot(T, canvas.width = 500, canvas.height = 400, degree = 180, layout = layout_as_tree, edge.arrow.mode = "-")





