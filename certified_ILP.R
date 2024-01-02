library("igraph")
library(ROI)
library(Rglpk)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# T - any graph
mat <- function(T){
  T <- as.undirected(T)
  M <- as_adjacency_matrix(T, sparse = FALSE)
  return(M)
}


certified_ILP <- function(n){
  T <- generuj(n) # random tree
  M <- mat(T)
  n <- vcount(T)
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n, type = "integer", lb = 0, ub = 1)%>%
    add_variable(a[i], i = 1:n, type = "integer", lb = 0, ub = 1)%>%
    set_objective(sum_expr(x[i], i = 1:n), "min") %>%
    add_constraint(x[i] + sum_expr(x[j] * M[i,j], j = 1:n) >= 1, i = 1:n) %>%
    add_constraint(n * a[i] + sum_expr((1 - x[j]) * M[i,j], j = 1:n) >= 2 * x[i], i = 1:n) %>%
    add_constraint(n * (a[i] - 1) + 2 + sum_expr((1 - x[j]) * M[i,j], j = 1:n) <= 2 * x[i], i = 1:n) 
  
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))
  solution <- get_solution(result, x[i])
  #return(solution)
  return(sum(solution$value))
}

