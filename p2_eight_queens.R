library(tidyverse)

# calculate fitness (number of non-attacking pairs) of a board
calcFitness = function(x){
  idx = c(1:length(x))
  non_att_pairs = choose(length(x),2)
  for(i in 1:(length(x)-1)){
    for(k in (i+1):length(x)){
      # check horizontally
      if(x[k]==x[i]){ 
        non_att_pairs = non_att_pairs - 1
      }
      # check diagonally
      if((k-i)==abs(x[k]-x[i])){
        non_att_pairs = non_att_pairs - 1
      }
    }
  }
  return(non_att_pairs)
}

# view board in matrix form
viewBoard = function(x){
  ridx = x
  cidx = 1:length(x)
  board = matrix(rep(' ',length(x)^2),nrow=length(x))
  for(i in cidx) {
    board[ridx[i],i] = 'X'
  }
  return(board)
}


# initialize population
# members: # of members in the population, size: board size
iniPopulation = function(members,size){
  pop = list()
  for(i in 1:members){
    pop[[i]] = sample(1:size,size,replace=T)
  }
  return(pop)
}

# get mutated
getMutated = function(x){
  mut_loc = sample(1:length(x),1)
  x[mut_loc] = sample(1:length(x),1)
  return(x)
}

# set up parameters for run
PopulationSize = 1000
BoardSize = 8
numPairs = choose(BoardSize,2) # total number of pairs
population = iniPopulation(PopulationSize,BoardSize) # initialize population
population0 = population
mutationProb = 0.5 # mutation probability
NumIterations = 500 # max number of generation
idx = NULL # used to check if fitness reaches maximum value aka no attacking pairs
final = NULL # final board

# some fitness statistics
avg_fitness = NULL 
q25_fitness = NULL
q75_fitness = NULL

for (iter in 1:NumIterations) {
  
  print(paste0('----iter: ',iter))
  newPopSize = 0
  newPop = list()
  
  fitness = lapply(population,calcFitness) # get fitness scores
  probSelection = as.double(fitness) / sum(unlist(fitness)) # get probabilities to select parents to breed
  
  avg_fitness = c(avg_fitness,mean(unlist(fitness))) # avg fitness for this generation
  #q25_fitness = c(q25_fitness,quantile(unlist(fitness),0.25))
  #q75_fitness = c(q75_fitness,quantile(unlist(fitness),0.75))
  
  while(newPopSize<PopulationSize){
    
    # get the first board in this generation that reaches max possible finess score C(board_size,2)
    if(sum(fitness==numPairs)>0){
      idx = which(fitness==numPairs)[1]
      final = population[[idx]]
      print('Solution Found!!!')
      break
    }
    
    # select parents using weighted probabilities calculated from fitness scores
    pars = sample(1:PopulationSize,size=2,prob=probSelection)
    par1 = population[[pars[1]]]
    par2 = population[[pars[2]]]
    
    # crossover 
    cutoff = sample(2:(BoardSize-1),1)
    
    # children
    chd1 = c(par1[1:cutoff],par2[(cutoff+1):BoardSize])
    if(runif(1)>=mutationProb) chd1 = getMutated(chd1) # mutate gene if passes mutation threshold
    newPopSize = newPopSize + 1
    newPop[[newPopSize]] = chd1
    
    chd2 = c(par2[1:cutoff],par1[(cutoff+1):BoardSize])  
    if(runif(1)>=mutationProb) chd2 = getMutated(chd2) # mutate gene if passes mutation threshold
    newPopSize = newPopSize + 1
    newPop[[newPopSize]] = chd2
  }
  if(!is.null(idx)) break
  population = newPop
}

# final board (if exists)
print(final)
if(!is.null(final)) viewBoard(final)

plotData = tibble(iteration=1:iter,
                  avg_fitness=avg_fitness#,
                  #q25_fitness=q25_fitness,
                  #q75_fitness=q75_fitness
                  )

# plot of average fitness scores across generations
ggplot(plotData) + 
  geom_path(aes(x=iteration,y=avg_fitness),size=0.5,color='black') +
  geom_point(aes(x=iteration,y=avg_fitness),size=1,color='black') + 
  scale_x_continuous(breaks=seq(1,iter,5)) + 
  scale_y_continuous(breaks=seq(choose(BoardSize,2)/2,choose(BoardSize,2),0.1)) + 
  theme_bw()

# example of several first generation boards
viewBoard(population0[[1]])
viewBoard(population0[[2]])
viewBoard(population0[[3]])

# example of several last generation boards
viewBoard(population[[1]])
viewBoard(population[[2]])
viewBoard(population[[3]])

# board that attains max possible fitness score
viewBoard(final)
