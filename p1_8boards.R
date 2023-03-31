# number of misplaced tile(s)
h1 = function(initial,goal,N){
  goal == initial
  excl = (initial != 0)
  return(sum(goal[excl] != initial[excl]))
}

# manhattan distance
h2 = function(initial,goal,N){
  col_pos = rep(1:N,times=N);row_pos = rep(1:N,each=N)
  steps = NULL
  for(i in initial){
    if(i==0) next
    col_diff = abs(col_pos[initial==i] - col_pos[goal==i])
    row_diff = abs(row_pos[initial==i] - row_pos[goal==i])
    steps = c(steps,col_diff+row_diff)
  }
  return(sum(steps))
}

# manhattan distance for misplaced tiles
h3 = function(initial,goal,N){
  
  #initial = c(1,2,3,7,4,5,0,8,6)
  #goal = c(1,2,3,4,5,6,7,8,0)
  
  col_pos = rep(1:N,times=N);row_pos = rep(1:N,each=N)
  steps = NULL
  mismatch = which(initial != goal)
  
  for(i in initial[mismatch]){
    if(i==0) next
    col_diff = abs(col_pos[initial==i] - col_pos[goal==i])
    row_diff = abs(row_pos[initial==i] - row_pos[goal==i])
    steps = c(steps,col_diff+row_diff)
  }
  return(sum(steps))
}


count_inversions = function(x){
  count=0;inversions=NULL
  for(i in 1:(length(x)-1)){
    if(x[i]==0) next
    x_after_i = x[(i+1):length(x)]
    smaller_vals = x_after_i[x_after_i < x[i]]
    smaller_vals = smaller_vals[smaller_vals!=0]
    if(length(smaller_vals)>0){
      inversions = c(inversions,paste0(x[i],'-',smaller_vals))
    }
    count = count + length(smaller_vals)
  }
  if((count %% 2) == 0){
    return('even')
  } else {
    return('odd')
  }
}

check_reachability = function(initial,goal){
  return(count_inversions(initial)==count_inversions(goal))
}


tile_move = function(x){
  
  N = sqrt(length(x))
  x0 = x
  
  # corner positions
  
  # top left
  if(x[1]==0){
    xs = list()
    o=sample(1:2)
    x[1]=x[2];x[2]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[1]=x[1+N];x[1+N]=0 # sequence 2
    #print(matrix(x,nrow=N))
    xs[[o[2]]] = x
  }
  
  # top right
  else if(x[N]==0){
    xs = list()
    o=sample(1:2)
    x[N]=x[N-1];x[N-1]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[N]=x[N*2];x[N*2]=0 # sequence 2
    #print(matrix(x,nrow=N))
    xs[[o[2]]] = x
  }
  
  # bottom left
  else if(x[N^2-N+1]==0){
    xs = list()
    o=sample(1:2)
    x[N^2-N+1]=x[N^2-N+2];x[N^2-N+2]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[N^2-N+1]=x[N^2-2*N+1];x[N^2-2*N+1]=0 # sequence 2
    #print(matrix(x,nrow=N))
    xs[[o[2]]] = x
  }
  
  # bottom right
  else if(x[N^2]==0){
    xs = list()
    o=sample(1:2)
    x[N^2]=x[N^2-1];x[N^2-1]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[N^2]=x[N^2-N];x[N^2-N]=0 # sequence 2
    #print(matrix(x,nrow=N))
    xs[[o[2]]] = x
  }
  
  # top border
  else if(0 %in% x[2:(N-1)]){
    xs = list()
    o=sample(1:3)
    i = which(x==0)
    x[i]=x[i-1];x[i-1]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[i]=x[i+1];x[i+1]=0 # sequence 2
    #print(matrix(x,nrow=N))
    xs[[o[2]]] = x
    x = x0
    x[i]=x[i+N];x[i+N]=0 # sequence 3
    #print(matrix(x,nrow=N))
    xs[[o[3]]] = x
  }
  
  # bottom border
  else if(0 %in% x[(N^2-N+2):(N^2-1)]){
    xs = list()
    o=sample(1:3)
    i = which(x==0)
    x[i]=x[i-1];x[i-1]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[i]=x[i+1];x[i+1]=0 # sequence 2
    #print(matrix(x,nrow=N))
    xs[[o[2]]] = x
    x = x0
    x[i]=x[i-N];x[i-N]=0 # sequence 3
    #print(matrix(x,nrow=N))
    xs[[o[3]]] = x
  }
  
  
  # left border
  else if(0 %in% x[(N+1):(N^2-2*N+1)]){
    xs = list()
    o=sample(1:3)
    i = which(x==0)
    x[i]=x[i+N];x[i+N]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[i]=x[i-N];x[i-N]=0 # sequence 2
    #print(matrix(x,nrow=N))
    xs[[o[2]]] = x
    x = x0
    x[i]=x[i+1];x[i+1]=0 # sequence 3
    #print(matrix(x,nrow=N))
    xs[[o[3]]] = x
  }
  
  # right border
  else if(0 %in% x[(2*N):(N^2-N)]){
    xs = list()
    o=sample(1:3)
    i = which(x==0)
    x[i]=x[i+N];x[i+N]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[i]=x[i-N];x[i-N]=0 # sequence 2
    #print(matrix(x,nrow=N))
    xs[[o[2]]] = x
    x = x0
    x[i]=x[i-1];x[i-1]=0 # sequence 3
    #print(matrix(x,nrow=N))
    xs[[o[3]]] = x
  }
  
  # everywhere else
  else {
    xs = list()
    o=sample(1:4)
    i = which(x==0)
    x[i]=x[i+N];x[i+N]=0 # sequence 1
    xs[[o[1]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[i]=x[i-N];x[i-N]=0 # sequence 1
    xs[[o[2]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[i]=x[i+1];x[i+1]=0 # sequence 1
    xs[[o[3]]] = x
    #print(matrix(x,nrow=N))
    x = x0
    x[i]=x[i-1];x[i-1]=0 # sequence 1
    xs[[o[4]]] = x
    #print(matrix(x,nrow=N))
  }
  
  return(xs)
  
}

print_result = function(x){
  cat('Counter:\n\n')
  print(x$counter)
  cat('\n\nSolution Path:\n\n')
  print(x$path)
  cat('\n\nSolution Path in Matrix Form:\n\n')
  print(lapply(x$path,function(x)matrix(x,nrow=sqrt(length(x)),byrow=T)))
}


# BF: best-first search

BF = function(initial,goal,heuristic){

  counter=1
  compr=list()
  stall_list=list()
  path=list()
  path[[counter]]=initial
  
  while(sum(initial==goal)!=length(goal)){
    print(counter)
    
    if(!check_reachability(initial,goal)){
      print('Difference in parity. Solution not reachable. Search will not be executed')
      return(list(counter=counter))
    }
    
    perms = tile_move(initial)
    compr = append(compr,perms)
  
    evaluation = lapply(compr,function(x)heuristic(x,goal,N=sqrt(length(initial))))
    initial = compr[which(unlist(evaluation)==min(unlist(evaluation)))] # chosen node
    compr = compr[-which(unlist(evaluation)==min(unlist(evaluation)))] # remove from comprehensive list
    initial = initial[[1]]

    counter=counter+1
    if(counter==5000) {
      return(list(counter=counter,path=path))
    }
    
    path[[counter]]=initial
  }
  return(list(counter=counter,path=path))
}

# A*

# BF: best-first search

Astar = function(initial,goal,heuristic){
  
  initial0=initial

  counter=1
  compr=list()
  stall_list=list()
  path=list()
  path[[counter]]=initial
  
  while(sum(initial==goal)!=length(goal)){
    print(counter)
    
    if(!check_reachability(initial,goal)){
      print('Difference in parity. Solution not reachable. Search will not be executed')
      return(list(counter=counter))
    }
    
    perms = tile_move(initial)
    compr = append(compr,perms)
    
    evaluation_start_to_current = lapply(compr,function(x)heuristic(x,initial0,N=sqrt(length(initial0))))
    evaluation_current_to_goal = lapply(compr,function(x)heuristic(x,goal,N=sqrt(length(initial))))
    
    evaluation = mapply(sum, evaluation_start_to_current, evaluation_current_to_goal, SIMPLIFY=FALSE)
    
    initial = compr[which(unlist(evaluation)==min(unlist(evaluation)))] # chosen node
    compr = compr[-which(unlist(evaluation)==min(unlist(evaluation)))] # remove from comprehensive list
    initial = initial[[1]]
    
    counter=counter+1
    if(counter==5000) {
      return(list(counter=counter,path=path))
    }
    
    path[[counter]]=initial
  }
  return(list(counter=counter,path=path))
}

# Example runs

# BF example

if(F){
  x = c(1,2,3,
        7,4,5,
        0,8,6)
  y = c(1,2,3,
        4,5,6,
        7,8,0)
  
  out = BF(x,y,h1)
  print_result(out)
  
  out = BF(x,y,h2)
  print_result(out)
  
  out = BF(x,y,h3)
  print_result(out)
  
  
  x = c(1,2,3,4,
        5,6,7,8,
        9,10,11,12,
        13,0,14,15)
  y = c(1,2,3,4,
        5,6,7,8,
        9,10,11,12,
        13,14,15,0)
  
  out = BF(x,y,h1)
  print_result(out)
  
  out = BF(x,y,h2)
  print_result(out)
  
  out = BF(x,y,h3)
  print_result(out)
  
  # Astar examples
  
  x = c(1,2,3,
        7,4,5,
        0,8,6)
  y = c(1,2,3,
        4,5,6,
        7,8,0)
  
  out = Astar(x,y,h1)
  print_result(out)
  
  out = Astar(x,y,h2)
  print_result(out)
  
  out = Astar(x,y,h3)
  print_result(out)
  
  x = c(1,2,3,4,
        5,6,7,8,
        9,10,11,12,
        13,0,14,15)
  y = c(1,2,3,4,
        5,6,7,8,
        9,10,11,12,
        13,14,15,0)
  
  out = Astar(x,y,h1)
  print_result(out)
  
  out = Astar(x,y,h2)
  print_result(out)
  
  out = Astar(x,y,h3)
  print_result(out)
}

# 1) input 2 vectors, 1 2 3 4 5 6 7 8 0 will be c(1,2,3,4,5,6,7,8,0)
# x = 
# y = c(1,2,3,4,5,6,7,8,0) # goal

# 2) 
# to run BF
# BF(x,y,h3) # (initial, goal, heuristic)

# to run A*
# Astar(x,y,h1) # (initial, goal, heuristic)

# 3)
# save run to an intermediate variable
# and use function print_result(<intermediate variable>) to print out steps and counters

# 4)
# This problem also include the extra credit case for 15-piece puzzle, you do not need to do anything
# beside inputting the correct size vector, either size 9 for 8-piece and size 16 for 15-piece.

# some examples are shown above for reference