# view board in matrix form
vb = function(b){
  matrix(b,nrow=sqrt(length(b)),byrow=T)
}


# permute r objects in n space
permn = function(n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE) {
  if (mode(n) != "numeric" || length(n) != 1 || n < 1 || (n%%1) != 
      0) 
    stop("bad value of n")
  if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) != 
      0) 
    stop("bad value of r")
  if (!is.atomic(v) || length(v) < n) 
    stop("v is either non-atomic or too short")
  if ((r > n) & repeats.allowed == FALSE) 
    stop("r > n and repeats.allowed=FALSE")
  if (set) {
    v <- unique(sort(v))
    if (length(v) < n) 
      stop("too few different elements")
  }
  v0 <- vector(mode(v), 0)
  if (repeats.allowed) 
    sub <- function(n, r, v) {
      if (r == 1) 
        matrix(v, n, 1)
      else if (n == 1) 
        matrix(v, 1, r)
      else {
        inner <- Recall(n, r - 1, v)
        cbind(rep(v, rep(nrow(inner), n)), matrix(t(inner), 
                                                  ncol = ncol(inner), nrow = nrow(inner) * n, 
                                                  byrow = TRUE))
      }
    }
  else sub <- function(n, r, v) {
    if (r == 1) 
      matrix(v, n, 1)
    else if (n == 1) 
      matrix(v, 1, r)
    else {
      X <- NULL
      for (i in 1:n) X <- rbind(X, cbind(v[i], Recall(n - 
                                                        1, r - 1, v[-i])))
      X
    }
  }
  sub(n, r, v[1:n])
}


# some parameters to create u (universal) list of all possible states
# as x goes first, the states always have one more x than o
vec = 1:9
board = rep(NA,9) 
u = list()

# for states with only x's
for(i in 1:9){
  tmp = board
  tmp[i] = 'x'
  u[[i]] = tmp
}

# the rest
for (k in 2:5) {
  mx = permn(n=9,r=k,v=vec)
  for (i in 1:nrow(mx)) {
    tmp = board
    xpos = mx[i,]
    mo = permn(n=length(vec)-length(mx[i,]),r=length(mx[i,])-1,v=vec[!(vec%in%mx[i,])])
    tmp[xpos] = 'x'
    for(j in 1:nrow(mo)) {
      tmp2 = tmp
      tmp2[mo[j,]] = 'o'
      u[[length(u)+1]] = tmp2
    }
  }
}

# remove duplicates as multiple x's and o's are indistinguishable under permutation
u = unique(u)

# check if player wins
checkwin = function(b,p){ # b:board, p:player
  b = matrix(b,nrow=sqrt(length(b)),byrow=T)
  if(sum(b[1,]==p,na.rm=T)==3 |
     sum(b[2,]==p,na.rm=T)==3 | 
     sum(b[3,]==p,na.rm=T)==3 | 
     sum(b[,1]==p,na.rm=T)==3 |
     sum(b[,2]==p,na.rm=T)==3 | 
     sum(b[,3]==p,na.rm=T)==3 | 
     sum(b[rbind(c(1,1),c(2,2),c(3,3))]==p,na.rm=T)==3 | 
     sum(b[rbind(c(1,3),c(2,2),c(3,1))]==p,na.rm=T)==3 
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# check if board y is a subset of board x
checkBoard = function(x,y){
  idx = which(!is.na(y))
  if(sum(x[idx]==y[idx],na.rm=T)==length(idx)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# remove dups
# dup = NULL
# for(i in 1:length(u)){
#   dup[i] = checkwin(u[[i]],"x") + checkwin(u[[i]],"o")
# }
# u[dup==2] = NULL



# Q matrix
score = matrix(rep(0,length(u)*9),ncol=9,byrow=T)
for(i in 1:length(u)){
  if(sum(is.na(u[[i]]))>0){score[i,!is.na(u[[i]])] = NA}
}

#  plays first
epsilon = 0.4
lr = 1
gamma = 0.9

reward_list = NULL
board_list = list()
ascoreall = NULL

B = 50000
delta = epsilon / B

for(k in 1:B){
  
  #-----TRAINING-----#
  print(paste0('k:', k))
  count = 0
  board = rep(NA,9)
  reward = 0
  
  # agent first move
  afmove = sample(which(is.na(board)),1)
  board[afmove] = 'x'
  idx = which(lapply(u,function(x)identical(x,board))==TRUE)
  
  while(TRUE) {
    
    # 1) human move
    hmove = sample(which(is.na(board)),1) # human random move
    board[hmove] = 'o'
    if(checkwin(board,'o')){reward = 0}
    
    # 2) agent move
    apmove = which(!is.na(score[idx,])) # agent possible moves
    apmove = apmove[apmove!=hmove]
    abmove = apmove[which(score[idx,apmove]==max(score[idx,apmove],na.rm=T))] # best among possible moves
    if(length(abmove)>1){abmove = sample(abmove,1)} # if agent have more than one best move, randomly select 1
    if(runif(1)>=epsilon | count==3){ # final agent move by using greedy epsilon, after 3 counts, there's only one spot left to fill in
      afmove = abmove # best move
    } else {
      afmove = sample(apmove[apmove!=abmove],1) # pick one random move
    }
    board[afmove] = 'x' 
    if(checkwin(board,'x')){reward = 1} 
    if(!checkwin(board,'x') & !checkwin(board,'o') & (sum(is.na(board))==0)){reward = 0.5} 
    
    # update Q matrix
    idx_old = idx 
    afmove_old = afmove
    idx = which(lapply(u,function(x)identical(x,board))==TRUE)
    score[idx_old,afmove_old] = max(score[idx_old,afmove_old] + lr*(reward+gamma*max(score[idx,],na.rm=T)-score[idx_old,afmove_old]),0)
    
    # check to stop game
    if(count==3 | reward %in% c(1,0.5)){break}
    count = count + 1
  }

  
  #-----TESTING (10 games)-----#
  ascore=0
  
  for(j in 1:10){
    
    count = 0
    board = rep(NA,9)
    reward = 0
    
    # agent first move
    afmove = sample(which(is.na(board)),1)
    board[afmove] = 'x'
    idx = which(lapply(u,function(x)identical(x,board))==TRUE)
    
    while(TRUE) {
      
      # human random move
      hmove = sample(which(is.na(board)),1)
      board[hmove] = 'o'
      if(checkwin(board,'o')){reward = 0}
      
      # agent possible moves
      apmove = which(!is.na(score[idx,]))
      apmove = apmove[apmove!=hmove]
      abmove = apmove[which(score[idx,apmove]==max(score[idx,apmove],na.rm=T))]
      if(length(abmove)>1){abmove = sample(abmove,1)} 
      afmove = abmove
      board[afmove] = 'x' 
      if(checkwin(board,'x')){reward = 1} 
      if(!checkwin(board,'x') & !checkwin(board,'o') & (sum(is.na(board))==0)){reward = 0.5} 
  
      
      idx = which(lapply(u,function(x)identical(x,board))==TRUE)
      
      if(count==3 | reward %in% c(1,0.5)){break}
      count = count + 1
    }
    ascore = ascore + reward
  }
  ascoreall = c(ascoreall,mean(ascore))
  if((k %% 50) == 0){
    results = tibble(epochs=1:length(ascoreall),avgscore=ascoreall)
    p = ggplot(results) + geom_path(aes(x=epochs,y=avgscore)) + ylab('Average score against random player')
    show(p)
  }
  epsilon = epsilon - delta
}


library(tidyverse)
results = tibble(epochs=1:B,avgscore=ascoreall[1:B])
p = ggplot(results) + geom_path(aes(x=epochs,y=avgscore)) + ylab('Average score against random player')
ggsave('p.pdf',p,width=15,height=8)
saveRDS(p,'plot.rds')
saveRDS(score,'score.rds')


# function to play game with agent
play = function(){
  
  count = 0
  board = rep(NA,9)
  reward = 0
  
  afmove = sample(which(is.na(board)),1)
  board[afmove] = 'x'
  idx = which(lapply(u,function(x)identical(x,board))==TRUE)
  
  while(TRUE) {
    
    print('Agent moved!')
    print(vb(board))
    
    # human random move
    hrmove = readline(prompt="Enter row position: ")
    hcmove = readline(prompt="Enter column position: ")
    hrmove = as.integer(hrmove)
    hcmove = as.integer(hcmove)
    if(hrmove==1 & hcmove==1) hmove=1
    if(hrmove==1 & hcmove==2) hmove=2
    if(hrmove==1 & hcmove==3) hmove=3
    if(hrmove==2 & hcmove==1) hmove=4
    if(hrmove==2 & hcmove==2) hmove=5
    if(hrmove==2 & hcmove==3) hmove=6
    if(hrmove==3 & hcmove==1) hmove=7
    if(hrmove==3 & hcmove==2) hmove=8
    if(hrmove==3 & hcmove==3) hmove=9
    
    hmove = as.integer(hmove)
    board[hmove] = 'o'
    print(vb(board))
    if(checkwin(board,'o')){
      print('You won!')
      break
    }
    
    # agent move
    apmove = which(!is.na(score[idx,]))
    apmove = apmove[apmove!=hmove]
    abmove = apmove[which(score[idx,apmove]==max(score[idx,apmove],na.rm=T))]
    if(length(abmove)>1){abmove = sample(abmove,1)} 
    afmove = abmove 
    board[afmove] = 'x' 
    if(checkwin(board,'x')){
      print('Agent won!')
      break
    }
    if(!checkwin(board,'x') & !checkwin(board,'o') & (sum(is.na(board))==0)){
      print('Draw!')
      break
    } 
    
    idx = which(lapply(u,function(x)identical(x,board))==TRUE)
    
    count = count + 1
  }
  return(vb(board))
}

# play()

