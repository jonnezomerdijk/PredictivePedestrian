# rm(list=ls())
source("Code/PredictivePedestrianA7.R")
source("Code/conflictingFunc.R")

#### Settings where to save output ----
output_dir <- output_dir
makeDirectories(nam,output_dir,doPDF=FALSE)

#### Define and plot space ---

# Space parameters
AAl <- 40; AAw <- 25 # assume a (rectangular) AAlength m x AAwidth m activity area 
dl <- 2              # door length
sw <- 1.2            # shelve width
pw <- 4              # pass width

# Hallway AAw wide and AAl long with 2 dMin wide doorways at either end
objects <-
  list(list(x=c(-.5,AAl+.5),          y=c(-.5,AAw+.5)), # space
       list(x=c(pw,AAl-pw),           y=c(-.5,sw-.5)), # bottom wall shelf
       list(x=c(AAl-sw,AAl+.5),       y=c(2*pw,AAw)), # right wall shelf
       list(x=c(-.5,sw),              y=c(-.5+pw,-sw+AAw/2.2)), # left wall bottom shelf
       list(x=c(-.5,sw),              y=c(.5+AAw-1.5*pw,.5+AAw-1.5*dl)), # left wall top shelf
       list(x=c(-.5,(AAl/2.5)),       y=c(.5+AAw-1.5*dl,AAw+.5)), # top wall left shelf
       list(x=c(AAl/2.5,AAl+.5),      y=c(AAw,AAw+.5)), # top wall right shelf
       list(x=c(-.5,dl),              y=c(-sw+AAw/1.65,AAw/1.65)), # exit counter
       list(x=c(-.5,AAl/2.3),         y=c(-sw+AAw/2.2,AAw/2.2)), # vertical divider shelf 1
       list(x=c(AAl/2.1+pw,AAl-pw),   y=c(-sw+AAw/2.2,AAw/2.2)), # vertical divider shelf 2
       list(x=c(AAl-2*sw,AAl-sw),     y=c(dl,1.5*pw)), # bottom right vert midshelf
       list(x=c(1.5*pw,AAl/2.2),      y=c(sw+dl,2*sw+dl)), # bottom left midshelf 1
       list(x=c(1.5*pw,AAl/2.2),      y=c(2*sw+2*dl,3*sw+2*dl)), # bottom left midshelf 2
       list(x=c(AAl/2+dl,AAl-1.5*pw), y=c(sw+dl,2*sw+dl)), # bottom right midshelf 1
       list(x=c(AAl/2+dl,AAl-1.5*pw), y=c(2*sw+2*dl,3*sw+2*dl)), # bottom right midshelf 2
       list(x=c(1.5*pw,AAl/2.1),      y=c(AAw-(sw+2.3*dl),AAw-2.3*dl)), # top left midshelf 1
       list(x=c(1.5*pw,AAl/2.1),      y=c(AAw-(sw+3.8*dl),AAw-3.8*dl)), # top left midshelf 2
       list(x=c(1.5*pw,AAl/2.1),      y=c(AAw-(sw+5.3*dl),AAw-5.3*dl)), # top left midshelf 3
       list(x=c(AAl/2.1+pw,AAl-pw),   y=c(AAw-(sw+.8*dl),AAw-.8*dl)), # top right midshelf 1
       list(x=c(AAl/2.1+pw,AAl-pw),   y=c(AAw-(sw+2.3*dl),AAw-2.3*dl)), # top right midshelf 2 
       list(x=c(AAl/2.1+pw,AAl-pw),   y=c(AAw-(sw+3.8*dl),AAw-3.8*dl)), # top right midshelf 3
       list(x=c(AAl/2.1+pw,AAl-pw),   y=c(AAw-(sw+5.3*dl),AAw-5.3*dl))) # top right midshelf 4

# List to save with trace 
pSpace <- list(AAl=AAl,AAw=AAw,dl=dl,sw=sw,pw=pw)

# Preplot space
if (plotSim) plotSpace(objects,pSpace)

# NOT SURE WHAT THIS IS ABOUT, COMMENTED OUT
# surface <- c()
# for (object in 1:length(objects)) {
#   surface[object] <-
#     (objects[object][[1]]$x[2]-objects[object][[1]]$x[1])*(objects[object][[1]]$y[2]-objects[object][[1]]$y[1])
# }
# surface[1] - sum(surface[2:length(surface)])


#### Parameters ----
nests <- list(Central=c(0,6,17,28),NonCentral=c(1:33)[-c(0,6,17,28)],
              acc=c(1:11),const=12:22,dec=c(0,23:33))
# All alternatives a member of 2 nests so alwasy alpha=.5
alpha <- setAlpha(nests)
# nest association = 1/(1-mu), mu = within nest precision, >= 1
nA <- rep(0,5); names(nA) <- names(nests)
p <- c(nA,
       c(rU = randomness,    # Utility randomness, divides utility
         bS = 1e5,        # Dont move
         bCA=1,aCA=1.5,   # Current angle
         bCAlr=10,        # left (<1) right (>1) preference        
         bGA=8,aGA=2,     # Goal angle
         bBA=4,aBA=2,     # Blocked angle
         bID=1.3,aID=2,   # Interpersonal distance
         dID=2,           # extra for outGroup
         bPS=4,aPS=2,     # Prefered speed
         sPref=1,         # ~ 3.5 km/h
         bGD=1,aGD=2,     # Group distance
         bFL=1,aFL=2,     # Follow the leader
         dFL=1            # extra for inGroup
       )
)
p <- toReal(p)

# Individual variability
# No variability
pSD  <- rep(0,length(p))
names(pSD) <- names(p)

# --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # ---

# Start 1 pedestrian from entrance in left bottom corner
p_n <- rbind(c(dl/2,dl/2))
dimnames(p_n) <- list(c("A_1"),c("x","y"))


# Goal stacks
Area1 <- rbind(c(1.5,4.5),c(1.5,9),c(5,10),c(10,10),c(15,10),
               c(10,7.8),c(15,7.8),c(10,6.2),c(15,6.2),c(10,4.5),c(15,4.5),
               c(10,3),c(15,3),c(5,.9),c(10,.9),c(15,.9),c(20,.9))
Area2 <- rbind(c(25,10),c(30,10),c(35,10),c(25,7.8),c(30,7.8),
               c(25,6.2),c(30,6.2),c(25,4.5),c(30,4.5),c(38.5,6.2),
               c(38.5,9),c(38.5,1.8),c(25,3),c(30,3),c(25,.9),c(30,.9),c(35,.9))
Area3 <- rbind(c(25,11.5),c(30,11.5),c(35,11.5),c(25,13),c(30,13),c(35,13),
               c(25,14.5),c(30,14.5),c(35,14.5),c(25,16),c(30,16),c(35,16),
               c(25,17.5),c(30,17.5),c(35,17.5),c(25,19),c(30,19),c(35,19),
               c(25,20.5),c(30,20.5),c(35,20.5),c(25,22),c(30,22),c(35,22),
               c(25,23.5),c(30,23.5),c(35,23.5),c(20,24.8),c(25,24.8),c(30,24.8),
               c(35,24.8),c(38.5,12.2),c(38.5,15.2),c(38.5,18.2),
               c(38.5,21.1),c(38.5,23.8))
Area4 <- rbind(c(5,11.5),c(10,11.5),c(15,11.5),c(10,13),c(15,13),
               c(10,14.5),c(15,14.5),c(10,16),c(15,16),c(10,17.5),c(15,17.5),
               c(10,19),c(15,19),c(10,20.5),c(15,20.5),c(1.5,21),
               c(5,22.4),c(10,22.4),c(15,22.4))
underWall <- rbind(c(5,10),c(10,10),c(15,10),c(25,10),c(30,10),c(35,10),c(5,22.4),
                   c(10,22.4),c(15,22.4),c(20,24.8),c(25,24.8),c(30,24.8),c(35,24.8),
                   c(10,3),c(15,3),c(25,3),c(10,6.2),c(15,6.2),c(25,6.2),c(30,6.2),
                   c(30,3),c(25,22),c(30,22),c(35,22),c(10,19),c(15,19),c(25,19),
                   c(30,19),c(35,19),c(10,16),c(15,16),c(25,16),c(30,16),c(35,16),
                   c(10,13),c(15,13),c(25,13),c(30,13),c(35,13),c(38.5,1.8))
aboveWall <- rbind(c(5,11.5),c(10,11.5),c(15,11.5),c(25,11.5),c(30,11.5),c(35,11.5),c(5,22.4),
                   c(10,20.5),c(15,20.5),c(25,23.5),c(30,23.5),c(35,23.5),c(10,7.8),c(15,7.8),
                   c(25,7.8),c(30,7.8),c(38.5,6.2),c(10,4.5),c(15,4.5),c(25,4.5),c(30,4.5),
                   c(5,.9),c(10,.9),c(15,.9),c(20,.9),c(25,.9),c(30,.9),c(35,.9),c(10,20.5),
                   c(15,20.5),c(25,20.5),c(30,20.5),c(35,20.5),c(10,17.5),c(15,17.5),
                   c(25,17.5),c(30,17.5),c(35,17.5),c(10,14.5),c(15,14.5),c(25,14.5),c(30,14.5),
                   c(35,14.5),c(5,11.5),c(10,11.5),c(15,11.5),c(25,11.5),c(30,11.5),c(35,11.5))
leftofWall <- rbind(c(38.5,9),c(38.5,12.2),c(38.5,15.2),c(38.5,18.2),c(38.5,21.1),c(38.5,23.8))
rightofWall <- rbind(c(1.5,4.5),c(1.5,9),c(1.5,21))
middleIsle <- rbind(c(20,2),c(20,5.6),c(20,8.8),c(20,10.7),c(21,12.4),
                    c(21,15.3),c(21,18),c(21,21),c(20,23.5))

AllAreas <- rbind(Area1,Area2,Area3,Area4)
Exits <- rbind(c(0,pw+AAw/2),c(0,AAw/2))
# Default path
default <- rbind(c(3,2),c(20,2),c(20,5.6),c(20,8.8),c(20,10.7),c(21,12.4),c(21,15.3),
                 c(21,18),c(21,21),c(20,24),c(28,24),c(20,24),c(15,21.5),
                 c(4,21.5),c(4,15.3))

# used in plotGrid
extras <- list(Area1=Area1,Area2=Area2,Area3=Area3,Area4=Area4,Exits=Exits,default=default)

# Preplot pedestrians and goals
# nams <- unlist(lapply(strsplit(row.names(p_n),"_"),function(x){x[1]}))
# points(p_n,pch=nams)
# for (d in 1:nrow(Area1)) draw_circle(Area1[d,1],Area1[d,2],.2, border="blue")
# for (d in 1:nrow(Area2)) draw_circle(Area2[d,1],Area2[d,2],.2, border="red")
# for (d in 1:nrow(Area3)) draw_circle(Area3[d,1],Area3[d,2],.2, border="green")
# for (d in 1:nrow(Area4)) draw_circle(Area4[d,1],Area4[d,2],.2, border="purple")
# for (d in 1:nrow(default)) draw_circle(default[d,1],default[d,2],.1, border="gray")
# for (d in 1:nrow(Exits)) draw_circle(Exits[d,1],Exits[d,2],.3, border="black")
#for (d in 1:nrow(mV)) drawSquare(list(x=c(mV[d,1],mV[d,2]),
#                                      y=c(mV[d,3],mV[d,4])))

## Goal stack functions ##

Goal_stack_ordered <- function(width=3,height=3)
  # Randomly picks a goal from each area in location order and ends with one of the exits
{
  G <- rbind(Area1[sample(nrow(Area1),1,replace=T),],Area2[sample(nrow(Area2),1,replace=T),],
             Area3[sample(nrow(Area3),1,replace=T),],Area4[sample(nrow(Area4),1,replace=T),],
             Exits[sample(nrow(Exits),1,replace=T),])
  colnames(G) <- c("x","y"); attr(G,"i") <- 1;
  mV <- matrix(NA,nrow(G),ncol=4)
  for (d in 1:nrow(G)) {
    if (all(G[d,] %in% middleIsle)) {
      w <- width*1.8; h <- height*.8} else {w <- width; h <- height}
    w1 <- w2 <- w
    h1 <- h2 <- h
    if (all(G[d,] %in% underWall)) h2 <- h/3
    if (all(G[d,] %in% aboveWall)) h1 <- h/3
    if (all(G[d,] %in% leftofWall)) w2 <- w/3
    if (all(G[d,] %in% rightofWall)) w1 <- w/3
    mV[d,] <- c(x=c(G[d,1]-(w1/2),G[d,1]+(w2/2)),
                y=c(G[d,2]-(h1/2),G[d,2]+(h2/2)))
  }
  colnames(mV) <- c("x1","x2","y1","y2")
  attr(G,"mustStop") <- mV
  attr(G,"stop") <- 0 # Give them all a stop attribute
  G
}

Goal_stack_random <- function(n=4,width=3,height=3)
  # Randomly picks goals from any area and ends with one of the exits
{
  G <- rbind(AllAreas[sample(nrow(AllAreas),n,replace=F),],
             Exits[sample(nrow(Exits),1),])
  colnames(G) <- c("x","y"); attr(G,"i") <- 1;
  mV <- matrix(NA,nrow(G),ncol=4)
  for (d in 1:nrow(G)) {
    if (all(G[d,] %in% middleIsle)) {
      w <- width*1.8; h <- height*.8} else {w <- width; h <- height}
    w1 <- w2 <- w
    h1 <- h2 <- h
    if (all(G[d,] %in% underWall)) h2 <- h/3
    if (all(G[d,] %in% aboveWall)) h1 <- h/3
    if (all(G[d,] %in% leftofWall)) w2 <- w/3
    if (all(G[d,] %in% rightofWall)) w1 <- w/3
    mV[d,] <- c(x=c(G[d,1]-(w1/2),G[d,1]+(w2/2)),
                y=c(G[d,2]-(h1/2),G[d,2]+(h2/2)))
  }
  colnames(mV) <- c("x1","x2","y1","y2")
  attr(G,"mustStop") <- mV
  attr(G,"stop") <- 0 # Give them all a stop attribute
  G
}

Default_path <- function(n=4) {
  # Uses all default path subgoals and adds 4 random
  #  goals from all areas, ends with one of the exits
  GR <- Goal_stack_random(n=n)
  G <- rbind(default,GR) # A difference between these two goal types is still needed
  colnames(G) <- c("x","y"); attr(G,"i") <- 1
  attr(G,"mustStop") <- rbind(matrix(NA,nrow(default),ncol=4),attr(GR,"mustStop"))
  attr(G,"stop") <- 0 # Give them all a stop attribute
  G
}

# Hand out goal stack type
goalstack <- function(type="Ordered",n=4) {
  if (type=="Ordered") P_n <- list(Goal_stack_ordered())
  if (type=="Random") P_n <- list(Goal_stack_random(n=n))
  if (type=="Seeking") P_n <- list(Default_path(n=n))
  P_n
}


## Current Retail script ##

P_n <- goalstack("Seeking",n=goalStackSize)
numLetter <- rep(1,52)
names(numLetter) <- c(LETTERS,letters)
attr(P_n,"numLetter") <- numLetter
attr(P_n,"numLetter")["A"] <- attr(P_n,"numLetter")["A"] + 1

# Head to first objective first
if (is.list(P_n)) {
  # find first element from each target list
  target <- matrix(unlist(lapply(1:length(P_n),function(x)(t(P_n[[x]])[1:2]))), 
                   ncol = 2, byrow=T)
} else {
  target <- matrix(P_n[1,], nrow=1)
}

a_n <- angle2(p_n,target)
names(a_n) <- row.names(p_n)

# All start with a velocity of 1
v_n <- rep(1,length(a_n))
names(a_n) <- row.names(p_n)
names(v_n) <- row.names(p_n)

# Give them all the same radius
r_n <- rep(0.65/2,length(a_n))

# Social group
group=1
names(group) <- row.names(p_n)
# groupProb <- c(.3,.3,.15,.15,.05,.05)
groupProb <- c(1,0)
# group members sync prefered velocity
matchvPref <- T

# Run from here to start with original state
startState <- state <- list(p=p_n,v=v_n,r=r_n,a=a_n,P=P_n,group=group)
state$pMat <- getpMat(length(a_n),p,pSD,row.names(state$p))

# Inspection options
# Show choice grid with centre dots proportional to probability of choice
showMind <- rep(FALSE,length(a_n))
# showMind[1] <- TRUE # Show a particular bug
# showMind <- rep(TRUE,length(a_n)) # show all
showAll = T # Full interaction for all pedestrians

# Print out chocies and choice probabability
printChoice <- rep(FALSE,length(a_n))
# printChoice[1] <- TRUE
# For anything with printChoice also plot compoments of utlity
# plotComponents <- TRUE
plotComponents <- T

# parallel process
cores <- n_cores

# store capacity
capacity <- max_cap


# Setup space
if (plotSim) {
  plotPed(p_n,getP(state),a_n,state$r,extras,objects,plotGoal=FALSE)
  for (i in 1:dim(p_n)[1]) draw_grid(p_n[i, ], v_n[i], a_n[i], plotPoints = FALSE)
}

avmin <- avminDist(state$p)
# Counter to keep track of iteration
N=1; cat(paste0(N,":",nrow(state$p),"\n"))
trace <- vector(mode="list")
# start_time <- Sys.time()
# NOTE: TO GET ITERATION RUNNING PROPERTLY SOUREE ABOVE FIRST THEN SOURCE REPEAT
repeat {
  # Determine file name
  file_name <- paste(output_dir, "/", nam, "/", "plot_", N, sep = "")
  # Add current state to trace
  trace <- c(trace, list(state))
  # Add reduced info goals to trace, comment out for full info
  # trace[[N]]$P <- getP(state)
  # Create new state
  state <- moveAll(state,objects,nests,alpha,delay=0,cores=cores,reach=25,
                   plotPath=TRUE,plotSim=plotSim,usebestAngle=FALSE,
                   fname=file_name,extras=extras,
                   interactionTime=interactionTime) # move states
  if (dim(state$p)[1]==0) break
  # Add chosen cell to state, comment out for simulation
  # trace[[N]]$cell <- state$cell
  
  # Remove pedestrians at exit state
  state <- exitPed(state, AAl) # if reached goal exit
  if (dim(state$p)[1]==0) break # no one left = break (preventing crash)

  N <- N+1 # new iteration
  cat(paste0(N,":",nrow(state$p),"\n"))
  if (N %% 3 == 0) { # nr of iterations before adding pedestrians 
    if (sum(apply(state$p,1,function(x){x[1]<10&x[2]<10})) < 8) { # add pedestrian if no one near entrance
      if (dim(state$p)[1] < capacity) { # if capacity is not reached
        groupSize <- sample(1:2,1,prob=groupProb) # set group size
        state <- addPed(state,startState,p,goaltype="Seeking",pSD,groupSize,matchvPref) # add pedestrian to state
      }
    }
  }
  if (N > n_iterations) break
}

# # makeGifs(nam,output_dir)
attr(trace,"objects") <- objects
attr(trace,"pSpace") <- pSpace
attr(trace,"extras") <- extras
saveRDS(trace, file = paste(output_dir,"/",nam, ".RDS", sep = ""))
# apply(avmin[!is.nan(avmin[, 1]), ], 2, min)
# traceDF <- harvestTrace(trace) # sets for estimation
# par(mfrow=c(2, 2)) # escape -> open following plots on stats
# plot(unlist(lapply(trace, function(x){dim(x$p)[1]})), ylab="Number of Pedestrians")
# hist(traceDF$cell, breaks=0:34)
# hist(traceDF$v, breaks="fd")
# hist(avmin[avmin[, 2] < 1, 2],breaks=seq(0, 1, length.out=20), xlab="Closest Distance", main="")
# save(trace, file="Retail.RData")
