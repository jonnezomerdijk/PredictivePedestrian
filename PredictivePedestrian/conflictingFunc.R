# NEW FUNCTIONS PREDICTIVE PEDESTRIAN
library(parallel)


line.line.intersection <- function(P1, P2, P3, P4, interior.only=FALSE) 
  ##' intersections inside L1 (from P1 to P2) and L2 (from P3 to P4).
  ##' @return Vector containing x,y coordinates of intersection of L1
  ##' and L2.  If L1 and L2 are parallel, this is infinite-valued.  If
  ##' \code{interior.only} is \code{TRUE}, then when the intersection
  ##' does not occur between P1 and P2 and P3 and P4, a vector
  ##' containing \code{NA}s is returned.
  ##' @source Weisstein, Eric W. "Line-Line Intersection."
  ##' From MathWorld--A Wolfram Web Resource.
  ##' \url{http://mathworld.wolfram.com/Line-LineIntersection.html}
  ##' @author David Sterratt
  ##' @export
##' @examples
##' ## Intersection of two lines
##' line.line.intersection(c(0, 0), c(1, 1), c(0, 1), c(1, 0))
##'
##' ## Two lines that don't intersect
##' line.line.intersection(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
{
  P1 <- as.vector(P1)
  P2 <- as.vector(P2)
  P3 <- as.vector(P3)
  P4 <- as.vector(P4)
  
  dx1 <- P1[1] - P2[1]
  dx2 <- P3[1] - P4[1]
  dy1 <- P1[2] - P2[2]
  dy2 <- P3[2] - P4[2]
  
  D <- det(rbind(c(dx1, dy1),
                 c(dx2, dy2)))
  if (D==0) {
    return(c(Inf, Inf))
  }
  D1 <- det(rbind(P1, P2))
  D2 <- det(rbind(P3, P4))
  
  X <- det(rbind(c(D1, dx1),
                 c(D2, dx2)))/D
  Y <- det(rbind(c(D1, dy1),
                 c(D2, dy2)))/D
  
  if (interior.only) {
    ## Compute the fractions of L1 and L2 at which the intersection
    ## occurs
    lambda1 <- -((X-P1[1])*dx1 + (Y-P1[2])*dy1)/(dx1^2 + dy1^2)
    lambda2 <- -((X-P3[1])*dx2 + (Y-P3[2])*dy2)/(dx2^2 + dy2^2)
    if (!((lambda1>0) & (lambda1<1) &
          (lambda2>0) & (lambda2<1))) {
      return(c(NA, NA))
    }
  }
  return(c(X, Y))
}


seesGoal <- function(p_n, P_n, objects) 
# can p see goal P (in any direction), or more genrally any point P_n, 
# or is it occluded by objects?
{
  for (i in 1:length(objects)) {
    intersects = c(
      line.line.intersection(p_n, P_n, c(objects[[i]]$x[1], objects[[i]]$y[1]), 
                c(objects[[i]]$x[2], objects[[i]]$y[1]), interior.only = TRUE),
      line.line.intersection(p_n, P_n, c(objects[[i]]$x[1], objects[[i]]$y[1]), 
                c(objects[[i]]$x[1], objects[[i]]$y[2]), interior.only = TRUE),
      line.line.intersection(p_n, P_n, c(objects[[i]]$x[1], objects[[i]]$y[2]), 
                c(objects[[i]]$x[2], objects[[i]]$y[2]), interior.only = TRUE),
      line.line.intersection(p_n, P_n, c(objects[[i]]$x[2], objects[[i]]$y[1]), 
                c(objects[[i]]$x[2], objects[[i]]$y[2]), interior.only = TRUE)
    )
    if (any(is.finite(intersects))) return(FALSE)
  }
  TRUE  # if none of the objects intersect the view
}


# Now returns NULL when just one pedestrian inside
iCones <- function(p1,a,p2,r,objects)
  # Intersecting cones for closest p2 pedestrian profiles (circles of radius r) from 
  # the perspective of p1 (calcualted by eObject) where the point on the profile 
  # at the cone midline can be seen (i.e., not occluded by objects) given heading 
  # angle a. 
  # Output NULL or cone number named vector of distances from p1 to point of intersection.
{
  
  fix <- function(x){
    # one end in, one out, fill in extreme cone for out
    if (all(is.na(x))) return(NA)
    if (all(is.na(x)==c(FALSE,TRUE))) x <- c(x[1],11) # cw out
    if (all(is.na(x)==c(TRUE,FALSE))) x <- c(1,x[2])  # ac out
    if (length(x)>1) x <- x[1]:x[2] # Fill in intervening
    x
  }
  if (dim(p2)[1]==0) return(NULL)
  ends <- eObjects(p1,p2,r)
  endCones <- apply(ends,3,function(x){Iangle(p1,a,x)})
  if ( dim(ends)[1]==1 ) 
    endCones <- matrix(endCones,nrow=1,dimnames=list(dimnames(p2)[1],NULL))
  cList <- vector(mode="list",length=dim(endCones)[1])
  names(cList) <- dimnames(endCones)[[1]]
  for (i in 1:length(cList)) cList[[i]] <- fix(endCones[i,])
  cList <- cList[unlist(lapply(cList,function(x){!any(is.na(x))}))]
  if (length(cList)==0) return(NULL)
  # List of candidate cones for each participant in front
  cList <- cList[!unlist(lapply(cList,is.null))]
  if (length(cList)==0) return(NULL)
  # end of unit line from p1 in direction of each cone
  coneLineEnds <- c_vd(1:11,as.vector(p1),rep(1,11),a)
  cDist <- vector(mode="list",length=length(cList)) # Distaces to objects in cones
  for (i in names(cList)) { # remove when cant see
    if (length(cList[[i]])==1) {
      if (!seesGoal(p1,p2[i,,drop=FALSE],objects)) cList[[i]] <- numeric(0) else
        cDist[[i]] <- dist1(p1,p2[i,,drop=FALSE])
    } else { # More than one cone to check
      for (j in 1:length(cList[[i]])) {  # Remove cant see
        # Intersection of cone line and end
        P_n <- matrix(
          line.line.intersection(p1,coneLineEnds[cList[[i]][j],],ends[i,,1],ends[i,,2]),
          nrow=1)
        if (!seesGoal(p1,P_n,objects)) cList[[i]][j] <- NA else {
          if (j==1) cDist[[i]] <- dist1(p1,P_n) else
            cDist[[i]] <- c(cDist[[i]],dist1(p1,P_n))
        }
      }
      cList[[i]] <- cList[[i]][!is.na(cList[[i]])]
    }
  }
  cList <- cList[!unlist(lapply(cList,function(x){length(x)==0}))]
  if (length(cList)==0) return(NULL)
  cDist <- cDist[!unlist(lapply(cDist,is.null))]
  for (i in 1:length(cList)) names(cDist[[i]]) <- cList[[i]]
  outCones <- out <- numeric(0)
  for (i in 1:11) {
    d <- unlist(lapply(cDist,function(x){x[names(x)==i]}))
    if (length(d)>0) {
      outCones <- c(outCones,i)
      out <- c(out,min(d))
    }
  }
  names(out) <- outCones
  out
}

# Now knows how to add just one person, and can add generate types of goal stacks
addPed <- function(state,startState,p,goaltype=NULL,pSD,group=NA,sPref=NA,useUpper=NA,useLetter=NA) 
  # Adds single ped unless the entry goal is occupied. Use group and spref to 
  # assign same group number and sPref (natural scale) for group members.
  # useUpper: NA = take next, TRUE = use next uppercase, FALSE = next lower case
  # useLetter: use this letter value if not NA
{
  # Check if startGoal is available.
  if (!is.null(attr(startState$P[[1]],"retrace"))) {
    if (occupied(attr(startState$P[[1]],"retrace"),state,toNatural(p["dMinBody"]))) {
      cat("\nEntry failure!\n")
      return(state)
    }
  }
  
  # Get new name
  numLetter <- attr(state$P,"numLetter")
  if (!is.na(useLetter)) let <- useLetter else {
    # Letters already used 
    pnams <- unlist(lapply(strsplit(row.names(state$p),"_"),function(x){x[1]}))
    # Next available upper and lower case letters (maybe none)
    lets <- c(LETTERS[!(LETTERS %in% pnams)][1],
              letters[!(letters %in% pnams)][1])
    if (is.na(lets[1])) # No unique upper, start using repeats
      lets[1] <- LETTERS[sample(1:26,1,prob=(1/numLetter[1:26])/sum(1/numLetter[1:26]))]
    if (is.na(lets[2])) # No unique lower, start using repeats
      lets[2] <- letters[sample(1:26,1,prob=(1/numLetter[27:52])/sum(1/numLetter[27:52]))]
    if (is.na(useUpper)) let <- lets[sample(1:2,1)] else # randomly pick upper/lower
      if (useUpper) let <- lets[1] else let <- lets[2]   # deliberatbely pick upper/lower
  }
  nam <- paste(let,numLetter[let],sep="_")
  numLetter[let] <- numLetter[let] + 1
  # Set up goal stack
  if (!is.null(goaltype)) startState$P <- goalstack(goaltype,1)
  # Set up pMat
  startState$pMat <- getpMat(1,p,pSD)
  if (!(all(is.na(sPref)))) 
    startState$pMat[,"sPref"] <- sPref
  if ((all(is.na(group)))) 
    startState$group <- max(state$group) + 1 else
      startState$group <- group
  # Add nam to startState
  row.names(startState$p) <- nam
  names(startState$a) <- nam
  names(startState$v) <- nam
  names(startState$r) <- nam
  names(startState$group) <- nam
  row.names(startState$pMat) <- nam
  state$p <- rbind(state$p,startState$p)
  state$v <- c(state$v,startState$v)
  state$a <- c(state$a,startState$a)
  state$r <- c(state$r,startState$r)
  state$group <- c(state$group,startState$group)
  state$pMat <- rbind(state$pMat,startState$pMat)
  state$cell <- c(state$cell,startState$cell)
  if (!is.list(state$P)) state$P <- list(state$P,startState$P) else state$P <- c(state$P,startState$P)
  if (!is.null(numLetter)) attr(state$P,"numLetter") <- numLetter
  state
}

# Now can also extract P_n from single pedestrian
# Extract goal matrix from state
getP <- function(state) {
  if (is.list(state$P)) {
    P_n <- do.call(rbind,lapply(state$P,function(x){x[attr(x,"i"),]}))
  } else  {
    P_n <- matrix(state$P[attr(state$P,"i"),],nrow=1); colnames(P_n) <- c("x","y")
  }
  P_n
}


howOpen <- function(n,P,state,objects) 
  # How open is the way from pedestrian n to P
{
  p1 <- state$p[n,,drop=FALSE]
  iC <- iCones(p1=p1,a=angle2(p1,matrix(P,nrow=1)),
               p2=state$p[-n,,drop=FALSE],r=state$r,objects)
  -sum(iC)
}


insertGoal <- function(j,state,type=NULL,manual=NULL,manualIndex=NULL,goalIndex=NULL,after=FALSE)
# Insert goal into goal stack with types retrace, waypoint, mustStop, or manual
{
  i <- attr(state$P[[j]],"i")
  stop <- attr(state$P[[j]],"stop")
  waypoint <- attr(state$P[[j]],"waypoint")
  if (any(!is.na(attr(state$P[[j]],"mustStop")[i,]))) currentIsMust <- T else currentIsMust <- F
  # if first goal assign NULL or mustVisit goal
  if (i==1) {
    P0 <- NULL
    V0 <- NULL
    # if any other goal assign previous goals
  } else if (after) {
    P0 <- state$P[[j]][1:i,,drop=FALSE]
    V0 <- attr(state$P[[j]],"mustStop")[1:i,,drop=FALSE]
  } else {
    P0 <- state$P[[j]][1:(i-1),,drop=FALSE]
    V0 <- attr(state$P[[j]],"mustStop")[1:(i-1),,drop=FALSE]
  }
  if (type=="mustVisit") {
    # add mustVisit goal and the next goals minus that goal
    if (after) {
      P1 <- rbind(state$P[[j]][goalIndex,],
                  state$P[[j]][-goalIndex,][(i+1):(dim(state$P[[j]])[1]-length(goalIndex)),,drop=F])
      V1 <- rbind(attr(state$P[[j]],"mustStop")[goalIndex,],
                  attr(state$P[[j]],"mustStop")[-goalIndex,][(i+1):(dim(state$P[[j]])[1]-length(goalIndex)),,drop=F])
    } else {
      P1 <- rbind(state$P[[j]][goalIndex,],
                  state$P[[j]][-goalIndex,][i:(dim(state$P[[j]])[1]-length(goalIndex)),,drop=F])
      V1 <- rbind(attr(state$P[[j]],"mustStop")[goalIndex,],
                  attr(state$P[[j]],"mustStop")[-goalIndex,][i:(dim(state$P[[j]])[1]-length(goalIndex)),,drop=F])
    }
    state$P[[j]] <- rbind(P0,
                          P1)
    attr(state$P[[j]],"mustStop") <- rbind(V0,
                                           V1)
    # If the current goal is already a mustVisit goal, add waypoint instead of replacing
    if (!currentIsMust) {
      attr(state$P[[j]],"waypoint") <- state$p[j,]
    } else {
      attr(state$P[[j]],"waypoint") <- rbind(state$p[j,],
                                             waypoint)
      rownames(attr(state$P[[j]],"waypoint")) <- NULL
    }
  } else {
    # add next goals
    P1 <- state$P[[j]][i:dim(state$P[[j]])[1],,drop=FALSE]
    V1 <- attr(state$P[[j]],"mustStop")[i:dim(state$P[[j]])[1],,drop=FALSE]
    if (!is.null(manualIndex)) {
      state$P[[j]] <- rbind(P0,
                            state$P[[j]][manualIndex,],
                            P1)  
    } else if (!is.null(manual)) {
      state$P[[j]] <- rbind(P0,
                            manual,
                            P1)
      rownames(state$P[[j]]) <- NULL
    } else {
      state$P[[j]] <- rbind(P0,
                            attr(state$P[[j]],type),
                            P1)
    }
    if (type == "waypoint") {nTrace <- (4*dim(waypoint)[1])
    } else if (type == "manual") {nTrace <- (4*dim(manual)[1])
    } else {nTrace <- 4*1}
    attr(state$P[[j]],"mustStop") <- rbind(V0,
                                           matrix(suppressWarnings(rep(NA,length.out=nTrace)),ncol=4),
                                           V1)
    rownames(attr(state$P[[j]],"mustStop")) <- NULL
    attr(state$P[[j]],"waypoint") <- waypoint
  }
  attr(state$P[[j]],"i") <- i
  attr(state$P[[j]],"retrace") <- state$p[j,]
  attr(state$P[[j]],"stop") <- -1
  state
}


checkReachableGoals <- function(j,state,default,objects)
  # checks for subgoals that can be seen and from which one could see the goal and insert if found
{
  default <- default[!duplicated(default),]
  i <- attr(state$P[[j]],"i")
  # Check which subgoals can be seen from ped
  visibleSubgoals <- rep(NA,nrow(default))
  for (d in 1:nrow(default)) visibleSubgoals[d] <- seesGoal(state$p[j,,drop=F],default[d,],objects)
  if (!is.null(visibleSubgoals)) {
    # Check which subgoals can be seen from goal
    reachableSubgoals <- rep(NA,nrow(default))
    for (d in 1:nrow(default)) reachableSubgoals[d] <- seesGoal(default[d,],state$P[[j]][i,,drop=F],objects)
    # Check which subgoals are in both vectors
    linkingSubgoals <- default[visibleSubgoals,,drop=F][which(which(visibleSubgoals) %in% which(reachableSubgoals)),]
    linkingSubgoals <- matrix(linkingSubgoals,ncol=2,
                              byrow=ifelse(length(linkingSubgoals)>2,F,T))
    if (is.null(linkingSubgoals)) {
      # Otherwise find links between visible and reachable subgoals
      visibleSubgoals <- default[visibleSubgoals,,drop=F]
      reachableSubgoals <- default[reachableSubgoals,,drop=F]
      nrS <- nrow(reachableSubgoals); nvS <- nrow(visibleSubgoals)
      if (nrS>0 & nvS>0) {
        linkingSubgoals2 <- NULL
        for (rsg in 1:nrS) for (vsg in 1:nvS) {
          if (seesGoal(visibleSubgoals[vsg,,drop=F],reachableSubgoals[rsg,,drop=F],objects)) {
            linkingSubgoals2 <- rbind(linkingSubgoals2,c(visibleSubgoals[vsg,,drop=F],
                                                         reachableSubgoals[rsg,,drop=F]))
          }
        }
        if (nrow(linkingSubgoals2) > 1) {
          open <- rep(NA,nrow(linkingSubgoals2))
          # if more than 1 subgoal linked order goals by how open the path is to the first subgoal
          for (m in 1:length(open)) open[m] <- howOpen(j,linkingSubgoals2[m,1:2],state,objects)
          if (all(open == 0)) {
            closestLink <- matrix(linkingSubgoals2[which.min(dist1(state$p[j,,drop=F],
                                                                   linkingSubgoals2[,1:2])),],ncol=2,byrow=T)
          } else {
            closestLink <- matrix(linkingSubgoals2[which.max(open),],ncol=2,byrow=T)
          }
          state <- insertGoal(j,state,type="manual",manual=closestLink)
        } else if (nrow(linkingSubgoals2) > 0) {
          closestLink <- matrix(linkingSubgoals2,ncol=2,byrow=T)
          state <- insertGoal(j,state,type="manual",manual=closestLink)
        }
      } else {
        state <- insertGoal(j,state,type="manual",manual=c(x=20,y=10.5))
      }
    } else if (nrow(linkingSubgoals) > 2) {
      open <- rep(NA,nrow(linkingSubgoals))
      # if more than 1 subgoal linked order goals by how open the path is
      for (m in 1:length(open)) open[m] <- howOpen(j,linkingSubgoals[m,],state,objects)
      if (all(open == 0)) {
        closestLink <- linkingSubgoals[which.min(dist1(state$p[j,,drop=F],linkingSubgoals)),]
      } else {
        closestLink <- linkingSubgoals[which.max(open),]
      }
      state <- insertGoal(j,state,type="manual",manual=closestLink)
    } else if (nrow(linkingSubgoals) > 0) {
      closestLink <- linkingSubgoals
      state <- insertGoal(j,state,type="manual",manual=closestLink)
    }
  }
  state
}


updateGoal <- function(state,default,objects,interactionTime=1,reach=14) 
  # Update goal (unless it is the last goal), two types, if mustVisit update if 
  # within close of achieving it, if not update if can see next goal (or could
  # see on turning).
{
  for (j in 1:length(state$P)) { # For each pedestrian
    i <- attr(state$P[[j]],"i")
    # Reset certain parameters
    if (attr(state$P[[j]],"stop") == -1 | attr(state$P[[j]],"stop") > interactionTime)
      attr(state$P[[j]],"stop") <- 0
    # If current goal unseen, trace back by insertion
    if (!seesCurrentGoal(j,state,objects)) {
      retraceable <- !is.null(attr(state$P[[j]],"retrace"))
      waypointable <- !is.null(attr(state$P[[j]],"waypoint"))
      # If goal can be seen from retrace insert retrace
      if (retraceable) {
        if (seesGoal(state$p[j,,drop=F],attr(state$P[[j]],"retrace"),objects) &
                     seesGoal(attr(state$P[[j]],"retrace"),state$P[[j]][i,],objects)) {
          state <- insertGoal(j,state,type="retrace"); waypointable = FALSE
        } else {
          retraceable <- FALSE
      }}
      # If goal can be seen from waypoint insert waypoint (if next goals unseen)
      if (waypointable & !anyNA(attr(state$P[[j]],"mustStop")[i-1,1])) {
        if (seesGoal(state$p[j,,drop=F],matrix(attr(state$P[[j]],"waypoint"),ncol=2)[1,],objects) &
            seesGoal(matrix(attr(state$P[[j]],"waypoint"),ncol=2)[1,],state$P[[j]][i,],objects)) {
          state <- insertGoal(j,state,type="waypoint")
        } else {
        waypointable <- FALSE
      }}
      if (!retraceable & !waypointable) {
        state <- checkReachableGoals(j,state,default,objects)
      }
    # If current goal seen and its not the last, update
    } else if (dim(state$P[[j]])[1] != i) {
      # if current is mustVisit goal, and next is not
      if (!anyNA(attr(state$P[[j]],"mustStop")[i,])) currentIsMust <- T else currentIsMust <- F
      if (!anyNA(attr(state$P[[j]],"mustStop")[(i+1),])) nextIsMust <- T else nextIsMust <- F
      if (currentIsMust & !nextIsMust) goalConflict <- T else goalConflict <- F
      mv <- which(!is.na(attr(state$P[[j]],"mustStop")[,1])) # assign mustVisit goals
      mv <- mv[which(mv > (i+1) & mv < dim(state$P[[j]])[1])] # from next goals (minus the exit)
      see_mV <- rep(NA,length(mv))
      if (length(mv) > 0) { # if there are any mustVisit goals left
        for (m in 1:length(mv)) { # for each of them
          # check if goal can be seen and is within reach
          if (seesGoal(state$p[j,],state$P[[j]][mv[m],],objects) &
              dist(state$p[j,],state$P[[j]][mv[m],,drop=F]) <= reach) see_mV[m] <- T else see_mV[m] <- F
        }
        seenIndex <- mv[which(see_mV)]
        if (goalConflict & length(seenIndex) > 0) { # add current goal
          seenIndex <- c(seenIndex,i)
        }
        # If multiple goals seen and within reach reorder
        if (length(seenIndex) > 1) {
          open <- rep(NA,length(seenIndex))
          # if more than 1 mustVisit goal seen order goals by howOpen
          for (m in 1:length(open)) open[m] <- howOpen(j,state$P[[j]][seenIndex[m],],state,objects)
          # if all paths open, order goals by distance
          if (all(open==0)) {
            if (goalConflict) {
              seenIndex <- seenIndex[order((dist1(state$p[j,],state$P[[j]][seenIndex,,drop=F])))]
            } else {
              seenIndex <- seenIndex[which.min((dist1(state$p[j,],matrix(state$P[[j]][seenIndex,,drop=F],ncol=2))))]
            }
          } else {
            if (goalConflict) seenIndex <- seenIndex[order(-open)] else seenIndex <- seenIndex[which.max(open)]
          }
        }
        # insert mustVisit goal(s) if there is at least one
        if (length(seenIndex) > 0 & !nextIsMust) {
          # if the current goal is first in order behind the conflicting goal(s)
          if (goalConflict) {
            if (which(seenIndex==i) == 1) state <- insertGoal(j,state,type="mustVisit",goalIndex=seenIndex[-1],after=TRUE)
            if (which(seenIndex==i) != 1) {
              seenIndex <- seenIndex[1]
              state <- insertGoal(j,state,type="mustVisit",goalIndex=seenIndex,after=FALSE)
            }
          } else {
            state <- insertGoal(j,state,type="mustVisit",goalIndex=seenIndex,after=FALSE)
          }
        }
      } # END mustVisit goals insertion
      # Do not update unless:
      doUpdate <- FALSE
      # In satisfaction area of a mustVisit goal
      if (!anyNA(attr(state$P[[j]],"mustStop")[i,])) {
        doUpdate <- inObject(state$p[j,],list(attr(state$P[[j]],"mustStop")[i,1],attr(state$P[[j]],"mustStop")[i,2]),
                             list(attr(state$P[[j]],"mustStop")[i,3],attr(state$P[[j]],"mustStop")[i,4]),
                             outside=FALSE)
        if (doUpdate) attr(state$P[[j]],"stop") <- 1
      } else {
        # Next subgoal can already be seen
        if (seesCurrentGoal(j,state,objects,offset=1) | dist(state$p[j,,drop=F],state$P[[j]][i,,drop=F]) < .5) {
          doUpdate <- TRUE
          attr(state$P[[j]],"stop") <- -1
        }
      }
      if (doUpdate) {
        # Go to next goal and store new trace
        attr(state$P[[j]],"i") <- attr(state$P[[j]],"i") + 1
        attr(state$P[[j]],"retrace") <- state$p[j,]
      }
      if (attr(state$P[[j]],"stop") > 0) {
        # stop for as many iterations as interactionTime
        if (attr(state$P[[j]],"stop") <= interactionTime) {
          attr(state$P[[j]],"stop") <- attr(state$P[[j]],"stop") + 1
        } else {
          attr(state$P[[j]],"stop") <- 0
        } 
      } # END stop iteration
    } # END update
  } # END pedestrian loop
  state
}


# Now includes grid option
plotPed <- function(p_n,P_n,a_n,r_n,extras,objects,cells=rep(1,length(a_n)),socialDistance=1.5,
                    len=.05,plotGoal=TRUE,plotGrid=TRUE,plotPath=FALSE,Pr=NULL,
                    plotDirectionArrow=TRUE,plotDestArrow=TRUE,plotCircle=FALSE,fname="")
  # Plot pedestrians, optionally with arrows indicating goal direction and 
  # circles indicating body and social distance. If stationary draws grey, if 
  # turning around draws red
{
  
  # @CT start ---------------------------------------------------------------
  if (fname != "") {
    pdf(paste(fname, ".pdf", sep = ""),
        width=20,height=12)
  }
  # @CT end -----------------------------------------------------------------
  
  par(mfrow=c(1,1))
  nams <- unlist(lapply(strsplit(row.names(p_n),"_"),function(x){x[1]}))
  plot(p_n,pch=nams,xlim=objects[[1]]$x,ylim=objects[[1]]$y)
  if (plotGrid) {
    grid(nx=objects[[1]]$x[2],ny=objects[[1]]$y[2],"gray")
  }
  if (plotGoal) {
    points(P_n,pch=nams,col="red")
    for (d in 1:nrow(extras$Area1)) draw_circle(extras$Area1[d,1],extras$Area1[d,2],.2, border="blue")
    for (d in 1:nrow(extras$Area2)) draw_circle(extras$Area2[d,1],extras$Area2[d,2],.2, border="red")
    for (d in 1:nrow(extras$Area3)) draw_circle(extras$Area3[d,1],extras$Area3[d,2],.2, border="green")
    for (d in 1:nrow(extras$Area4)) draw_circle(extras$Area4[d,1],extras$Area4[d,2],.2, border="purple")
    for (d in 1:nrow(extras$Exits)) draw_circle(extras$Exits[d,1],extras$Exits[d,2],.3, border="black")
    for (d in 1:nrow(extras$default)) draw_circle(extras$default[d,1],extras$default[d,2],.1, border="gray")
  }
  for (i in 1:length(objects)) drawSquare(objects[[i]])
  if (plotPath) {
    for (d in 1:(nrow(default)-1)) {
      # if horizontal move is greater than vertical move cut on horizontal arrow and vice versa 
      hor <- ifelse(default[d+1,1]-default[d,1] > 1, .5,
                    ifelse(default[d+1,1]-default[d,1] < -1, -.5, 0))
      vert <- ifelse(default[d+1,2]-default[d,2] > 1, .5,
                     ifelse(default[d+1,2]-default[d,2] < -1, -.5, 0))
      arrows(x0=default[d,1],x1=default[d+1,1]-hor,y0=default[d,2],y1=default[d+1,2]-vert,
             col="darkgray",code=2,length=.13,lty="dotdash")
    }
  }
  # ahead
  end <- p_n+aTOd(a_n)/2
  if (plotDirectionArrow) for (i in 1:length(a_n))
    arrows(p_n[i,1],p_n[i,2],end[i,1],end[i,2],len=len)
  # destination
  a_n <- Dn(p_n,P_n)
  end <- p_n+aTOd(a_n)/2
  if (plotDestArrow) for (i in 1:length(a_n))
    arrows(p_n[i,1],p_n[i,2],end[i,1],end[i,2],len=len,col="red")
  if (plotCircle) for (i in 1:nrow(p_n)) {
    if (cells[i]==0) col <- "lightgrey" else 
      if (cells[i]==-1) col <- "red" else col <- NA
      draw_circle(p_n[i, 1], p_n[i, 2], r_n[i], border="red",col=col)
      draw_circle(p_n[i, 1], p_n[i, 2], socialDistance/2, border="blue")
  }
  
  # @CT start ---------------------------------------------------------------
  if (fname != "") {
    dev.off()
  }
  # @CT end -----------------------------------------------------------------
  
}


# Now includes a response to the stop attribute (namely to stop and re-angle)
# angle=c(-72.5,-50,-32.5,-20,-10,0,10,20,32.5,50,72.5); plotGrid=NULL; sStop=0.2
move <- function(n,state,P_n,p_pred,nests,alpha,objects,iInfo,
                 plotGrid=NULL,printChoice=NULL,sStop=0.2, # speed to resume after stop
                 usebestAngle=FALSE,
                 angle=c(-72.5,-50,-32.5,-20,-10,0,10,20,32.5,50,72.5)) 
  # For one pedestrian calcuate object blocking, utility, cell probability, 
  # and animate move. Returns new state
{
  
  p <- toNatural(state$pMat[n,])
  muM <- getmuM(p) 
  centres <- c_vd(1:33,p1=state$p[n,],v1=state$v[n],a1=state$a[n])
  ok <- okObject(n,objects,state,centres)
  ok <- bodyObjectOK(state$r[n],centres,objects,ok)
  # Dont move where someone is now
  ok <- okBodyBody(n,state,centres,ok)

  if (!any(ok) | state$v[n] < .2) { # stop and turn around
    state$a[n] <- (state$a[n] + 180) %% 360
    state$v[n] <- sStop
    cell <- -1
  } else {
    V <- utility(p,n,state,P_n,p_pred,centres,objects,ok,iInfo=iInfo)
    Pr <- pCNLs(V,muM,nests,alpha)
    names(Pr) <- 0:33
    if ( !is.null(plotGrid) && plotGrid[n] ) 
      draw_grid(state$p[n,],state$v[n],state$a[n],plotPoints=TRUE,Pr=Pr[-1])
    cell <- sample.int(length(V),1,TRUE,prob=Pr)-1
    if ( !is.null(printChoice) && printChoice[n]) {
      if (cell==0) cat("Standing still\n\n") else
        cat(paste("\nPedestrian =",row.names(state$p)[n]," Choice =",cell," Ring =",
                  c("accelerate","constant","slow")[ringNum(cell)],
                  " Cone =",coneNum(cell),"\n\n"))
      cat(paste("Probability of standing still =",round(Pr[1],3)))
      print(matrix(round(Pr[-1],2),nrow=3,byrow=TRUE,
                   dimnames=list(speed=c("accelerate","constant","slow"),cone=1:11)))
      cat("\n")
    }
    if (cell != 0) {
      state$p[n,] <- c_vd(cell,state$p[n,],state$v[n],state$a[n])
      state$v[n] <- pmax(state$v[n]*c(1.5,1,.5)[ringNum(cell)],sStop)
      state$a[n] <- (state$a[n] - angle[coneNum(cell)]) %% 360
    } else { # stopped
      state$v[n] <- sStop
      # if (usebestAngle) state$a[n] <- bestAngle(p,n,state,P_n,p_pred,objects,iInfo)
    }
  }
  # Re-angle at sight of new goals
  if (attr(state$P[[n]],"stop") < 0) {
    state$a[n] <- angle2(state$p[n,,drop=F],state$P[[n]][attr(state$P[[n]],"i"),,drop=F])
    state$v[n] <- .4
  # Stop and re-angle at sight of new mustVisit goals
  } else if (attr(state$P[[n]],"stop") > 0) {
    state$a[n] <- angle2(state$p[n,,drop=F],state$P[[n]][attr(state$P[[n]],"i"),,drop=F])
    cell <- -1
  }
  out <- list(p=state$p[n,],v=state$v[n],a=state$a[n],r=state$r[n],group=state$group[n],
              pMat=state$pMat[n,],cell=cell)
  attr(out,"ok") <- ok
  
  if (!inObject(state$p[n,,drop=F],xlim=objects[[1]]$x,ylim=objects[[1]]$y,outside=FALSE))
    stop(n)
  
  out
}


# Now includes reach and plotGrid options
# delay=0;cores=1;collisionFix=TRUE
# showMind=NULL;printChoice=NULL;plotComponents=FALSE;plotGoal=TRUE;plotPath=FALSE
# plotCircle=TRUE;plotGrid=TRUE;usebestAngle=FALSE;reach=15;plotSim=TRUE;fname=""

# default=NULL

# reach=25;plotPath=TRUE;fname=file_name

moveAll <- function(state,objects,nests,alpha,delay=0,cores=1,collisionFix=TRUE,
                    showMind=NULL,printChoice=NULL,plotComponents=FALSE,plotGoal=TRUE,plotPath=FALSE,
                    plotCircle=TRUE,plotGrid=TRUE,reach=15,plotSim=TRUE,usebestAngle=FALSE,
                    fname="",extras=NULL,interactionTime=1)
  # Move all pedestrians, optionally print and plot state, and pause.
{
  
  makeState <- function(state.list,nams,P_n) {
    p <- t(apply(state.list,2,function(x){x$p}))
    row.names(p) <- nams
    v <- apply(state.list,2,function(x){x$v})
    names(v) <- nams
    a <- apply(state.list,2,function(x){x$a})
    names(a) <- nams
    r <- apply(state.list,2,function(x){x$r})
    names(r) <- nams
    group <- apply(state.list,2,function(x){x$group})
    names(group) <- nams
    pMat <- t(apply(state.list,2,function(x){x$pMat}))
    row.names(pMat) <- nams
    cell <- apply(state.list,2,function(x){x$cell})
    list(p=p,v=v,a=a,r=r,P=P_n,group=group,pMat=pMat,cell=cell)
  }
  
  # Information about interactions among pedestrians
  if (is.list(state$P) & length(state$P)>1) # only relevant if more than one
    iInfo <- getiInfo(state) else iInfo <- NULL
  # Extract goals
  if (is.list(state$P)) P_n <- getP(state) else P_n <- state$P
  # Predicted positions
  p_pred <- predictPed(state$p,state$v,state$a,state$cell)
  
  # Inspect utility
  if (!is.null(showMind)) inspect <- showMind else inspect <- FALSE
  if (!is.null(printChoice)) inspect <- inspect | printChoice
  if (any(inspect)) for (n in 1:length(inspect)) if (inspect[n]) {
    if (is.null(state$cell)) state$cell <- rep(1,length(state$a))
    plotPed(state$p,P_n,state$a,state$r,objects,state$cell,plotGoal=plotGoal,plotCircle=plotCircle)
    # save tmp to get ok attribute
    tmp <- move(n,state=state,P_n=P_n,p_pred=p_pred,nests=nests,alpha=alpha,iInfo=iInfo,
                objects=objects,plotGrid=showMind,printChoice=printChoice)
    invisible(readline(prompt="Press [enter] to continue, [esc] to terminate"))
    cat("\n")
    if (!is.null(printChoice) && printChoice[n] & plotComponents) {
      if (!any(attr(tmp,"ok"))) {
        cat("No cells OK, turning around!\n")  
      } else {
        centres <- c_vd(1:33,p1=state$p[n,],v1=state$v[n],a1=state$a[n])
        plotUtility(n,state,P_n,p_pred,centres,objects,nests,alpha,
                    ok=attr(tmp,"ok"),iInfo) 
      }
      invisible(readline(prompt="Press [enter] to continue, [esc] to terminate"))
    }
  }
  
  # for (n in 1:length(state$a[1:4])) {
  #   out <- move(n,state,P_n,p_pred,nests,alpha,objects,iInfo)
  #   cat("###################################################\n")
  #   print(n)
  #   print(out)
  #   cat("\n\n")
  # }
  
  # Main update
  oldState <- state
  state.list <- mcmapply(move,1:length(state$a),mc.cores=cores,MoreArgs = 
                           list(state=state,P_n=P_n,p_pred=p_pred,objects=objects,alpha=alpha,nests=nests,
                                iInfo=iInfo,usebestAngle=FALSE))
  try(state <- makeState(state.list,nams=row.names(state$p),P_n=state$P))
  if (class(state)=="try-error") {
    save(state.list,oldState,file="bugR.RData")
    stop("Bug in makeState")
  }
  
  # Check and fix collisions
  if (collisionFix) {
    if (is.list(state$P)) P_n <- getP(state) else P_n <- state$P
    iInfo <- getiInfo(state)
    p_pred <- predictPed(state$p,state$v,state$a,state$cell)
    state <- fixCollision(state,oldState,P_n,p_pred,objects,iInfo,cores=cores)
  }
  # @CT Only plot if plotSim = TRUE
  if (plotSim) {
    plotPed(state$p,P_n,state$a,state$r,extras$default,objects,state$cell,plotGoal=plotGoal,plotPath=plotPath,plotCircle=plotCircle,fname=fname)
  }
  if (is.list(state$P)) state <- updateGoal(state,default=extras$default,objects,
                                            reach=reach,interactionTime=interactionTime) # Goal stack
  Sys.sleep(delay)
  state
}
