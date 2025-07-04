
require(parallel)
# require(pdftools)
# require(magick)


#### GEOMETRY & DRAWING

## Distance functions ----

dist <- function(p1,p2) 
# Distance from p1 to p2, same sized xy column matrices
{
  sqrt(apply((p1-p2)^2,1,sum))
}

dist1 <- function(p1,p2) 
# distance from p1 to p2, p1 is a single point, p2 is a matrix
{
  sqrt(apply((t(p2)-as.vector(p1))^2,2,sum))
}

## Angle functions ----

angle2s <- function(p1,p2) 
# Shortest angle anti-clockwise from p1 as orign to p2 (> -180 to 180)
{
  round((atan2(p2[,2]-p1[,2],p2[,1]-p1[,1])*180/pi),10)
}
# angle2s(p1=matrix(rep(c(0,0),each=8),nrow=8),
#        p2=matrix(c(1,1,0,-1,-1,-1,0,1,0,1,1,1,0,-1,-1,-1),nrow=8))

angle2 <- function(p1,p2) 
# Anti-clockwise angle from p1 as origin to p2 (x,y pairs matrices) 0 to <360
{
  round((atan2(p2[,2]-p1[,2],p2[,1]-p1[,1])*180/pi),10) %% 360
}
# angle2(p1=matrix(rep(c(0,0),each=8),nrow=8),
#        p2=matrix(c(1,1,0,-1,-1,-1,0,1,0,1,1,1,0,-1,-1,-1),nrow=8))


# # Shortest angle counter-clockwise from p1 to p2 matrix (> -180 to 180)
# angle2s <- function(p1,p2) {round((atan2(p2[,2],p2[,1])-atan2(p1[,2],p1[,1]))*180/pi,10)}
# # angle2s(p1=matrix(rep(c(1,0),each=8),nrow=8),
# #        p2=matrix(c(1,1,0,-1,-1,-1,0,1,0,1,1,1,0,-1,-1,-1),nrow=8))
# 
# # Anti-clockwise angle from p1 to p2 matrix (x,y pairs matrices) 0 to <360
# angle2 <- function(p1,p2) {angle2s(p1,p2) %% 360}
# # angle2(p1=matrix(rep(c(1,0),each=8),nrow=8),
# #        p2=matrix(c(1,1,0,-1,-1,-1,0,1,0,1,1,1,0,-1,-1,-1),nrow=8))


aTOd <- function(a) 
# angle to L2 normalized carteisn direction, so sqrt(d(x)^2+d(y)^2)=1)  
{
  cbind(x=cos(a*pi/180),y=sin(a*pi/180))
}


Iangle <- function(p1,a1,p2,border=c(-85,-60,-40,-25,-15,-5,5,15,25,40,60,85)) 
# Which angle cone (1..11, NA means outside of view) is p2 in relative to 
# p1 heading at angel a1 
{
  
  tomp <- function(x) { # -180 ... 180
    neg <- x < -180; x[neg] <- 360+x[neg]
    pos <- x > 180; x[pos] <- x[pos] - 360
    x
  }
  
  a <- (angle2(p1,p2) - a1)
  
  out <- 12-.bincode(tomp(a),border)
  names(out) <- row.names(p2)
  out
}  
# for (n in 1:length(state$a)) {
#   print(names(state$a)[n])
#   print(Icell(p1=state$p[n,,drop=FALSE],a1=state$a[n],v1=state$v[n],
#               p2=state$p[-n,,drop=FALSE],P1=state$P[n,,drop=FALSE],
#               a2=state$a[-n]))
# }



Dn <- function(p_n,P_n) 
# Angle to destination for all pedestrians
{
  out <- numeric(dim(p_n)[1])
  for (i in 1:dim(p_n)[1])
    out[i] <- angle2(p_n[i,,drop=FALSE],P_n[i,,drop=FALSE])
  out
}


minAngle <- function(a1,a2) 
# Shortest absolute angle between a1 and a2
{
  pmin(abs(a1-a2),abs(pmin(a1,a2)+(360-pmax(a1,a2))))
}


headingAngle <- function(a2,a1,
  angles=c(72.5,50,32.5,20,10,0,350,340,327.5,310,287.5))
# absolute angular difference between 11 directions with zero cone having
# angle a1 and a2
{
  sapply((angles+a1) %% 360,minAngle,a2=a2)  
}
  

## Velocity functions ---- 

scaleVel <- function(v,tStep=0.5) 
  # Scale velocity by time step
  v*tStep

## Position functions ----

c_vd <- function(cells,p1,v1,a1,
  vels=matrix(rep(c(1.5,1,.5),each=11),ncol=3),
  angles=matrix(rep(c(72.5,50,32.5,20,10,0,350,340,327.5,310,287.5),times=3),ncol=3)) 
# Calcualte cell centres for set of cells (index 1..33) for p1 heading at 
# velocity v1 at angle a1 given time step tStep seconds.
{
  t(p1 + t(scaleVel(v1)*vels[cells]*aTOd((angles[cells] + a1) %% 360)))
}


  
## Draw shapes ----

draw_circle <- function (x, y, radius, nv = 100, border = NULL, col = NA, lty = 1, 
    density = NULL, angle = 45, lwd = 1, correctAspect = FALSE) 
  # Augment plotrix function to allow removal of correction for plot aspect ratio
{
    getYmult <- function () 
    {
      if (dev.cur() == 1) {
          warning("No graphics device open.")
          ymult <- 1
      }
      else {
          xyasp <- par("pin")
          xycr <- diff(par("usr"))[c(1, 3)]
          ymult <- xyasp[1]/xyasp[2] * xycr[2]/xycr[1]
      }
      return(ymult)
  }
  
  xylim <- par("usr")
    plotdim <- par("pin")
    if (correctAspect) ymult <- getYmult() else ymult=1
    angle.inc <- 2 * pi/nv
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    if (length(col) < length(radius)) 
        col <- rep(col, length.out = length(radius))
    for (circle in 1:length(radius)) {
        xv <- cos(angles) * radius[circle] + x
        yv <- sin(angles) * radius[circle] * ymult + y
        polygon(xv, yv, border = border, col = col[circle], lty = lty, 
            density = density, angle = angle, lwd = lwd)
    }
    invisible(list(x = xv, y = yv))
}

drawSquare <- function(lims) 
  # Draw a square
{
  lines(c(lims$x[1],lims$x[1]),c(lims$y[1],lims$y[2]))
  lines(c(lims$x[2],lims$x[2]),c(lims$y[1],lims$y[2]))
  lines(c(lims$x[1],lims$x[2]),c(lims$y[1],lims$y[1]))
  lines(c(lims$x[1],lims$x[2]),c(lims$y[2],lims$y[2]))
}

# k in 1..33
coneNum <- function(k) 1 + ((k-1) %% 11)
ringNum <- function(k) 1 + (k-1) %/% 11

# Draw chocie grid for p1 heading at angle a1 with velocity v1
draw_grid <- function(p1,v1,a1,plotPoints=FALSE,Pr=NULL,col="lightgrey",
  vels=rep(c(1.75,1.25,.75),each=12),
  angles=rep(c(85,60,40,25,15,5,355,345,335,320,300,275),times=3)) 
{
  ends <- t(p1 + t(scaleVel(v1)*vels*aTOd((angles + a1) %% 360)))
  for (i in 1:12) lines(c(p1[1],ends[i,1]),c(p1[2],ends[i,2]),col=col)
  lines(c(p1[1],ends[1,1]),c(p1[2],ends[1,2]),col=col)
  for (i in 2:12) {
    lines(c(p1[1],ends[i,1]),c(p1[2],ends[i,2]),col=col)
    for (j in 1:3) 
      lines(c(ends[(j-1)*12+(i-1),1],ends[(j-1)*12+i,1]),
            c(ends[(j-1)*12+(i-1),2],ends[(j-1)*12+i,2]),col=col)
  }
  if (plotPoints) {
    if (!is.null(Pr)) cexs <- Pr*3 else cexs <- rep(.25,33) 
    points(c_vd(1:33,p1,v1,a1),pch=16,cex=cexs)
  }
}


## Draw goals and pedestrians ----

plotGoals <- function(G,radius=0.75) 
  # Plots circles at goals (rows of G, columns = x,y coords)  
{
  invisible(apply(matrix(G,ncol=2),1,function(xy){
  draw_circle(xy[1],xy[2],radius)}))
}

plotPath <- function(G,radius=0.75,delay=0.5) 
  # Fills circles following goal path 
{
  for (i in 1:dim(G)[1]){ 
    draw_circle(G[i,1],G[i,2],radius,col="grey")
    Sys.sleep(delay)
    draw_circle(G[i,1],G[i,2],radius,col="white")
    Sys.sleep(delay)
  }
}

plotPed <- function(p_n,P_n,a_n,r_n,objects,cells=rep(1,length(a_n)),socialDistance=1.5,
                    len=.05,plotGoal=TRUE,plotGrid=FALSE,plotPoints=FALSE,Pr=NULL,
                    plotDirectionArrow=TRUE,plotDestArrow=TRUE,plotCircle=TRUE) 
  # Plot pedestrians, optionally with arrows indicating goal direction and 
  # circles indicating body and social distance. If stationary draws grey, if 
  # turning around draws red
{
  par(mfrow=c(1,1))
  nams <- unlist(lapply(strsplit(row.names(p_n),"_"),function(x){x[1]}))
  plot(p_n,pch=nams,xlim=objects[[1]]$x,ylim=objects[[1]]$y)
  if (plotGoal) points(P_n,pch=nams,col="red")
  for (i in 1:length(objects)) drawSquare(objects[[i]])
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
}

## Animation and trace ----

makeDirectories <- function(nam,outputName="output",doPDF=TRUE) 
  # Setup for figure and trace storage 
{
  if (!dir.exists(outputName)) dir.create(outputName)
  if (doPDF) {
    plot_dir <- paste0(outputName,"/",nam,"/")
    if (!dir.exists(plot_dir)) dir.create(plot_dir)
  }
}


makeGifs <- function(nam,fnam="output",density=45,fps=4,
                     useNam=TRUE,fnamOut=TRUE) 
  # if nam is a character reads in pdfs else nam is pdfs
  # saves out animated gif, invisibly returns
{
  
  getPDFs <- function(dName =".",density=45) 
    # read in a bunch of pdfs, density sets size
  {
    iNames <- dir(dName,pattern="*.pdf")
    # Put in numeric order
    iNames <- iNames[order(as.numeric(unlist(lapply(
      strsplit(unlist(strsplit(iNames,".pdf")),"_"),function(x){x[2]}))))]
    movie <- image_read_pdf(paste(dName,iNames[[1]],sep="/"),density=density)
    for (i in iNames[-1])
     movie <- c(movie,image_read_pdf(paste(dName,i,sep="/"),density=density))
    movie
  }

  if (!is.character(nam)) pdfs <- nam else {
    if (!useNam) dName <- fnam else dName <- paste(fnam,nam,sep="/")
    pdfs <- getPDFs(dName,density=density)
  }
  # image_write(pdfs,path=paste0(nam,".pdf"),format="pdf")
  gifs  <- image_animate(pdfs,fps=fps)
  if (fnamOut) outNam <- paste0(fnam,"/",nam,".gif") else 
    outNam <- paste0(nam,".gif")
  image_write(gifs,path=outNam,format="gif") 
  invisible(pdfs)
}


plotSpace <- function(objects,pSpace,plotGrid=TRUE) 
  # Plots space    
{
  par(mfrow=c(1,1))
  plot(NA,xlim=objects[[1]]$x, ylim=objects[[1]]$y,xlab="x",ylab="y")
  rect(objects[[1]]$x[1],objects[[1]]$y[1],objects[[1]]$x[2],objects[[1]]$y[2])
  if (plotGrid) grid(nx=pSpace$AAl,ny=pSpace$AAw,col="gray",lty="dotted")
  for (i in 2:length(objects)) drawSquare(objects[[i]])
}

# delay=0.1;plotGrid=TRUE;plotCircle=TRUE
# saveGifs=FALSE;tmpdir="tmp";froot="trace";density=45;fps=4;keepPdfs=FALSE
plotTrace <- function(trace,delay=0.1,plotGrid=TRUE,plotCircle=TRUE,
  saveGifs=FALSE,tmpdir="tmp",froot="trace",density=45,fps=4,keepPdfs=FALSE) 
  # Animates trace in R  
{
  if (saveGifs) {
    if (!dir.exists(tmpdir)) dir.create(tmpdir)
    fname <- paste(tmpdir,froot,sep="/")
    delay <- 0
  } else {
    plotSpace(attr(trace,"objects"),attr(trace,"pSpace"),plotGrid=plotGrid)
    fname <- ""
  }
  for (i in 1:length(trace)) {
    if (fname != "") fnam <- paste0(fname,"_",i) else fnam <- ""
    plotPed(trace[[i]]$p,getP(trace[[i]]),trace[[i]]$a,trace[[i]]$r,fname=fnam,
            attr(trace,"extras"),attr(trace,"objects"),
            plotCircle = plotCircle)
    Sys.sleep(delay)
  }
  if (saveGifs) makeGifs(froot,tmpdir,density=density,fps=fps,
                         useNam=FALSE,fnamOut=FALSE)
  if (!keepPdfs) unlink(tmpdir,TRUE)
}

## Line intersection and "sees" functions ----

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

seesCurrentGoal <- function(n,state,objects,offset=0)
  # seesGoal for current goal
  seesGoal(state$p[n,],state$P[[n]][attr(state$P[[n]],"i")+offset,],objects)

seesMany <- function(p1,ps,objects) 
  # Can position p see points in ps matrix (x,y columns)  
{
  apply(ps,1,seesGoal,P_n=p1,objects=objects)  
}



#### PREDICTION ----

predictPed <- function(p,v,a,cell=NULL,stayStop=TRUE) 
# Predict next position of pedestrians on current heading. If stayStop
# pedestrians who have just turned or stopped predicted to stay stopped.
{
  out <- p + scaleVel(v)*aTOd(a)
  if (stayStop & !is.null(cell)) out[cell<1,] <- p[cell<1,]
  out
}


occupied <- function(P,r,state) 
  # Position P (start position) not occupied or predicted to be occupied, where
  # occupied = pedestrian bodies touch, i.e., sum of radii
{
  any(dist1(P,state$p)<(r+state$r)) | 
  any(dist1(P,predictPed(state$p,state$v,state$a,state$cell))<(r+state$r))
}


getiInfo <- function(state) 
  # Dummy, used to collect information about interactions for current state
{
  NULL
}

#### OPERATIONAL UTILITY ----

## PS: Prefered speed ----

psUtility <- function(p,v) 
  # Prefered speed utility for cell 1..33 given current speed v
  -p["bPS"]*c(rep(abs(v*1.5-p["sPref"])^p["aPS"],11),
              rep(abs(v-p["sPref"])^p["aPS"],11),
              rep(abs(v/2-p["sPref"])^p["aPS"],11))  

## ID: Interpersonal distance ----

# p1=state$p[n,,drop=FALSE];a1=state$a[n]; r=state$r
predClose <- function(n,p1,a1,p2,r,centres,p_pred)
  # matrix of predicted distance between edge of bodies from ped n to others in  
  # front, each column is a cell each row an in front ped, if none returns null
{
  if (dim(p_pred)[1]==1) return(NULL)
  # remove self and pedestrians you cant see 
  occluded <- c(1:dim(p2)[1])[-n][!seesMany(p1,ps=p2[-n,,drop=FALSE],objects)]
  p_pred <- p_pred[-c(n,occluded),,drop=FALSE]
  if (dim(p_pred)[1]<=1) return(NULL)
  p2 <- p2[-c(n,occluded),,drop=FALSE]
  # peds in field of vision now and when moved
  inFront <- (minAngle(a1,angle2(p1,p2)) < 85) & (minAngle(a1,angle2(p1,p_pred)) < 85) 
  if ( !any(inFront) ) return(NULL)
  # Distace to predicted positions
  d <- matrix(apply(centres,1,dist1,p2=p_pred[inFront,,drop=FALSE]),
         nrow=sum(inFront),dimnames=list(names(inFront[inFront]),NULL))
  d <- d - (state$r[n]+state$r[names(inFront)[inFront]]) # subtract bodies
  d
}



idUtility <- function(p,n,ID,ok,group)
  # Inter-personal distance utility for cell 1..33. b parameter divided by
  # sum over power of distances between bodies for cell to all inFront peds
{
  if (is.null(ID) | !any(ok)) # none in front return -Inf for cells blocked by objects
    return(as.vector(ifelse(ok,0,-Inf)))
  # Group dependent b, bigger for outgroup by dID
  namesInGroup <- names(group[-n][group[-n]==group[n]])
  bID <- ifelse(dimnames(ID)[[1]] %in% namesInGroup,p["bID"],p["bID"]+p["dID"])
  # object or ped clash
  ok <- ok & apply(ID,2,function(x){all(x>0)})
  out <- ifelse(ok,0,-Inf)
  # repulsion
  if (p["bID"]!=0) out[ok] <- -apply(bID/(ID[,ok,drop=FALSE]^p["aID"]),2,sum)
  as.vector(out)
}

## BA: Blocked angle ----

# p1=state$p[13,,drop=FALSE]; p2=state$p[-13,,drop=FALSE]; r=0.75/2
eObjects <- function(p1,p2,r) 
  # "egocentric" objects = end points of line segments at right angles to a line
  # from point p1 (a matrix with columns x and y) to p2, where p2 a matrix of x,y 
  # coordinates of n circular pedestrian objects (one per row) each of radius r 
  # (a vector, same length as rows in p2 or a single value replicated), where the  
  # the line lengths are equal to the width of the object at p2 from the 
  # perspective of p1.
  # Output is an n (peds) x 2 (x,y) by ends (ac=anticlockwise, cw=clockwise) array
{
  d <- dist1(p1,p2)
  a12 <- angle2(p1,p2)
  r <- rep(r,length.out=dim(p2)[1]) # in case a single value
  theta <- atan(r/d)*180/pi
  ac <- t(as.vector(p1)+ t(aTOd((a12+theta) %% 360)*d))   # anti-clockwise
  cw <- t(as.vector(p1) + t(aTOd((a12-theta) %% 360)*d))  # clockwise
  # points(acw,pch=16)
  # points(cw,pch=16)
  array(c(ac,cw),dim=c(dim(ac),2),
        dimnames=list(names(d),dimnames(ac)[[2]],ends=c("ac","cw")))
}

# p1=state$p[n,,drop=FALSE]; p2=p_pred[-n,,drop=FALSE]; a=state$a[n]; r=state$r
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

iCones2Cells <- function(iC,v,vels=c(1.5,1,.5)) 
  # Turns output of iCones into cell-named vector of distances to each ring
  # for pedestrian moving at speed v
{
  out <- rep(iC,times=3)-rep(scaleVel(v)*vels,each=length(iC))
  names(out) <- rep(as.numeric(names(iC)),times=3)+ rep(c(0,11,22),each=length(iC))
  out
}


# for (i in 1:length(state$a)) {
#   # iC=iCones(p1=state$p[i,,drop=FALSE],p2=state$p[-i,,drop=FALSE],a=state$a[i],
#   #              r=0.75/2,objects=objects)
#   iC=iCones(p1=state$p[i,,drop=FALSE],p2=p_pred[-i,,drop=FALSE],a=state$a[i],
#                r=0.75/2,objects=objects)
#   print(names(state$a)[i])
#   print(iC)
#   cat("\n")
#   print(round(iCones2Cells(iC,state$v[i]),2))
#   cat("\n\n")
# }


blockedAngle <- function(n,state,p_pred,objects) 
  # Distance from each cell to closest pedestrian profile in each code
  # adjusted from ring. Outputs a cell-naemd vector of distances.  
{
  iC <- iCones(p1=state$p[n,,drop=FALSE],a=state$a[n],p2=p_pred[-n,,drop=FALSE],
               r=state$r,objects)
  iCones2Cells(iC,state$v[n])
}


baUtility <- function(p,BA) {
  if (is.null(BA)) return(rep(0,33))
  cells <- rep(0,33)
  cells[as.numeric(names(BA))] <- p["bBA"]/(pmax(BA,0)^p["aBA"])
  -cells
}


howOpen <- function(n,P,state,objects) 
  # How open is the way from pedestrian n to P
{
  p1 <- state$p[n,,drop=FALSE]
  p_pred <- predictPed(state$p,state$v,state$a,state$cell)
  iC <- iCones(p1=p1,a=angle2(p1,matrix(P,nrow=1)),
               p2=p_pred[-n,,drop=FALSE],r=state$r,objects)
  -sum(p["bBA"]/(iC^p["aBA"]))
}


## GA: Goal angle ----

destinationAngle <- function(a,p1,P1,
  angles=c(72.5,50,32.5,20,10,0,350,340,327.5,310,287.5))
# absolute angular difference between 11 directions with zero cone having
# angle a and destination angle for pedestrian at p1 heading to P1
# a=state$a[n];p1=state$p[n,,drop=FALSE];P1=P_n[n,,drop=FALSE]
{
  sapply((angles+a) %% 360,minAngle,a2=angle2(p1,P1))
}
# p1=matrix(c(0,0),nrow=1); P1=matrix(c(1,-1),nrow=1); a=270
# angle2(p1,P1)
# sapply((angles+a) %% 360,minAngle,a2=angle2(p1,P1))   


gaUtility <- function(p,GA) 
  # Goal angle utility, GA = goal angle (degrees/90) for 11 cones  
  -rep(p["bGA"]*GA^p["aGA"],times=3)

## CA: Current angle and side preference ----

caUtility <- function(p,angles=c(10,20,32.5,50,72.5)/90) 
  # Current angle utility  
{
  ap <- angles^p["aCA"]
  -rep(c(rep(p["bCA"]*p["bCAlr"],5),1,rep(p["bCA"]/p["bCAlr"],5))*c(ap[5:1],0,ap),times=3)
}

#### SOCIAL UTILITY ----

## FL: Follow the leader ----


# onlyGroup=FALSE;preferGroup=TRUE;pickBest=FALSE
getLeaders <- function(n,state,centres,objects,
                       onlyGroup=FALSE,preferGroup=TRUE,pickBest=FALSE) 
  # Which cell for ped n (p1) are other pedestrians (p2) in? 
  # onlyGroup => p2 must be in p1's group, preferGroup => group members first.
  # If more than one remaining in a cell pick the one whose current heading is 
  # closest to the target heading of p1.
  # Return a list of matices, "leaders" with column for each potential leader,
  # rows for their cell normalized (0-1) angle agreement and inGroup status, dists with
  # row per leader and columns of cells with distance from cell to chosen cell
{
  p1=state$p[n,,drop=FALSE]; a1=state$a[n]; v1=state$v[n] 
  # remove peds cant see
  occluded <- c(n,c(1:length(state$v))[-n][!seesMany(p1,ps=state$p[-n,,drop=FALSE],objects)])
  a2=state$a[-occluded];p2=state$p[-occluded,,drop=FALSE]
  if (dim(p2)[1]==0) return(NULL)
  if (!is.list(state$P)) P1=state$P[n,,drop=FALSE] else
    P1 <- state$P[[n]][attr(state$P[[n]],"i"),,drop=FALSE]
  I_n <- Iangle(p1,a1,p2) # is in cone
  I_n <- I_n[!is.na(I_n)]
  if (length(I_n)==0) return(NULL)
  # Subset in rings 1-3
  ring <- as.numeric(as.character(
    cut(dist1(p1,p2[names(I_n),,drop=FALSE]),c(0,scaleVel(v1)*c(.5,1,5)),c("3","2","1"))))
  names(ring) <- names(I_n)
  ring <- ring[!is.na(ring)]
  if (length(ring)==0) return(NULL)
  candidates <- I_n[names(ring)] + 11*(ring-1)
  inGroup <- state$group[names(candidates)]==state$group[n]
  if (onlyGroup) {
    if (!any(inGroup)) return(NULL) else 
      candidates <- candidates[inGroup]
  } else if (preferGroup & any(inGroup)) candidates <- candidates[inGroup]
  # Differnce in angle between leader heading and destination
  angles <- sapply(a2[names(candidates)],minAngle,a2=Dn(p1,P1))
  ok <- angles < 90
  if (!any(ok)) return(NULL)
  candidates <- candidates[ok]
  angles <- angles[ok]
  if (!any(duplicated(candidates))) leaders <- candidates else {
    leaders <- unique(candidates)
    for (i in 1:length(leaders)) names(leaders)[i] <- 
      names(candidates)[candidates==leaders[i]][which.min(angles[candidates==leaders[i]])] 
    angles <- angles[names(leaders)]
  }
  # Distances to leader cells
  d <- array(dim=c(length(leaders),33),dimnames=list(names(leaders),1:33))
  for (i in 1:length(leaders)) d[i,] <- dist1(centres[leaders[i],],centres)
  if (pickBest) best <- which.min(angles) else best <- 1:length(angles)
  list(dists=d[best,,drop=FALSE],leaders=rbind(cell=leaders,
    angleAgree=(90-angles[names(leaders)])/90,inGroup=inGroup[names(leaders)])[,best,drop=FALSE])
}


flUtility <- function(p,FL)
{
  if (is.null(FL)) return(numeric(33))
  # Ingroup and same direciton weighted b for each leader
  b <- (p["bFL"]+p["dFL"]*FL$leaders["inGroup",])*FL$leaders["angleAgree",]
  # Sum of utilities for leaders
  -apply(sapply(1:length(b),function(i){b[i]*FL$dists[i,]^p["aFL"]}),1,sum)
}


## GD: Group distance ----

getBuddy <- function(n,group,a,p_pred,centres,objects,pickBest=FALSE) 
  # Distance of your cells from cell centre closest to buddy
{
  # remove peds cant see and self
  occluded <- c(n,c(1:length(state$v))[-n][
    !seesMany(state$p[n,,drop=FALSE],ps=state$p[-n,,drop=FALSE],objects)])
  p_pred <- p_pred[-occluded,,drop=FALSE]
  if ( dim(p_pred)[1]==0 ) return(NULL)
  # NB: Not checking in front as assume you know where your group members are.
  #     If they are behind this fomulation will tend to slow you down so they can
  #     catch up.
  inGroup <- group[-occluded]==group[n]
  p_pred <- p_pred[inGroup,,drop=FALSE]
  nped <- dim(p_pred)[1]
  if ( nped==0 ) return(NULL)
  # Potential buddy matrix of difference between cone angles (rows) 
  # and heading angle for potential buddy (columns)
  headingDifference <- sapply(a[row.names(p_pred)],headingAngle,a1=a[n])
  # Most parallel cones for each potential companison
  parallelCone <- apply(headingDifference,2,which.min)
  # Distances from predicted buddy position to acc, const and dec rings in each cone
  d <- sapply(1:nped,function(x){
        dist1(p_pred[names(parallelCone)[x],],
              centres[c(parallelCone[x],parallelCone[x]+11,parallelCone[x]+22),])
      })
  # Ring closest to buddies
  ring <- apply(d,2,which.min)
  # Cell closest to buddy
  cell <- cbind(parallelCone,parallelCone+11,parallelCone+22)[cbind(1:nped,ring)]
  names(cell) <- names(parallelCone)
  # Heading difference for each buddy
  angleAgree <- (90-headingDifference[cbind(parallelCone,1:length(parallelCone))])/90
  # Distances to buddies cells
  d <- array(dim=c(nped,33),dimnames=list(names(angleAgree),1:33))
  for (i in 1:nped) d[i,] <- dist1(centres[cell[i],],centres)
  if (pickBest) best <- which.min(angleAgree) else best <- 1:nped
  list(buddies=rbind(cell,angleAgree)[,best,drop=FALSE],
       dists=d[best,,drop=FALSE])
}


gdUtility <- function(p,GD) 
{
  if (is.null(GD)) return(numeric(33))
  # Sum of utilities for buddies
  -apply(sapply(1:dim(GD$dists)[1],function(i){
    p["bGD"]*GD$buddies["angleAgree",i]*GD$dists[i,]^p["aGD"]
  }),1,sum)
}


#### OVERALL UTILITY ----


utility <- function(p,n,state,P_n,p_pred,centres,objects=list(),ok,iInfo) 
  # Model utility
{
  
  # Pre-compute
  ID <- predClose(n,p1=state$p[n,,drop=FALSE],a1=state$a[n],p2=state$p,
                  r=state$r,centres,p_pred)
  GA <- destinationAngle(state$a[n],state$p[n,,drop=FALSE],P_n[n,,drop=FALSE])/90
  FL <- getLeaders(n,state,centres,objects)
  GD <- getBuddy(n,group=state$group,a=state$a,p_pred,centres,objects)
  BA <- blockedAngle(n,state,p_pred,objects)
  # Utility sum
  out <- idUtility(p,n,ID,ok,group=state$group) + 
         psUtility(p,state$v[n]) +
         baUtility(p,BA) +
         gaUtility(p,GA) + 
         caUtility(p) +
         flUtility(p,FL) +
         gdUtility(p,GD)
         
  # Stop baseline (set by gwUtility) and and scaling
  c(-p["bS"],out)/p["rU"]
}


plotUtility <- function(n,state,P_n,p_pred,centres,objects,nests,alpha,ok,iInfo) 
{
  
  plotLines <- function() {
    abline(v=11.5,lty=3)
    abline(v=22.5,lty=3)
  }
  
  p <- toNatural(state$pMat[n,])
  muM <- getmuM(p)
  # Pre-compute
  ID <- predClose(n,p1=state$p[n,,drop=FALSE],a1=state$a[n],p2=state$p,
                  r=state$r,centres,p_pred)
  GA <- destinationAngle(state$a[n],state$p[n,,drop=FALSE],P_n[n,,drop=FALSE])/90
  FL <- getLeaders(n,state,centres,objects)
  GD <- getBuddy(n,group=state$group,a=state$a,p_pred,centres,objects)
  BA <- blockedAngle(n,state,p_pred,objects)
  # Utility components
  ID <- idUtility(p,n,ID,ok,group=state$group) 
  ID[ID==-Inf] <- NA # Cant plot -Inf
  BA <- baUtility(p,BA)
  BA[BA==-Inf] <- NA # Cant plot -Inf
  PS <- psUtility(p,state$v[n])
  GA <- gaUtility(p,GA) 
  CA <- caUtility(p)
  FL <- flUtility(p,FL)
  GD <- gdUtility(p,GD)

  V <- utility(p,n,state,P_n,p_pred,centres,objects,ok,iInfo=iInfo)
  Pr <- pCNLs(V,muM,nests,alpha) 
  par(mfrow=c(2,5))
  u <- c(ID,PS,BA,GA,CA,FL,GD)
  ylim <- c(min(u,na.rm=TRUE),max(u,na.rm=TRUE))
  plot(ID,ylab="Interpersonal Distance",xlab="Cell",ylim=ylim); plotLines()
  plot(BA,ylab="Blocked Angle",xlab="Cell",ylim=ylim); plotLines()
  plot(PS,ylab="Prefered Speed",xlab="Cell",ylim=ylim); plotLines() 
  plot(GA,ylab="Goal Angle",xlab="Cell",ylim=ylim); plotLines() 
  plot(CA,ylab="Current Angle",xlab="Cell",ylim=ylim); plotLines() 
  plot(FL,ylab="Follow the Leader",xlab="Cell",ylim=ylim); plotLines() 
  plot(GD,ylab="Group Distance",xlab="Cell",ylim=ylim); plotLines() 
  if (any(is.finite(V[-1]))) {
    plot(1:33,V[-1],pch=16,xlab="Cell",ylab="Utility",
       main=paste("V(0)=",V[1])); plotLines()
  } else plot(NA,NA,xlim=c(1,33),ylim=c(0,1),xlab="Cell",ylab="STOPPED!",
              main=paste("V(0)=",V[1]))
  plot(0:33,Pr,xlab="Cell",ylab="Probability"); plotLines()
}

#### PARAMETER MANAGEMENT ----

getmuM <- function(p) 
  # Transform nest association to precisions (mu)  
{
  1/(1-p[c("Central","NonCentral","acc","const","dec")])
}


toNatural <- function(p,
  gt1p=c(""),  
  negp=c(""),                             
  posp=c("rU",                              # Utility randomness
         "bS",                              # Stand still threshold
         "bGD","aGD",                       # Group distance
         "bFL","aFL","dFL",                 # Follow the leader
         "bCA","bCAlr","aCA",               # Current direction
         "bBA","aBA",                       # Blocked angle
         "bGA","aGA",                       # Goal angle
         "bPS","aPS","sPref",               # Prefered velocity
         "bID","aID","dID",                 # Interpersonal distance
         "Bb","dB"),                        # Buddy
  pp=c("Central","NonCentral","acc","const","dec") # DCM nest association                                  
  ) 
  # Tranfrom parameters from real line to natural
{
  p[gt1p] <- exp(p[gt1p])+1
  p[negp] <- -exp(p[negp])
  p[posp] <- exp(p[posp])
  p[pp] <- pnorm(p[pp])
  p[!is.na(p)]
}


toReal <- function(p,
  gt1p=c(""),  
  negp=c(""),           
  posp=c("rU",                              # Utility randomness
         "bS",                              # Stand still
         "bGD","aGD",                       # Group distance
         "bFL","aFL","dFL",                 # Follow the leader
         "bCA","bCAlr","aCA",               # Current direction
         "bBA","aBA",                       # Blocked angle
         "bGA","aGA",                       # Goal angle
         "bPS","aPS","sPref",               # Prefered velocity
         "bID","aID","dID",                 # Interpersonal distance
         "Bb","dB"),                        # Buddy
  pp=c("Central","NonCentral","acc","const","dec") # DCM nest association                                 
  ) 
  # Transform parameters from natural to real line
{
  p[gt1p] <-  log(p[gt1p]-1)
  p[negp] <-  log(-p[negp])
  p[posp] <-  log(p[posp])
  p[pp] <- qnorm(p[pp])
  p[!is.na(p)]
}

#### DCM ----

setAlpha <- function(nests) 
  # Nest weight  
{
  unest <- unlist(nests)
  if (min(unest)==0) 
    shift <- 1 else  # zero cell
    shift <- 0
  unest <- unest + 1
  alts <- sort(unique(unest))
  alphas <- numeric(length(alts))
  for (i in alts) {
    alphas[i] <- 1/sum(i==unest)    
  }
  alpha <- nests
  for (i in 1:length(alpha)) alpha[[i]] <- alphas[shift+alpha[[i]]]
  alpha
}


pCNLs <- function(V,muM=rep(1,length(nests)),nests,alpha,mu=1)
  # Crossed nested logit probabilities for each cell
{
  
  # Probability of alternatives within nests
  pAinNest <- function(Vlist,nests,alpha,muM) {
    pim <- nests
    for (m in 1:length(nests)) { # alternatives in each nest
      tmp <- alpha[[m]]*exp(muM[m]*Vlist[[m]])  
      bad <- tmp==Inf # Overflow
      if (any(bad)) tmp <- ifelse(bad,1,0)
      if (all(tmp==0)) pim[[m]] <- tmp else pim[[m]] <- tmp/sum(tmp)
    }
    pim
  }
  
  # Probability of nest m
  pNest <- function(Vlist,nests,alpha,mu,muM) {
    mu_muM <- mu/muM
    tmp <- sapply(1:length(nests),function(m){
      (sum(alpha[[m]]*exp(muM[m]*Vlist[[m]])))^mu_muM[m]
    })
    bad <- tmp==Inf # Overflow
    if (any(bad)) tmp <- ifelse(bad,1,0)
    if (all(tmp==0)) tmp else tmp/sum(tmp)
  }
  
  # Nest probabilties
  if (any(unlist(nests)==0))
    nests <- lapply(nests,function(x){x+1})
  Vlist <- lapply(nests,function(x){V[x]})
  # Set largest V to zero to avoid numerical issues
  maxV <- max(unlist(lapply(Vlist,max)))
  Vlist <- lapply(Vlist,function(x){x-maxV})
  pN <- pNest(Vlist,nests,alpha,mu,muM)
  pAN <- pAinNest(Vlist,nests,alpha,muM)
  for (m in 1:length(nests)) pAN[[m]] <- pAN[[m]]*pN[[m]]
  pA <- V
  indx <- unlist(nests)
  pAN <- unlist(pAN)
  for (i in 1:length(pA)) pA[i] <- sum(pAN[indx==i])
  pA
}

#### PEDESTRIAN & GOAL MANAGEMENT ----


getpMat <- function(n,p,pSD,rowNames=NULL) 
  # Get n new p on real line as n row matrix 
  matrix(rnorm(n*length(p),p,pSD),ncol=length(p),byrow=TRUE,
         dimnames=list(rowNames,names(p)))


exitPed <- function(state,AAl,close=1) 
  
{
  done <- unlist(lapply(state$P,function(x){dim(x)[1]==attr(x,"i")})) # last goal
  for (i in 1:length(done)) if (done[i]) done[i] <- # close to last goal
    dist(state$P[[i]][dim(state$P[[i]])[1],,drop=FALSE],state$p[i,,drop=TRUE]) < close
  if (any(done)) {
    nams <- unlist(lapply(strsplit(row.names(state$p),"_"),function(x){x[1]}))[done]
    state$p <- state$p[!done,,drop=FALSE]
    state$a <- state$a[!done]
    state$v <- state$v[!done]
    state$r <- state$r[!done]
    state$cell <- state$cell[!done]
    numLetter <- attr(state$P,"numLetter")
    state$P <- state$P[!done]
    state$group <- state$group[!done]
    state$pMat <- state$pMat[!done,,drop=FALSE]
    if (!is.null(numLetter)) 
      attr(state$P,"numLetter") <- numLetter
  }
  state
}


addPedTogether <- function(state,startState,p,pSD,groupSize=1,
                   matchvPref=FALSE, # Same sPref for group members  
                   groupSpace=1)   # initial spacing 
  # Adds pair of peds (or groupSize groups of peds spaced at groupSpace)
  # entering on the left and right.
{
  
  makeSpace <- function(groupSize,groupSpace=.5)
    # Make grid for group member displacements around centroid
    # Assumes entry from the left or right in pairs next to each other
  {
    if (groupSize>6) error("Cant accomodate groups > 6")
    if (groupSize==2) { # 2x2 matrix
      out <- cbind(dx=c(0,0),dy=groupSpace*(1:2 - 1.5))
    } else if (groupSize<5) { # 4x2 matrix
      tmp <- groupSpace*(1:2 - 1.5)
      out <- cbind(dx=rep(tmp,2),dy=rep(tmp,each=2))
    } else { # 6x2 matrix
      tmpy <- groupSpace*(1:2 - 1.5)
      tmpx <- groupSpace*(1:3 - 2)
      out <- cbind(dx=rep(tmpx,2),dy=rep(tmpy,each=3))
    }
    # randomly take required number of 2x2 or 3x2 matries
    out[sample(1:dim(out)[1],groupSize),]
  }
        
  pnams <- unlist(lapply(strsplit(row.names(state$p),"_"),function(x){x[1]}))
  lets <- c(LETTERS[!(LETTERS %in% pnams)][1],
            letters[!(letters %in% pnams)][1])
  numLetter <- attr(state$P,"numLetter")
  if ( !is.null(numLetter) ) {
    if (is.na(lets[1])) 
      lets[1] <- LETTERS[sample(1:26,1,prob=(1/numLetter[1:26])/sum(1/numLetter[1:26]))]
    if (is.na(lets[2])) 
      lets[2] <- letters[sample(1:26,1,prob=(1/numLetter[27:52])/sum(1/numLetter[27:52]))]
    nams <- character(0)
    if (groupSize>1) { # Space out group members
      space <- makeSpace(groupSize,groupSpace)
      out <- t(t(startState$p) + space[1,])
      for (i in 2:groupSize) out <- rbind(out,t(t(startState$p) + space[i,]))
      startState$p <- out
    }
    for (i in 1:groupSize) {
      nams <- c(nams,paste(lets,numLetter[lets],sep="_"))
      numLetter[lets] <- numLetter[lets] + 1
      if (i>1) {
        startState$a <- c(startState$a,startState$a[1:2])
        startState$v <- c(startState$v,startState$v[1:2])
        startState$r <- c(startState$r,startState$r[1:2])
        startState$P <- c(startState$P,startState$P[1:2])
        startState$cell <- c(startState$cell,0) # Predict stationary
        newp <- getpMat(2,p,pSD)
        if (matchvPref) newp[,"sPref"] <- startState$pMat[1:2,"sPref"]
        startState$pMat <- rbind(startState$pMat,newp)
      } else startState$pMat <- getpMat(2,p,pSD)
    }
  } else nams <- lets
  startState$group <- max(state$group) + rep(1:2,times=groupSize)
  row.names(startState$p) <- nams
  names(startState$a) <- nams
  names(startState$v) <- nams
  names(startState$r) <- nams
  names(startState$group) <- nams
  row.names(startState$pMat) <- nams
  state$p <- rbind(state$p,startState$p)
  state$v <- c(state$v,startState$v)
  state$a <- c(state$a,startState$a)
  state$r <- c(state$r,startState$r)
  state$group <- c(state$group,startState$group)
  state$pMat <- rbind(state$pMat,startState$pMat)
  state$P <- c(state$P,startState$P)
  state$cell <- c(state$cell,startState$cell)
  if (!is.null(numLetter)) attr(state$P,"numLetter") <- numLetter
  state
}


addPed <- function(state,startState,p,pSD,group=NA,sPref=NA,useUpper=NA,useLetter=NA) 
  # Adds single ped unless the entry goal is occupied. Use group and spref to 
  # assign same group number and sPref (natural scale) for group members.
  # useUpper: NA = take next, TRUE = use next uppercase, FALSE = next lower case
  # useLetter: use this letter value if not NA
{
  # Check if startGoal is available.
  if (occupied(attr(startState$P[[1]],"retrace"),startState$r,state)) {
    cat("\nEntry failure!\n")
    return(state)
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
  state$P <- c(state$P,startState$P)
  state$cell <- c(state$cell,startState$cell)
  if (!is.null(numLetter)) attr(state$P,"numLetter") <- numLetter
  state
}



getP <- function(state) 
  # Extract goal matrix from state
{
  do.call(rbind,lapply(state$P,function(x){x[attr(x,"i"),]}))
}


updateGoal <- function(state,objects,close=2) 
  # Update goal (unless it is the last goal), two types, if mustVisit update if 
  # within close of achieving it, if not update if can see next goal (or could
  # see on turning).
  {
  
  for (j in 1:length(state$P)) { # Each pedestrian
    if (!seesCurrentGoal(j,state,objects)) { # retrace
      i <- attr(state$P[[j]],"i")
      if (i==1) {
        P0 <- NULL 
        V0 <- NULL
      } else {
        P0 <- state$P[[j]][1:(i-1),,drop=FALSE]
        V0 <- attr(state$P[[j]],"mustVisit")[1:(i-1)]
      }
      P1 <- state$P[[j]][i:dim(state$P[[j]])[1],,drop=FALSE]
      V1 <- attr(state$P[[j]],"mustVisit")[i:dim(state$P[[j]])[1]]
      state$P[[j]] <- rbind(P0,attr(state$P[[j]],"retrace"),P1)
      attr(state$P[[j]],"mustVisit") <- c(V0,FALSE,V1)
      attr(state$P[[j]],"i") <- i
      attr(state$P[[j]],"retrace") <- state$p[j,]
    } else  { # Update Goal
      i <- attr(state$P[[j]],"i")
      if (dim(state$P[[j]])[1] != i) { # Not last goal
        doUpdate <- seesCurrentGoal(j,state,objects,offset=1) # see next goal 
        if (doUpdate & attr(state$P[[j]],"mustVisit")[i]) 
          doUpdate <- dist(state$P[[j]][i,,drop=FALSE],state$p[j,]) < close
        if (doUpdate) attr(state$P[[j]],"i") <- attr(state$P[[j]],"i") + 1
      } else doUpdate <- FALSE
      # Store new position
      attr(state$P[[j]],"retrace") <- state$p[j,]
      # Some error checking
      if (!seesCurrentGoal(j,state,objects)) {
        print(state$P[[j]])
        print(state$p[j,])
        print(doUpdate)
        stop(paste0("Goal update can't be seen for ",dimnames(state$p)[[1]][j]," (",j,")"))
      }
    }
  }
  state
}


#### OBJECT BLOCKING ----

# Is xy inside or outside of the square
inObject <- function(xy,xlim,ylim,outside=TRUE) 
{
  ok <- (xy[1] > xlim[1]) & (xy[1] < xlim[2]) & (xy[2] > ylim[1]) & (xy[2] < ylim[2])
  if (outside) !ok else ok
}



isBlocked <- function(centres,p_n,objects,ok=logical(33))
  # Is line of site from p_n to centres blocked?
{
  # inner ring blocked?
  ok[23:33] <- apply(centres[23:33,],1,seesGoal,P_n=p_n,objects=objects)
  remain <- c(12:22)[ok[23:33]]
  if (length(remain)>0) ok[remain] <- 
    apply(centres[remain,,drop=FALSE],1,seesGoal,P_n=p_n,objects=objects)
  remain <- c(1:11)[ok[12:22]]
  if (length(remain)>0) ok[remain] <- 
    apply(centres[remain,,drop=FALSE],1,seesGoal,P_n=p_n,objects=objects)
  !ok
}


okObject <- function(n,objects,state,centres)
  # Boolean indicating if each cell is in the room and not blocked by objects  
{
  # Inside the room (defined by objects[[1]])
  ok <- apply(centres,1,inObject,xlim=objects[[1]]$x,ylim=objects[[1]]$y,outside=FALSE)
  if (length(objects)>1) # Are cells blocked by objects?
    blocked <- isBlocked(centres,state$p[n,],objects[2:length(objects)])
  if (!is.null(blocked)) ok <- ok & !blocked
  matrix(ok,ncol=3)
}


object2lines <- function(o) 
  # Turns an rectangular object into a specificaiton of its constituant lines
  # Output a 2 (x/y) x 4 (lines) x 2 (begin/end point) array  
{
  array(c(o$x[1],o$y[1],o$x[1],o$y[2],o$x[1],o$y[1],o$x[2],o$y[1],
          o$x[2],o$y[1],o$x[2],o$y[2],o$x[1],o$y[2],o$x[2],o$y[2]),  
  dim=c(2,4,2),dimnames=list(c("x","y"),c("L1","L2","L3","L4"),c("P1","P2")))
}


bodyObjectOverlap <- function(oL,r,okCentres) 
  # For body radius r at each coordinate in centres is there overlap with 
  # any of the constituant lines in oL that specify an object. 
  # returns boolean vector, TRUE for cells with overlap
{
  # right angles to each object line
  a <- unique((angle2(p1=t(oL[,,"P1"]),p2=t(oL[,,"P2"]))+90) %% 180)
  # dx and dy to move along line
  x <- r*sin(a*pi/180); y <- r*cos(a*pi/180)
  apply(okCentres,1,function(p){ # for each centre check for overlap
    # Lines 
    segments <- array(c(p-rbind(x,y),p+rbind(x,y)),dim=c(2,length(x),2),
      dimnames=list(c("x","y"),NULL,c("P1","P2")))
    out <- FALSE 
    for (j in 1:dim(segments)[2]) {
      out <- out | any(sapply(1:(dim(oL)[2]),function(i){
        all(is.finite(
            line.line.intersection(segments[,j,"P1"],segments[,j,"P2"],
                                   oL[,i,"P1"],oL[,i,"P2"],TRUE)
        )) 
      }))
    }
    out
  })
}

bodyObjectOK <- function(r,centres,objects,ok) 
  # Is the cell OK or does body radius r positioned at centres overlap with an object? 
{
  if (!any(ok)) return(NULL)
  oLines <- lapply(objects,object2lines)
  # Does it overlap
  out <- !logical(33)
  out[ok] <- apply(matrix(unlist(lapply(oLines,bodyObjectOverlap,r=r,
    okCentres=centres[as.vector(ok),,drop=FALSE])),nrow=sum(ok)),1,function(x){any(x)})
  # If it doesnt overlap it is OK
  matrix(!out,nrow=11)
}


okBodyBody <- function(n,state,centres,ok) 
  # Is the cell OK or does body radius r positioned at centres overlap with another body?   
{
  if (!any(ok)) return(NULL)
  d <- state$r[n] + state$r[-n]
  out <- !logical(33)
  # Any overlap?
  out[ok] <- apply(centres[ok,,drop=FALSE],1,function(x){
    any(dist1(matrix(x,nrow=1),state$p[-n,,drop=FALSE]) < d)
  })
  # If it doesnt overlap it is OK
  matrix(!out,nrow=11)
}


#### FIX COLLISIONS ---- 

bestAngle <- function(p,n,state,P_n,p_pred,objects,iInfo,cores=1) 
  # Scan around in 10 degree increments when stopped to find best utility
{  
  getVA <- function(i,n,p,state,P_n,objects,iInfo) 
  {
    state$a[n] <- i*10  
    centres <- c_vd(1:33,p1=state$p[n,],v1=state$v[n],a1=state$a[n])
    ok <- okObject(n,objects,state,centres)
    V <- utility(p,n,state,P_n,p_pred,centres,objects,ok,iInfo)[-1]
    c(V=max(V),a=state$a[n])
  }

  VA <- mcmapply(getVA,0:35,mc.cores=cores,MoreArgs = 
    list(n=n,p=p,state=state,P_n=P_n,objects=objects,iInfo=iInfo))
  VA[2,which.max(VA[1,])]
}


revertState <- function(n,state,oldState,P_n,p_pred,objects,iInfo,
                        reorient=FALSE,usebestAngle=FALSE,cores=1,sStop=0.2) 
  # return n in state to oldState position, angle and goal, but stop  
{
  p <- toNatural(state$pMat[n,])
  state$p[n,] <- oldState$p[n,]
  state$P[[n]] <- oldState$P[[n]]
  state$v[n] <- sStop
  if (reorient & usebestAngle) {
    cat("Choosing best angle \n")
    state$a[n] <- bestAngle(p,n,state,P_n,p_pred,objects,iInfo,cores=cores)
  } else state$a[n] <- oldState$a[n] 
  state$cell[n] <- 0
  state
}


fixCollision <- function(state,oldState,P_n,p_pred,objects,iInfo,usebestAngle=FALSE,cores=1) 
  # Find cases where new positions cause body clash and revert back to old 
  # position in a stationary state. If this has been the case for more than
  # one iteration reorient using 
{
  nped <- dim(state$p)[1]
  bads <- numeric(0)
  
  for (n in 1:nped) {
    bad <- c(1:nped)[-n][
      dist1(state$p[n,,drop=FALSE],state$p[-n,,drop=FALSE]) < 
        (state$r[n] + state$r[-n])]
    if (length(bad)>0) {
      bads <- c(bads,n)
      cat(paste("\nClash involving",names(state$v)[n],"with",names(state$v[bad]),"\n"))
      state <- revertState(n,state,oldState,P_n,p_pred,objects,iInfo,
        reorient= n %in% oldState$bads,usebestAngle=usebestAngle,cores=cores)
      cat("\n")
    }
  }
  state$bads <- bads
  # for (n in 1:(nped-1)) {
  #   bad <- c((n+1):nped)[
  #     dist1(state$p[n,,drop=FALSE],state$p[-c(1:n),,drop=FALSE]) < 
  #       (state$r[n] + state$r[-c(1:n)])]
  #   if (length(bad)>0) {
  #     bads <- c(bads,n,bad)
  #     cat(paste("\nClash involving",names(state$v)[n],"\n"))
  #     state <- revertState(n,state,oldState,P_n,p_pred,objects,iInfo,
  #       reorient= n %in% oldState$bads,usebestAngle=usebestAngle,cores=cores)
  #     for (i in bad) {
  #       cat(paste("   with",names(state$v)[i],"\n"))
  #       state <- revertState(i,state,oldState,P_n,p_pred,objects,iInfo,
  #         reorient = i %in% oldState$bads,usebestAngle=usebestAngle,cores=cores)
  #     }
  #     cat("\n")
  #   }
  # }
  # state$bads <- bads
  # for (n in 1:(nped-1)) {
  #   bad <- c((n+1):nped)[
  #     dist1(state$p[n,,drop=FALSE],state$p[-c(1:n),,drop=FALSE]) < (
  #     state$r[n] + state$r[-c(1:n)])]
  #   if (length(bad)>0) {
  #     cat(paste("Second try: Clash involving",names(state$v)[n],"\n"))
  #     state <- revertState(n,state,oldState)
  #     for (i in bad) {
  #       cat(paste("   with",names(state$v)[i],"\n"))
  #       state <- revertState(i,state,oldState) 
  #     }
  #     cat("\n")
  #   }
  # }
  state
}


#### ITERATE SIMULATION ----


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

  if (!any(ok) | state$v[n] < .2) { # stop and turn around
    state$a[n] <- (state$a[n] + 180) %% 360
    state$v[n] <- sStop
    cell <- -1
  } else { 
    V <- utility(p,n,state,P_n,p_pred,centres,objects,ok,iInfo=iInfo)
    Pr <- pCNLs(V,muM,nests,alpha)
    names(Pr) <- 0:33
    if ( !is.null(plotGrid) && plotGrid ) 
      draw_grid(state$p[n,],state$v[n],state$a[n],plotPoints=TRUE,Pr=Pr[-1])
    cell <- sample.int(length(V),1,TRUE,prob=Pr)-1
    if ( !is.null(printChoice) && printChoice) {
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
  out <- list(p=state$p[n,],v=state$v[n],a=state$a[n],r=state$r[n],group=state$group[n],
              pMat=state$pMat[n,],cell=cell)
  attr(out,"ok") <- ok
  
  if (!inObject(state$p[n,],xlim=objects[[1]]$x,ylim=objects[[1]]$y,outside=FALSE))
    stop(n)
  
  out
}



# plotGoal=TRUE;plotCircle=TRUE; delay=0; cores=1; usebestAngle=FALSE
# showMind=NULL; printChoice=NULL; collisionFix=TRUE 
moveAll <- function(state,objects,nests,alpha,delay=0,cores=1,collisionFix=TRUE,
    showMind=NULL,printChoice=NULL,plotComponents=FALSE,plotGoal=TRUE,plotCircle=TRUE,
    usebestAngle=FALSE)
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
         objects=objects,plotGrid=showMind,printChoice=printChoice,usebestAngle=usebestAngle)
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
         iInfo=iInfo,usebestAngle=usebestAngle))
  state <- makeState(state.list,nams=row.names(state$p),P_n=state$P)
  
  # Check and fix collisions
  if (collisionFix) {
    if (is.list(state$P)) P_n <- getP(state) else P_n <- state$P
    iInfo <- getiInfo(state)
    p_pred <- predictPed(state$p,state$v,state$a,state$cell)
    state <- fixCollision(state,oldState,P_n,p_pred,objects,iInfo,cores=cores)
  }
  plotPed(state$p,getP(state),state$a,state$r,objects,state$cell,plotGoal=plotGoal,plotCircle=plotCircle)
  if (is.list(state$P)) state <- updateGoal(state,default=extras$default,objects,
                                            reach=reach,interactionTime=interactionTime) # Goal stack
  Sys.sleep(delay)
  state
}



#### !!!! Estimation NOT UPDATED !!!!  ----
 
 
##### Prepare traces ----
prepareTrace <- function(trace,objects,nests,alpha,p,cores=1)
{
  prepareState <- function(state,objects)
  {
    N <- dim(state$p)[1]
    centres <- oks <- dCs <- leader <- inFront <- groups <- vector(mode="list",length=N)
    dAngle <- matrix(nrow=N,ncol=11)
    dP <- matrix(nrow=N,ncol=33)
    p_pred <- predictPed(state$p,state$v,state$a,state$cell)
    for (n in 1:N) {
      centres[[n]] <- c_vd(1:33,p1=state$p[n,],v1=state$v[n],a1=state$a[n])
      ok <- apply(centres[[n]],1,inObject,xlim=objects[[1]]$x,ylim=objects[[1]]$y,FALSE)
      if (length(objects)>1) {
        blocked <- apply(array(apply(b_vd(p1=state$p[n,],v1=state$v[n],a1=state$a[n]),2:3,
          inObject,xlim=objects[[2]]$x,ylim=objects[[2]]$y,outside=FALSE),dim=c(5,2,12)),c(2,3),any)
        for (i in 2:length(objects)) {
          ok <- ok & apply(centres[[n]],1,inObject,xlim=objects[[i]]$x,ylim=objects[[i]]$y)
          if (i > 2) blocked <- blocked | apply(array(apply(
            b_vd(p1=state$p[n,],v1=state$v[n],a1=state$a[n]),2:3,inObject,
            xlim=objects[[i]]$x,ylim=objects[[i]]$y,outside=FALSE),dim=c(5,2,12)),c(2,3),any)
        }
        for (i in 2:12) blocked[,i-1] <- blocked[,i-1] & blocked[,i]
        blocked <- apply(rbind(rep(FALSE,11),blocked[2:1,-12]),2,function(x) {
          if (x[3]) x[1:2] <- TRUE else if(x[2]) x[1] <- FALSE; x
        })
      } else blocked <- NULL
      ok <- t(apply(matrix(ok,ncol=3),1,function(x){
        if (!x[3]) x[1:2] <- FALSE else if(!x[2]) x[1] <- FALSE; x
      }))
      if (!is.null(blocked)) ok <- ok &!t(blocked)
      oks[[n]] <- ok
      dCs[[n]] <- predClose(n,p1=state$p[n,,drop=FALSE],a1=state$a[n],centres[[n]],p_pred)
      dAngle[n,] <- destinationAngle(state$a[n],state$p[n,,drop=FALSE],state$P[n,,drop=FALSE])/90
      dP[n,] <- dist1(state$P[n,],centres[[n]])
      tmp <- getLeaders(n,state)
      if (!is.null(tmp)) leader[[n]] <- tmp
      inFront[[n]] <- minAngle(state$a[n],angle2(state$p[n,,drop=FALSE],state$p[-n,,drop=FALSE])) < 85
      groups[[n]] <- getBuddy(n,group=state$group,a=state$a,p_pred,centres[[n]])
    }


    c(state,list(centres=centres,ok=oks,dC=dCs,p_pred=p_pred,
                 destAngle=dAngle,dP=dP,leader=leader,inFront=inFront,groups=groups))
  }

  out <- mclapply(trace,prepareState,objects=objects,mc.cores=cores)
  attr(out,"nests") <- nests
  attr(out,"alpha") <- alpha
  pMat <- do.call(rbind,lapply(trace,function(x){x$pMat}))
  attr(out,"pMat") <- pMat[unique(row.names(pMat)),]
  attr(out,"ps.names") <- dimnames(attr(out,"pMat"))
  out
}


getSubjects <- function(Ltrace) {

  # Converts trace to subject data structure
  getSubject <- function(LT,s) {
    isin <- row.names(LT$p)==s
    if ( any(isin) ) {
      out <- LT
      out$n <- c(1:length(isin))[isin]
      out$cell <- out$cell[isin]
      out$pMat <- LT$pMat[isin,]
      out$centres <- LT$centres[isin][[1]]
      out$ok <- LT$ok[isin][[1]]
      out$dC <- LT$dC[isin][[1]]
      out$leader <- LT$leader[isin][[1]]
      out$inFront <- LT$inFront[isin][[1]]
      out$groups <- LT$groups[isin][[1]]
      out
    } else NULL
  }

  snams <- unique(unlist(lapply(Ltrace,function(x){row.names(x$p)})))
  out <- vector(mode="list",length=length(snams))
  names(out) <- snams
  for (s in snams) {
    tmp <- lapply(Ltrace,getSubject,s=s)
    out[[s]] <- tmp[!unlist(lapply(tmp,is.null))]
    attr(out[[s]],"p") <- out[[s]][[1]]$pMat
    for (i in 1:length(out[[s]])) out[[s]][[i]]$pMat <- NULL
  }
  out
}

# SLtrace <- getSubjects(Ltrace)


#### Core estimation functions ----
# Model utility
utilityL <- function(p,n,state,P_n,p_pred,centres,ok,dC,GA,dP,
                     leader,groups,inFront,
        # absolute angular difference between straight ahead and each cone
        directionAngle=rep(c(72.5,50,32.5,20,10,0,10,20,32.5,50,72.5),times=3))
{
    
  idUtility(p,n,ID,ok,group=groups) +
  psUtility(p,state$v[n]) +
  gaUtility(p,destAngle) +
  caUtility(p) +
  flUtility(p,FL)
  
  buddyUtility(p,groups)
   
}


pCNL <- function(cell,V,muM=rep(1,length(nests)),nests,alpha,mu=1,
  cellNest=cbind(t(matrix(c(rep(2:3,5),c(1,3),rep(2:3,5),rep(c(2,4),5),c(1,4),
                    rep(c(2,4),5),rep(c(2,5),5),c(1,5),rep(c(2,5),5)),nrow=2)),
           cbind(c(1:5,1,6:15,2,16:25,3,26:30),c(1:11,1:11,1:11))))
{
  
  # Probability of alternatives within nests
  pAinNest <- function(Vlist,nests,alpha,muM) {
    pim <- nests
    for (m in 1:length(nests)) { # alternatives in each nest
      tmp <- alpha[[m]]*exp(muM[m]*Vlist[[m]])  
      bad <- tmp==Inf # Overflow
      if (any(bad)) tmp <- ifelse(bad,1,0)
      if (all(tmp==0)) pim[[m]] <- tmp else pim[[m]] <- tmp/sum(tmp)
    }
    pim
  }
  
  # Probability of nest m
  pNest <- function(Vlist,nests,alpha,mu,muM) {
    mu_muM <- mu/muM
    tmp <- sapply(1:length(nests),function(m){
      (sum(alpha[[m]]*exp(muM[m]*Vlist[[m]])))^mu_muM[m]
    })
    bad <- tmp==Inf # Overflow
    if (any(bad)) tmp <- ifelse(bad,1,0)
    if (all(tmp==0)) tmp else tmp/sum(tmp)
  }
  
  # Nest probabilties
  Vlist <- lapply(nests,function(x){V[x]})
  # Set largest V to zero to avoid numerical issues
  maxV <- max(unlist(lapply(Vlist,max)))
  Vlist <- lapply(Vlist,function(x){x-maxV})
  pN <- pNest(Vlist,nests,alpha,mu,muM)
  pAN <- pAinNest(Vlist,nests,alpha,muM)
  pAN[[cellNest[[cell,1]]]][cellNest[cell,3]]*pN[cellNest[cell,1]] + 
  pAN[[cellNest[[cell,2]]]][cellNest[cell,4]]*pN[cellNest[cell,2]] 
}


#### Non-subject functions ----

# stateL <- stateLs[[1]]
likeState <- function(stateL,p,nests,alpha) 
{
  
  like1 <- function(n,p,stateL,nests,alpha) 
  {
    if ( stateL$cell[n]==0 ) 1 else {
      p <- p[names(stateL$v)[n],]
      V <- utilityL(p,n,state=stateL[c("p","v","a")],
        P_n=stateL$P,p_pred=stateL$p_pred,centres=stateL$centres[[n]],
        ok=stateL$ok[[n]],groups=stateL$groups[[n]],
        destAngle=stateL$destAngle[n,],dP=stateL$dP[n,],leader=stateL$leader[[n]],
        inFront=stateL$inFront[[n]])
      pCNL(stateL$cell[n],V,muM=getmuM(p),nests,alpha) 
    }
  }

# for (i in 1:length(stateL)) like1(i,p,stateL,nests,alpha)
  sapply(1:length(stateL$v),like1,p=p,stateL=stateL,nests=nests,alpha=alpha)  
}



# stateLs <- Ltraces[[1]]
# for (i in 1:length(stateLs)) likeState(stateLs[[i]],p,nests,alpha)
likeStates <- function(stateLs,p,nests,alpha) {
  unlist(lapply(stateLs,likeState,p=p,nests=nests,alpha=alpha))
}

sumlogLike <- function(p,Ltrace,constant=NULL,cores=1,minLike=1e-10) 
{
  dimnames(p) <- attr(Ltrace,"ps.names")
  if (!is.null(constant)) {
    p <- cbind(matrix(rep(constant,each=dim(p)[1]),ncol=length(constant)),
               t(apply(p,1,toNatural)))
    colnames(p)[1:length(constant)] <- names(constant)
  } else p <- t(apply(p,1,toNatural))
  imat <- suppressWarnings(matrix(1:length(Ltrace),ncol=cores))
  imat[duplicated(imat)] <- NA
  Ltraces <- apply(imat,2,function(x){Ltrace[x[!is.na(x)]]})
  out <- mclapply(Ltraces,likeStates,p=p,nests=attr(Ltrace,"nests"),
                  alpha=attr(Ltrace,"alpha"),mc.cores=cores)
  sum(log(pmax(unlist(out),minLike)))
}


msumlogLike <- function(p,Ltrace,constant=NULL,cores=1,minLike=1e-10,mult=-1) 
  # sum log likelihood, p on reals, constant (on natural) added in 
{
  dimnames(p) <- attr(Ltrace,"ps.names")
  if (!is.null(constant)) {
    p <- cbind(matrix(rep(constant,each=dim(p)[1]),ncol=length(constant)),
               t(apply(p,1,toNatural)))
    colnames(p)[1:length(constant)] <- names(constant)
  } else t(apply(p,1,toNatural))
  imat <- suppressWarnings(matrix(1:length(Ltrace),ncol=cores))
  imat[duplicated(imat)] <- NA
  Ltraces <- apply(imat,2,function(x){Ltrace[x[!is.na(x)]]})
  out <- mclapply(Ltraces,likeStates,p=p,nests=attr(Ltrace,"nests"),
                  alpha=attr(Ltrace,"alpha"),mc.cores=cores)
  mult*sum(log(pmax(unlist(out),minLike)))
}


profilePed <- function(p.name,min.p,max.p,pMat,dat,cores=1,
                      n.point=50,digits=2,ylim=NA,verbose=FALSE) 
  # for parameter p.name in pMat for n subjects draws the profile likelihood for 
  # data in min.p (<0) to p.max (>0) around p.name column mena, and returns the 
  # maximum (on a grid of resolution n.point) 
{
  if (!(p.name %in% colnames(pMat)))
    stop("p.name not in p.vector")
  ps <- seq(min.p,max.p,length.out=n.point)
  ll <- numeric(n.point)
  for (i in 1:n.point) 
  {
    p <- pMat
    p[,p.name] <- p[,p.name] + (ps[i]-mean(p[,p.name]))  
    ll[i] <- sumlogLike(p,dat,cores=cores)
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  names(ll) <- round(ps,digits)
  if (any(is.na(ylim)))
    plot(ps,ll,type="l",xlab=p.name,ylab="log-likelihood") else
      plot(ps,ll,type="l",xlab=p.name,ylab="log-likelihood",ylim=ylim)
  abline(v=mean(pMat[,p.name]))
  # ll[ll==max(ll)]
  ps[ll==max(ll)]
}


#### Estimation per subject ----

# 
# stateS <- stateLs[[1]]
# Sstate <- stateS[[1]]

likeStateS <- function(stateS,nests,alpha) 
{
  
  like1 <- function(p,Sstate,nests,alpha) 
  {
    if ( Sstate$cell==0 ) 1 else {
      n <- Sstate$n
      V <- utilityL(p,n,state=Sstate[c("p","v","a")],
        P_n=Sstate$P,p_pred=Sstate$p_pred,centres=Sstate$centres,
        ok=Sstate$ok,dC=Sstate$dC,
        destAngle=Sstate$destAngle[n,],dP=Sstate$dP[n,],leader=Sstate$leader,
        inFront=Sstate$inFront,groups=Sstate$groups)
      pCNL(Sstate$cell,V,muM=getmuM(p),nests,alpha)
    }
  }

  p <- attr(stateS,"p")
  lapply(stateS,like1,p=p,nests=nests,alpha=alpha)  
}

# stateLs <- Ltraces[[1]]
likeStatesS <- function(stateLs,nests,alpha) {
  unlist(lapply(stateLs,likeStateS,nests=nests,alpha=alpha))
}

sumlogLikeS <- function(p,Ltrace,constant=NULL,cores=1,minLike=1e-10) 
{
  dimnames(p) <- attr(Ltrace,"ps.names")
  if (!is.null(constant)) {
    p <- cbind(matrix(rep(constant,each=dim(p)[1]),ncol=length(constant)),
               t(apply(p,1,toNatural)))
    colnames(p)[1:length(constant)] <- names(constant)
  } else p <- t(apply(p,1,toNatural))
  for (i in names(Ltrace)) attr(Ltrace[[i]],"p") <- p[i,]
  imat <- suppressWarnings(matrix(1:length(Ltrace),ncol=cores))
  imat[duplicated(imat)] <- NA
  Ltraces <- apply(imat,2,function(x){Ltrace[x[!is.na(x)]]})
  out <- mclapply(Ltraces,likeStatesS,nests=attr(Ltrace,"nests"),
                  alpha=attr(Ltrace,"alpha"),mc.cores=cores)
  sum(log(pmax(unlist(out),minLike)))
}


# p=attr(Ltrace,"pMat")
msumlogLikeS <- function(p,Ltrace,constant=NULL,cores=1,minLike=1e-10,mult=-1) 
  # sum minus log likelihood, p on reals, constant (on natural) added in 
{
  dimnames(p) <- attr(Ltrace,"ps.names")
  if (!is.null(constant)) {
    p <- cbind(matrix(rep(constant,each=dim(p)[1]),ncol=length(constant)),
               t(apply(p,1,toNatural)))
    colnames(p)[1:length(constant)] <- names(constant)
  } else t(apply(p,1,toNatural))
  for (i in names(Ltrace)) attr(Ltrace[[i]],"p") <- p[i,]
  imat <- suppressWarnings(matrix(1:length(Ltrace),ncol=cores))
  imat[duplicated(imat)] <- NA
  Ltraces <- apply(imat,2,function(x){Ltrace[x[!is.na(x)]]})
  out <- mclapply(Ltraces,likeStatesS,nests=attr(Ltrace,"nests"),
                  alpha=attr(Ltrace,"alpha"),mc.cores=cores)
  mult*sum(log(pmax(unlist(out),minLike)))
}


profilePedS <- function(p.name,min.p,max.p,pMat,dat,cores=1,
                        n.point=50,digits=2,ylim=NA,verbose=FALSE) 
  # for parameter p.name in pMat for n subjects draws the profile likelihood for 
  # data in min.p (<0) to p.max (>0) around p.name column mena, and returns the 
  # maximum (on a grid of resolution n.point) 
{
  if (!(p.name %in% colnames(pMat)))
    stop("p.name not in p.vector")
  ps <- seq(min.p,max.p,length.out=n.point)
  ll <- numeric(n.point)
  for (i in 1:n.point) 
  {
    p <- pMat
    p[,p.name] <- p[,p.name] + (ps[i]-mean(p[,p.name]))  
    ll[i] <- sumlogLikeS(p,dat,cores=cores)
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  names(ll) <- round(ps,digits)
  if (any(is.na(ylim)))
    plot(ps,ll,type="l",xlab=p.name,ylab="log-likelihood") else
      plot(ps,ll,type="l",xlab=p.name,ylab="log-likelihood",ylim=ylim)
  abline(v=mean(pMat[,p.name]))
  # ll[ll==max(ll)]
  ps[ll==max(ll)]
}



#### Analysis ----


# Average and mimimum distance among a set of points p
avminDist <- function(p) {
  avDist <- 0; minDist <- Inf
  N <- dim(p)[1]
  for (i in 1:N) {
    d <- dist1(p[i,],p[-c(1:i),,drop=FALSE])
    avDist <- avDist + sum(d)
    minDist <- min(c(minDist,d))
  }
  avDist <- avDist/(N*(N-1)/2)
  c(av=avDist,min=minDist)
}


# Matrix of x,y,v,a for each pedestrian at each time point
harvestTrace <- function(trace) 
{
  out=do.call(rbind,lapply(trace,function(x){x$p}))
  P <- do.call(rbind,lapply(trace,function(x){x$P}))
  dimnames(P)[[2]] <- c("X","Y")
  v <- do.call(c,lapply(trace,function(x){x$v}))
  a <- do.call(c,lapply(trace,function(x){x$a}))
  cell <- do.call(c,lapply(trace,function(x){x$cell}))
  cone <- coneNum(cell)
  ring <- ringNum(cell)
  cbind.data.frame(out,P,v=v,a=a,cell=cell,cone=cone,ring=ring)
}

