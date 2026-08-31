#require(installr)
#updateR()

## --------------------------------- AMDSU Book R Master File --------------------------------------
## This code file reproduces all the analyses in the book

#install.packages("smacof", repos="http://R-Forge.R-project.org" )

## Note that some functions require R-packages that you have to install first. To install a
## package, go to "Tools" and then to "Install Packages.." in RStudio. 
## Or use the install.packages() command shown above for the case of installing smacof.
## You need to install a package only once. (However, regularly check for updates!)
## Once installed, you can call the package with the library() or the require() command. 
## If a package has not been installed, you will get an error or a warning message 
## when you call it. 
## Required packages are: 
## - smacof
## - gplots
## - CircE
## - scatterplot3d
## - rgl
## - MASS

require("smacof")  ## loads smacof package; always needed in jobs below

## ---------------------------------- Chapter 1: First steps ----------------------------------------
## p. 5
diss <- sim2diss(crimes, method = "corr")  ## correlations-->dissimilarities
result <- mds(diss, type = "interval")  ## run MDS and store in ``result''
result    ## shows some basic information about the MDS job
names(result)  ## shows names of what is contained in ``results''
plot(result)  ## plots MDS configuration 
dat0 <- as.matrix(crimes)
dat <- dat0[ lower.tri(dat0, diag=FALSE) ]
dist0 <- as.matrix(result$confdist)
dist <- dist0[lower.tri(dist0, diag=FALSE)]
plot( dat, dist, type="p", pch=20, xlab="Correlations", ylab="Distances in MDS space" ) ## Fig. 1.7

## p. 6
result <- uniscale(diss)  ## run MDS in 1 dim and store in ``result''
## Note! uniscale() runs for VERY long if the number of variables is not VERY small!
## if so, use: result <- mds(diss, ndim=1, type="ratio") with the (small) risk of not getting the best-possible solution
result    ## shows some basic information about the MDS job
plot(result)  ## plots unidimensional configuration horizontally
## Next two lines plots unidimensional configuration vertically
plot(rep(0, length(result$conf)), -result$conf,  axes = FALSE, ann = FALSE, pch = 20, type = "o", xlim = c(-0.2, 0.8))
text(rep(0.10, length(result$conf)), -result$conf, names(result$conf))
## Produce data vs. distances regression plot (Fig. 1.10)
dist0 <- as.matrix(result$confdist)
dist <- dist0[lower.tri(dist0, diag=FALSE)]
plot( dat, dist, type = "p", pch = 20, xlab = "Correlations", ylab = "Distances in MDS space" )
fit.line <- lm(dist~dat)
abline(fit.line, col="red")

## or: 
## use solution of uniscale() as initial configuration (see p. 81f. in text: uniscale() is ratio MDS)
result1 <- uniscale(diss) ## avoids suboptimal local minima solutions
## allow for an interval- rather than a ratio-preserving mapping of data into distances: 
result2 <- mds(diss, init=as.matrix(result1$conf), type="interval", ndim=1, verbose=TRUE) 
## or simply run the following, without first uniscale() (solution is a suboptimal local minimum):
## result2 <- mds(diss, type="interval", ndim=1, verbose=TRUE) 
plot(rep(0, length(result2$conf)), -result2$conf, axes = FALSE, ann = FALSE, pch = 20, type = "o", xlim = c(-0.2, 0.8), cex=2)
text(rep(0.05, length(result2$conf)), -result2$conf, names(result$conf), pos=4)
text(rep(-0.05, length(result2$conf)), -result2$conf, pos=2, sprintf("%.2f",round(-1*result2$conf, 2) ))


## p. 8
data <- matrix(c(NA,NA,NA,NA, 3,10, 7, 8, 4,
                 NA,NA,NA,NA, 7, 8, 2, 5, 7,
                 NA,NA,NA,NA, 4, 7, 5, 6, 1,
                 NA,NA,NA,NA, 9, 1, 6, 7,10,
                 3, 7, 4, 9, NA,NA,NA,NA,NA, 
                 10, 8, 7, 1, NA,NA,NA,NA,NA,
                 7, 2, 5, 6, NA,NA,NA,NA,NA,
                 8, 5, 6, 7, NA,NA,NA,NA,NA,
                 4, 7, 1,10, NA,NA,NA,NA,NA), nrow = 9, ncol = 9)
colnames(data) <- c( "A", "B", "C", "D", "1", "2", "3", "4", "5" )  
diss <- sim2diss(data, method = 10)  # convert ratings into dissimilarities
result <- mds(diss, type="ordinal")
X <- result$conf
# ------------------  Configuration Plot (Fig. 1.11) --------------------------
plot(result, col="cadetblue", pch=16, label.conf=list(cex=1.2, pos=5), 
     ylim=c(-1.2, 1.2), cex.axis=1, cex.lab=1, 
     xlab="Dimension 1", ylab="Dimension 2", main="")
# ------------------  Shepard Diagram (Fig. 1.12) -----------------------------
dat <- data[lower.tri(diss)]; dist <- as.vector(result$confdist)
dhat <- as.vector(result$dhat)
plot(dat, dist, pch=21, cex=2, ylim=c(0.8, 2.5), xlim=c(0, 11), 
     xlab="Preference Ratings", ylab="Distances in Unfolding Space" )
points(dat, dhat, pch=16, col="cadetblue"); dat2 <- dat[order(dat, -dhat)]
dhat2 <- dhat[order(dat, -dhat)]; lines(dat2, dhat2, col="red")


## ------------------------------ Chapter 2: The purpose of MDS and Unfolding -----------------
## p. 15  (Fig. 2.2)
diss <- sim2diss(wish, method = 7)
res <- mds(diss, type = "ordinal")
plot(res)

## p. 20 (Fig. 2.6)
idiss <- sim2diss(intelligence[,paste0("T", 1:8)])
fit <- mds(idiss)
fit$conf[,2] <- -fit$conf[,2] ## Flip dimension 2 to make it equal to book version
plot(fit)

## p. 23 (Fig. 2.10-2.12)
r <- cor(PVQ40, use = "pairwise.complete.obs")
diss <- sim2diss(r, method = "corr") 
res <- mds(delta = diss, type = "ordinal") ## ordinal MDS
codes <- substring(colnames(PVQ40), 1, 2)
plot(res, main = "", hull.conf = list(hull = TRUE, ind = codes, col = "coral1", lwd = 2))
plot(res) ## Configuration plot without convex hulls
plot(res, plot.type = "Shepard")

## p. 23, Fig. 2.13
R <- cor(PVQ40agg); diss <- sim2diss(R)
result <- mds(diss, init = "torgerson", type = "ordinal"); plot(result)
erg <- fitCircle(result$conf[,1], result$conf[,2])
draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "black", lty = 2)


## p. 26, Fig. 2.14 and 2.15
c <- rowMeans(PVQ40agg) - PVQ40agg ## center ratings
diss <- max(c) - c ## turn preference ratings into dissimilarities
result <- unfolding(diss); 
plot(result, cex=1, xlab="", ylab="", main="", pch=16, asp=1,
     col.rows="#1010CC66", col.columns="black",
     label.conf.rows=list(col="blue", cex=.6),
     label.conf.columns=list(col="black", cex=1.1))
erg <- fitCircle(result$conf.col[,1], result$conf.col[,2])
draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "black", lty = 2)
plot(result, plot.type = "Shepard", pch = 1, cex = 1)

data.r <- na.omit(PVQ40) ## eliminate persons with missing values
data1 <- data.r - rowMeans(data.r) ## center ratings
diss <- max(data1)-data1; unf <- unfolding(diss)
plot(unf, what="columns", col.columns=1, asp=1, xlim=c(-1.3,1.3), ylim=c(-1.3,1.3),
        label.conf.columns=list(col="black"), main="", pch=16)
## external variables ------------------------------------------------------
gender <- attr(data.r, "Gender")[-attr(data.r, "na.action")]
points(unf$conf.row, pch=gender, cex=1, lwd=1.2) ## pch by gender
circle <- fitCircle(unf$conf.col[,1], unf$conf.col[,2])
draw.circle(circle[[1]], circle[[2]], radius=circle[[3]])
## discriminant analysis for gender ----------------------------------------
require(MASS); Y <- unf$conf.row; z <- as.data.frame(cbind(gender, Y))
fit <- lda(gender ~ Y, na.action="na.omit", data=z) ## discriminant gender
slope <- fit$scaling[2]/fit$scaling[1]
abline(a=0, b=slope, lty=1, col="red")
angle = atan(slope)*180/pi
px <- 1.4 ## x coordinate for label on line: define
py<-0+slope*px
text(x=px-.1, y=py-.1, "gender", srt=angle, cex=.8)
LDS <- as.data.frame(predict(fit)); L4 <- LDS[,4]
tt <- t.test(L4 ~ gender); tt ## t-test for gender discrimination
## multiple regression for age ---------------------------------------------
age <- attr(data.r, "Age")[-attr(data.r, "na.action")]
f <- lm(age ~ Y[,1]+Y[,2])
wy <- f$coefficients[3]; wx <- f$coefficients[2]
slope <- wy/wx; abline(a=0, b=slope, lty=2, col="blue")
age.pred <- predict(f); r <- cor(age, age.pred ); r ## fit of age line
angle = atan(slope)*180/pi
px <- 1.4  ## x coordinate for label on line: define
py<-0+slope*px
text(x=px-.2, y=py-.2, "age", srt=angle, cex=.8)




## ------------------------------- Chapter 3: Fit ------------------------------------
## p. 29:  Fig. 3.1
data0<-as.matrix(wish)
diss<-sim2diss(data0, method=7)
out<-mds(diss, type="ordinal")
## Shepard Diagram -------------
dat <- data0[lower.tri(data0)]
dist <- as.vector(out$confdist)
dhat <- as.vector(out$dhat)
ylim1 <- c(0, 1.8); xlim1 <- c(2, 7)
plot(dat, dist, pch=21, cex=2, ylim=ylim1, xlim=xlim1, cex.axis=1.4 , cex.lab=1.6,
     xlab="Similarity ratings (data)", ylab="Distances in MDS space" , lwd=1.8 )
points(dat, dhat, pch=16, col="blue", cex=1.5)
dat2 <- dat[order(dat, -dhat)]
dhat2 <- dhat[order(dat, -dhat)]
lines(dat2, dhat2, col="red", lwd=2)
for (i in 1:length(dat)){ segments(dat[i], dist[i], dat[i], dhat[i], col="blue", lwd=1.5, lty=2) }

## p. 29 (example of p. 15): Fig. 3.1, simplified
diss <- sim2diss(wish, method = 7)
res <- mds(diss, type = "ordinal")
plot(res, plot.type = "Shepard", shepard.x = wish)


## p. 35: histogram of permutation test stress values
my.results <- mds(diss)
ex <- permtest(my.results)
hist(ex$stressvec, xlim = c(ex$stress.obs-.05, max(ex$stressvec)+.05))
abline(v=ex$stress.obs, col="red")
points(ex$stress.obs, 0, cex=2, pch=16, col="red")

## p. 38, Figs. 3.4 and 3.5
plot(res, plot.type = "bubbleplot")
plot(res, plot.type = "stressplot")

## p. 39: heatmap of errors, Fig 3.6
require(gplots)
diss <- sim2diss(wish, method=7) 
res <- mds(diss, type="ordinal")
RepErr <- as.matrix((res$dhat - res$confdist)^2)
yr <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)
heatmap.2( RepErr, cellnote = round(RepErr,2), Rowv = NA, Colv="Rowv", 
           lwid = c(1, 40), lhei = c(1, 40), margins = c(8, 8),
           key=FALSE, notecol="black", trace="none", col=yr, symm=TRUE, dendrogram="none" )     

## p. 40
R <- cor(PVQ40agg)
diss <- sim2diss(R)
result <- mds(diss, type = "ordinal")  ## 2D interval MDS

## p.40, Jackknife (Fig. 3.7)
JK <- jackknife(result)
plot(JK, col.p = "black", col.l = "black", lwd = 1.2)

## p.40, Bootstrap (Fig. 3.8)
set.seed(123)
resboot <- bootmds(result, data = PVQ40agg, method.dat = "pearson", nrep = 500)
resboot
plot(resboot)



## ----------------------------- Chapter 5: Variants -------------------------------
## p. 56, Fig. 5.1

idiss <- sim2diss(intelligence[,paste0("T", 1:8)])
fit.ratio    <- mds(idiss)    # type = "ratio" is the default
fit.interval <- mds(idiss, type = "interval")
fit.ordinal  <- mds(idiss, type = "ordinal")
fit.spline   <- mds(idiss, type = "mspline")
op <- par(mfrow = c(1, 4))    # Four plots next to each other
plot(fit.ordinal,  plot.type = "Shepard", main = paste("Stress", round(fit.ordinal$stress, 2)))
plot(fit.interval, plot.type = "Shepard", main = paste("Stress", round(fit.interval$stress, 2)))
plot(fit.ratio,    plot.type = "Shepard", main = paste("Stress", round(fit.ratio$stress, 2)))
plot(fit.spline,   plot.type = "Shepard", main = paste("Stress", round(fit.spline$stress, 2)))
par(op)    # Return to default pot settings


## p. 59: Drift vector intro example, Fig. 5.2
P <- matrix( c(0,  4,  6, 13, 
               5,  0, 37, 21, 
               4, 38,  0, 16, 
               8, 31, 18,  0), nrow=4, ncol=4, byrow=TRUE) 
S <- (P + t(P))/2  # Symmetric part
A <- (P - t(P))/2  # Skewsymmetric part
n <- dim(P)[1]
diss <- sim2diss(S, method=40) 
res1 <- mds(diss, type="interval")
plot(res1, cex=2.5, main="", ylim=c(-.6,1.1), xlim=c(-.4, 1.1))
X <- res1$conf
for (i in 1:n) { 
  sx <- 0
  sy <- 0
  x  <- X[i,1]
  y  <- X[i,2]
  for (j in 1:n) { 
    c <- A[i,j]/mean(P)
    a <- X[j,1]
    b <- X[j,2]
    slope <- (b - y)/(a - x)
    if (c != 0) { 
      angle <- atan(slope); 
      length1 <- abs(c)
      x1 <- x + length1 * cos(angle)
      y1 <- y + length1 * sin(angle) 
      arrows(x, y, x1, y1, length=0.10, col="blue", lwd=2, lty=1) 
    }  ## arrows
    if (i != j) { 
      sx <- sx + x1 - x 
      sy <- sy + y1 - y 
    } 
  } 
  arrows(x, y, x+sx, y+sy, length=0.20, col="red", lwd=4, lty=1.5) 
}  ## drift vector

## p. 59: Morse code asymmetries, Fig. 5.3
fit.drift <- driftVectors(morse2, type = "ordinal")
fit.drift
plot(fit.drift)

## p. 61, Fig. 5.4 
res.helm <- indscal(helm)
res.helm  ## gives Stress-1 etc.
plot(res.helm)  ## plots the group space
names(res.helm) ## shows elements of object res.helm
res.helm$cweights$CD4
res.helm$cweights$N6a

sort(res.helm$sps)  ## stress per subject (%)

## p. 61, plot 2 extreme persons, Fig. 5.3 (middle and right panel)
op <- par(mfrow = c(1,2))
plot(res.helm$conf$CD4, pch = 20, cex = 0.8, main = "CD4", xlim = c(-1,1), ylim = c(-1,1), asp = 1)
text(res.helm$conf$CD4, labels = rownames(res.helm$conf$CD4), cex = 0.8, pos = 3)
plot(res.helm$conf$N6a, pch = 20, cex = 0.8, main = "N6a", xlim = c(-1,1), ylim = c(-1,1), asp = 1)
text(res.helm$conf$N6a, labels = rownames(res.helm$conf$N6a), cex = 0.8, pos = 3)
par(op)


## p. 64
EW.diss <- list(east = sim2diss(EW_eng$east), west = sim2diss(EW_eng$west))
res <- smacofIndDiff(EW.diss, type="ordinal", constraint="identity")
res; summary(res)  ## gives Stress, coordinates, SPP values
plot(res, main="East+West combined")
plot(res, plot.type = "Shepard")

## p . 65
## Example that shows that setting the weight matrix to a power of the similarities 
## can change the solution. The same random starts are imposed.
idiss <- sim2diss(intelligence[,paste0("T", 1:8)])
set.seed(123); fit          <- mds(idiss, type = "ratio", init = "random")   
set.seed(123); fit.wminpow  <- mds(idiss, weightmat = idiss^10,  type = "ratio", init = "random")   ## Emphasize fitting of large similarities
set.seed(123); fit.wpluspow <- mds(idiss, weightmat = idiss^-10, type = "ratio", init = "random")  ## Emphasize fitting of small similarities
op <- par(mfrow = c(2, 3))    # three plots next to each other
plot(fit,          main = paste("Stress", round(fit$stress, 2)))
plot(fit.wminpow,  main = paste("Stress", round(fit.wminpow$stress, 2)))
plot(fit.wpluspow, main = paste("Stress", round(fit.wpluspow$stress, 2)))
plot(fit,          plot.type = "Shepard", main = paste("Stress", round(fit$stress, 2)))
plot(fit.wminpow,  plot.type = "Shepard", main = paste("Stress", round(fit.wminpow$stress, 2)))
plot(fit.wpluspow, plot.type = "Shepard", main = paste("Stress", round(fit.wpluspow$stress, 2)))
par(op)    # Return to default pot settings


## ---------------------- Chapter 6: Confirmatory MDS --------------------------
## p.69
## MDS with theory-based initial configuration
fit.expl <- mds(rectangles, type = "ordinal", init = rect_constr) 
## Confirmatory MDS enforcing the design grid 
fit.cfdiag <- smacofConstraint(rectangles, constraint = "diagonal",
                type = "ordinal", ties = "secondary",  
                init = fit.expl$conf, external = rect_constr, 
                constraint.type = "ordinal")  
## Confirmatory MDS:  coordinates are a linear combination of the design grid 
fit.cflin  <- smacofConstraint(rectangles, constraint = "linear", 
                type = "ordinal", ties = "secondary", 
                init = fit.expl$conf, external = rect_constr, 
                constraint.type = "ordinal")  

op <- par(mfrow = c(1, 3))
## Make exploratory MDS plot with lines
plot(fit.expl, main = "Expl. MDS", cex = 2)  
conf <- fit.expl$conf
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])
fit.expl

## Make confirmatory MDS (C = diag) plot with lines
plot(fit.cfdiag, main = "Conf. MDS (C = diag)", cex = 2)
conf <- fit.cfdiag$conf
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])
fit.cfdiag

## Make confirmatory MDS (C is not diag) plot with lines

plot(fit.cflin, main = "Conf. MDS (C is not diag)", cex = 2)
conf <- fit.cflin$conf
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])
fit.cflin

par(op)

## p. 71
## The plots generated for the morse codes are not exactly the same as on p. 71 
## due to rotation and possibly local minima
res <- mds(morse, type = "ordinal")
res.con <- smacofConstraint(morse, type = "ordinal", constraint = "linear",
            external = morsescales[, 2:3], init = res$conf,
            constraint.type = "ordinal", constraint.ties = "primary", eps = 1e-8)

op <- par(mfrow = c(2,2))
res
res.con
plot(res, main = "Unconstrained MDS")
plot(res.con, main = "Regionally Constrained MDS")
plot(res,     plot.type = "Shepard")
plot(res.con, plot.type = "Shepard")
par(op)


## p. 73, Fig. 6.4
diss <- sim2diss(wish, method=max(wish))
res1 <- smacofSphere(diss, type = "ordinal")
res2 <- smacofSphere(diss, type = "ordinal", algorithm = "dual", penalty = 22)
res3 <- mds(diss, type = "ordinal")
res1$stress ## gives Stress values of each solution
res2$stress 
res3$stress 
op <- par(mfrow = c(1,3))
plot(res1, main="Circular MDS (primal)")
plot(res2, main="Circular MDS 2 (dual)")
plot(res3, main="Exploratory MDS")
par(op)

## p. 74
require(CircE)
R <- cor(PVQ40agg)
## CircE commands (with lots of default agruments): 
lower1 <- c(0,0,0,270,270,180,180,180,90,90) ## lower bounds for point angles
upper1 <- c(90,90,90,360,360,270,270,270,180,180) ## upper bounds
res <- CircE.BFGS(R, v.names = colnames(R), m = 1, N = 10, upper = upper1, lower = lower1, 
                  equal.com = FALSE, equal.ang = FALSE)
CircE.Plot(res, ef = 0.1)


## ------------------------- chapter 7 ---------------------------------
## p. 78
set.seed(123)
diss <- sim2diss(wish, method = 7)
random.multistart <- function(diss, type = "ordinal", nrep = 100) {
  s1 <- 1
  for (i in 1:nrep) { 
    out <- mds(diss, type=type, init = "random")
    if (out$stress < s1) { 
      object <- out
      s1 <- out$stress 
    }
  }
  return(object)
}
result <- random.multistart(diss, type = "ordinal", nrep = 500)
result


## p. 80, Fig. 7.1
diss <- sim2diss(wish, method = 7)
set.seed(3)
solutions <- icExplore(diss, type = "ordinal", nrep = 75)
solutions
plot(solutions)


## p. 82, Fig. 7.2
diss <- sim2diss(KIPT)
fit1 <- mds(diss, type = "ordinal", eps = 1e-11)
fit2 <- mds(diss, type = "interval")
fit3 <- mds(diss, type = "ratio")
op <- par(mfrow = c(1,2))
plot(fit1, main="Ordinal MDS")
plot(fit2, main="Interval MDS")
par(op)

## p. 83: Fig. 7.3
p <- par(mfrow = c(1,3))
plot(fit1, plot.type="Shepard", main=paste0("Ordinal MDS (Stress1 = ", round(fit1$stress, 2), ")"))
plot(fit2, plot.type="Shepard", main=paste0("Interval MDS (Stress1 = ", round(fit2$stress, 2), ")"))
plot(fit3, plot.type="Shepard", main=paste0("Ratio MDS (Stress1 = ", round(fit3$stress, 2), ")"))
par(op)

## p. 84: Procrustes rotation 
labels.short <- c("interesting","independent","responsibility","meaningful", 
  "advancement","recognition","help others","useful","social","secure job", 
  "income", "spare time", "healthy")
attr(EW_eng$west, "Labels") <- attr(EW_eng$east, "Labels") <- labels.short
res.west <- mds(sim2diss(EW_eng$west, method="corr"), type="ordinal")
res.east <- mds(sim2diss(EW_eng$east, method="corr"), type="ordinal", 
                init=res.west$conf) ##note the init!
fit2 <- Procrustes(res.west$conf, res.east$conf)
plot(fit2)
## compute overall similarity measures: r and c
r <- cor(as.vector(res.west$conf), as.vector(fit2$Yhat))
c <- fit2$congcoef ## Congruence coefficient on distances


## p. 85: Similarity of random MDS solutions
Procrustes.test <- function(n,m,nrep=500) { set.seed(333); c <- vector()
r <- vector(); X <- matrix(runif(n*m, -1, 1), nrow=n, ncol=m)
X <- scale(X, scale=FALSE)
for (i in 1:nrep) { Y <- matrix(runif(n*m, -1, 1), nrow=n, ncol=m)
fit <- Procrustes(X, Y); c[i] <- fit$congcoef
r[i] <- cor(c(X), c(fit$Yhat))}
cr <- list("c"=c, "r"=r) }; z <- Procrustes.test(13,2) 
z99 <- quantile(z$c, .99); r99 <- quantile(z$r, .99) ## 99% quantiles 
cat("c(99%)=", round(z99,2), " r(99%)=", round(r99,2), sep = '')


## p. 88
## This code is precisely equal to the code on p. 87: biplot axes are presented by lines here
diss <- sim2diss(wish, method = 7)
ecdev <- c(3,1,3,3,8,3,7,9,4,7,10,6) ## economic development of country
inhabs <- c(87,17,8,30,51,500,3,100,750,235,201,20) ## number of inhabitants
ex <- mds(diss, type = "ordinal"); ex; plot(ex, label.conf=list(pos=5))
fit1 <- lm(ecdev ~ ex$conf); fit2 <- lm(inhabs ~ ex$conf)
cor(fit1$fitted.values, ecdev)
cor(fit2$fitted.values, inhabs)
abline(a=0, b=fit1$coefficients[3]/fit1$coefficients[2], lty=2, col="red" )
abline(a=0, b=fit2$coefficients[3]/fit2$coefficients[2], lty=2, col="blue" )

## Checking for different local minima (to find Fig. 7.6)
diss <- sim2diss(wish, method = 7)
set.seed(1)
solutions <- icExplore(diss, type = "ordinal", nrep = 75, returnfit=TRUE)
solutions; plot(solutions)
start1 <- solutions$mdsfit[[3]]$conf; start2 <- solutions$mdsfit[[2]]$conf
ecdev <- c(3,1,3,3,8,3,7,9,4,7,10,6) ## economic development of country
inhabs <- c(87,17,8,30,51,500,3,100,750,235,201,20) ## number of inhabitants
## ---------------------------------------------------------------------
op <- par(mfrow=c(1,2))
ex1 <- mds(diss, type = "ordinal", init=start1) ## with solution #3 as start
fit1 <- lm(ecdev ~ ex1$conf); fit2 <- lm(inhabs ~ ex1$conf)
cor(fit1$fitted.values, ecdev); cor(fit2$fitted.values, inhabs)
plot(ex1, main="", label.conf=list(label=TRUE, pos=5, cex=1), xlab="", ylab="",
     xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, frame.plot=TRUE, asp=1, 
     type = "p", pch = 16, cex = 1 )
abline(a=0, b=fit1$coefficients[3]/fit1$coefficients[2], lty=2, col="red" )
abline(a=0, b=fit2$coefficients[3]/fit2$coefficients[2], lty=2, col="blue" )
ex2 <- mds(diss, type = "ordinal", init=start2) ## with solution #2 as start
out <- Procrustes(ex1$conf, ex2$conf) ## fit 2nd solution to 1st solution  
fit1 <- lm(ecdev ~ out$Yhat); fit2 <- lm(inhabs ~ out$Yhat)
cor(fit1$fitted.values, ecdev); cor(fit2$fitted.values, inhabs)
plot(out$Yhat, main="", pch=16, cex=1 , xlim=c(-1,1), ylim=c(-1,1), asp=1, 
     axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
thigmophobe.labels(out$Yhat[,1], out$Yhat[,2], labels=attr(wish, "Labels"), cex=1)
abline(a=0, b=fit1$coefficients[3]/fit1$coefficients[2], lty=2, col="red" )
abline(a=0, b=fit2$coefficients[3]/fit2$coefficients[2], lty=2, col="blue" )
par(op)



## ------------------------------- Chapter 8 ------------------------------------------

## p. 96: 3d example (Fig. 8.1)
diss <- max(PVQ40agg) - PVQ40agg ## Preferences into dissimilarities
## Initial configuration of ten basic personal values: TUV-theory based ---
tuv <- matrix(c(.50,-.87, .71,-.71, .87,-.50, .87,.50, .50,.87, -.50,.87,
    -.71,.71, -.87,.50, -.87,-.50, -.50,-.87) , nrow=10, ncol=2, byrow=TRUE)
s2 <- cbind(tuv, matrix(0, nrow=10, ncol=1)) ## Add column of zeros for 3d start
## Initial configuration of the individuals: random -----------------------
nobs <- dim(PVQ40agg)[1]; set.seed(33)
s1 <- matrix(runif(3*nobs, min=0, max=1), nrow=nobs, ncol=3)
## Compute unfolding solution and rotate to pc plane of value points ------
result <- unfolding(delta=diss, ndim=3, itmax=6000, init=list(s1, s2))
e <- svd(result$conf.col)        ## Find rotation matrix to principal axes
X <- result$conf.col %*% e$v     ## Rotate value points (=X) to principal axes
Y <- result$conf.row %*% e$v;    ## Rotate Y (=persons) to principal axes of X
eV <- sum(X[, 1:2]^2) / sum(X^2) ## expl. var. of X in values pc plane
## Plot 3d unfolding solution --------------------------------------------
require(scatterplot3d)
lim1 <- c(-3,+3) 
s3d <- scatterplot3d(X, type="h", xlab="Dimension 1", ylab="Dimension 2",
                     zlab="Dimension 3", xlim=lim1, ylim=lim1, zlim=lim1,  
                     cex.symbols=2,color="red", pch=21, bg="red", asp = 1)
text(s3d$xyz.convert(X), labels=rownames(X), pos = 2)
X.lines <-  rbind(X, X[1, ])
s3d$points3d(x = X.lines[, 1], y = X.lines[, 2], z= X.lines[, 3], type = "l", col = "blue", lty = 2, lwd = 2) 
s3d$points3d(x = Y[, 1], y = Y[, 2], z = Y[, 3], pch = 21, bg = grey(0.1, alpha = .4),  
             col = grey(0.1, alpha = .6), xlab = "", ylab = "" ); 
plot(fit, plot.type = "stressplot" ) ## plot SPPs persons/values
result; permtest(result)             ## Print result and do permutation test


## p. 96: plot 3d unfolding solution through rgl with 3d rotation
require(rgl)
options(rgl.useNULL = TRUE) # Switch off Quartz on Mac OS
options(rgl.printRglwidget = TRUE) # Plot in regular R-Studio Viewer
#rgl.init()
plot3d(X, size=10, col = "red", xlab = "Dimension 1", ylab = "Dimension 2",
       zlab = "Dimension 3", xlim = c(-3, 3), ylim = c(-3, 3), zlim = c(-3, 3))
aspect3d("iso")
text3d(x = X[, 1], y = X[, 2], z= X[, 3], text=rownames(X), adj=1.2)
X.lines <-  rbind(X, X[1, ])
lines3d(x = X.lines[, 1], y = X.lines[, 2], z= X.lines[, 3],  col = "blue", lty = 2, lwd = 2) 
points3d(x = Y[, 1], y = Y[, 2], z= Y[, 3],  col = "grey", lty = 2, lwd = 2) 


## p. 98: first 6 lines copied from code of p. 24; gives diss and uses PVQ40agg
R      <- cor(PVQ40agg); 
diss   <- sim2diss(R)
result <- mds(diss, init = "torgerson", type = "ratio")
plot(result)
erg    <- fitCircle(result$conf[,1], result$conf[,2])
draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "black", lty = 2)


## p. 96: This code does not completely reconstruct solutions in Fig. 8.1 and 8.3
##        probably due to local minima.
nobs <- dim(PVQ40agg)[1]
set.seed(33) ## sets a seed for the random number generators 
## Initial configuration of the items
s2 <- matrix(c(-0.34,-0.94,-0.87,-0.50,-1.00, 0.00,-0.77, 0.64,-0.26, 0.97,
                0.50, 0.87, 0.94, 0.34, 0.87,-0.50, 0.72,-0.72, 0.50,-0.87,
                runif(10, min=0, max=1)), nrow=10, ncol=3, byrow=FALSE)
## Initial configuration of the individuals
s1 <- matrix(runif(3*nobs, min=0, max=1), nrow=nobs, ncol=3) 
pref <- max(PVQ40agg) - PVQ40agg  ## Preferences into Dissimilarities 
result <- smacofRect(delta=pref, ndim=3, itmax=6000, init = list(s1, s2))
result; permtest(result)
e <- svd(result$conf.col)
X <- e$u %*% diag(e$d); 
X <- result$conf.col %*% e$v  # Rotation to principal axes
eV <- sum(X[, 1:2]^2) / sum(X^2) # expl.var. in value plane
Y <- result$conf.row %*% e$v; # Rotation to principal axes


## Test p. 101: Section 8.5. unfolding with transformations
## Compute ratio unfolding for a start configuration
res <- unfolding(delta=max(PVQ40agg) - PVQ40agg, type="ratio",
                 ndim=2, itmax=10000, omega=0, verbose=TRUE)
## Initial configuration of the individuals
resunf <- unfolding(delta = max(PVQ40agg) - PVQ40agg, type="mspline", 
                    conditionality="row", ndim=2, itmax=10000, 
                    omega=0.1, spline.intKnots=1, verbose=TRUE,
                    init=list(res$conf.row, res$conf.col))
plot(resunf, plot.type="Shepard")
plot(resunf, plot.type="confplot")

## p. 101: Do weighted unfolding (weights are variances of pref per row)  
pref <- max(PVQ40agg) - PVQ40agg
nper <- dim(pref)[1]
var.per.row <- apply(pref, 1, var)
W <- matrix(var.per.row, nrow = nper, ncol = ncol(pref)) 
W[W==0] <- min( W[W!=min(W)] ) ## avoiding zero rows
out <- unfolding(pref, weightmat = W)

## p. 102, Fig. 8.4
vmu <- function (P, ndim = 2, center = TRUE, scale = FALSE) { 
  ## P is an N (persons) by m (objects) matrix of similarities (preferences)
  m <- dim(P)[2]
  S <- svd(t(scale(t(P), center = center, scale = scale)))
  X <- m^( 1/2) * S$v                 ## X = objects
  Y <- m^(-1/2) * S$u %*% diag(S$d)   ## Y = persons
  row.names(X) <- colnames(P)
  row.names(Y) <- rownames(P)
  return(list(X = X[, 1:ndim], Y = Y[, 1:ndim], VAF = sum(S$d[1:ndim]^2)/sum(S$d^2), d = S$d)) 
} 
res <- vmu(PVQ40agg)
plot(1.2 * res$X[, 1], 1.2 * res$X[, 2], type = "n", asp = 1, xlab = "", ylab = "")
abline(0, res$Y[133,2]/res$Y[133,1], col = "gray60")
abline(h = 0, v = 0, col = "gray60", lty = 2)
arrows(rep(0, nrow(res$Y)), rep(0, nrow(res$Y)), res$Y[, 1], res$Y[, 2], col = "red", length = 0.1)
text(res$X[, 1], res$X[, 2], rownames(res$X), cex = 1.5)
text(res$Y[133,1], res$Y[133,2], labels="133")

round(PVQ40agg[133,], 2)   ## PVQ40agg for person 133
round(PVQ40agg[133,] - mean(PVQ40agg[133,]), 2)   ## PVQ40agg minus mean values for person 133
round(t(res$X %*% res$Y[133,]), 2)   ## VMU reconstructed values for person 133
round(t(res$X %*% res$Y[133,]/(sum(res$Y[133,]^2)^.5)), 2)   ## Projected values for person 133




## -------------------------------------- Chapter 10: software ------------------------------------
wish.new <- sim2diss(wish, method = 7)  
wish.new
res.wish <- mds(wish.new, type = "ordinal")
res.wish
names(res.wish)
res.wish$confdist
round(res.wish$confdist, 2)
plot(res.wish)
plot(res.wish, plot.type = "Shepard")
plot(res.wish, plot.type = "bubbleplot")
plot(res.wish, plot.type = "stressplot")
