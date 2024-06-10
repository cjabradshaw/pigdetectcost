## pig detection vs. cost model
## Corey Bradshaw
## May 2024

## libraries
library(truncnorm)
library(plot.matrix)
library(reshape2)
library(adehabitatHR)

# functions
redim_matrix <- function(
    mat,
    target_height = 100,
    target_width = 100,
    summary_func = function(x) mean(x, na.rm = TRUE),
    output_type = 0.0, #vapply style
    n_core = 1 # parallel processing
) {
  if(target_height > nrow(mat) | target_width > ncol(mat)) {
    stop("Input matrix must be bigger than target width and height.")
  }
  seq_height <- round(seq(1, nrow(mat), length.out = target_height + 1))
  seq_width  <- round(seq(1, ncol(mat), length.out = target_width  + 1))
  # complicated way to write a double for loop
  do.call(rbind, parallel::mclapply(seq_len(target_height), function(i) { # i is row
    vapply(seq_len(target_width), function(j) { # j is column
      summary_func(
        mat[
          seq(seq_height[i], seq_height[i + 1]),
          seq(seq_width[j] , seq_width[j + 1] )
        ]
      )
    }, output_type)
  }, mc.cores = n_core))
}

## Kangaroo Island parameters
A.KI <- 4405 # area of KI (km2)
search.prop.KI <- 1/3
KI.search.area <- A.KI * search.prop.KI


## cost parameters by method

######################################################
## Cost of detection dogs (based on 2-week operation)	
######################################################
mop <- 400 # Management and operational planning (10 days work)	400	day
trav <- 75 # Travel cost for detection work per day	75	day
travoth <- 200 # Fuel/ferry/travel to KI/labour to and from KI (from Canberra)	200	day
handl2dog <- 1300 # Daily rate of handler with 2 dogs	1300	day
pirsa.sal <- 350 # PIRSA staff accompanying handler (salary)	350	day
handl.accom.food <- 100 # accommodation and food for handler	100	day
pirsa.car.accom.food <- 100 # PIRSA staff car, food and accommodation	100	day
tot.dog.cost.day <- sum(c(mop, trav, travoth, handl2dog, pirsa.sal, handl.accom.food, pirsa.car.accom.food)) # Total (daily cost)	2525
dog.hrspday <- 10
dog.dayspwk <- 5
tot.dog.cost.hr <- tot.dog.cost.day/dog.hrspday # Hourly cost 253	
dog.area.cov.phr <- 3*0.5 # Area covered per hour (square km)	3 km x 0.5 km
dog.area.cov.pmo <- dog.area.cov.phr*dog.hrspday*dog.dayspwk*4
dog.detect.pr <- 0.5 # Detection probability if dogs walk along same creek as pigs 50%

##########################################################
## Cost of thermal helicopter (based on 2-week operation)
##########################################################
preopplan <- 800 # PIRSA pre-operation planning  (based on 20 days staff time)	800	day
opplan <- 300 # PIRSA daily operational planning and management	300	day
heli <- 13500 # Helicopter fees ($3700 x 3.65 h, includes accommodation and travel)	13500	day
pil.cam <- 200 # Food for pilot and camera operator	200	day
tot.heli.cost.day <- sum(c(preopplan,opplan,heli,pil.cam)) # Total (daily cost)	14800	
heli.hrspday <- 3.65
heli.dayspwk <- 5
tot.heli.cost.hr <- tot.heli.cost.day/heli.hrspday # Hourly cost 	4055	
heli.area.cov.phr <- 45*.150 # area covered per hour (square km)	45km/h x 150m
heli.area.cov.pmo <- heli.area.cov.phr*heli.hrspday*heli.dayspwek*4
heli.detect.pr <- 0.10 # Detection probability if heli flies over pigs 10%

####################################################
## Cost of managing cameras (based on >500 cameras)
####################################################
no.cams <- 500
camera <- 55000/100 # Camera/battery/solar/picket/SD	600	one-off per camera
caminstal <- 0 # Installation cost 70	one-off per camera
camsubscr <- 150000/300 # Subscriptions: AI system + mobile plan	500	per year per camera
cammaint <- 36000/500 # Maintenance (labour and travel, $360k for 500 camera)	720	per year per camera
camrem <- 0 # Removal of camera	70	one-off per camera
cammgmt <- 0 # Management	120	per year per camera
tot.pcam.cost <- sum(c(camera, caminstal, camrem))
tot.pcam.maint.cost.pyr <- sum(c(camsubscr, cammaint, cammgmt))
tot.pcam.maint.pday <- tot.pcam.maint.cost.pyr/365 # Daily cost of each camera
tot.pcam.maint.phr <- tot.pcam.maint.pday/24 # Hourly cost of each camera ($/h)
cam.area.cov.phr <- 130/1000000 # Area covered per camera per hour (square km) 0.0001300	60 deg FoV, 15m detectability
cam.detect.pr <- 1 # Detection probability if pig walks into detection zone of camera	90%

##########################################################
## Cost of eDNA (based on 1 week sampling trip, 25 sites)
##########################################################
kits.psite <- 35 # Sample kits (per site)	35	
edna.staff <- 84 # Staff cost sampling (per site)	84	per site
edna.trav <- 80 # Ferry travel, accommodation, food and vehicle cost (per site)	80	
edna.reag <- 20 # Reagents for qPCR (per sample, triplicate for each site)	20	
edna.lab <- 40 # PIRSA laboratory time (per sample, triplicate for each site)	40	
tot.edna <- sum(c(kits.psite,edna.staff,edna.trav, edna.reag, edna.lab)) # Total cost (per site)	259	
tot.edna.ptrip <- tot.edna*25 # Total cost of field and lab work (per trip)	6475	
edna.area.cov.psite <- 10000/1000000 # area covered per site (square km)	0.0100000	
edna.detect.pr <- 0.8 # Detection probability if samples taken from same stream as pigs	80%

##########################
## Cost of ground surveys
##########################
survey.labour <- 324000 # labour for 3 staff and 3 cars (90% of 360k per year)
survey.labour.pp.pyr <- survey.labour/3
survey.hrspday <- 7.5
survey.dayspwk <- 5
survey.labour.pp.phr <- survey.labour.pp.pyr/48/survey.dayspwk/survey.hrspday # hourly cost of each person ($/h) (work 7.5 hours/5 days a week/48 weeks a year)
survey.labour.pp.pmo <- survey.labour.pp.phr*survey.hrspday*survey.dayspwk*4
survey.area.cov.pp.phr <- 1 # area covered per person per hour (square km)
survey.area.cov.pp.pmo <- survey.area.cov.pp.phr*survey.hrspday*survey.dayspwk*4 # area covered per person per month (square km)
survey.detect.pr <- 0.9 # detection probability if pig in same area 90%


## pig movement coverage (Kay et al. Move Ecol 2017 doi:10.1186/s40462-017-0105-1)
dist.max.avg.pday.lo <- 0.8 # lower average max dist moved/day (km)
dist.max.avg.pday.up <- 2.12 # upper average max dist moved/day (km)
dist.max.pday.sd.lo <- 0.51 # lower SD max dist moved/day (km)
dist.max.pday.sd.up <- 0.8 # upper SD max dist moved/day (km)
dist.avg.phr.lo <- 0.35 # lower avg dist moved/hr (km)
dist.avg.phr.up <- 0.42 # upper avg dist moved/hr (km)
dist.avg.phr.mn <- mean(c(dist.avg.phr.lo,dist.avg.phr.up))
hrge.avg.phr.mn <- pi*dist.avg.phr.mn^2
dist.sd.phr.lo <- 0.34 # lower sd dist moved/hr (km)
dist.sd.phr.up <- 0.51 # upper sd dist moved/hr (km)
hrge.mo.mn <- 3.4 # average monthly home range size (MCP method) (km2)
hrge.mo.sd <- 4.6 # sd monthly home range size (MCP method) (km2)
hrge.mo.md <- 1.8 # median monthly home range size (MCP method) (km2)




## cost scenarios
A.KI <- 4405 # area of KI (km2)
search.prop.KI <- 1/3
KI.search.area <- A.KI * search.prop.KI

## dogs
pigs.vec <- c(seq(1,10,1), seq(15,100,5))
pigs.lab <- paste("pigs",pigs.vec,sep="")
pigs.prop.tosearch <- (KI.search.area - (hrge.mo.mn*pigs.vec))/KI.search.area

dog.search.tot.hrs <- (1/dog.detect.pr)*(pigs.prop.tosearch*KI.search.area)/dog.area.cov.phr # hours required to search entire KI.search.area
dog.costs <- round(tot.dog.cost.hr*dog.search.tot.hrs,0)
plot(pigs.vec, dog.costs, type='l', xlab="# pigs", ylab="cost ($)")

prob.detect.pig <- dog.detect.pr * (hrge.mo.mn*pigs.vec)/(dog.area.cov.pmo)
dog.teams.vec <- seq(0,1.77,0.02)
dog.teams.lab <- paste("dogteams",dog.teams.vec,sep="")
costs.pdogteam.pmo <- tot.dog.cost.hr*dog.teams.vec*dog.hrspday*dog.dayspwk*4
dogs.pr.mat <- matrix(data=NA, ncol=length(dog.teams.vec), nrow=length(pigs.vec))

for (p in 1:length(pigs.vec)) {
  for (d in 1:length(costs.pdogteam.pmo)) {
    dogs.pr.mat[p,d] <- (dog.teams.vec[d]*prob.detect.pig[p])
  }
}
colnames(dogs.pr.mat) <- costs.pdogteam.pmo
rownames(dogs.pr.mat) <- pigs.vec
plot(dogs.pr.mat, xlab="cost ($)", ylab="pigs", main="")

# standardise to max pr = 1
dogs.max.pr <- range(dogs.pr.mat, na.rm=T)[2]
dogs.max.pr
dogs.pr.mat.st <- dogs.pr.mat/dogs.max.pr
colnames(dogs.pr.mat.st) <- costs.pdogteam.pmo/dogs.max.pr

setwd("/Users/brad0317/Documents/Papers/Invasive species/Pigs/KI pig detection/out")
write.table(dogs.pr.mat.st, "dogscostppigpr.csv", sep=",", row.names = T, col.names = T)


# helicopters
heli.search.tot.hrs <- (1/heli.detect.pr)*(pigs.prop.tosearch*KI.search.area)/heli.area.cov.phr # hours required to search entire KI.search.area
heli.costs <- round(heli.area.cov.phr*heli.search.tot.hrs,0)
plot(pigs.vec, heli.costs, type='l', xlab="# pigs", ylab="cost ($)")

prob.detect.pig <- heli.detect.pr * (hrge.mo.mn*pigs.vec)/(heli.area.cov.pmo)
helidays.vec <- seq(1,14.35,0.1)
helidays.lab <- paste("helidays",helidays.vec,sep="")
costs.heli.pmo <- tot.heli.cost.hr*helidays.vec*heli.hrspday*heli.dayspwk*4
heli.pr.mat <- matrix(data=NA, ncol=length(helidays.vec), nrow=length(pigs.vec))

for (p in 1:length(pigs.vec)) {
  for (h in 1:length(costs.heli.pmo)) {
    heli.pr.mat[p,h] <- (helidays.vec[h]*prob.detect.pig[p])
  }
}
colnames(heli.pr.mat) <- costs.heli.pmo
rownames(heli.pr.mat) <- pigs.vec
plot(heli.pr.mat, xlab="cost ($)", ylab="pigs", main="")

# standardise to max pr = 1
heli.max.pr <- range(heli.pr.mat, na.rm=T)[2]
heli.max.pr
heli.pr.mat.st <- heli.pr.mat/heli.max.pr
colnames(heli.pr.mat.st) <- costs.heli.pmo/heli.max.pr

setwd("/Users/brad0317/Documents/Papers/Invasive species/Pigs/KI pig detection/out")
write.table(heli.pr.mat.st, "helicostppigpr.csv", sep=",", row.names = T, col.names = T)

# cameras
tot.pcam.cost <- sum(c(camera, caminstal, camrem))
tot.pcam.maint.cost.pyr <- sum(c(camsubscr, cammaint, cammgmt))
tot.pcam.maint.pday <- tot.pcam.maint.cost.pyr/365 # Daily cost of each camera
tot.pcam.maint.phr <- tot.pcam.maint.pday/24 # hourly cost of each camera ($/h)

KI.search.area/cam.area.cov.phr # cameras to cover total search area

prob.see.pig.mo <- cam.detect.pr * (hrge.mo.mn*pigs.vec)/KI.search.area * ((cam.area.cov.phr)/hrge.mo.mn) # prob any given site will detect pig in 1 month
cams.vec <- seq(1,114000,2000)
cams.lab <- paste("cams",cams.vec,sep="")
costs.cam.pmo <- (cams.vec*tot.pcam.cost) + (cams.vec*tot.pcam.maint.pday*30)
costs.cam.disc.pmo <- costs.cam.pmo - 500*camera # subtract cost for 500 cameras already purchased

cams.pr.mat <- matrix(data=NA, ncol=length(cams.vec), nrow=length(pigs.vec))
for (p in 1:length(pigs.vec)) {
  for (c in 1:length(costs.cam.disc.pmo)) {
    cams.pr.mat[p,c] <- (cams.vec[c]*prob.see.pig.mo[p])
  }
}
colnames(cams.pr.mat) <- costs.cam.disc.pmo
rownames(cams.pr.mat) <- pigs.vec
plot(cams.pr.mat, xlab="cost ($)", ylab="pigs", main="")

# standardise to max pr = 1
cams.max.pr <- range(cams.pr.mat, na.rm=T)[2]
cams.max.pr
cams.pr.mat.st <- cams.pr.mat/cams.max.pr
colnames(cams.pr.mat.st) <- costs.cam.disc.pmo/cams.max.pr

setwd("/Users/brad0317/Documents/Papers/Invasive species/Pigs/KI pig detection/out")
write.table(cams.pr.mat.st, "camscostppigpr.csv", sep=",", row.names = T, col.names = T)


# eDNA
edna.search.tot <- (1/edna.detect.pr)*(pigs.prop.tosearch*KI.search.area)/edna.area.cov.psite
edna.costs <- round(tot.edna.ptrip*edna.search.tot,0)
plot(pigs.vec, heli.costs, type='l', xlab="# pigs", ylab="cost ($)")

prob.detect.pig <- edna.detect.pr * (hrge.mo.mn*pigs.vec)/KI.search.area * (edna.area.cov.psite/hrge.mo.mn) # prob any given site will detect pig in 1 month
trips.vec <- seq(1,1850,50)
trips.lab <- paste("trips",trips.vec,sep="")
costs.edna.pmo <- tot.edna.ptrip*trips.vec

edna.pr.mat <- matrix(data=NA, ncol=length(trips.vec), nrow=length(pigs.vec))
for (p in 1:length(pigs.vec)) {
  for (r in 1:length(costs.edna.pmo)) {
    edna.pr.mat[p,r] <- (trips.vec[r]*prob.detect.pig[p])
  }
}
colnames(edna.pr.mat) <- costs.edna.pmo
rownames(edna.pr.mat) <- pigs.vec
plot(edna.pr.mat, xlab="cost ($)", ylab="pigs", main="")

# standardise to max pr = 1
edna.max.pr <- range(edna.pr.mat, na.rm=T)[2]
edna.max.pr
edna.pr.mat.st <- edna.pr.mat/edna.max.pr
colnames(edna.pr.mat.st) <- costs.edna.pmo/edna.max.pr

setwd("/Users/brad0317/Documents/Papers/Invasive species/Pigs/KI pig detection/out")
write.table(edna.pr.mat.st, "ednacostppigpr.csv", sep=",", row.names = T, col.names = T)

# ground surveys
survey.search.tot.hrs <- (1/survey.detect.pr)*(pigs.prop.tosearch*KI.search.area)/survey.area.cov.pp.phr # hours required to search entire KI.search.area
survey.costs <- round(survey.labour.pp.phr*survey.search.tot.hrs,0)
plot(pigs.vec, survey.costs, type='l', xlab="# pigs", ylab="cost ($)")

prob.detect.pig <- survey.detect.pr * (hrge.mo.mn*pigs.vec)/(survey.area.cov.pp.pmo)
survey.hrs.vec <- seq(0,0.49,0.01)
survey.hrs.lab <- paste("survey hrs",survey.hrs.vec,sep="")

costs.pp.pmo <- survey.labour.pp.phr*survey.hrs.vec*survey.dayspwk*survey.hrspday*4
survey.pr.mat <- matrix(data=NA, ncol=length(survey.hrs.vec), nrow=length(pigs.vec))

for (p in 1:length(pigs.vec)) {
  for (d in 1:length(costs.pp.pmo)) {
    survey.pr.mat[p,d] <- (survey.hrs.vec[d]*prob.detect.pig[p])
  }
}
colnames(survey.pr.mat) <- costs.pp.pmo
rownames(survey.pr.mat) <- pigs.vec
plot(survey.pr.mat, xlab="cost ($)", ylab="pigs", main="")

# standardise to max pr = 1
survey.max.pr <- range(survey.pr.mat, na.rm=T)[2]
survey.max.pr
survey.pr.mat.st <- survey.pr.mat/survey.max.pr
colnames(survey.pr.mat.st) <- costs.pp.pmo/survey.max.pr

setwd("/Users/brad0317/Documents/Papers/Invasive species/Pigs/KI pig detection/out")
write.table(survey.pr.mat.st, "surveycostppigpr.csv", sep=",", row.names = T, col.names = T)


# relative costs

dogs.max.cost <- as.numeric(colnames(dogs.pr.mat.st)[dim(dogs.pr.mat.st)][2])
heli.max.cost <- as.numeric(colnames(heli.pr.mat.st)[dim(heli.pr.mat.st)][2])
cams.max.cost <- as.numeric(colnames(cams.pr.mat.st)[dim(cams.pr.mat.st)][2])
edna.max.cost <- as.numeric(colnames(edna.pr.mat.st)[dim(edna.pr.mat.st)][2])
survey.max.cost <- as.numeric(colnames(survey.pr.mat.st)[dim(survey.pr.mat.st)][2])

max.cost.vec <- c(dogs.max.cost,heli.max.cost,cams.max.cost,edna.max.cost,survey.max.cost)
max.cost.vec[which(max.cost.vec == max(max.cost.vec))]
min.sub <- which(max.cost.vec == min(max.cost.vec))
max.cost.vec[min.sub]

rel.cost.2survey <- max.cost.vec/max.cost.vec[min.sub]
type.lab <- c("dogs", "heli", "eDNA", "cameras", "survey")
rel.cost.type.dat <- data.frame("relcost"=rel.cost.2survey, "method"=type.lab)
rel.cost.type.dat


## translate costs to units
# dogs
(dog.teams.vec*dog.hrspday*dog.dayspwk*4)[87]
costs.pdogteam.pmo[87]

# helicopters
(helidays.vec*heli.hrspday*heli.dayspwk*4)[122]
costs.heli.pmo[122]

# cameras
cams.vec[57]
costs.cam.disc.pmo[57]

# edna
trips.vec[37]
costs.edna.pmo[37]

# surveys
(survey.hrs.vec*5*7.5*4)[43]
costs.pp.pmo[43]






##############################
## stochastic cost scenarios

# deterministic parameters
# pigs
pigs.vec <- c(seq(1,10,1), seq(15,100,5))
pigs.lab <- paste("pigs",pigs.vec,sep="")

# dogs
dog.search.tot.hrs <- (1/dog.detect.pr)*(pigs.prop.tosearch*KI.search.area)/dog.area.cov.phr # hours required to search entire KI.search.area
dog.costs <- round(tot.dog.cost.hr*dog.search.tot.hrs,0)
dog.teams.vec <- seq(1,19,1)
dog.teams.lab <- paste("dogteams",dog.teams.vec,sep="")
costs.pdogteam.pmo <- tot.dog.cost.hr*dog.teams.vec*30*dog.hrspday

# helicopters
heli.search.tot.hrs <- (1/heli.detect.pr)*(pigs.prop.tosearch*KI.search.area)/heli.area.cov.phr # hours required to search entire KI.search.area
heli.costs <- round(heli.area.cov.phr*heli.search.tot.hrs,0)
helidays.vec <- seq(1,19,1)
helidays.lab <- paste("helidays",helidays.vec,sep="")
costs.heli.pmo <- tot.heli.cost.hr*helidays.vec*heli.hrspday

# cameras
tot.pcam.cost <- sum(c(camera, caminstal, camrem))
tot.pcam.maint.cost.pyr <- sum(c(camsubscr, cammaint, cammgmt))
tot.pcam.maint.pday <- tot.pcam.maint.cost.pyr/365 # Daily cost of each camera
tot.pcam.maint.phr <- tot.pcam.maint.pday/24 # Hourly cost of each camera ($/h)
cams.vec <- seq(1,120000,2000)
cams.lab <- paste("cams",cams.vec,sep="")
costs.cam.pmo <- (cams.vec*tot.pcam.cost) + (cams.vec*tot.pcam.maint.pday*30)

# eDNA
edna.search.tot <- (1/edna.detect.pr)*(pigs.prop.tosearch*KI.search.area)/edna.area.cov.psite
edna.costs <- round(tot.edna.ptrip*edna.search.tot,0)
trips.vec <- seq(1,1850,50)
trips.lab <- paste("trips",trips.vec,sep="")
costs.edna.pmo <- tot.edna.ptrip*trips.vec


stoch.iter <- 1000
stoch.itdiv <- stoch.iter/10
dogs.arr <- array(data=NA, dim=c(length(pigs.vec), length(dog.teams.vec), stoch.iter))
heli.arr <- array(data=NA, dim=c(length(pigs.vec), length(helidays.vec), stoch.iter))
cams.arr <- array(data=NA, dim=c(length(pigs.vec), length(cams.vec), stoch.iter))
edna.arr <- array(data=NA, dim=c(length(pigs.vec), length(trips.vec), stoch.iter))
  
for (s in 1:stoch.iter) {
  # pigs
  hrge.mo.it <- exp(rtruncnorm(1, a=0, b=Inf, log(hrge.mo.mn), log(hrge.mo.md))) # in km2
  pigs.prop.tosearch <- (KI.search.area - (hrge.mo.it*pigs.vec))/KI.search.area
  
  ## dogs
  prob.detect.pig <- dog.detect.pr * (hrge.mo.it*pigs.vec)/KI.search.area * (dog.area.cov.phr/hrge.mo.it)
  dogs.pr.mat <- matrix(data=NA, ncol=length(dog.teams.vec), nrow=length(pigs.vec))
  for (p in 1:length(pigs.vec)) {
    for (d in 1:length(costs.pdogteam.pmo)) {
      dogs.pr.mat[p,d] <- (dog.teams.vec[d]*prob.detect.pig[p])
    }
  }
  colnames(dogs.pr.mat) <- costs.pdogteam.pmo
  rownames(dogs.pr.mat) <- pigs.vec
  
  dogs.arr[,,s] <- dogs.pr.mat
  
  # helicopters
  prob.detect.pig <- heli.detect.pr * (hrge.mo.it*pigs.vec)/KI.search.area * (heli.area.cov.phr/hrge.mo.it)
  heli.pr.mat <- matrix(data=NA, ncol=length(helidays.vec), nrow=length(pigs.vec))
  
  for (p in 1:length(pigs.vec)) {
    for (h in 1:length(costs.heli.pmo)) {
      heli.pr.mat[p,h] <- (dog.teams.vec[h]*prob.detect.pig[p])
    }
  }
  colnames(heli.pr.mat) <- costs.heli.pmo
  rownames(heli.pr.mat) <- pigs.vec

  heli.arr[,,s] <- heli.pr.mat
  
  # cameras
  prob.see.pig.mo <- cam.detect.pr * (hrge.mo.it*pigs.vec)/KI.search.area * ((cam.area.cov.phr)/hrge.mo.it)
  cams.pr.mat <- matrix(data=NA, ncol=length(cams.vec), nrow=length(pigs.vec))
  for (p in 1:length(pigs.vec)) {
    for (c in 1:length(costs.cam.pmo)) {
      cams.pr.mat[p,c] <- (cams.vec[c]*prob.see.pig.mo[p])
    }
  }
  colnames(cams.pr.mat) <- costs.cam.pmo
  rownames(cams.pr.mat) <- pigs.vec
  
  cams.arr[,,s] <- cams.pr.mat
  
  # eDNA
  prob.detect.pig <- edna.detect.pr * (hrge.mo.it*pigs.vec)/KI.search.area * (edna.area.cov.psite/hrge.mo.it)
  edna.pr.mat <- matrix(data=NA, ncol=length(trips.vec), nrow=length(pigs.vec))
  for (p in 1:length(pigs.vec)) {
    for (r in 1:length(costs.edna.pmo)) {
      edna.pr.mat[p,r] <- (trips.vec[r]*prob.detect.pig[p])
    }
  }
  colnames(edna.pr.mat) <- costs.edna.pmo
  rownames(edna.pr.mat) <- pigs.vec
  
  edna.arr[,,s] <- edna.pr.mat
  
  if (s %% stoch.itdiv==0) print(s) 
}

## mean probs
# dogs
dogs.pr.mat.mn <- apply(dogs.arr, MARGIN=c(1,2), mean, na.rm=T)
dogs.pr.mat.mn.st <- dogs.pr.mat.mn/max(dogs.pr.mat.mn)
costs.pdogteam.pmo.st <- costs.pdogteam.pmo/max(dogs.pr.mat.mn)
colnames(dogs.pr.mat.mn.st) <- costs.pdogteam.pmo.st

# heli
heli.pr.mat.mn <- apply(heli.arr, MARGIN=c(1,2), mean, na.rm=T)
heli.pr.mat.mn.st <- heli.pr.mat.mn/max(heli.pr.mat.mn)
costs.heli.pmo.st <- costs.heli.pmo/max(heli.pr.mat.mn)
colnames(heli.pr.mat.mn.st) <- costs.heli.pmo.st

# cameras
cams.pr.mat.mn <- apply(cams.arr, MARGIN=c(1,2), mean, na.rm=T)
cams.pr.mat.mn.st <- cams.pr.mat.mn/max(cams.pr.mat.mn)
costs.heli.pmo.st <- costs.cam.pmo/max(cams.pr.mat.mn)
colnames(cams.pr.mat.mn.st) <- costs.heli.pmo.st

# eDNA
edna.pr.mat.mn <- apply(edna.arr, MARGIN=c(1,2), mean, na.rm=T)
edna.pr.mat.mn.st <- edna.pr.mat.mn/max(edna.pr.mat.mn)
costs.edna.pmo.st <- costs.edna.pmo/max(edna.pr.mat.mn)
colnames(edna.pr.mat.mn.st) <- costs.edna.pmo.st

setwd("/Users/brad0317/Documents/Papers/Invasive species/Pigs/KI pig detection/out")
write.table(dogs.pr.mat.mn.st, "dogscostppigprstoch.csv", sep=",", row.names = T, col.names = T)
write.table(heli.pr.mat.mn.st, "helicostppigprstoch.csv", sep=",", row.names = T, col.names = T)
write.table(cams.pr.mat.mn.st, "camscostppigprstoch.csv", sep=",", row.names = T, col.names = T)
write.table(edna.pr.mat.mn.st, "ednacostppigprstoch.csv", sep=",", row.names = T, col.names = T)



## create coverage grid to estimate area visited per hour and per day with average daily movements
siter <- 100
visited.km2.ph.vec <- rep(NA, siter)
for (s in 1:siter) {
  mat.dim <- 10000 # metres x 10 (to facilitate processing)
  base.mat <- matrix(data=0, ncol=mat.dim, nrow=mat.dim)
  st.r <- round(mat.dim/2, 0)
  st.c <- round(mat.dim/2, 0)
  
  n.time.steps <- 24*30 # hours (1 month)
  itdiv <- n.time.steps/10
  Moore <- seq(1,8,1)
  Moore.comp <- c("NW", "N", "NE", "E", "SE", "S", "SW", "W")
  Moore.dr <- c(-1,-1,-1,0,1,1,1,0)
  Moore.dc <- c(-1,0,1,1,1,0,-1,-1)
  Moore.dir <- data.frame("Moore"=Moore, "dir"=Moore.comp, "dr"=Moore.dr, "dc"=Moore.dc)
  run.mat <- base.mat
  run.mat[st.r,st.c] <- 1
  curr.r <- dest.r <- st.r
  curr.c <- dest.c <- st.c
  
  for (t in 1:n.time.steps) {
    
    # set to current cell
    curr.r <- dest.r
    curr.c <- dest.c
    
    # choose Moore-neighbourhood direction (NW = 1 ... W = 8)
    direct <- Moore.dir[sample(Moore,1, replace=F),]
    
    # sample distance moved/hr
    dist.mn <- runif(1, min=dist.avg.phr.lo, max=dist.avg.phr.up)
    dist.sd <- runif(1, min=dist.sd.phr.lo, max=dist.sd.phr.up)
    dist.it <- exp(rtruncnorm(1, a=0, b=Inf, log(dist.mn), log(dist.sd))) # in km
    
    # number of sq m cells visited in this time step
    dist.m.it <- round(dist.it*1000, 0)
    
    # Euclidian number of cells visited in this time step
    if (direct$dir == "N" | direct$dir == "E" | direct$dir == "S" | direct$dir == "W") {
      cells.it <- dist.m.it
    }
    if (direct$dir == "NW" | direct$dir == "NE" | direct$dir == "SE" | direct$dir == "SW") {
      cells.it <- round(sqrt(dist.m.it^2 - (dist.m.it/2)^2), 0)
    }
    
    # path & destination cells visited
    if (direct$dir == "N" | direct$dir == "E" | direct$dir == "S" | direct$dir == "W") {
      mov.row <- direct$dr * cells.it
      mov.col <- direct$dc * cells.it
    }
    if (direct$dir == "NW" | direct$dir == "NE" | direct$dir == "SE" | direct$dir == "SW") {
      drc <- sqrt(cells.it^2 - (cells.it/2)^2)
      mov.row <- round(direct$dr * drc, 0)
      mov.col <- round(direct$dc * drc, 0)
    }
    
    # destination cell
    dest.r <- curr.r + mov.row
    dest.c <- curr.c + mov.col
    
    # determine if destination cell is beyond matrix boundary
    # if so, resample new destimation cell
    dest.r <- ifelse(dest.r > mat.dim, st.r, dest.r)
    dest.r <- ifelse(dest.r < 1, st.r, dest.r)
    dest.c <- ifelse(dest.c > mat.dim, st.c, dest.c)
    dest.c <- ifelse(dest.c < 1, st.c, dest.c)
    
    # fill in destination cell with presence
    run.mat[dest.r, dest.c] <- run.mat[dest.r, dest.c] + 1
    
    # fill in pathway with presences
    if (direct$dir == "N" | direct$dir == "E" | direct$dir == "S" | direct$dir == "W") {
      spath.r <- curr.r:dest.r
      if (length(spath.r)==1) {
        spath.r <- curr.r
      } # end if
      spath.c <- curr.c:dest.c
      if (length(spath.c)==1) {
        spath.c <- curr.c
      }
      straight.vec <- run.mat[spath.r, spath.c]
      if(length(straight.vec)==1) {
        straight.vec <- straight.vec - 1
      }
      if(length(straight.vec)>=2) {
        straight.vec[1] <- straight.vec[1] - 1
        straight.vec[length(straight.vec)] <- straight.vec[length(straight.vec)] - 1
      }
      run.mat[spath.r, spath.c] <- straight.vec + 1
    } # end if
    
    if (direct$dir == "NW" | direct$dir == "NE" | direct$dir == "SE" | direct$dir == "SW") {
      min.r <- min(c(curr.r, dest.r))
      max.r <- max(c(curr.r, dest.r))
      min.c <- min(c(curr.c, dest.c))
      max.c <- max(c(curr.c, dest.c))
      
      if (length(min.r:max.r) > 1 & length(min.c:max.c) > 1) {
        diag.vec <- diag(run.mat[min.r:max.r, min.c:max.c])
        diag(run.mat[min.r:max.r, min.c:max.c]) <- diag.vec + 1
      }
      if (length(min.r:max.r) == 1 | length(min.c:max.c) == 1) {
        straight.vec <- run.mat[min.r:max.r, min.c:max.c]
        run.mat[min.r:max.r, min.c:max.c] <- straight.vec + 1
      }
    } # end if
    
    #if (t %% itdiv==0) print(t) 
    
  } # end t
  
  #run.mat[which(run.mat==0)] <- NA
  
  # proportion of matrix visited by 1 pig
  visited.m2 <- length(which(run.mat > 0))
  visited.km2 <- visited.m2/1e6
  visited.km2.ph.vec[s] <- visited.km2/n.time.steps
  
  print(s)  
}
hist(visited.km2.ph.vec, main="", xlab="visited area/hr (km2)")
visited.km2.ph.mn <- mean(visited.km2.ph.vec, na.rm=T)
visited.km2.ph.lo <- quantile(visited.km2.ph.vec, probs=0.025, na.rm=T)
visited.km2.ph.up <- quantile(visited.km2.ph.vec, probs=0.975, na.rm=T)
abline(v=visited.km2.ph.mn, lty=1, col="red", lwd=3)
abline(v=visited.km2.ph.lo, lty=2, col="red", lwd=3)
abline(v=visited.km2.ph.up, lty=2, col="red", lwd=3)



run.mat.red <- redim_matrix(run.mat, target_height = 1000, target_width = 1000) 
dim(run.mat)
dim(run.mat.red)
run.mat.red[which(run.mat.red==0)] <- NA
length(which(run.mat.red>0))/dim(run.mat.red)[1]^2
min.val.red <- min(run.mat.red, na.rm=T)
max.val.red <- max(run.mat.red, na.rm=T)
hist(run.mat.red[is.na(run.mat.red)==F])

image(
  t(run.mat.red), 
  axes = FALSE,
  col = colorRampPalette(c("white", "darkorange", "black"))(30), # our colour palette
  breaks = c(seq(0, 0.1, length.out = 30), max.val.red) # colour-to-value mapping
)
box() # adding a box around the heatmap

length(which(run.mat>0))/mat.dim^2


## minimum convex polygon
run.xyz <- melt(run.mat.red)
names(run.xyz) <- c("x", "y", "z")
head(run.xyz)
run.xy <- run.xyz[,1:2]
head(run.xy)
xysp <- SpatialPoints(run.xy)

run.mcp.area <- mcp.area(xysp, percent=95, , unin="m", unout="km2")
run.mcp.area*100

run.kern <- kernelUD(xysp, h="href")
kernel.area(run.kern, percent=seq(50,95,5), unin="m", unout="km2") * 100

run.clus <- clusthr(xysp)
as.data.frame(run.clus[[3]])
hrge.mo.mn




#####################################################3
# dummy matrix
dum.dim <- 10
dum.mat <- matrix(data=0, ncol=dum.dim, nrow=dum.dim)

st.r <- round(dum.dim/2, 0)
st.c <- round(dum.dim/2, 0)

Moore <- seq(1,8,1)
Moore.comp <- c("NW", "N", "NE", "E", "SE", "S", "SW", "W")
Moore.dr <- c(-1,-1,-1,0,1,1,1,0)
Moore.dc <- c(-1,0,1,1,1,0,-1,-1)
Moore.dir <- data.frame("Moore"=Moore, "dir"=Moore.comp, "dr"=Moore.dr, "dc"=Moore.dc)
run.mat <- dum.mat
run.mat[st.r,st.c] <- 1
curr.r <- dest.r <- st.r
curr.c <- dest.c <- st.c


curr.r <- dest.r
curr.c <- dest.c

direct <- Moore.dir[sample(Moore,1, replace=F),]
dist.m.it <- sample(1:5,1,replace=F)
cells.it <- round(sqrt(dist.m.it^2 - (dist.m.it/2)^2), 0)

if (direct$dir == "N" | direct$dir == "E" | direct$dir == "S" | direct$dir == "W") {
  mov.row <- direct$dr * cells.it
  mov.col <- direct$dc * cells.it
}
if (direct$dir == "NW" | direct$dir == "NE" | direct$dir == "SE" | direct$dir == "SW") {
  drc <- sqrt(cells.it^2 - (cells.it/2)^2)
  mov.row <- round(direct$dr * drc, 0)
  mov.col <- round(direct$dc * drc, 0)
}

dest.r <- curr.r + mov.row
dest.c <- curr.c + mov.col

dest.r <- ifelse(dest.r > dum.dim, sample(1:dum.dim, 1, replace=F), dest.r)
dest.r <- ifelse(dest.r < 1, sample(1:dum.dim, 1, replace=F), dest.r)
dest.c <- ifelse(dest.c > dum.dim, sample(1:dum.dim, 1, replace=F), dest.c)
dest.c <- ifelse(dest.c < 1, sample(1:dum.dim, 1, replace=F), dest.c)

run.mat[dest.r, dest.c] <- run.mat[dest.r, dest.c] + 1

if (direct$dir == "N" | direct$dir == "E" | direct$dir == "S" | direct$dir == "W") {
  spath.r <- curr.r:dest.r
  if (length(spath.r)==1) {
    spath.r <- curr.r
  } # end if
  spath.c <- curr.c:dest.c
  if (length(spath.c)==1) {
    spath.c <- curr.c
  }
  straight.vec <- run.mat[spath.r, spath.c]
  if(length(straight.vec)==1) {
    straight.vec <- straight.vec - 1
  }
  if(length(straight.vec)>=2) {
    straight.vec[1] <- straight.vec[1] - 1
    straight.vec[length(straight.vec)] <- straight.vec[length(straight.vec)] - 1
  }
  run.mat[spath.r, spath.c] <- straight.vec + 1
} # end if

if (direct$dir == "NW" | direct$dir == "NE" | direct$dir == "SE" | direct$dir == "SW") {
  min.r <- min(c(curr.r, dest.r))
  max.r <- max(c(curr.r, dest.r))
  min.c <- min(c(curr.c, dest.c))
  max.c <- max(c(curr.c, dest.c))
  
  if (length(min.r:max.r) > 1 & length(min.c:max.c) > 1) {
    diag.vec <- diag(run.mat[min.r:max.r, min.c:max.c])
    diag(run.mat[min.r:max.r, min.c:max.c]) <- diag.vec + 1
  }
  if (length(min.r:max.r) == 1 | length(min.c:max.c) == 1) {
    straight.vec <- run.mat[min.r:max.r, min.c:max.c]
    run.mat[min.r:max.r, min.c:max.c] <- straight.vec + 1
  }
} # end if


#run.mat[which(run.mat==0)] <- NA

plot(run.mat)
length(which(run.mat>0))/dum.dim^2
