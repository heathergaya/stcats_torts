#Last edited 13 July 2020 by Heather Gaya

#Multistate Model
#For now, no sex included: B=1, H=2,J=3,S=4,A=5, unknown/not alive = 6
#Three treatments -  direct release =1, headstart =2 ,natural =3
#p= values vary by treatment type/release date
#put Phi on logit 

library(runjags)
setwd("~/Desktop/U Georgia/Rebecca_Project")

input.data<-read.csv("sci_data.csv")
input.data<-input.data[,1:15]

input.data$year<-input.data$Cap_Year-min(input.data$Cap_Year)+1 #year 1 = 2006, year 8 = 2013

#Reduce encounter period to years 
head(input.data[order(input.data$year),])

## Turn stage into numbers instead of letters
input.data$Stage.no <- as.numeric(factor(input.data$Stage, levels = c("B", "H", "J", "S", "A", "D")))

#And put treatment as a number as well
input.data$treatment.no <- as.numeric(input.data$Treatment)

#create a factor -- might have animals out of sequence , missing
#get rid of missing levels
input.data$Tort<-factor(input.data$ID)

x<-levels(input.data$Tort)
input.data$animal.no<-match(input.data$Tort,x)

n.occas<-max(input.data$year)
n.animal<-length(x)

#array of zeros
y=array(0,dim=c(n.animal,n.occas))

input.data<-input.data[order(input.data$animal.no,input.data$year),]


animal.no<-unique(input.data$animal.no)

captures<-subset(input.data,select=c(animal.no,year,Stage.no))

nrow(captures)

#replace with  captures
for(i in 1:nrow(captures)){
  #this is needed in case we have missing levels
  row<-captures[i,1]
  col<-captures[i,2]
  y[row,col]<-as.numeric(captures[i,3])
}

ind.hist<-cbind(animal.no,y)
all.dat<-merge(ind.hist, input.data, by="animal.no", all.x=T, all.y=F)
all.dat<-all.dat[!duplicated(all.dat$animal.no),]


n.states <- 6 #B, H, J, S, A, U/D
n.occasions <- 8  #8 years
CH <- y

# Recode CH matrix: note, a 0 is not allowed in WinBUGS!

rCH <- CH  # Recoded CH
rCH[rCH==0] <- 6
nind<-dim(rCH)[1]
# Specify model in JAGS language
##### model #######
modelstring.Tort = "
model
    {
    ##   phi is indexed by i (animal) and j (age class)
    
    for (i in 1:nind){    #for all animals
      for (j in 2:5){     # age class 2 to 5
        l.phi[i,j] <- mu + phi.a[j] + phi.o[L[i]]+ phi.ao[L[i],j]  
        phi[i,j] <- ilogit(l.phi[i,j])
      }
      phi[i,1] <- 1 #survival for unborn individuals is meaningless
    }
    
    for (j in 2:5){
    phi.age[j]<-ilogit(mu+phi.a[j])
    }
    
    for (i in 1:3){                               #treatments
      phi.treat[i]<-ilogit(mu+phi.o[i])
      for (j in 2:5){                             #age groups
       phi.est[i,j]<-ilogit(mu + phi.a[j]+phi.o[i]+phi.ao[i,j])   #treatment, age
      }
    }
    
    mu ~ dnorm(0,0.37)
    
    
    
    ##  Jags doesn't like mix of random and non-random elements in same vector, so use phi.a0 and phi.o0 for random elements
    ##  and use phi.a and phi.o for fixed versions of these values 
    
    for (t in 1:2){                     #first 2 treatments
      phi.o0[t] ~ dnorm(0, 0.37)
      phi.o[t] <- select.o * phi.o0[t]    ##select.o turns ON/OFF treatment effect
    }
    
    phi.o[3] <- -sum(phi.o[1:2])  #final treatment level = neg sum the other 2
    
    for (u in 1:4){   #birth to subadult ages
      phi.a0[u] ~ dnorm(0, 0.37)
      phi.a[u] <- phi.a0[u]   
    }
 
    phi.a[5] <- -sum(phi.a[1:4])  #adult level = neg sum the other classes
    

    for (t in 1:2) {  # first 2 treatment  
      for (u in 1:4) {  #age
        phi.ao0[t,u] ~ dnorm(0, 0.37)
        phi.ao[t,u] <- select.int * phi.ao0[t,u]        ## select.int IS A SWITCH THAT TURNS ON/OFF INTERCEPT
        }
      phi.ao[t,5] <- -sum(phi.ao[t,1:4])  #5th age = neg sum of first 4 #treatment, age
      }
    
    for (u in 1:4){
    phi.ao[3,u] <- -sum(phi.ao[1:2,u])  #treat 3 = neg sum of first 2
    }
    
    phi.ao[3,5] <- -sum(phi.ao[1:2,1:4])

   
    
#Transitions 
    psiBH ~ dunif(0, 1)  #you are born and detectible 
    psiBJ ~ dunif(0,1)    #you are born, probably headstarted to get chunky, and become a juvi 
    psiHJ ~ dunif(0, 1)  #hatch to juvi
    psiJS ~ dunif(0, 1) #juvi to sub
    psiSA ~ dunif(0, 1) #sub to adult
    

## Probability of detection varies with age and treatment
## 1 = DR, 2 = headstart, 3 = natural

    pB[1] <- 1  #we know when direct releases are born/released
    pB[2] <- 1  #we know when headstarts are born/release
    pB[3] <- 0  #we don't know when natural ones are born
     
    #adding in time variable 
    for (t in 1:n.occasions){
    pH[1,t] ~ dunif(0, .75)
    pH[2,t] ~ dunif(0, .75)
    pH[3,t] ~ dunif(0, .75)
    
    pJ[1,t] ~ dunif(0,1)
    pJ[2,t] ~ dunif(0,1)
    pJ[3,t] ~ dunif(0,1)
    
    pS[t] ~ dunif(0, 1)
    pA[t] ~ dunif(0, 1)
    }
    
    pH.mean[1] <- mean(pH[1,])
    pH.mean[2] <- mean(pH[2,])
    pH.mean[3] <- mean(pH[3,])
    
    pJ.mean[1] <- mean(pJ[1,])
    pJ.mean[2] <- mean(pJ[2,])
    pJ.mean[3] <- mean(pJ[3,])
    
    pS.mean <- mean(pS[])
    
    
    
    
    
    # Define state-transition and observation matrices 	
    for (i in 1:nind){
    
    
    # Define probabilities of state S(t+1) given S(t)
    for (t in f[i]:(n.occasions-1)){
    
    #Note to self: phi = survival, psi = transition
    
    
    ps[1,i,t,1]  <- 1-psiBH-psiBJ #not born
    ps[1,i,t,2]  <- psiBH   # you *are* born
    ps[1,i,t,3]  <- psiBJ * phi[i,2]  #you are born, survive your headstarting time, and arrive on the landscape as a juvi
    ps[1,i,t,4]  <- 0
    ps[1,i,t,5]  <- 0
    ps[1,i,t,6]  <- 0
    
    ps[2,i,t,1]  <- 0
    ps[2,i,t,2]  <- phi[i,2]* (1- psiHJ)  #survive but stay a hatchling
    ps[2,i,t,3]  <- phi[i,2]* psiHJ #transition to juvi
    ps[2,i,t,4]  <- 0
    ps[2,i,t,5]  <- 0
    ps[2,i,t,6]  <- (1-phi[i,2])  #die
    
    ps[3,i,t,1]  <- 0
    ps[3,i,t,2]  <- 0
    ps[3,i,t,3]  <- phi[i,3] * (1-psiJS)  #stays a juvi
    ps[3,i,t,4]  <- phi[i,3] * psiJS    #become a subadult    
    ps[3,i,t,5]  <- 0
    ps[3,i,t,6]  <- 1-phi[i,3]
    
    ps[4,i,t,1]  <- 0
    ps[4,i,t,2]  <- 0
    ps[4,i,t,3]  <- 0
    ps[4,i,t,4]  <- phi[i,4] * (1-psiSA)
    ps[4,i,t,5]  <- phi[i,4] * psiSA
    ps[4,i,t,6]  <- 1-phi[i,4]
    
    ps[5,i,t,1]  <- 0
    ps[5,i,t,2]  <- 0
    ps[5,i,t,3]  <- 0
    ps[5,i,t,4]  <- 0
    ps[5,i,t,5]  <- phi[i,5]  #stay an adult
    ps[5,i,t,6]  <- 1-phi[1,5]
    
    ps[6,i,t,1]  <- 0
    ps[6,i,t,2]  <- 0
    ps[6,i,t,3]  <- 0
    ps[6,i,t,4]  <- 0
    ps[6,i,t,5]  <- 0
    ps[6,i,t,6]  <- 1 #stay dead
    
    # Define probabilities of observing t  given true state of the system S t 
    po[1,i,t,1] <- pB[L[i]]   #based on treatment
    po[1,i,t,2] <- 0
    po[1,i,t,3] <- 0
    po[1,i,t,4] <- 0
    po[1,i,t,5] <- 0
    po[1,i,t,6] <- 0
    
    po[2,i,t,1] <- 0
    po[2,i,t,2] <- pH[L[i],t]   
    po[2,i,t,3] <- 0
    po[2,i,t,4] <- 0
    po[2,i,t,5] <- 0
    po[2,i,t,6] <- 1-pH[L[i],t]
    
    po[3,i,t,1] <- 0
    po[3,i,t,2] <- 0
    po[3,i,t,3] <- pJ[L[i],t]
    po[3,i,t,4] <- 0
    po[3,i,t,5] <- 0
    po[3,i,t,6] <- 1-pJ[L[i],t]
    
    po[4,i,t,1] <- 0
    po[4,i,t,2] <- 0
    po[4,i,t,3] <- 0
    po[4,i,t,4] <- pS[t]
    po[4,i,t,5] <- 0
    po[4,i,t,6] <- 1-pS[t]
    
    po[5,i,t,1] <- 0
    po[5,i,t,2] <- 0
    po[5,i,t,3] <- 0
    po[5,i,t,4] <- 0
    po[5,i,t,5] <- pA[t]
    po[5,i,t,6] <- 1-pA[t]
    
    po[6,i,t,1] <- 0
    po[6,i,t,2] <- 0
    po[6,i,t,3] <- 0
    po[6,i,t,4] <- 0
    po[6,i,t,5] <- 0
    po[6,i,t,6] <- 1  #dead, gone, etc.
    } #t
    } #i
    ####    } #L
    
    for (i in 1:nind){
    # Define latent state at first capture
    z[i,f[i]] <- y[i,f[i]]
    for (t in (f[i]+1):n.occasions){
    # State process: draw S(t) given S(t-1)
    z[i,t] ~ dcat(ps[z[i,t-1],i, t-1,])
    # Observation process: draw O(t) given S(t)
    y[i,t] ~ dcat(po[z[i,t],i, t-1,])
    } #t
    } #i 
    
    } #Wow that's a long model
    
    "

nind<-dim(rCH)[1]
########## Initial Value Functions #########
## Alternative function to initialize states for size-class based state structure; i.e., 1 -> 2 -> 3 -> etc. with dead state
## Input to ch.init: ch = capture history, f = vector of first capture occasions, ad.state = first adult state,
##                   dead = recently dead (assumed to be last adult state+1)
ch.init <- function(ch, f, ad.state, dead){
  ch2 <- ch                               ## Make a duplicate of CH matrix
  states <- max(ch, na.rA=TRUE)           ## Number of known capture states
  known.states <- 1:(states-1)            ## List of known capture states
  for (i in 1:dim(ch)[1]){                ## Loop over all animals
    ch.last <- ch[i,f[i]]                     ## Save current state for use in later captures
    x <- which(ch[i,] %in% known.states)      ## Column indices of all captures
    if (x[1]<dim(ch)[2]){                     ## If first capture was not in final period, then do steps below
      for (j in (f[i]+1):dim(ch)[2]){             ## Loop over all occasions (after first capture)
        if (ch[i,j] %in% known.states){           ## Is the capture one of the known states?
          ch.last <- ch[i,j]                          ## Update last assigned state
          ch2[i,j] <- NA                              ## Then replace the capture with NA (no need to initialize)
        } else {                                  ## No capture this occasion
          x <- x[(x>j)]                           ## List of all captures following this occasion
          q <- length(x)                          ## Number of captures following this occasion
          if (q==0) { ## Was the animal NEVER SEEN alive after this occasion?
              ch2[i,j] <- ch.last                         ## YES: Then use state assigned at last occasion
              ch.last <- ch2[i,j]                         ## And update last assigned state
            } 
          
          else {                                ## NO: Other live captures follow this occasion
            next.cap <- min(x)                        ## Find occasion of the very next capture
            if (ch.last==ch[i,next.cap] |             ## Is last state assigned the same as next capture state, OR
                ch.last>=ad.state){                   ##      is the last state assigned one of the adult states?
              ch2[i,j] <- ch.last                         ## Then use state assigned at last occasion
            } else {                                  ## NO: neither condition is true
              ifelse(ch.last<ad.state-1,                  ## Is last state assigned more than 1 step away from adult state?
                     ch2[i,j] <- ch.last+1,                   ## YES: increment state by 1
                     ch2[i,j] <- ch[i,next.cap])
                     #ch2[i,j] <- ch.last)              ## NO: assign state to be the adult state at next capture
            }
            ch.last <- ch2[i,j]                       ## Update last assigned state
          }
        }
      }  ## end j
    }
    ch2[i,1:f[i]] <- NA                       ## Assign NA up through first capture (no need to initialize)
  }  ## end i
  return(ch2)
}

# Function to create known latent states
known.state.ms <- function(ms, notseen){
  # notseen: label for 'not seen'
  state <- ms
  state[state==notseen] <- NA
  for (i in 1:dim(ms)[1]){
    m <- min(which(!is.na(state[i,])))
    state[i,m] <- NA
  }
  return(state)
}

##### Find dates when headstarts/releases were released #####
release.date <- function(all.dat, rCH){
  full <- all.dat
  ch <- rCH  
  full <- subset(full, MOC == "release" & Cap_Year > 2006)
  release <- full$Cap_Year-2006 #before they were released, they were age class 1 (pre-hatch)
  for (i in 1:nrow(full)){
    ch[full$animal.no[i],release[i]] <- 1
  }
  return(ch)
}

release.detect <- function(all.dat, rCH){
  full <- all.dat
  full2 <- subset(full, MOC == "release")
  release2 <- full2$Cap_Year-2005 #we have 100% detection of these guys up until (and including) the year they are released 
  R.mat <- matrix(2, nrow = nrow(all.dat), ncol = dim(rCH)[2])  #unknown detection for all individuals 
  for (i in 1:nrow(full2)){
    R.mat[full2$animal.no[i],1:release2[i]] <- 1
  }
  return(R.mat)
}

rCH <- release.date(all.dat, rCH)
rCH[163,1:3] <- c(1,1,3)  #fixing a headstart that was released as a subadult 


get.first <- function(x) min(which(x!=6))
f <- apply(rCH, 1, get.first)
f

####### Run the model ############
z.known <- known.state.ms(rCH,6)
#120 died 2008, 130 died 2008
z.known[c(120,130),3:8] <- 6
jags.data <- list(select.o = 0, select.int = 0,
                  y = rCH, f = f, n.occasions = dim(rCH)[2], 
                  nind = dim(rCH)[1],L=all.dat$treatment.no, 
                  z=z.known)
#R = R.mat

z.init <- ch.init(rCH,f,5,6)
z.init[c(120,130), 1:8] <- NA
inits<-function(){list(phi.a0=rnorm(4,0, 0.5), psiBJ = runif(1,0,1), psiBH = runif(1,0,1),
                       phi.o0 = rnorm(2, 0,0.5),psiJS =runif(1, 0, 1), 
                       psiSA = runif(1, 0, 1), 
                       mu= rnorm(1,0.37), z=z.init,
                      pJ =array(runif(24, 0, .7), dim = c(3,8)),
                      pH =array(runif(24, 0, .7), dim = c(3,8)), 
                      pS =runif(8, 0, 1), pA =runif(8, 0, 1))}

# MCMC settings
ni <- 2000
nt <- 2
nb <- 8000
nc <- 3


## Run very basic model (age)
parameters1 <- c("psiHJ", "psiJS", "psiSA", "psiBJ", "psiBH", "pJ", "pS", "pA", "pH",
                 "phi.age","mu", 
                 "phi.est","phi.a","pH.mean", "pJ.mean", "pS.mean")
tort_mod1 <- run.jags(modelstring.Tort, data = jags.data, inits = inits, monitor = parameters1,
                   n.chains = nc, sample = ni,method = 'parallel', burnin = nb, summarise = T)
## starting off with p(age) consistent across time,no fancy models, just the basics 

write.csv(summary(tort_mod1), file="results.age.mod.may10.csv" )

dic1 <- extract.runjags(tort_mod1, "dic", method = "parallel")

## Time for the treatment model 
parameters2 <- c("psiHJ", "psiJS", "psiSA", "pJ", "pS", "pA", "pH",
                 "phi.age", "mu", 
                 "phi.est","phi.a","phi.treat",
                 "phi.o","pH.mean", "pJ.mean", "pS.mean")
jags.data$select.o <- 1

tort_mod2 <- run.jags(modelstring.Tort, data = jags.data,inits= inits, monitor = parameters2,
                      n.chains = nc, method = 'parallel',sample = ni, burnin = nb, silent.jags = F)
write.csv(summary(tort_mod2), file="results.treatment.mod.may10.csv" )
dic2 <- extract.runjags(tort_mod2, "dic")

## Interactive model 
parameters3 <- parameters2
jags.data$select.o <- jags.data$select.int <- 1
tort_mod3 <- run.jags(modelstring.Tort, data = jags.data, inits = inits, monitor = parameters3,
                      n.chains = nc, method = 'parallel',sample = ni, burnin = nb)
write.csv(summary(tort_mod3), file="results.interactive.mod.may10.csv" )

dic3 <- extract.runjags(tort_mod3, "dic")
print(dic1) 
print(dic2) 
print(dic3) 
