#Setup workspace------------------------------------------------------------------
rm(list = ls(all = TRUE))

library(jagsUI)

setwd("C:/Users/steph/Desktop/PhD/Research/Fecundity")
#Read in data

ConfFem = read.csv("ConfidentFemales.csv")
ConfMum = read.csv("ConfidentMums.csv")

#All females
YearSightingsMatrix = read.csv("AllFemaleAnnualCH.csv")
YearSightingsMatrix = merge(ConfFem, YearSightingsMatrix, by.x = "IndividualID", by.y = "X",all.x =TRUE, all.y = FALSE)
rownames(YearSightingsMatrix) = YearSightingsMatrix$IndividualID
YearSightingsMatrix = YearSightingsMatrix[,-c(1,2)]



#Informative prior calculation-------
#Calculate an informative prior on survival - introducing the beta distribution given the mean (0.91) and the sd (0.02). Under Beta distribution the mean is alpha / (alpha + beta), and the sd is SQRT((alph*beta)/((alpha+beta)^2*(alpha+beta+1))
mu = 0.91
sd = 0.02
var = sd^2

alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
beta <- alpha * (1 / mu - 1)

rm(ConfFem,mu,sd,var)


#Define Model (editable model structure) -----------------------------------------
tmp = "
model {
  ##
  # ------------------------------------------------------------------------------
  # Parameters:
  # phi.NB: survival probability of a non-breeder
  # phi.B: survival probabilty of a breeder
  # psi.NB.B: transition probability from non-breeder to breeder
  # psi.B.NB: transition probability from breeder to non-breeder
  # p.NB: recapture probability of a non-breeder
  # p.B: recapture probability of a breeder
  # pi.B: Probability of being in initial state breeder
  # delta.B: Probability of seeing a breeder and correctly ascertaining they are a breeder
  # delta.NB: Probability of seeing a non-breeder and correctly ascertaining they are a non-breeder
  # ------------------------------------------------------------------------------
  #States (z):
  # 1 Alive & Non-Breeder
  # 2 Alive & Breeder
  # 3 Dead
  # Observations (y):
  # 1 Observed as non-breeder
  # 2 Observed as breeder
  # 3 Not observed
  # --------------------------
  
  # Constraints on priors
  for (i in 1:n){
    for (t in 1:(T-1)){
      phi.B[i,t] = mean.phi[1]
      phi.NB[i,t] = mean.phi[2]
      psi.B.NB[i,t] = mean.psi[1]
      psi.NB.B[i,t] = mean.psi[2]
      delta.B[i,t] = mean.delta[1]
      delta.NB[i,t] = mean.delta[2]

    }
    
    for (t in 1:12){
      p.B[i,t] = mean.p
      p.NB[i,t] = mean.p
    }
    for (t in 13:14){
      p.B[i,t] = 0.0001
      p.NB[i,t] = 0.0001
        }
    for (t in 15:(T-1)){
      p.B[i,t] = mean.p
      p.NB[i,t] = mean.p
    }
  }
  
  # Priors - uniform priors are uninformative
  for(u in 1:2){
    mean.phi[u] ~ dbeta(alpha, beta) 
    #mean.phi[u] ~ dunif(0, 1)
    mean.psi[u] ~ dunif(0, 1) 
    mean.delta[u] ~ dunif(0, 1)
  }
  mean.p ~ dunif(0, 1)
  #mean.phi ~ dbeta(alpha, beta) 
  pi.B ~ dunif(0, 1)
  
 
  # Probability matrices-----------------------------------------------------------------
  ## Gamma = state-based processes (survival and transistion; and 
  ## Omega = observation based processes (capture)
  for(i in 1:n){
 
  # Initial States matrices
    Pi[i,1] = pi.B                                        #Pr of having initial state B; t is not relevant as the initial state is always the first capture
    Pi[i,2] = 1 - pi.B                                    #Pr of having initial state NB
    Pi[i,3] = 0                                           #Pr of having initial state Dead
    

    for(t in fc[i]:(T - 1)){
      # Probability matrix of state z(t+1) given z(t)
      gamma[1,i,t,1] = phi.NB[i,t] * (1 - psi.NB.B[i,t])  #State (z) Alive and Non-breeder at t+1, when alive and non breeder at t
      gamma[1,i,t,2] = phi.NB[i,t] * psi.NB.B[i,t]        #State (z) Alive and Breeder at t+1, when alive and non breeder at t
      gamma[1,i,t,3] = 1 - phi.NB[i,t]                    #State (z) Dead at t+1, when alive and non breeder at t
      gamma[2,i,t,1] = phi.B[i,t] * psi.B.NB[i,t]         #State (z) Alive and Non-breeder at t+1, when alive and breeder at t
      gamma[2,i,t,2] = phi.B[i,t] * (1-psi.B.NB[i,t])     #State (z) Alive and Breeder at t+1, when alive and breeder at t
      gamma[2,i,t,3] = 1 - phi.B[i,t]                     #State (z) Dead at t+1, when alive and breeder at t
      gamma[3,i,t,1] = 0                                  #State (z) Alive and Non-breeder at t+1, when Dead at t
      gamma[3,i,t,2] = 0                                  #State (z) Alive and Breeder at t+1, when Dead at t
      gamma[3,i,t,3] = 1                                  #State (z) Dead at t+1, when Dead at t
      

      # probabilities of y(t) given z(t) 
      omega[1,i,t,1] = p.NB[i,t] * delta.NB[i,t]          #Pr (Alive NB t -> observed and detected NB)
      omega[1,i,t,2] = 0                                  #Pr (Alive NB t -> observed, but are detected as a breeder)
      omega[1,i,t,3] = p.NB[i,t] * (1 - delta.NB[i,t])    #Pr (Alive NB t -> observed, but are in an unknown state)
      omega[1,i,t,4] = 1 - p.NB[i,t]                      #Pr (Alive NB t -> not detected t)
      omega[2,i,t,1] = 0                                  #Pr (Alive B t -> observed and detected NB)
      omega[2,i,t,2] = p.B[i,t] * delta.B[i,t]            #Pr (Alive B t -> observed and detected B)
      omega[2,i,t,3] = p.B[i,t] * (1- delta.B[i,t])       #Pr (Alive B t -> observed, but are in an unknown state)
      omega[2,i,t,4] = 1 - p.B[i,t]                       #Pr (Alive B t ->  not detected t)
      omega[3,i,t,1] = 0                                  #Pr (dead t -> detected B t)
      omega[3,i,t,2] = 0                                  #Pr (dead t -> detected NB t)
      omega[3,i,t,3] = 0                                  #Pr (dead t -> detected in unknown state)
      omega[3,i,t,4] = 1                                  #Pr (dead  -> not-detected t)
      # NB: Traditionally capture-recapture models begin at the first capture of an individual, here we start the model from the age at which an individual who is definitely alive would be mature. In some instances, the first time an individual may be considered mature (e.g. X years after first capture) they were not captured, but were definitely alive. If the traditional approach is used the model would include an omega.inits probability matrix, which would exclude capture probabibilities (as the first instance an individual is captured gives a p of 1) but include the delta probabilities. Here we treat initial mature occassion with both the capture probability and the delta probability.

    } 
  }
  # Likelihood 
  for (i in 1:n){
    
    # Define latent state at first capture
    z[i,fc[i]] ~ dcat(Pi[i,1:3]) 
    y[i,fc[i]] ~ dcat(omega[z[i,fc[i]],i,fc[i],1:4])
    
    for (t in (fc[i] + 1):T){
    
      # State process using the gamma matrix (transition state probabilities), calculate the likely state of individual i at time t based on the state of individual i at t-1
      z[i,t] ~ dcat(gamma[z[i,t-1], i, t-1, 1:3]) #Can be individual or time  specific
      
      # Observation process, y(t) given z(t) -> using the omega matrix
      y[i,t] ~ dcat(omega[z[i,t], i, t-1, 1:4]) 
    }
  }
}"
cat(tmp, file = "./Multistate Model Development/Model4_MultiEvent_MatureFemalesCH_inclYearGap_UncertainDetection.txt") 


#Model data setup -------------------------------------------------------------------------------------------------------
obs.matrix <- as.numeric(as.matrix(YearSightingsMatrix[,1:(ncol(YearSightingsMatrix))]))
obs.matrix<-matrix(obs.matrix,nrow = nrow(YearSightingsMatrix),ncol = ncol(YearSightingsMatrix))


# Determine the first instance where a known breeder would be mature
t.mature <- function(x) {
  mature.calf = min(which(x == 2))
  mature.juv = min(which(x == 3)) - 1
  mature.age = min(which(x!=0)) + 6
  x = min(c(mature.calf,mature.juv,mature.age))
  if(x == 0){
    x = 1
  }
  return(x)
}

fmc = apply(obs.matrix,1,FUN = t.mature) # Challenge 1: Some individuals will never have been observed in a state where we know they would be mature


## Solution 1: Determine and remove all individuals where we cannot differentiate mature or immature status (seen for < 6 years | Never sighted with a calf or 1+)
mature.inds = rep(NA,nrow(obs.matrix)) 
for(i in 1:nrow(obs.matrix)){
  if(fmc[i] > ncol(obs.matrix)){
    mature.inds[i] = 0
  }else if(length(which(obs.matrix[i,fmc[i]:ncol(obs.matrix)] > 0)) > 0){
    mature.inds[i] = 1
  }else{
    mature.inds[i] = 0
  }
}

obs.matrix = obs.matrix[-which(mature.inds == 0),]
fmc = apply(obs.matrix,1,FUN = t.mature)


#Recode obs.matrix (0 is not allowed), seen correctly id'd NB = 1, seen correctly id'd B = 2, seen but state not ascertained = 3, not seen = 4
obs.matrix[obs.matrix == 1] = 5 #If sighted without calf or juvenile unknown if it was a breeder that 1) lost calf, 2) had not yet calved or 3) true non-breeder. Assign unused value for differentiation
obs.matrix[obs.matrix == 3] = 1 #If seen with juvenile we can assume that the mother is a non-breeder that season.
obs.matrix[obs.matrix == 5] = 3 #Simple change from random number to the correct code
obs.matrix[obs.matrix==0] = 4 #Not observed = 4

# Final JAGS data list:
jags.data = list (y = obs.matrix,
                  fc = fmc,
                  n = nrow(obs.matrix),
                  T = ncol(obs.matrix),
                  alpha = alpha,
                  beta = beta)




# Initial values ----
#Function for determining initial values
me.init.z <- function(ch, f){
  for (i in 1:nrow(ch)) {
    for (t in 1:ncol(ch)) {
      if (t >= f[i] & ch[i,t] == 4) {ch[i,t] <- which(rmultinom(1, 1, c(1/2,1/2))==1)}
      if (t >= f[i] & ch[i,t] == 3) {ch[i,t] <- which(rmultinom(1, 1, c(1/2,1/2))==1)}
      if (t < f[i]) {ch[i,t] <- NA}
    }
  }
  return(ch)
}

inits = function(){list(mean.phi = runif(2,0,1),
                        mean.psi = runif(2,0,1),
                        mean.p = runif(1,0,1),
                        mean.delta = runif(2,0,1),
                        pi.B = runif(1,0,1),
                        z = me.init.z(ch = obs.matrix, f=fmc))}

# Parameters to monitor --------------------------------------------------------------------
parameters = c("mean.phi", "mean.psi", "mean.p","mean.delta","pi.B")

# MCMC settings ----
n.iter = 10000
n.burnin = 1000
n.chains = 3
n.thinning = 1

# Run Model --------------------------------------------------------------------------------
multievent.annualcaptures_Iprior_phi <- jags(data = jags.data,
                            inits = inits,
                            parameters.to.save = parameters,
                            model.file = "./Multistate Model Development/Model4_MultiEvent_MatureFemalesCH_inclYearGap_UncertainDetection.txt",
                            n.chains = n.chains,
                            n.thin = n.thinning,
                            n.iter = n.iter,
                            n.burnin = n.burnin)




print(multievent.annualcaptures_Iprior_phi,digits=3)
traceplot(multievent.annualcaptures_Iprior_phi)


# Model validation -------------------------------------------------------------------------



#Fecundity step = Matrix model -------------------------------------------------------------
#Using transition probabilities to determine pop. fecundity
uncertain.pop = matrix(NA, 10000, 30)
uncertain.pop[1:5000,1] = 1 #B
uncertain.pop[5001:10000,1] = 0 #NB


for(t in 2:ncol(uncertain.pop)){
  for(i in 1:nrow(uncertain.pop)){
    psiNB.B = 0.377
    psiB.NB = 0.885
    if(psiB.NB > 1){
      psiB.NB = 1
    }else if(psiB.NB < 0){
      psiB.NB = 0
    }
    if(psiNB.B > 1){
      psiNB.B = 1
    }else if(psiNB.B < 0){
      psiNB.B = 0
    }
    
    if(uncertain.pop[i,(t-1)] == 0){
      uncertain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c((1-psiNB.B),psiNB.B))
    }else if(uncertain.pop[i,(t-1)] == 1){
      uncertain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c(psiB.NB, (1-psiB.NB)))
    }
  }
}

x.var = 1:ncol(uncertain.pop)
y.var = apply(uncertain.pop,2,function(x) length(which(x == 1)))/10000

points(x.var,y.var,ylim=c(0,1),pch=16,col="red")
mean(y.var[-c(1:5)])
sd(y.var[-c(1:5)])
abline(h=mean(y.var[-c(1:5)]))



##Uncertainty-----
uncertain.pop = matrix(NA, 1000, 30)
certain.pop[1:500,1] = 1 #B
certain.pop[501:1000,1] = 0 #NB

PosteriorTransitionProbs = multistate.annualcaptures_I.prior.phi.constant$sims.list$mean.psi
PosteriorTransitionProbs = PosteriorTransitionProbs[sample(1:27000,1000,replace = FALSE),]

UncertainFecundEst = rep(NA,5)


for(a in 1:nrow(PosteriorTransitionProbs)){
  for(t in 2:ncol(certain.pop)){
    for(i in 1:nrow(certain.pop)){
      psiNB.B = PosteriorTransitionProbs[a,2]
      psiB.NB = PosteriorTransitionProbs[a,1]
      if(psiB.NB > 1){
        psiB.NB = 1
      }else if(psiB.NB < 0){
        psiB.NB = 0
      }
      if(psiNB.B > 1){
        psiNB.B = 1
      }else if(psiNB.B < 0){
        psiNB.B = 0
      }
      
      if(certain.pop[i,(t-1)] == 0){
        certain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c((1-psiNB.B),psiNB.B))
      }else if(certain.pop[i,(t-1)] == 1){
        certain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c(psiB.NB, (1-psiB.NB)))
      }
    }
  }
  y.var1 = apply(certain.pop,2,function(x) length(which(x == 1)))/1000
  UncertainFecundEst[a] = y.var1[30]
}

Uncertain.fecund - qt(0.975,df=(length(UncertainFecundEst)-1))*sd(UncertainFecundEst)/sqrt(length(UncertainFecundEst))
Uncertain.fecund + qt(0.975,df=(length(UncertainFecundEst)-1))*sd(UncertainFecundEst)/sqrt(length(UncertainFecundEst))






#Model selection???----
psi.B = multievent.annualcaptures_Iprior_phi$sims.list$mean.psi[,1]
psi.NB = multievent.annualcaptures_Iprior_phi$sims.list$mean.psi[,2]
phi.NB = multievent.annualcaptures_Iprior_phi$sims.list$mean.phi[,2]
phi.B = multievent.annualcaptures_Iprior_phi$sims.list$mean.phi[,1]
p = multievent.annualcaptures_Iprior_phi$sims.list$mean.p

p.yi = rep(NA, N.cal)
for(i in 1:N.cal) f
p.yi[i] = mean(dnorm(y.cal[i], beta0 + beta1*log(x.cal[i])/log(10), sigma))
g
llpd = sum(log(p.yi))
llpd

#Look up how to calculate for ll for capture recapture data.... surely there is a tutorial....
 
