#Allowing for imperfect detection probability of Breeders
#Define Model ----
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
  # #pi.B: proportion in state breeder at first capture
  # #beta.B: probability of correcly being identified as a breeder
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
      p.B[i,t] = mean.p[1]
      p.NB[i,t] = mean.p[2]
    }
  }
  
  # Priors - uniform priors are uninformative
  
  for(u in 1:2){
    mean.phi[u] ~ dunif(0, 1)
    mean.psi[u] ~ dunif(0, 1)
    mean.p[u] ~ dunif(0, 1)
  }
  pi.B ~ dunif(0, 1)
  beta.B ~ dunif(0, 1)
 
  ## Initial states probability vector
      delta[1] = pi.B
      delta[2] = 1 - pi.B
      delta[3] = 0

  #Make everything general right now, vary with time
  
  for(i in 1:n){
    for(t in fc[i]:(T - 1)){
    
      # Probability matrix of state z(t+1) given z(t)
      gamma[1,i,t,1] = phi.NB[i,t] * (1 - psi.NB.B[i,t])      #State (z) Alive and Non-breeder at t+1, when alive and non breeder at t
      gamma[1,i,t,2] = phi.NB[i,t] * psi.NB.B[i,t]            #State (z) Alive and Breeder at t+1, when alive and non breeder at t
      gamma[1,i,t,3] = 1 - phi.NB[i,t]                        #State (z) Dead at t+1, when alive and non breeder at t
      gamma[2,i,t,1] = phi.B[i,t] * psi.B.NB[i,t]             #State (z) Alive and Non-breeder at t+1, when alive and breeder at t
      gamma[2,i,t,2] = phi.B[i,t] * (1-psi.B.NB[i,t])         #State (z) Alive and Breeder at t+1, when alive and breeder at t
      gamma[2,i,t,3] = 1 - phi.B[i,t]                         #State (z) Dead at t+1, when alive and breeder at t
      gamma[3,i,t,1] = 0                                      #State (z) Alive and Non-breeder at t+1, when Dead at t
      gamma[3,i,t,2] = 0                                      #State (z) Alive and Breeder at t+1, when Dead at t
      gamma[3,i,t,3] = 1                                      #State (z) Dead at t+1, when Dead at t
      
      # probabilities of y(t) given z(t)
      omega[1,i,t,1] = p.NB[i,t]                  ##omega[1,i,t,1] = p.NB[t] * beta.NB[t]          #Pr (Alive NB t -> detected NB t)
      omega[1,i,t,2] = 0                          ##omega[1,i,t,2] = p.NB[t] * (1 - beta.NB[t])    #Pr (Alive NB t -> detected B t) -> should be 0
      omega[1,i,t,3] = 1 - p.NB[i,t]              ##omega[1,i,t,3] = 1 - p.NB[t]                   #Pr (Alive NB t -> not detected t)
      omega[2,i,t,1] = p.B[i,t] * (1 - beta.B)      ##omega[2,i,t,1] = p.B[t] * (1- beta.B[t])       #Pr (Alive B t -> detected NB t)
      omega[2,i,t,2] = p.B[i,t] * beta.B          ##omega[2,i,t,2] = p.B[t] * beta.B[t]            #Pr (Alive B t -> detected B t)
      omega[2,i,t,3] = 1 - p.B[i,t]               ##omega[2,i,t,3] = 1 - p.B[t]                    #Pr (Alive B t ->  not detected t)
      omega[3,i,t,1] = 0                          ##omega[3,i,t,1] = 0                             #Pr (dead t -> detected B t)
      omega[3,i,t,2] = 0                          ##omega[3,i,t,2] = 0                             #Pr (dead t -> detected NB t)
      omega[3,i,t,3] = 1                          ##omega[3,i,t,3] = 1                             #Pr (dead  -> not-detected t)
      

    } 
      omega.inits[1,i,1] = 1
      omega.inits[1,i,2] = 0
      omega.inits[1,i,3] = 0
      omega.inits[2,i,1] = 1- beta.B
      omega.inits[2,i,2] = beta.B 
      omega.inits[2,i,3] = 0
      omega.inits[3,i,1] = 0
      omega.inits[3,i,2] = 0
      omega.inits[3,i,3] = 1
  }
  # Likelihood 
  for (i in 1:n){
    
    # Define latent state at first capture
    z[i,fc[i]] ~ dcat(delta[1:3]) # Assumption of mistaken state ID
    y[i,fc[i]] ~ dcat(omega.inits[z[i,fc[i]],i, 1:3]) # Assumption of mistaken state ID
    
    for (t in (fc[i] + 1):T){
    
      # State process using the gamma matrix (transition state probabilities), calculate the likely state of individual i at time t based on the state of individual i at t-1
      z[i,t] ~ dcat(gamma[z[i,t-1], i, t-1, 1:3]) #Can be individual or time  specific
      
      # Observation process, y(t) given z(t) -> using the omega matrix
      y[i,t] ~ dcat(omega[z[i,t], i, t-1, 1:3]) 
    }
  }
}"
cat(tmp, file = "MultiStateModel3_ImperfectDetectionBreeder.txt")

# Data for model ---- 
obs.matrix <- as.numeric(as.matrix(YearSightingsMatrix[,1:(ncol(YearSightingsMatrix)-2)]))
obs.matrix<-matrix(obs.matrix,nrow = nrow(YearSightingsMatrix),ncol = ncol(YearSightingsMatrix)-2)

get.first <- function(x) min(which(x!=0))
fc = apply(obs.matrix,1,FUN = get.first)

#Recode obs.matrix (0 is not allowed), seen NB = 1 seen B = 2, not seen = 3
obs.matrix[obs.matrix==0] = 3

jags.data = list (y = obs.matrix,
                  fc = fc,
                  n = nrow(obs.matrix),
                  T = ncol(obs.matrix))

# Set initial values ----
#Create a function to set initial values for z
ms.inits.z <- function(ch, f){
  states = max(ch, na.rm = TRUE) #identify the state that corresponds to....
  known.states = 1:(states - 1)
  v = which(ch == states)
  ch[v] <- sample(known.states,length(v), replace = TRUE)
  for(i in 1:nrow(ch)){ch[i,1:f[i]] <- NA}
  return(ch)
}

# Actual initial values:
inits = function(){list(mean.phi = runif(2,0,1),
                        mean.psi = runif(2,0,1),
                        mean.p = runif(2,0,1),
                        beta.B = runif(1,0,1),
                        pi.B = runif(1,0,1),
                        z = ms.inits.z(ch = obs.matrix, f=fc))}

# Parameters to monitor ----
parameters = c("mean.phi", "mean.psi", "mean.p")

# MCMC settings ----
n.iter = 1000
n.burnin = 500
n.chains = 3
n.thinning = 1

# Run Model ----
multistate.model3 <- jags(data = jags.data,
                          inits = inits,
                          parameters.to.save = parameters,
                          model.file = "MultiStateModel3_ImperfectDetectionBreeder.txt",
                          n.chains = n.chains,
                          n.thin = n.thinning,
                          n.iter = n.iter,
                          n.burnin = n.burnin)


##Multistate.model2: Varies survival (phi), recapture (p) and transition between states (psi) over time.

print(multistate.model3,digits=3)

traceplot(multistate.model1)





#Additional things to estimate N_hat1[t] = estimated number of breeding females (nB[t]/p.B[t]), N_hat2.t = estimated number of non-breeding females(nNB[t]/p.NB[t]): gamma_hat.t = estimated proportion of breeders in the population (N_hat1/(N_hat1+N_hat2))

# Post-hoc model checks
out1 = coda.samples(model = multistate.model1, variable.names = parameters, n.iter = n.iter) # Not sure about this...
out1_df = as_draws_df(out1)
mcmc_trace(out1_df)

