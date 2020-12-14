library(MplusAutomation)

mod <- readModels("C:\\Users\\Michael\\Box\\Active\\PhD Files\\Research\\LV Interactions\\Mplus Code\\ForR", recursive = TRUE)
mod <- readModels("/Users/frisby/Box/Active/PhD Files/Research/LV Interactions/Mplus Code/ForR", recursive = TRUE)

str(mod)
length(mod)
names(mod)
mod$parameters

get_params<-as.data.frame(mod$parameters$unstandardized)
get_variance<-as.data.frame(mod$sampstat$covariance)


temp <- get_params[grep("\\.ON",get_params$paramHeader),]
#gsub("^.*\\.","\\.",temp$paramHeader)
psi_names <- unique(unlist(lapply(temp[,1], function(x) strsplit(x,"\\.ON")[[1]])))
phi_names <- unique(temp[,2])[which(!unique(temp[,2])%in%psi_names)]

psi_mat <- matrix(0, length(psi_names), length(psi_names))
colnames(psi_mat) <- rownames(psi_mat) <- psi_names

phi_mat <- matrix(0, length(phi_names), length(phi_names))
colnames(phi_mat) <- phi_names
rownames(phi_mat) <- phi_names

beta_mat <- matrix(0, length(psi_names), length(psi_names))
colnames(beta_mat) <- psi_names
rownames(beta_mat) <- psi_names

gamma_mat <- matrix(0, length(phi_names), length(psi_names))
colnames(gamma_mat) <- phi_names
rownames(gamma_mat) <- psi_names

### PSI
tempwith <- get_params[grep("\\.WITH",get_params$paramHeader),]
poswith <- which(tempwith$param %in% psi_names)
psi_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.WITH")[[1]])))
### for off diagonals for psi
for(i in poswith){
  rowj=which(rownames(psi_mat) %in% psi_nameswith[i])
  colk=which(colnames(psi_mat) %in% tempwith[i,2])
  psi_mat[rowj,colk]=tempwith$est[i]
  psi_mat[colk,rowj]=tempwith$est[i]
}

### for diagonals for psi

resvar<- get_params[get_params$paramHeader=="Residual.Variances",]
pos_varpsi <- which(resvar$param %in% psi_names)
resvar_psi<-resvar[pos_varpsi,]

for(i in 1:length(pos_varpsi)){
  diagi<- which(psi_names %in% resvar_psi$param[i])
  psi_mat[diagi,diagi]<- resvar_psi$est[i]
}


### PHI
tempwith <- get_params[grep("\\.WITH",get_params$paramHeader),]
poswith <- which(tempwith$param %in% phi_names)
phi_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.WITH")[[1]])))
### for off diagonals of phi

for(h in 1:poswith){
    for(i in 1:nrow(tempwith)){
        rowj=which(rownames(phi_mat) %in% phi_nameswith[h])
        colk=which(colnames(phi_mat) %in% tempwith[i,2])
        phi_mat[rowj,colk]=tempwith$est[i]
        phi_mat[colk,rowj]=tempwith$est[i]
    }
}

### for diagonals for psi (latent)

resvar<- get_params[get_params$paramHeader=="Variances",]
pos_varphi <- which(resvar$param %in% phi_names)
resvar_phi<-resvar[pos_varphi,]

for(i in 1:length(pos_varphi)){
  diagi<- which(phi_names %in% resvar_phi$param[i])
  phi_mat[diagi,diagi]<- resvar_phi$est[i]
}

### for diagonals for psi (observed)

for(i in nrow(get_variance)){
    diagi <- which(phi_names %in% rownames(get_variance))
    phi_mat[diagi, diagi] <- get_variance[i,i]
}



### BETA

tempwith <- get_params[grep("\\.ON",get_params$paramHeader),]
poswith <- which(tempwith$param %in% psi_names)
beta_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.ON")[[1]]))) #no longer needed
lhs <- unlist(lapply(tempwith[,1], function(x) strsplit(x,"\\.ON")[[1]]))

### for off diagonals for psi
for(h in poswith){
    rowj=which(rownames(beta_mat) %in% lhs[h])
    colk=which(colnames(beta_mat) %in% tempwith$param[h])
    beta_mat[rowj,colk] = tempwith$est[h]
}


### GAMMA

gamma_mat <- matrix(0, length(phi_names), length(psi_names))
colnames(gamma_mat) <- phi_names
rownames(gamma_mat) <- psi_names

tempwith <- get_params[grep("\\.ON",get_params$paramHeader),]
lhs <- unlist(lapply(tempwith[,1], function(x) strsplit(x,"\\.ON")[[1]]))
poswith <- which(tempwith$param %in% phi_names)

for(h in poswith){
    rowj=which(rownames(gamma_mat) %in% lhs[h])
    colk=which(colnames(gamma_mat) %in% tempwith$param[h])
    gamma_mat[rowj,colk] = tempwith$est[h]
}


### SOLUTION

Imat <- diag(nrow(beta_mat))  # Create the identity matrix to match the dimensions of the Beta matrix you created.
sol <- solve((Imat - beta_mat))%*%(gamma_mat%*%phi_mat%*%t(gamma_mat) + psi_mat)%*%t(solve((Imat - beta_mat))) # Calculate Frisby & Diemer 2020 solution.

paste0("Variances")
diag(sol) # See variance calculations for all endogenous latent variables. 

print("Standard Deviations")
sqrt(diag(sol)) # See standard deviation calculations for all endogenous latent variables.
