######################################################################################
# THE CODE BELOW IS INTENDED AS AN EXAMPLE OF HOW TO CONSTRUCT AND MULTIPLE MATRICES
# FOR CALCULATING THE VARIANCE OF ENDOGENOUS LATENT MODERATORS.
#
# ALL NUMBERS ARE PRESUMED TO COME FROM MPLUS OUTPUT DESCRIBING A MODEL
# WITH LATENT ENDOGENOUS MODERATORS.
#
# Author: Michael Frisby [mbfrisby@umich.edu]
######################################################################################






### Construct the Beta matrix

# The Beta matrix includes only regression coefficients of endogenous variables between themselves.
# It should be read as the ROW regressed onto the COLUMN.
# For example, the first row first column reads SYSJUS regressed onto SYSJUS. Since this did not happen, the value is 0.
# The last row first column reads ACTION regressed onto SYSJUS. Since SYSJUS predicted action with a regression coefficient
# of -0.061, the entry for that cell is -0.061.
# SYSJUS was not predicted by any endogenous variables, so its row consists of all zeroes. 
# Regression coefficients are reflected in the ON statements in Mplus output.
# Values are entered row-wise.

# First, build an empty Beta matrix to be used as a reference when inserting values into vectors.
# The 0 creates a matrix of all zeros, the two 7s give 7 rows and 7 columns.
# Note: The number of rows and columns may be different for each model. These numbers will need to be adjusted to fit YOUR model.
# Note: This will be a square matrix, meaning that if it has 6 rows, it will have 6 columns; if it has 4 rows, it will have 4 columns.
beta <- matrix(0, 7, 7) 
rownames(beta) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION") # Provide Row Names
colnames(beta) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION") # Provide Column Names
beta  # Show the empty matrix just constructed.

# Now we need to insert values into this matrix.

# These lines construct the vectors of regression coefficients used to build the Beta matrix.
# b.r1 refers to Beta row 1; b.r2 refers to Beta row 2; etc.
# You may need to insert more rows/columns depending on YOUR model.
# Note: Do NOT include interaction terms in the Beta matrix. Those will go into the Gamma matrix.
b.r1 <- c(0, 0, 0, 0, 0, 0, 0)  # SYSJUS was not predicted by any endogenous variables, so its row consists of all zeroes. 
b.r2 <- c(0, 0, 0, 0, 0, 0, 0)  # TRUGOV was not predicted by any endogenous variables, so its row consists of all zeroes. 
b.r3 <- c(0, 0, 0, 0, 0, 0, 0)  # PAR was not predicted by any endogenous variables, so its row consists of all zeroes. 
b.r4 <- c(0, 0, 0, 0, 0, 0, 0)  # PEER was not predicted by any endogenous variables, so its row consists of all zeroes. 
b.r5 <- c(.025, -.04, -.055, .657, 0, 0, 0)   # PEFF was predicted by SYSJUS, TRUGOV, PAR, and PEER; its regression coefficients are entered 
b.r6 <- c(-.011, -.006, .055, .152, .45, 0, 0)  # ACTID was predicted by SYSJUS, TRUGOV, PAR, PEER, and PEFF
b.r7 <- c(-.061, -.067, -.028, .342, .180, .179, 0) # ACTION was predicted by SYSJUS, TRUGOV, PAR, PEER, PEFF, and ACTID

# These lines rebuild the matrix from the vectors just created, and provide appropriate row and column names.
# Remember to change the 7s to reflect the dimensions of YOUR matrix.
beta <- matrix(c(b.r1, b.r2, b.r3, b.r4, b.r5, b.r6, b.r7), 7, 7, byrow = TRUE)
rownames(beta) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION")
colnames(beta) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION") 
beta # Check to make sure your entries are correct.




### Construct the Gamma matrix

# The Gamma matrix includes only regression coefficients of endogenous variables onto exogenous variables and interactions.
# It should be read as the ROW regressed onto the COLUMN.
# For example, the first row first column reads SYSJUS regressed onto PARED. 
# Since PARED predicted SYSJUS with a regression coefficient of 0.112, its values is entered at this location.
# The last row reads ACTION regressed onto PARED. 
# Since PARED predicted action with a regression coefficient of 0.06, the entry for that cell is 0.06.
# Regression coefficients are reflected in the ON statements in Mplus output.
# Values are entered into vectors row-wise.

# First, build an empty Gamma matrix to be used as a reference when inserting values into vectors.
# The 0 creates a matrix of all zeros, the two 7s give 7 rows and 7 columns.
# Note: The number of rows and columns may be different for each model. These numbers will need to be adjusted to fit YOUR model.
# Note: This may not necessarily be a square matrix like Beta. It is square in this example by coincidence. 
gamma <- matrix(0, 7, 7) 
rownames(gamma) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION") # Provide Row Names
colnames(gamma) <- c("PARED", "SOPPS", "GENDER", "PARXACTI", "PARXPEFF", "PEXACTID", "PEXPEFF") # Provide Column Names
gamma   #Show the empty matrix just constructed.

# Now we need to insert values into the matrix.

# These lines construct the vectors of regression coefficients used to build the Gamma matrix.
# g.r1 refers to Gamma row 1; g.r2 refers to Gamma row 2; etc.
# You may need to insert more rows/columns depending on YOUR model.
# Note: DO include interaction terms in the Gamma matrix. 
g.r1 <- c(.112, .136, .026, 0, 0, 0, 0)
g.r2 <- c(.002, .151, .023, 0, 0, 0, 0)
g.r3 <- c(.242, .212, -.013, 0, 0, 0, 0)
g.r4 <- c(.154, .235, -.003, 0, 0, 0, 0)
g.r5 <- c(.009, .021, -.003, 0, 0, 0, 0)
g.r6 <- c(-.04, -.003, .022, 0, -0.004, 0, 0.174)
g.r7 <- c(.06, .036, -.014, 0.174, -0.088, -0.045, 0.272)

# These lines rebuild the matrix from the vectors just created, and provide appropriate row and column names.
# Remember to change the 7s to reflect the dimensions of YOUR matrix.
gamma <- matrix(c(g.r1, g.r2, g.r3, g.r4, g.r5, g.r6, g.r7), 7, 7, byrow = TRUE)
rownames(gamma) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION")
colnames(gamma) <- c("PARED", "SOPPS", "GENDER", "PARXACTI", "PARXPEFF", "PEXACTID", "PEXPEFF")
gamma # Check to make sure your entries are correct.







### Construct the Psi matrix

# The Psi matrix includes only error variances and covariances between endogenous variables, NOT including interactions.
# It should be read as the error covariance between row and column. A variable covarying with itself is its variance.
# For example, the first row first column reads SYSJUS covarying with SYSJUS, i.e., the error variance of SYSJUS.
# Error variance can be found under the RESIDUAL VARIANCES section of Mplus output. This value is .572.
# The first row second column reads the covariance between SYSJUS error and TRUGOV error. 
# Since PARED predicted SYSJUS with a regression coefficient of 0.112, its values is entered at this location.
# Error covariances can be found in the WITH statements of Mplus output. This value is 0.181. 
# Note: If no WITH statement exists for values in the Psi matrix, you may assume the entry value is zero.
# Values are entered into vectors row-wise.


# First, build an empty Psi matrix to be used as a reference when inserting values into vectors.
# The 0 creates a matrix of all zeros, the two 7s give 7 rows and 7 columns.
# Note: The number of rows and columns may be different for each model. These numbers will need to be adjusted to fit YOUR model.
# Note: This will be a square matrix, meaning that if it has 6 rows, it will have 6 columns; if it has 4 rows, it will have 4 columns.
# Note: This should be the same dimensions as the Beta matrix.
psi <- matrix(0, 7, 7)
rownames(psi) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION") # Provide Row Names
colnames(psi) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION") # Provide Column Names
psi # Show the empty matrix just constructed.


# These lines construct the vectors of regression coefficients used to build the Psi matrix.
# psi.r1 refers to Psi row 1; psi.r2 refers to Psi row 2; etc.
# You may need to insert more rows/columns depending on YOUR model.
# Note: Do NOT include interaction terms in the Beta matrix. Those will go into the Phi matrix.
# Note: Be sure to include covariances in both rows where they occur, e.g., 0.181 for TRUGOV and SYSJUS appears in rows 1 AND 2
psi.r1 <- c(.572, .181, 0, 0, 0, 0, 0)  # SYSJUS has an error variance of .572, and its error covaried with the error of TRUGOV at .181
psi.r2 <- c(.181, .746, 0, 0, 0, 0, 0)  # TRUGOV has an error variance of .746, and its error covaried with the error of SYSJUS at .181
psi.r3 <- c(0, 0, .622, .193, 0, 0, 0)  # PAR has an error variance of .622, and its error covaried with the error of PEER at .193
psi.r4 <- c(0, 0, .193, .294, 0, 0, 0)  # PEER has an error variance of .294, and its error covaried with the error of PAR at .193
psi.r5 <- c(0, 0, 0, 0, .263, 0, 0)     # PEER has an error variance of .263, and its error did not covary with anything else.
psi.r6 <- c(0, 0, 0, 0, 0, .116, 0)     # ACTID has an error variance of .116, and its error did not covary with anything else.
psi.r7 <- c(0, 0, 0, 0, 0, 0, .045)     # ACTION has an error variance of .045, and its error did not covary with anything else.

psi <- matrix(c(psi.r1, psi.r2, psi.r3, psi.r4, psi.r5, psi.r6, psi.r7), 7, 7, byrow = TRUE)
rownames(psi) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION")
colnames(psi) <- c("SYSJUS", "TRUGOV", "PAR", "PEER", "PEFF", "ACTID", "ACTION") 
psi # Check to make sure your entries are correct.







### Construct the Phi matrix

# The Phi matrix includes only variances and covariances between exogenous variables, INCLUDING interactions.
# It should be read as the covariance between row and column. A variable covarying with itself is its variance.
# For example, the first row first column reads PARED covarying with PARED, i.e., the variance of SYSJUS.
# Variance can be found under the VARIANCES section of Mplus output. This value is 1.
# The first row second column reads the covariance between PARED and SOpps. This value is -.062, and is entered at this location.
# Covariances between exogenous variables will be found in WITH statements. 
# Note: If no WITH statement exists for values in the Psi matrix, you may assume the entry value is zero.
# Values are entered into vectors row-wise.
phi <- matrix(0, 7, 7)
rownames(phi) <- c("PAERD", "SOpps", "Gender", "PARXACTI", "PARXPEFF", "PEXACTID", "PEXPEFF") # Provide Row Names
colnames(phi) <- c("PARED", "SOpps", "Gender", "PARXACTI", "PARXPEFF", "PEXACTID", "PEXPEFF") # Provide Column Names
phi # Show the empty matrix just constructed.

# First, build an empty Phi matrix to be used as a reference when inserting values into vectors.
# The 0 creates a matrix of all zeros, the two 7s give 7 rows and 7 columns.
# Note: The number of rows and columns may be different for each model. These numbers will need to be adjusted to fit YOUR model.
# Note: This may not necessarily be a square matrix like Psi. It is square in this example by coincidence.
phi.r1 <- c(1, -.062, 0, 0, 0, 0, 0)  # PARED has a variance of 1, and it covaries with SOpps at -.062
phi.r2 <- c(-0.062, 1, 0, 0, 0, 0, 0) # SOpps has a variance of 1, and it covaries with PARED at -.062
phi.r3 <- c(0, 0, 1, 0, 0, 0, 0)      # GENDER has a variance of 1 and covaries with no other variable
phi.r4 <- c(0, 0, 0, 0, 0, 0, 0)      # The interaction term between PAR and ACTID has a variance of zero and covaries with no other variable.
phi.r5 <- c(0, 0, 0, 0, 0, 0, 0)      # The interaction term between PAR and PEFF has a variance of zero and covaries with no other variable.
phi.r6 <- c(0, 0, 0, 0, 0, 0, 0)      # The interaction term between PEER and ACTID has a variance of zero and covaries with no other variable.
phi.r7 <- c(0, 0, 0, 0, 0, 0, 0)      # The interaction term between PEER and PEFF has a variance of zero and covaries with no other variable.

phi <- matrix(c(phi.r1, phi.r2, phi.r3, phi.r4, phi.r5, phi.r6, phi.r7), 7, 7, byrow = TRUE)
rownames(phi) <- c("PAERD", "SOpps", "Gender", "PARXACTI", "PARXPEFF", "PEXACTID", "PEXPEFF")
colnames(phi) <- c("PARED", "SOpps", "Gender", "PARXACTI", "PARXPEFF", "PEXACTID", "PEXPEFF")
phi # Check to make sure your entries are correct.




# Once the matrices are build, the variance of endogenous latent variables can be calculated according to the expression found in 
# Frisby & Diemer, 2020.

# The following code does not need to be adjusted.

Imat <- diag(nrow(beta))  # Create the identity matrix to match the dimensions of the Beta matrix you created.
sol <- solve((Imat - beta))%*%(gamma%*%phi%*%t(gamma) + psi)%*%t(solve((Imat - beta))) # Calculate Frisby & Diemer 2020 solution.
diag(sol) # See variance calculations for all endogenous latent variables. 
sqrt(diag(sol)) # See standard deviation calculations for all endogenous latent variables.






##########################################################################
# TECHNICAL NOTE RE: LIMITATIONS REGARDING PRODUCT VARIANCE.
# If you know the variance of the produce term(s) per Isserlis's Theorem (see paper), you can enter them...
# ... on the corresponding diagonal of the phi matrix. This will incorprate this additional bit of variance detailed in the limitations.
# e.g., phi.r4 <- c(0, 0, 0, var.par.act, 0, 0, 0), where var.par.act is the variance of the product term of PAR and ACTID.
# var.par.act can be calculated by extracting and multiplying components of the above solution.
# e.g., the following code will provide the variance of the product term of PAR and ACTID
# vr.par <- 0.71931530
# vr.actid <- 0.2488882744
# cv.par.act <- 0.13674373
# vr.par.act <- vr.par*vr.actid*(cv.par.act)^2
##########################################################################