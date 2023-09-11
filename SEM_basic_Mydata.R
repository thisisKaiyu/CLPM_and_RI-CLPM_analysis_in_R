# RP1_Kaiyu_Huang: SEM ANALYSES
# Prosocial behaviors (sharing behaviors and social mindfulness) and Well-being
# SEM specifications
# basic SEMs: CLPM, RI-CLPM 
# variables: Hypothesis1: a-sharing behaviors; m-wellbeing; 
#            Hypothesis2: a-social mindfulness; m-wellbeing.

setwd("~/Documents/OneDrive - Vrije Universiteit Amsterdam/Research Project 1/Paper/Data&Scripts")
## CLPM(T=4) Model 1 (free) ####
CLPM <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  

# ~ regression; ~~ variance

a1~~0*a1 #variance of a1 is 0 
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 

FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) 
FFm2 =~ 1*m2 # we set each factor loading to 1; but here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 
FFa1 =~ 1*a1 # define the latent variable f*a1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 


FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 


FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim

FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa 
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 

FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 

FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)
# FFa2~~Omegaam*FFm2 #covariance of FFa2 and FFm2 is Omegaam
# FFa3~~Omegaam*FFm3 
# FFa4~~Omegaam*FFm4 



FFm2~ betam*FFm1+gammam*FFa1 #FFm2 regresses onto FFm1 and FFa1 with betam (auto-regaressive;green arrows) and gammam (cross-lagged; red arrows) parameters
FFa2~ betaa*FFa1+gammaa*FFm1   

FFm3~ betam*FFm2+gammam*FFa2  
FFa3~ betaa*FFa2+gammaa*FFm2

FFm4~ betam*FFm3+gammam*FFa3 
FFa4~ betaa*FFa3+gammaa*FFm3

'
## CLPM(T=4) Model 2a (unilateral with m -> a = 0) ####

CLPM_uni_2a <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  


a1~~0*a1 #variance of a1 is 0
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 


# Ia~~0*FFa1 # covariance of Ia and FFa1 is fixed to 0 -> Ia, Im, FFa1, and FFm1 are orthogonal factors
# Ia~~0*FFm1 
# Im~~0*FFa1 
# Im~~0*FFm1
# 
# Ia=~1*a1+1*a2+1*a3+1*a4 #define Ia 
# Im=~1*m1+1*m2+1*m3+1*m4 #define Im 
# 
# Ia~0*1 #the intercept of Ia is 0?? 
# Im~0*1 #the intercept of Im is 0?? 
# Ia~~taua*Ia #variance of Ia is taux 
# Im~~taum*Im #variance of Im is tauy
# Ia~~tauam*Im #covariance of Ia and Im is tauam

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)

FFm2~ betam*FFm1+gammam*FFa1 #FFm2 regresses onto FFm1 and FFa1 with betam (green arrows) and gammam (red arrows)  parameters
FFa2~ betaa*FFa1+0*FFm2 # gammaa (red arrows: m -> a) is fixed as 0

FFm3~ betam*FFm2+gammam*FFa2 
FFa3~ betaa*FFa2+0*FFm2

FFm4~ betam*FFm3+gammam*FFa3 
FFa4~ betaa*FFa3+0*FFm3


FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 


FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 


# FFa2~~Omegaam*FFm2 #covariance of FFa2 and FFm2 is Omegaam
# FFa3~~Omegaam*FFm3 
# FFa4~~Omegaam*FFm4 


FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 


FFa1 =~ 1*a1 #define the latent variable f*a1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
'

# CLPM(T=4) Model 2b (unilateral with a -> m = 0) ####

CLPM_uni_2b <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  


a1~~0*a1 #variance of a1 is 0
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 


# Ia~~0*FFa1 # covariance of Ia and FFa1 is fixed to 0 -> Ia, Im, FFa1, and FFm1 are orthogonal factors
# Ia~~0*FFm1 
# Im~~0*FFa1 
# Im~~0*FFm1
# 
# Ia=~1*a1+1*a2+1*a3+1*a4 #define Ia 
# Im=~1*m1+1*m2+1*m3+1*m4 #define Im 
# 
# Ia~0*1 #the intercept of Ia is 0?? 
# Im~0*1 #the intercept of Im is 0?? 
# Ia~~taua*Ia #variance of Ia is taux 
# Im~~taum*Im #variance of Im is tauy
# Ia~~tauam*Im #covariance of Ia and Im is tauam

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)

FFm2~ betam*FFm1+0*FFa1 # FFm2 regresses onto FFm1 and FFa1 with betam (green arrows) and gammam (red arrows)  parameters;
FFa2~ betaa*FFa1+gammaa*FFm1 

FFm3~ betam*FFm2+0*FFa2 # gammam (red arrows: a -> m) is fixed as 0 
FFa3~ betaa*FFa2+gammaa*FFm2

FFm4~ betam*FFm3+0*FFa3 
FFa4~ betaa*FFa3+gammaa*FFm3



FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 


FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 

# FFa2~~Omegaam*FFm2 #covariance of FFa2 and FFm2 is Omegaam
# FFa3~~Omegaam*FFm3 
# FFa4~~Omegaam*FFm4 


FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 


FFa1 =~ 1*a1 #define the latent variable f*m1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
'

## CLPM(T=4) Model 3 (full model with constraint parameters) #### 
### both directional cross-lagged parameters are fixed to 0
CLPM_full <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  


a1~~0*a1 #variance of a1 is 0
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 


# Ia~~0*FFa1 # covariance of Ia and FFa1 is fixed to 0 -> Ia, Im, FFa1, and FFm1 are orthogonal factors
# Ia~~0*FFm1 
# Im~~0*FFa1 
# Im~~0*FFm1
# 
# Ia=~1*a1+1*a2+1*a3+1*a4 #define Ia 
# Im=~1*m1+1*m2+1*m3+1*m4 #define Im 
# 
# Ia~0*1 #the intercept of Ia is 0?? 
# Im~0*1 #the intercept of Im is 0?? 
# Ia~~taua*Ia #variance of Ia is taux 
# Im~~taum*Im #variance of Im is tauy
# Ia~~tauam*Im #covariance of Ia and Im is tauam

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)

FFm2~ betam*FFm1+0*FFa1 #FFm2 regresses onto FFm1 and FFa1 with betam (self-feedback) and gammam (coupling) parameters
FFa2~ betaa*FFa1+0*FFm1 #the green & red arrows

FFm3~ betam*FFm2+0*FFa2 #gammam is fixed as 0 
FFa3~ betaa*FFa2+0*FFm2 #gammaa is also fixed as 0

FFm4~ betam*FFm3+0*FFa3 
FFa4~ betaa*FFa3+0*FFm3


FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 


FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 


# FFa2~~Omegaam*FFm2 #covariance of FFa2 and FFm2 is Omegaam
# FFa3~~Omegaam*FFm3 
# FFa4~~Omegaam*FFm4 


FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 


FFa1 =~ 1*a1 #define the latent variable f*a1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
'




## RI-CLPM(T=4) Model 1 (free) ####
RICLPM <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  

# ~ regression; ~~ variance

a1~~0*a1 #variance of a1 is 0 
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 

FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) 
FFm2 =~ 1*m2 # we set each factor loading to 1; but here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 
FFa1 =~ 1*a1 # define the latent variable f*a1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 


FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 


FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim

FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa 
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 

FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 

FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)
# FFa2~~Omegaam*FFm2 #covariance of FFa2 and FFm2 is Omegaam
# FFa3~~Omegaam*FFm3 
# FFa4~~Omegaam*FFm4 



FFm2~ betam*FFm1+gammam*FFa1 #FFm2 regresses onto FFm1 and FFa1 with betam (auto-regaressive;green arrows) and gammam (cross-lagged; red arrows) parameters
FFa2~ betaa*FFa1+gammaa*FFm1   

FFm3~ betam*FFm2+gammam*FFa2  
FFa3~ betaa*FFa2+gammaa*FFm2

FFm4~ betam*FFm3+gammam*FFa3 
FFa4~ betaa*FFa3+gammaa*FFm3


Ia~~0*FFa1 # covariance of Ia and FFa1 is fixed to 0 -> Ia, Im, FFa1, and FFm1 are orthogonal factors
Ia~~0*FFm1 
Im~~0*FFa1 
Im~~0*FFm1

Ia=~1*a1+1*a2+1*a3+1*a4 #define Ia 
Im=~1*m1+1*m2+1*m3+1*m4 #define Im 

Ia~0*1 #the intercept of Ia is 0?? 
Im~0*1 #the intercept of Im is 0?? 
Ia~~taua*Ia #variance of Ia is taux 
Im~~taum*Im #variance of Im is tauy
Ia~~tauam*Im #covariance of Ia and Im is tauam


FFa1~gammaa*FFm1 #？？？

'


## RI-CLPM(T=4) Model 2a (unilateral with m -> a = 0) ####

RICLPM_uni_2a <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  


a1~~0*a1 #variance of a1 is 0
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 


Ia~~0*FFa1 # covariance of Ia and FFa1 is fixed to 0 -> Ia, Im, FFa1, and FFm1 are orthogonal factors
Ia~~0*FFm1 
Im~~0*FFa1 
Im~~0*FFm1

Ia=~1*a1+1*a2+1*a3+1*a4 #define Ia 
Im=~1*m1+1*m2+1*m3+1*m4 #define Im 

Ia~0*1 #the intercept of Ia is 0?? 
Im~0*1 #the intercept of Im is 0?? 
Ia~~taua*Ia #variance of Ia is taux 
Im~~taum*Im #variance of Im is tauy
Ia~~tauam*Im #covariance of Ia and Im is tauam

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)

FFm2~ betam*FFm1+gammam*FFa1 #FFm2 regresses onto FFm1 and FFa1 with betam (green arrows) and gammam (red arrows)  parameters
FFa2~ betaa*FFa1+0*FFm2 # gammaa (red arrows: m -> a) is fixed as 0

FFm3~ betam*FFm2+gammam*FFa2 
FFa3~ betaa*FFa2+0*FFm2

FFm4~ betam*FFm3+gammam*FFa3 
FFa4~ betaa*FFa3+0*FFm3


FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 


FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 


# FFa2~~Omegaam*FFm2 #covariance of FFa2 and FFm2 is Omegaam
# FFa3~~Omegaam*FFm3 
# FFa4~~Omegaam*FFm4 


FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 


FFa1 =~ 1*a1 #define the latent variable f*a1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
'

# RI-CLPM(T=4) Model 2b (unilateral with a -> m = 0) ####

RICLPM_uni_2b <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  


a1~~0*a1 #variance of a1 is 0
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 


Ia~~0*FFa1 # covariance of Ia and FFa1 is fixed to 0 -> Ia, Im, FFa1, and FFm1 are orthogonal factors
Ia~~0*FFm1 
Im~~0*FFa1 
Im~~0*FFm1

Ia=~1*a1+1*a2+1*a3+1*a4 #define Ia 
Im=~1*m1+1*m2+1*m3+1*m4 #define Im 

Ia~0*1 #the intercept of Ia is 0?? 
Im~0*1 #the intercept of Im is 0?? 
Ia~~taua*Ia #variance of Ia is taux 
Im~~taum*Im #variance of Im is tauy
Ia~~tauam*Im #covariance of Ia and Im is tauam

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)

FFm2~ betam*FFm1+0*FFa1 # FFm2 regresses onto FFm1 and FFa1 with betam (green arrows) and gammam (red arrows)  parameters;
FFa2~ betaa*FFa1+gammaa*FFm1 

FFm3~ betam*FFm2+0*FFa2 # gammam (red arrows: a -> m) is fixed as 0 
FFa3~ betaa*FFa2+gammaa*FFm2

FFm4~ betam*FFm3+0*FFa3 
FFa4~ betaa*FFa3+gammaa*FFm3



FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 


FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 

# FFa2~~Omegaam*FFm2 #covariance of FFa2 and FFm2 is Omegaam
# FFa3~~Omegaam*FFm3 
# FFa4~~Omegaam*FFm4 


FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 


FFa1 =~ 1*a1 #define the latent variable f*m1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
'

## RI-CLPM(T=4) Model 3 (full model with constraint parameters) #### 
### both directional cross-lagged parameters are fixed to 0
RICLPM_full <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  


a1~~0*a1 #variance of a1 is 0
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 


Ia~~0*FFa1 # covariance of Ia and FFa1 is fixed to 0 -> Ia, Im, FFa1, and FFm1 are orthogonal factors
Ia~~0*FFm1 
Im~~0*FFa1 
Im~~0*FFm1

Ia=~1*a1+1*a2+1*a3+1*a4 #define Ia 
Im=~1*m1+1*m2+1*m3+1*m4 #define Im 

Ia~0*1 #the intercept of Ia is 0?? 
Im~0*1 #the intercept of Im is 0?? 
Ia~~taua*Ia #variance of Ia is taux 
Im~~taum*Im #variance of Im is tauy
Ia~~tauam*Im #covariance of Ia and Im is tauam

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)

FFm2~ betam*FFm1+0*FFa1 #FFm2 regresses onto FFm1 and FFa1 with betam (self-feedback) and gammam (coupling) parameters
FFa2~ betaa*FFa1+0*FFm1 #the green & red arrows

FFm3~ betam*FFm2+0*FFa2 #gammam is fixed as 0 
FFa3~ betaa*FFa2+0*FFm2 #gammaa is also fixed as 0

FFm4~ betam*FFm3+0*FFa3 
FFa4~ betaa*FFa3+0*FFm3


FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 


FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 


# FFa2~~Omegaam*FFm2 #covariance of FFa2 and FFm2 is Omegaam
# FFa3~~Omegaam*FFm3 
# FFa4~~Omegaam*FFm4 


FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 


FFa1 =~ 1*a1 #define the latent variable f*a1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
'


