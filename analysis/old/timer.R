# Start the clock!
ptm <- proc.time()
innertol<-1e-06
kchoose=1 
meth_choose=3
source('01_00_sim_gmm_accel.R')
# Stop the clock
accel_time<-proc.time() - ptm

# current fastest setup:
#kchoose=1 
#meth_choose=3#