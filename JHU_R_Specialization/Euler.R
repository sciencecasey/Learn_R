#Use Euler's Method to solve Newton's Law of Cooling after specific times
#Newton's Law: dT/dt = K[M(t) - T(t)]; 
# K= constant, M(t) = temp of Medium at time t, T(t)= temp of body at time T
# h = step value
# Euler's Method: Find next value at time t+h
  
#given constants
K = .03 #per minute
medTemp= 291 #kelvins
#initial temperature of the body
bodyTempVector = c(355) #kelvins
#step
h = .1 #minute

#find value after 60 minutes
for(i in 1:600){
   newTemp = bodyTempVector[i]+h*(K*(medTemp-bodyTempVector[i]))
   bodyTempVector = c(bodyTempVector, newTemp)
}
rm(newTemp)
