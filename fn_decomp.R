################################################################################
# LE Decomposition (Arriaga)

# This is a very simple life table function
# The input values are
# 1. a vector of single age-specific death rates
# with a length of 111 (age 0 to age 100+).
# 2. an indicator for the sex of either Male ("m") or
# Female ("f") you are computing.

life.table<-function(mx,sex){
  N<-length(mx)
  ax<-rep(0.5,N)
  # We assume the people who died during the age
  # interval lived half of the year. A very common
  # assumption.
  if(sex=="m"){
    ax[1]<-ifelse(mx[1]<0.107,0.045+mx[1]*2.684,0.330)}
  if(sex=="f"){
    ax[1]<-ifelse(mx[1]<0.107,0.053+2.800*mx[1],0.350)
  }
  # a "ifelse" function to compute the ax for the infant
  # age since the distribution of death for infants are
  # very different from adult years
  qx<-mx/(1+(1-ax)*mx)
  # Chiang's conversion from age-specific mortality
  # to age-specific probability of death
  qx[N] <- 1
  # Everyone dies in a life table eventually
  # with the last value of qx = 1
  px<-1-qx
  lx<-100000
  for(y in 1:(N-1)){
    lx[y+1]<-lx[y]*px[y]
  }
  lx <- ifelse(lx<0,0,lx)
  dx<-lx*qx
  # Calculating the death distribution of the
  # life table
  Lx<-lx[-1]+ax[-N]*dx[-N]
  Lx[N]<-ifelse(mx[N]>0,lx[N]/mx[N],0)
  # Person-year lived within each age interval.
  Tx<-c()
  for(y in 1:N){
    Tx[y]<-sum(Lx[y:N])
  }
  ex<-Tx/lx
  # Calculate life expectancy at each age
  Age<-0:100
  ALL<-data.frame(Age,mx,lx,dx,Lx,Tx,ex)
  return(ALL)
}

# The Arriaga decomposition function
# The input consists of two sets of vector containing
# age-specific mortality rates.
# At the same time, an indicator of sex of
# either Male ("m") or Female ("f")
# needs to be specified.
# A separate indicator of whether the results comes in
# contributions by age (breakdown = F)
# or contributions by age and direct & indirect+interaction effect
# (breakdown = T).
arriaga <- function(nmx1,nmx2,sex,breakdown=F){
  LT1 <- life.table(nmx1,sex)
  LT2 <- life.table(nmx2,sex)
  # Creating the two life tables
  lx1 <- LT1$lx
  lx2 <- LT2$lx
  Lx1 <- LT1$Lx
  Lx2 <- LT2$Lx
  Tx1 <- LT1$Tx
  Tx2 <- LT2$Tx
  # Specifying the life table statistics we
  # need to perform the Arriaga decomposition.
  if(breakdown==FALSE){
    delta <- rep(0,101)
    for (i in 1:100){
      delta[i] <-
        (lx1[i]/lx1[1])*(Lx2[i]/lx2[i]-Lx1[i]/lx1[i])+
        (Tx2[i+1]/lx1[1])*(lx1[i]/lx2[i]-lx1[i+1]/lx2[i+1])
      delta[101] <-
        (lx1[101]/lx1[1])*(Tx2[101]/lx2[101]-Tx1[101]/lx1[101])
    }
  }
  if(breakdown==T){
    direct <- rep(0,101)
    indirect <- rep(0,101)
    for (i in 1:100){
      direct[i] <-
        (lx1[i]/lx1[1])*(Lx2[i]/lx2[i]-Lx1[i]/lx1[i])
      direct[101] <-
        (lx1[101]/lx1[1])*(Tx2[101]/lx2[101]-Tx1[101]/lx1[101])
      indirect[i] <-
        (Tx2[i+1]/lx1[1])*(lx1[i]/lx2[i]-lx1[i+1]/lx2[i+1])
    }
    delta <- data.frame(
      age=0:100,
      direct = direct,
      indirect = indirect
    )
  }
  # depends on whether you want to separate the direct and indirect
  # effect, the "delta" results can either be a vector of contributions
  # or a data.frame containing the direct and indirect components of
  # the decomposition.
  return(delta)
}

# Same as above but allows comparison between sex
arriaga2 <- function(nmx1,nmx2,sex1,sex2,breakdown=F){
  LT1 <- life.table(nmx1,sex1)
  LT2 <- life.table(nmx2,sex2)
  # Creating the two life tables
  lx1 <- LT1$lx
  lx2 <- LT2$lx
  Lx1 <- LT1$Lx
  Lx2 <- LT2$Lx
  Tx1 <- LT1$Tx
  Tx2 <- LT2$Tx
  # Specifying the life table statistics we
  # need to perform the Arriaga decomposition.
  if(breakdown==FALSE){
    delta <- rep(0,101)
    for (i in 1:100){
      delta[i] <-
        (lx1[i]/lx1[1])*(Lx2[i]/lx2[i]-Lx1[i]/lx1[i])+
        (Tx2[i+1]/lx1[1])*(lx1[i]/lx2[i]-lx1[i+1]/lx2[i+1])
      delta[101] <-
        (lx1[101]/lx1[1])*(Tx2[101]/lx2[101]-Tx1[101]/lx1[101])
    }
  }
  if(breakdown==T){
    direct <- rep(0,101)
    indirect <- rep(0,101)
    for (i in 1:100){
      direct[i] <-
        (lx1[i]/lx1[1])*(Lx2[i]/lx2[i]-Lx1[i]/lx1[i])
      direct[101] <-
        (lx1[101]/lx1[1])*(Tx2[101]/lx2[101]-Tx1[101]/lx1[101])
      indirect[i] <-
        (Tx2[i+1]/lx1[1])*(lx1[i]/lx2[i]-lx1[i+1]/lx2[i+1])
    }
    delta <- data.frame(
      age=0:100,
      direct = direct,
      indirect = indirect
    )
  }
  # depends on whether you want to separate the direct and indirect
  # effect, the "delta" results can either be a vector of contributions
  # or a data.frame containing the direct and indirect components of
  # the decomposition.
  return(delta)
}
