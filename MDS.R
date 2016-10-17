#!!______DATA_PREPARATION______!!#
#
#---9-I framework 
#
n<-4
Mytry<-matrix(0, ncol = 4*n, nrow = 4*n)
Mytry[1:n,1:n]<-diag(x = 1, n,n)
Mytry[(n+1):(2*n),1:n]<-diag(x = 1, n,n)
Mytry[(2*n+1):(3*n),(1:n)]<-diag(x = 1, n,n)
Mytry[(2*n+1):(3*n),(n+1):(2*n)]<-diag(x = 1, n,n)
Mytry[(3*n+1):(4*n),(n+1):(2*n)]<-diag(x = 1, n,n)
Mytry[(3*n+1):(4*n),(2*n+1):(3*n)]<-diag(x = 1, n,n)
Mytry[(3*n+1):(4*n),(3*n+1):(4*n)]<-diag(x = 1, n,n)
Mytry[(n+1):(2*n),(2*n+1):(3*n)]<-diag(x = 1, n,n)
Mytry[(1:n),(3*n+1):(4*n)]<-diag(x = 1, n,n)
#
#Using A~G to simplfy the representation
#
A<-matrix(0,ncol = n, nrow = n)
B<-matrix(0,ncol = n, nrow = n)
C<-matrix(0,ncol = n, nrow = n)
D<-matrix(0,ncol = n, nrow = n)
E<-matrix(0,ncol = n, nrow = n)
J<-matrix(0,ncol = n, nrow = n)
G<-matrix(0,ncol = n, nrow = n)

#---Construct the skeleton of A~G 
#
H<-list()
H[[1]]<-rbind(c(0,1,0,0),c(0,0,1,0),c(0,0,0,1),c(1,0,0,0))
H[[4]]<-rbind(c(0,0,0,1),c(1,0,0,0),c(0,1,0,0),c(0,0,1,0))
H[[2]]<-rbind(c(0,0,1,0),c(0,0,0,1),c(0,1,0,0),c(1,0,0,0))
H[[5]]<-rbind(c(0,0,0,1),c(0,0,1,0),c(1,0,0,0),c(0,1,0,0))
H[[3]]<-rbind(c(0,1,0,0),c(0,0,0,1),c(1,0,0,0),c(0,0,1,0))
H[[6]]<-rbind(c(0,0,1,0),c(1,0,0,0),c(0,0,0,1),c(0,1,0,0))
#
#!!____CYCLE____!!#
zero<-exp(-10)
#
#!!______FUNCTION_______!!#
#
checkMDS<-function(Y,n){
  ##--n is the ncol of Y--##
  ##--only check for 3x3 and 4x4--##
  flag<-1
  for(i in 1:4){
    for(j in 1:4){
      if(round(det(Y[-((n*i-n+1):(n*i)),-((n*j-n+1):(n*j))]))%%2<zero){
        flag<-0
        return(flag)
      }
    }
  }
  if(round(det(Y))%%2<zero){
    flag<-0
  }
  return(flag)
}
#
iterinverse<-function(x){
  if(x==1) return(4)
  if(x==2) return(5)
  if(x==3) return(6)
  if(x==4) return(1)
  if(x==5) return(2)
  if(x==6) return(3)
}
#
fullcycle<-function(SAin,SB,SC,SD,SE,SJ,SG,SMytry){
#without losing generality, let skeleton(A) = H3.
#(ai,aj),...,(gi,gj)is the #-location.
#bhi,...,bhj is the h-class.
  sceklist<-list()
  ceknum<-0#parameter standing for the count of checkMDS
  cycnum<-0#parameter standing for the count of possible A~G despite of checkMDS
  gi<-0
  gj<-0
  I<-diag(x=1,4,4)
  #Ain
for(ahi in 1:6){#the h-class of Ain
  SAin<-H[[ahi]]
  for(ai in 1:n){###ai is the "#"-location of A-inverse
    for(aj in 1:n){
      if(SAin[ai,aj]<1){
        SAin[ai,aj]<-1
        #B
        for(bhi in setdiff((1:6),ahi)){#the h-class of B
          SB<-H[[bhi]]
          for(bi in setdiff((1:n),ai)){
            for(bj in setdiff((1:n),aj)){
              if(SB[bi,bj]<1){
                SB[bi,bj]<-1
                #E
                for (ehi in setdiff((1:6),c(ahi,bhi))){
                  SE<-H[[ehi]]
                  for(ei in setdiff((1:n),c(ai,bi))){
                    for(ej in setdiff((1:n),c(aj,bj))){
                      if(SE[ei,ej]<1){
                        SE[ei,ej]<-1
                        #J
                        for (Jhi in setdiff((1:6),c(ahi,ehi))){
                          SJ<-H[[Jhi]]
                          for(Ji in setdiff((1:n),c(ai,ei))){
                            for(Jj in setdiff((1:n),c(aj,ej))){
                              if(SJ[Ji,Jj]<1){
                                SJ[Ji,Jj]<-1
                                if(round(det(SE+SB%*%SJ))%%2!=0){
                                #C
                                for (chi in setdiff((1:6),c(ahi,Jhi,iterinverse(bhi)))){
                                  SC<-H[[chi]]
                                  for(ci in setdiff((1:n),c(ai,Ji))){
                                    for(cj in setdiff((1:n),c(aj,Jj))){
                                      if(SC[ci,cj]<1){
                                        SC[ci,cj]<-1
                                        if(round(det(SJ+SC%*%SE))%%2!=0){
                                        #D
                                        for (dhi in setdiff((1:6),c(ahi,chi,iterinverse(ehi)))){
                                          SD<-H[[dhi]]
                                          for(di in setdiff((1:n),c(ai,ci))){
                                            for(dj in setdiff((1:n),c(aj,cj))){
                                              if(SD[di,dj]<1){
                                                SD[di,dj]<-1
                                                if(round(det(SC+SJ%*%SD))%%2!=0){
                                                #G
                                                for (ghi in setdiff((1:6),c(ahi,dhi,bhi,iterinverse(Jhi)))){
                                                  SG<-H[[ghi]]
                                                  for(gi in setdiff((1:n),c(ai,bi,di))){
                                                    for(gj in setdiff((1:n),c(aj,bj,dj))){
                                                      if(SG[gi,gj]<1){
                                                        SG[gi,gj]<-1
                                                        cycnum<-cycnum+1
                                                        #####DONE THE VALUE,NEXT THE CHECK
if((round(det(I+SB%*%SC))%%2!=0)&&(round(det(I+SE%*%SD))%%2!=0)&&(round(det(I+SG%*%SJ))%%2!=0)&&(round(det(SD+SG%*%SC))%%2!=0)&&(round(det(SB+SE%*%SG))%%2!=0)&&(round(det(SG+SD%*%SB))%%2!=0))
                                                        {
                                                          #---
                                                          SMytry[(3*n+1):(4*n),1:n]<-solve(SAin)
                                                          SMytry[(n+1):(2*n),(n+1):(2*n)]<-SB
                                                          SMytry[(2*n+1):(3*n),(2*n+1):(3*n)]<-SC
                                                          SMytry[1:n,(2*n+1):(3*n)]<-SD
                                                          SMytry[(n+1):(2*n),(3*n+1):(4*n)]<-SE
                                                          SMytry[(2*n+1):(3*n),(3*n+1):(4*n)]<-SJ
                                                          SMytry[1:n,(n+1):(2*n)]<-SG
                                                          ceknum<-ceknum+1
                                                          sceklist[[ceknum]]<-SMytry
                                                          #
                                                          if(checkMDS(SMytry,n)!=0){
                                                            print("Hello PKU!")
                                                            print(ceknum)
                                                            return(SMytry)
                                                          }
                                                        }
                                                        ##back to the start point.
                                                        SG[gi,gj]<-0
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  SD[di,dj]<-0}
                                                }
                                              }
                                            }
                                          }
                                        SC[ci,cj]<-0}
                                      }
                                    }
                                  }
                                }
                              SJ[Ji,Jj]<-0}
                            }
                          }
                        }
                      SE[ei,ej]<-0}
                    }
                  }
                }
              SB[bi,bj]<-0}
            }
          }
        }
      SAin[ai,aj]<-0}
    }
  }
  print(cycnum)
  print(ceknum)
  print("pity")
}
  return(sceklist)
}
###
#to fix the bugs in det
###
ceklist<-fullcycle(A,B,C,D,E,J,G,Mytry)
#
###NO RESULY OF#7
##