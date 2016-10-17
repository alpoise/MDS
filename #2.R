#!!______DATA_PREPARATION______!!#
#
#---8-I framework 
#
n<-4
I<-diag(x=1,n,n)
Mytry<-matrix(0, ncol = 4*n, nrow = 4*n)
Mytry[1:n,(2*n+1):(3*n)]<-I
Mytry[1:n,(3*n+1):(4*n)]<-I
Mytry[(n+1):(2*n),(n+1):(2*n)]<-I
Mytry[(n+1):(2*n),(3*n+1):(4*n)]<-I
Mytry[(2*n+1):(3*n),1:n]<-I
Mytry[(2*n+1):(3*n),(2*n+1):(3*n)]<-I
Mytry[(3*n+1):(4*n),1:n]<-I
Mytry[(3*n+1):(4*n),(n+1):(2*n)]<-I
#
#Using A~H to simplfy the representation
#
A<-matrix(0,ncol = n, nrow = n)
B<-matrix(0,ncol = n, nrow = n)
C<-matrix(0,ncol = n, nrow = n)
D<-matrix(0,ncol = n, nrow = n)
E<-matrix(0,ncol = n, nrow = n)
FF<-matrix(0,ncol = n, nrow = n)
G<-matrix(0,ncol = n, nrow = n)
H<-matrix(0,ncol = n, nrow = n)

#---Construct the skeleton of A~G 
#
L<-list()
#OL<-list()
#iterl<-1
L[[1]]<-rbind(c(0,1,0,0),c(0,0,1,0),c(0,0,0,1),c(1,0,0,0))
L[[4]]<-rbind(c(0,0,0,1),c(1,0,0,0),c(0,1,0,0),c(0,0,1,0))
L[[2]]<-rbind(c(0,0,1,0),c(0,0,0,1),c(0,1,0,0),c(1,0,0,0))
L[[5]]<-rbind(c(0,0,0,1),c(0,0,1,0),c(1,0,0,0),c(0,1,0,0))
L[[3]]<-rbind(c(0,1,0,0),c(0,0,0,1),c(1,0,0,0),c(0,0,1,0))
L[[6]]<-rbind(c(0,0,1,0),c(1,0,0,0),c(0,0,0,1),c(0,1,0,0))
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
      if(det(Y[-((n*i-n+1):(n*i)),-((n*j-n+1):(n*j))])%%2 < zero){
        flag<-0
        return(flag)
      }
    }
  }
  if(det(Y)%%2 < zero){
    flag <- 0
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
fullcycle<-function(SA,SB,SC,SD,SE,SF,SG,SH,SMytry){
  #(ai,aj),...,(gi,gj)is the #-location.
  #ali,alij,...hli,hlj is the L-class.
  count<-0
  countt<-0
  #A
  for(ali in 1:1){#the h-class of SA #
    #without losing generality, let skeleton(A) = L1.
    SA<-L[[ali]]
    for(ai in 1:n){###ai is the "#"-location of A-inverse
      for(aj in 1:n){
        if(SA[ai,aj]<1){
          SA[ai,aj]<-1
          #B
          for(bli in setdiff((1:6),ali)){
            SB<-L[[bli]]
            for(bi in setdiff((1:n),ai)){
              for(bj in setdiff((1:n),aj)){
                if(SB[bi,bj]<1){
                  SB[bi,bj]<-1
                  #C
                  for (cli in setdiff((1:6),bli)){
                    SC<-L[[cli]]
                    for(ci in setdiff((1:n),bi)){
                      for(cj in setdiff((1:n),bj)){
                        if(SC[ci,cj]<1){
                          SC[ci,cj]<-1
                          if(det(SB+SC%*%SA)%%2!=0){
                            #D
                            for (dli in setdiff((1:6),c(cli,iterinverse(ali)))){
                              SD<-L[[dli]]
                              for(di in setdiff((1:n),ci)){
                                for(dj in setdiff((1:n),cj)){
                                  if(SD[di,dj]<1){
                                    SD[di,dj]<-1
                                    if(det(I+SA%*%SD)%%2!=0 && det(SC+SB%*%SD)%%2!=0){
                                      #E
                                      for (eli in setdiff((1:6),c(dli,iterinverse(ali),iterinverse(bli)))){
                                        SE<-L[[eli]]
                                        for(ei in setdiff((1:n),di)){
                                          for(ej in setdiff((1:n),dj)){
                                            if(SE[ei,ej]<1){
                                              SE[ei,ej]<-1
                                              if(det(I+SA%*%SE)%%2!=0 && det(I+SB%*%SE)%%2!=0 && det(SD+SE%*%SC)%%2!=0){
                                                #F
                                                count<-count+1
                                                if(subcircle(SA,SB,SC,SD,SE,SF,SG,SH,SMytry,ai,aj,bi,bj,ci,cj,di,dj,ei,ej,ali,bli,cli,dli,eli) == 1)    
                                                  return(1)               
                                              }
                                              SE[ei,ej]<-0
                                            }
                                          }
                                        }
                                      }
                                    }
                                    SD[di,dj]<-0
                                  }
                                }
                              }
                            }
                          }
                          SC[ci,cj]<-0
                        }
                      }
                    }
                  }
                  SB[bi,bj]<-0
                }
              }
            }
          }
          SA[ai,aj]<-0
        }
      }
    }
  }
  print("pity")
  return(0)
  #print(countt)
}
###
subcircle<-function(SA,SB,SC,SD,SE,SF,SG,SH,SMytry,ai,aj,bi,bj,ci,cj,di,dj,ei,ej,ali,bli,cli,dli,eli){
  for (fli in setdiff((1:6),c(eli,iterinverse(ali),iterinverse(bli),iterinverse(cli)))){
    SF<-L[[fli]]
    for(fi in setdiff((1:n),ei)){
      for(fj in setdiff((1:n),ej)){
        if(SF[fi,fj] < 1){
          SF[fi,fj]<-1
          if(det(I+SA%*%SF)%%2!=0 && det(I+SB%*%SF)%%2!=0 && det(I+SC%*%SF)%%2!=0 && det(SE+SD%*%SF)%%2!=0){
            #G
            for (gli in setdiff((1:6),c(fli,iterinverse(bli),iterinverse(cli),iterinverse(dli)))){
              SG<-L[[gli]]
              for(gi in setdiff((1:n),fi)){
                for(gj in setdiff((1:n),fj)){
                  if(SG[gi,gj]<1){
                    SG[gi,gj]<-1
                    if(det(I+SB%*%SG)%%2!=0 && det(I+SC%*%SG)%%2!=0 && det(I+SD%*%SG)%%2!=0 && det(SF+SG%*%SE)%%2!=0){
                      #H
                      for (hli in setdiff((1:6),c(gli,ali,iterinverse(cli),iterinverse(dli),iterinverse(eli)))){
                        SH<-L[[hli]]
                        for(hi in setdiff((1:n),c(ai,gi))){
                          for(hj in setdiff((1:n),c(aj,gj))){
                            if(SH[hi,hj]<1){
                              SH[hi,hj]<-1
                              if(det(I+SC%*%SH)%%2!=0 && det(I+SD%*%SH)%%2!=0 && det(I+SH%*%SE)%%2!=0 && det(SG+SF%*%SH)%%2!=0 && det(SH+SA%*%SG)%%2!=0 && det(SA+SH%*%SB)%%2!=0)
                              {
                                #####DONE THE VALUE,NEXT THE CHECK
                                SMytry[1:n,1:n]<-SA
                                SMytry[1:n,(n+1):(2*n)]<-SH
                                SMytry[(n+1):(2*n),1:n]<-SB
                                SMytry[(n+1):(2*n),(2*n+1):(3*n)]<-SC
                                SMytry[(2*n+1):(3*n),(n+1):(2*n)]<-SG
                                SMytry[(2*n+1):(3*n),(3*n+1):(4*n)]<-SF
                                SMytry[(3*n+1):(4*n),(2*n+1):(3*n)]<-SD
                                SMytry[(3*n+1):(4*n),(3*n+1):(4*n)]<-SE
                                if(checkMDS(SMytry,n)!=0){
                                  print("Hello PKU!")
                                  write.csv(SMytry,"HelloPKU.csv")
                                }
                                write.csv(SMytry,"M2.csv")
                                return(1)
                              }
                              ##back to the start point.
                              SH[hi,hj]<-0
                            }
                          }
                        }
                      }
                    }
                    SG[gi,gj]<-0
                  }
                }
              }
            }
          }
          SF[fi,fj]<-0
        }
      }
    }
  }
  return(0)
}
###
output<-fullcycle(A,B,C,D,E,FF,G,H,Mytry)
##