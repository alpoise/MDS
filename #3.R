MDScheck1<-function(Z,n){
	for(i in 1:4){
		for(j in 1:4){
			if( det(Z[(n*(i-1)+1):(n*i),(n*(i-1)+1):(n*i)])%%2==0 ){
				return(0)
			}
		}
	}
	return(1)
}
###
MDScheck2<-function(Z,n){
	for(i in 1:3){
		for(ii in i:4){
			for(j in 1:3){
				for(jj in j:4){
				Zij<-Z[(n*(i-1)+1):(n*i),(n*(j-1)+1):(n*j)]
				Ziij<-Z[(n*(ii-1)+1):(n*ii),(n*(j-1)+1):(n*j)]
				Zijj<-Z[(n*(i-1)+1):(n*i),(n*(jj-1)+1):(n*jj)]
				Ziijj<-Z[(n*(ii-1)+1):(n*ii),(n*(jj-1)+1):(n*jj)]
				Ztemp1<-rbind(Zij,Ziij)
				Ztemp2<-rbind(Zijj,Ziijj)
				Ztemp<-cbind(Ztemp1,Ztemp2)
					if( det(Ztemp)%%2==0){
						return(0)
					}
				}
			}
		}
	}
	return(1)
}
##
MDScheck3<-function(Z,n){
	for(i in 1:4){
		for(j in 1:4){
			if( det(Z[-((n*(i-1)+1):(n*i)),-((n*(i-1)+1):(n*i))])%%2==0 ){
				return(0)
			}
		}
	}
	return(1)
}
##
MDScheck4<-function(Z){
	if(det(Z)%%2==0){
		return(0)
	}
	return(1)
}
##
M2<-read.csv("M2.csv")
M<-as.matrix(M2[,-1])
count<-0
for(i in 1:16){
	for(j in 1 :16){
		if(M[i,j]==0){
			count<-count+1
		  M[i,j]<-1
				if(MDScheck1(M,4)==1 && MDScheck2(M,4)==1 && MDScheck3(M,4)==1 && MDScheck4(M)==1){
					print("Hello,PKU!")
					print(i,j)
				}
			M[i,j]<-0
		}
	}
}
print(count)
