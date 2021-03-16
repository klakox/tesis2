imputar<-function(MC,DC,metodo){
  
  library(MASS)
  library(condMVNorm)
  library(norm)
  library(corpcor)
  
  mu<-DC%>%
    dplyr::filter(weekdays(Fecha)=="lunes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    apply(2,mean)
  mu2<-DC%>%
    dplyr::filter(weekdays(Fecha)=="martes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    apply(2,mean)
  mu3<-DC%>%
    dplyr::filter(weekdays(Fecha)=="miércoles")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    apply(2,mean)
  mu4<-DC%>%
    dplyr::filter(weekdays(Fecha)=="jueves")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    apply(2,mean)
  mu5<-DC%>%
    dplyr::filter(weekdays(Fecha)=="viernes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    apply(2,mean)
  mu6<-DC%>%
    dplyr::filter(weekdays(Fecha)=="sábado")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    apply(2,mean)
  mu7<-DC%>%
    dplyr::filter(weekdays(Fecha)=="domingo")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    apply(2,mean)
  
  if(metodo=="MV"){
  
  s<-DC%>%
    dplyr::filter(weekdays(Fecha)=="lunes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov()
  s2<-DC%>%
    dplyr::filter(weekdays(Fecha)=="martes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov()
  s3<-DC%>%
    dplyr::filter(weekdays(Fecha)=="miércoles")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov()
  s4<-DC%>%
    dplyr::filter(weekdays(Fecha)=="jueves")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov()
  s5<-DC%>%
    dplyr::filter(weekdays(Fecha)=="viernes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov()
  s6<-DC%>%
    dplyr::filter(weekdays(Fecha)=="sábado")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov()
  s7<-DC%>%
    dplyr::filter(weekdays(Fecha)=="domingo")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov()
  }else{
  if(metodo=="ES"){
  s<-DC%>%
    dplyr::filter(weekdays(Fecha)=="lunes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov.shrink(lambda=0.2, lambda.var=0)
 
  s2<-DC%>%
    dplyr::filter(weekdays(Fecha)=="martes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov.shrink(lambda=0.2, lambda.var=0)
  
  s3<-DC%>%
    dplyr::filter(weekdays(Fecha)=="miércoles")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov.shrink(lambda=0.2, lambda.var=0)

  s4<-DC%>%
    dplyr::filter(weekdays(Fecha)=="jueves")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov.shrink(lambda=0.2, lambda.var=0)
  
  s5<-DC%>%
    dplyr::filter(weekdays(Fecha)=="viernes")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov.shrink(lambda=0.2, lambda.var=0)
  
  s6<-DC%>%
    dplyr::filter(weekdays(Fecha)=="sábado")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov.shrink(lambda=0.2, lambda.var=0)
  
  s7<-DC%>%
    dplyr::filter(weekdays(Fecha)=="domingo")%>%
    as.data.frame()%>%
    dplyr::select(-Fecha)%>%
    cov.shrink(lambda=0.2, lambda.var=0)
  }
  }
    
 
  dias_c<-substr(weekdays(MC$Fecha),1,3)
  minp<-as.data.frame(MC[,-1])
  
  pos_diaf<-c()  
  
  
  
  for(i in 1:nrow(minp)){
    
    nl<-which(is.na(minp[i,]))
    
    
    
    if(length(nl)>0){
      pos_diaf[i]<-i
      
      vectorimp<-as.numeric(minp[i,])
      
      if(dias_c[i]=="lun"){
        
        
        est<-condMVN(mu, s, dependent.ind=which(is.na(vectorimp)), given.ind=which(!is.na(vectorimp)) , X.given=na.omit(vectorimp), 
                     check.sigma=FALSE)
        
        gen<-mvrnorm(n = 1, est$condMean, est$condVar)
        
        p=0
        
        while(p<100 & length(which(gen<0))!=0 ){
          
          gen<-mvrnorm(n = 1, est$condMean, est$condVar)
          
          p=p+1 
          
        }
        
        
        minp[i,which(is.na(vectorimp))]<-gen
        
        
      }else{
        if(dias_c[i]=="mar"){
          
          
          est<-condMVN(mu2, s2, dependent.ind=which(is.na(vectorimp)), given.ind=which(!is.na(vectorimp)) , X.given=na.omit(vectorimp), 
                       check.sigma=FALSE)
          
          gen<-mvrnorm(n = 1, est$condMean, est$condVar)
          
          p=0
          
          while(p<100 & length(which(gen<0))!=0 ){
            
            gen<-mvrnorm(n = 1, est$condMean, est$condVar)
            
            p=p+1 
            
          }
          
          
          minp[i,which(is.na(vectorimp))]<-gen
          
          
        }else{
          if(dias_c[i]=="mié"){
            
            
            est<-condMVN(mu3, s3, dependent.ind=which(is.na(vectorimp)), given.ind=which(!is.na(vectorimp)) , X.given=na.omit(vectorimp), 
                         check.sigma=FALSE)
            
            gen<-mvrnorm(n = 1, est$condMean, est$condVar)
            
            p=0
            
            while(p<100 & length(which(gen<0))!=0 ){
              
              gen<-mvrnorm(n = 1, est$condMean, est$condVar)
              
              p=p+1 
              
            }
            
            
            minp[i,which(is.na(vectorimp))]<-gen
            
          }else{
            if(dias_c[i]=="jue"){
              
              
              est<-condMVN(mu4, s4, dependent.ind=which(is.na(vectorimp)), given.ind=which(!is.na(vectorimp)) , X.given=na.omit(vectorimp), 
                           check.sigma=FALSE)
              
              gen<-mvrnorm(n = 1, est$condMean, est$condVar)
              
              p=0
              
              while(p<100 & length(which(gen<0))!=0 ){
                
                gen<-mvrnorm(n = 1, est$condMean, est$condVar)
                
                p=p+1 
                
              }
              
              
              minp[i,which(is.na(vectorimp))]<-gen
              
              
            }else{
              
              if(dias_c[i]=="vie"){
                
                
                
                est<-condMVN(mu5, s5, dependent.ind=which(is.na(vectorimp)), given.ind=which(!is.na(vectorimp)) , X.given=na.omit(vectorimp), 
                             check.sigma=FALSE)
                
                gen<-mvrnorm(n = 1, est$condMean, est$condVar)
                
                p=0
                
                while(p<100 & length(which(gen<0))!=0 ){
                  
                  gen<-mvrnorm(n = 1, est$condMean, est$condVar)
                  
                  p=p+1 
                  
                }
                
                
                minp[i,which(is.na(vectorimp))]<-gen
                
                
                
              }else{
                
                if(dias_c[i]=="sáb"){
                  
                  
                  est<-condMVN(mu6, s6, dependent.ind=which(is.na(vectorimp)), given.ind=which(!is.na(vectorimp)) , X.given=na.omit(vectorimp), 
                               check.sigma=FALSE)
                  
                  gen<-mvrnorm(n = 1, est$condMean, est$condVar)
                  
                  p=0
                  
                  while(p<100 & length(which(gen<0))!=0 ){
                    
                    gen<-mvrnorm(n = 1, est$condMean, est$condVar)
                    
                    p=p+1 
                    
                  }
                  
                  
                  minp[i,which(is.na(vectorimp))]<-gen
                  
                  
                  
                }else{
                  
                  if(dias_c[i]=="dom"){
                    
                    
                    est<-condMVN(mu7, s7, dependent.ind=which(is.na(vectorimp)), given.ind=which(!is.na(vectorimp)) , X.given=na.omit(vectorimp), 
                                 check.sigma=FALSE)
                    
                    gen<-mvrnorm(n = 1, est$condMean, est$condVar)
                    
                    p=0
                    
                    while(p<100 & length(which(gen<0))!=0 ){
                      
                      gen<-mvrnorm(n = 1, est$condMean, est$condVar)
                      
                      p=p+1 
                      
                    }
                    
                    
                    minp[i,which(is.na(vectorimp))]<-gen
                    
                    
                  }
                  
                  
                }
                
                
              }
              
              
            }
            
            
            
          }
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  Fecha<-MC[,1]
  
  return(cbind(Fecha, minp))
}
