
server <- function(input, output) {
  
  imputar<-function(MC,DC,metodo){
    
    
    
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
  
  
  basepm<-read_delim("https://raw.githubusercontent.com/klakox/villarreal-ale/main/6.-univalle-2013-2018.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)
  

  a<-basepm %>%
    dplyr::select(-c("NO2  (ug/m3)" , "O3  (ug/m3)"  )) %>%
    dplyr::rename(Fecha=`Fecha & Hora`, PM2.5=`PM2,5 (ug/m3)`) %>%
    summarize(Fecha,hora=as.factor(as.numeric(str_sub(Fecha,-5,-4))+1),PM2.5=PM2.5) %>%
    mutate(Fecha=as.Date(Fecha,format='%m/%d/%Y'),PM2.5=as.numeric(PM2.5)) %>%
    filter(year(Fecha) =="2018") %>%
    cast(Fecha~hora)
  
  
  dcomp<-a %>%
    filter(apply(a, 1, function(x){sum(is.na(x))})==0)
  
  nrow(dcomp)
  
  ### Días incompletos
  
  dinc<-a %>%
    filter(apply(a, 1, function(x){sum(is.na(x))})>0)
  
  ###días imcompletos 1 a 6 datos faltantes
  
  dinc1_6<-dinc %>%
    filter(apply(as.data.frame( dinc), 1, function(x){sum(is.na(x))})<=6)
  
  
  imputed<-imputar(MC=dinc1_6,DC=dcomp,metodo ="MV" )
  
  
  imputed2<-imputar(MC=dinc1_6,DC=dcomp,metodo ="ES" )
  
  dia<-reactive({ input$input_type
  })
  
  
  fil<- reactive({ imputed %>%
      dplyr::filter(weekdays(Fecha)==dia())%>%
      melt(id="Fecha")
  })   
  
  
  fil2<- reactive({ imputed2 %>%
      dplyr::filter(weekdays(Fecha)==dia())%>%
      melt(id="Fecha")
  })   
  
  
  output$plot2 <- renderPlotly({
   
    
    pimp<- reactive({ ggplot(fil(),aes(y=value, x=variable))+
      geom_boxplot(fill="lightgreen", alpha=0.8) + 
      xlab("Horas")+
      geom_hline(yintercept=0, linetype="dashed", color = "red",size=0.5)+
      ylim(min(fil()$value), max(fil()$value))+
      ylab(" $PM_{2.5} (\\mu/m^3)$ ")+
      theme(axis.text.x = element_text(size = rel(0.65)))+
      ggtitle(paste("Boxplots ", dia()))
    }) 
    
    ggpl<-reactive({ ggplotly(pimp())})
    
    ggpl()
      })
  
  
  
  output$plot33 <- renderPlotly({
   
    
    pimp2<- reactive({ ggplot(fil2(),aes(y=value, x=variable))+
        geom_boxplot(fill="lightgreen", alpha=0.8) + 
        xlab("Horas")+
        geom_hline(yintercept=0, linetype="dashed", color = "red",size=0.5)+
        ylim(0,max(fil2()$value))+
        ylab(" $PM_{2.5} (\\mu/m^3)$ ")+
        theme(axis.text.x = element_text(size = rel(0.65)))+
        ggtitle(paste("Boxplots ", dia()))    }) 
    
    ggpl2<-reactive({ ggplotly(pimp2())%>%
        config(mathjax = "cdn")
      
    })
    ggpl2()
  })
  
}


