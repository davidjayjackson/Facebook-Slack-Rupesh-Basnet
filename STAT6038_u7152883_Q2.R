#Question 2
  #Solution for (a)
    #Let's First import the data
      mydata<-read.csv(choose.files(),header = TRUE)
    #Now, lets make the variable of mydata publicly available
      head(mydata)
      attach(mydata)
      
  #Now, to create the scatter plot
       plot(Metab,Life,xlab = "Metabolic Rate",ylab = "Lifespan"
           ,main = "Scatter plot for quick Glance",pch=20,col="blue")
      #To identify the possible leverage points, I am labelling the points with their commonnames
      
      Index<-identify(Metab,Life,labels=CommonName)

  # Now to find out the Common names and species of those likely leverage points
      (mydata[Index,c(1,2)])
    
#Question 2 (b)
      plot(Metab,Life,xlab = "Metabolic Rate",ylab = "Lifespan"
           ,main = "Scatter plot for quick Glance:A closer look",pch=20,col="blue",
           xlim = c(min(Metab),((sort(Metab,decreasing = TRUE))[3])),
                    ylim = c(min(Life),45))

#Question 2 (c)
      #to apply the natural log transformation on Metab
      
      TMetab<-log(Metab)
      
      #Also lets calculate correlation coefficinet
      Cor.TMetab.Life<-cor(TMetab,Life)
      Cor.TMetab.Life
      
      #FOR Exploratory Data Analysis
      par(mfrow=c(1,2))
      boxplot(TMetab,Life,names = c("TMetab","Life"),col = c("blue","green")
              ,main="Box plot for EDA")
      plot(TMetab,Life,pch=20,col="green",main = "Scatter plot for EDA")
      abline(coef =lm(Life~TMetab)$coefficients,lty=4,col="blue")   ##To insert a fitted regression line
      Index1<-identify(TMetab,Life,labels = CommonName) ##identifying possible outliers
      print(mydata[Index1,c(1,2,4,5)])   ##Printing the possible outliers
       dev.off()
       
      #Now to fit a linear regression model
      
       mymodel<-lm(Life~TMetab)
##To provide the fitted results
       
     summary(mymodel)
  ##To conduct model diagnostics, we will need the following four plots
     
     par(mfrow=c(2,2))
     plot(mymodel,which = c(1,2,4))
      plot(hatvalues(mymodel),type="h", ylab="Leverage",xlab = "Observation Index",
           main="Leverage Check")
      abline(h=4/length(Life),lty=2,col="red")   
      dev.off()     
      
##Question 2 (d)
      #TO apply natural log transformation and sqraure root transformation
      #to the response variable
      NLife<-log(Life)      #Natursal log Transformation of the response
      SLife<-sqrt(Life)     #Square root transformation of response
  #For the correlation coeffeicients
      #Matrix1<-matrix(NA,2,2)
  #To calculate sample correaltion for these two
      corel<-c(cor(TMetab,NLife),cor(TMetab,SLife))
      names(corel)<-c("Correlation NLife vs TMetab","Correlation SLife vs TMetab")   
      corel
  #Now for Scatter plots
      par(mfrow=c(1,2))
    plot(TMetab,NLife,pch=20,col="red",main = "Natural Log")
    abline(coef = lm(NLife~TMetab)$coefficients)
    plot(TMetab,SLife,pch=20,col="red",main = "Square Root")    
    abline(coef = lm(SLife~TMetab)$coefficients)
    
##Question 2 (e)
    #Fitting the model
     model2<-lm(NLife~TMetab)
     summary(model2)    
     
  ##Lets conduct model diagnostics
  par(mfrow=c(2,2))
  plot(model2,which = c(1,2,4))    
  plot(hatvalues(model2),type="h",ylab="Levegare",main = "Leverage Observation")
  abline(h=4/length(NLife),lty=4,col="red")
  Index3<-identify(hatvalues(model2))
  mydata[Index3,c(1,2,4,5)]
  dev.off()
##Question 2 (f)
  #The estimated slope of our model is
  print(model2$coefficients[2])

  #TO obtain a 95% confidence interval for the slope parameter
  Con_Interval<-confint(model2,parm = 2, level = 0.95)
  Con_Interval

#Question 2 (g)
  #The Anova table is given by
  anova(model2)
  #To find rejection region
   crit_val<-qf(0.05,df1=1,df2=93, lower.tail = F)  
  crit_val

  #The P value is given by
  pf(327.77,1,93,lower.tail = F)   #Since the F-statistics is 327.77
  
 ##Question 2 (h)
  #The metabolic rate here is 8000kj per day so,
  a<-8000
  Pre<-predict(model2,data.frame(TMetab=log(a)),interval ="prediction"
          ,conf.level=0.9 )                   ##90% prediction interval
  
  exp(Pre)          #converting the values of life to normal from natural log
    
  
#Question 2 (i)
  #We are here fitting a model for mass and the metabolic rate where
    mass<-Mass^(3/4)
    corel1<-cor(mass,Metab)
    corel1
    model3<-lm(Metab~mass)
  summary(model3)    
  #For the Critical Value
  Cvalue<-c(qt(0.025,93,lower.tail = T),qt(0.025,93,lower.tail = F))
  names(Cvalue)=c("Lower Critical Value","Upper Critical Value")  
  print(Cvalue)  
  #For P-value,as our test statistics is 91.895
  p.val<-2*pt(91.8856,93,lower.tail = F)
  p.val
  #let's plot the results
  par(mfrow=c(2,2))
  plot(mass,Metab,pch=1,col="red",main = "Scatter Plot for EDA",xlab = "Transformed Mass")
  abline(coef = model3$coefficients,col="blue",lty=3)
  plot(model3,which = c(1,2,4))  
  ##
  dev.off()
  ##THE END
  