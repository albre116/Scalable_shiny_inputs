####################################
#### Forms reimbursement server.r ###
####################################


library(shiny)
library(plyr)
library(ggplot2)
library(zoo)
library(vioplot)
#setwd("C:/SVN/sdiprima/reimbursement")
load("Forms.RData") # load the dataframe

shinyServer(function(input, output, session){ # pass in a session argument
  
  # prep data once and then pass around the program
  
  ### Data C, Step 1 replaces any data point that has an infusion date out of range 
  formsinDate <- reactive({
    tmp<-Forms    
    for (i in 1:ncol(Forms)){
      tmp_cmd<-paste("datesInRange=intersect(which(Date>=input$dateRange_",i,"[1]),which(Date<=input$dateRange_",i,"[2]))",sep="")
      eval(parse(text=tmp_cmd))
      datesOutRange=setdiff(c(1:length(Date)), datesInRange)
      if (length(datesOutRange)>0){
        tmp[datesOutRange,i]=NA
      }
    }
    tmp 
  })
  
  ### Data B, Step 2 replaces any data point that is in range - this is so that old data can be added in later
  formsoutDate <- reactive({
    tmp<-matrix(data=NA,nrow=nrow(Forms),ncol=ncol(Forms))
    for (i in 1:ncol(Forms)){
      tmp_cmd<-paste("datesInRange=intersect(which(Date>=input$dateRange_",i,"[1]),which(Date<=input$dateRange_",i,"[2]))",sep="")
      eval(parse(text=tmp_cmd))
      datesOutRange=setdiff(c(1:length(Date)), datesInRange)
      if (length(datesOutRange)>0){
        tmp[datesOutRange,i]=Forms[datesOutRange,i]
      }
    }
    tmp
    
  })
  
  
  ### Step 3 takes in a dataset, imputes all -1 values, then returns.  Ignores NA
  imputeNeg <-function(thisData){
    c=1
    for (c in 1:ncol(thisData)){
      thisColumn=thisData[,c]
      missingInds=which(is.na(thisData[,c]))
      negInds=which(thisData[,c]<0)
      sampInds=setdiff(c(1:nrow(thisData)),missingInds)
      sampInds=setdiff(sampInds,negInds)
      sampleVals=thisColumn[sampInds]
      if (length(sampleVals)>1){
        negVals=-1*sample(sampleVals, length(negInds),replace=TRUE)
        thisColumn[negInds]=negVals
        thisData[,c]=thisColumn
      } else if (length(sampleVals)==1){
        negVals=-1*rep(sampleVals, length(negInds))
        thisColumn[negInds]=negVals
        thisData[,c]=thisColumn
      }
    }
    
    return(thisData )
  }  
 
  
  ### Step 4.in filter dataset to NA thoseoutside a number of days, keep those in bounds
  formsinRange <- reactive({ 
    tmp<-imputeNeg(formsinDate())
    for (i in 1:ncol(Forms)){
      tmp_cmd<-paste("daysInRange=intersect(which(abs(tmp[,i])>=input$dayCut_",i,"[1]),which(abs(tmp[,i])<=input$dayCut_",i,"[2]))",sep="")
      eval(parse(text=tmp_cmd))
      daysOutRange=setdiff(c(1:nrow(Forms)), daysInRange)
      if (length(daysOutRange)>0){
        tmp[daysOutRange,i]=NA
      }
    } 
    tmp
  })
  
  ### Data D, Step 4.out filter dataset to NA thoseoutside a number of days, keep those out of bounds
  formsoutRange <- reactive({ 
    tmp<-imputeNeg(formsinDate())
    for (i in 1:ncol(Forms)){
      tmp_cmd<-paste("daysInRange=intersect(which(abs(tmp[,i])>=input$dayCut_",i,"[1]),which(abs(tmp[,i])<=input$dayCut_",i,"[2]))",sep="")
      eval(parse(text=tmp_cmd))
      daysOutRange=setdiff(c(1:nrow(Forms)), daysInRange)
      if (length(daysInRange)>0){
        tmp[daysInRange,i]=NA
      }
    } 
    tmp
  })
  
  
  ### Step 5 takes in a dataset to impute and an out of date inf, imputes all NA values that are not from the old set, then returns. 
  imputeNA  <-function(thisData,oldData,outData){

    for (c in 1:ncol(thisData)){
      thisColumn=thisData[,c]
      missingInds=which(is.na(thisData[,c]))
      oldInds=which(!is.na(oldData[,c]))
      outInds=which(!is.na(outData[,c]))
      sampInds=setdiff(c(1:nrow(thisData)),missingInds)
      sampleVals=thisColumn[sampInds]
      missingInds=setdiff(missingInds,oldInds)
      missingInds=setdiff(missingInds,outInds)
      if (length(sampleVals)>1){
        missingVals=sample(c(sampleVals), length(missingInds),replace=TRUE)   
        thisColumn[missingInds]=missingVals
        thisData[,c]=thisColumn
      } else if (length(sampleVals)==1){
        missingVals=sample(c(sampleVals), length(missingInds),replace=TRUE)   
        thisColumn[missingInds]=missingVals
        thisData[,c]=thisColumn
      }
    }
    return (thisData)

  }
  
  
  # Data A build final fully imputed and filtered dataset
  getData <- reactive({
    imputeNA(formsinRange(),formsoutDate(),formsoutRange())
  })
  
  
  # Data A build final fully imputed and filtered dataset
  getDataWNew <- reactive({
    historic=getData()
    new=createNewDay()
    data=rbind(historic,new)
    data
  })
  
  
  #  converts a matrix of number of days to dates.  Uses (inf)Date as start point
  daysToDate <- function(daysData,dateList){
    for (c in 1:ncol(daysData)){
      thisCol=abs(daysData[,c])
      thisDate=as.Date(dateList+thisCol)
      if (c==1){
        dateMat=as.data.frame(thisDate)
      } else {
        dateMat=cbind(dateMat,thisDate)
      }
    }
    return(dateMat)
  }

  
#### summary for tab
  
  output$s<-renderPrint({
    print("Summary of Historic only.")
    print(summary(getData()))
    print("")
    print("Summary of Historic and New.")
    print(summary(getDataWNew()))
    
  })
  
  
#### expected plot for A
  output$expected<-renderPlot({
    #data=getDataWNew()
    data=getData()
    formcost=c(rep.int(15,nrow(data)),rep.int(135,nrow(data)),rep.int(110,nrow(data)),rep.int(85,3*nrow(data)),rep.int(65,8*nrow(data)))
    datalist=array(unlist(data))
    
    negInds=which(datalist<0)
    formcost[negInds]=0
    notnainds=which(!is.na(datalist))
    nndatalist=abs(datalist[notnainds])
    nnformcost=formcost[notnainds]
    
    
    plot(abs(nndatalist),as.numeric(nnformcost),main="Expected Spending per Person",ylab="Cost",xlab="Days")
    #abline(lm(as.numeric(nnformcost)~nndatalist), col="red") # regression line (y~x)
    #lines(lowess(nndatalist,as.numeric(nnformcost)), col="blue") # lowess line (x,y)
    lines(smooth.spline(nndatalist,as.numeric(nnformcost),df=10), col="green") # lowess line (x,y)
    #legend("topright", legend=c("Linear Regression","Lowess","Smoothed Spline"),lty=c(1,1,1),
    #       col=c("red","blue","green"))
    
    
  })
  
  
  
  
  #### density plot for A
  output$density<-renderPlot({
    #data=getDataWNew()
    data=getData()
    formcost=c(rep.int(15,nrow(data)),rep.int(135,nrow(data)),rep.int(110,nrow(data)),rep.int(85,3*nrow(data)),rep.int(65,8*nrow(data)))
    
    datalist=array(unlist(data))
    
    negInds=which(datalist<0)
    formcost[negInds]=NA
    datalist[negInds]=NA
    notnainds=which(!is.na(datalist))
    nndatalist=abs(datalist[notnainds])
    nnformcost=formcost[notnainds]
    
    densityTable=table(nndatalist,nnformcost)
    
    densityList=densityTable%*%c(15,65,85,110,135)
    
    densityProb=densityList/sum(densityList)

    #d=data.frame(x=as.numeric(rownames(densityList)),y=as.numeric(densityList))
    #d=density(densityList)
    
    p=rmultinom(1, prob=as.numeric(densityProb), size=100000)
    fullP=numeric()
    for (r in 1:nrow(densityList)) {
      fullP=c(fullP,rep(x=as.numeric(rownames(densityList)[r]),times=p[r]))
    }
    
    d=density(fullP)
    
    normY=d$y/sum(d$y)
    sumY=normY*sum(fullP)/1000000
    
    plot(d$x,sumY,main="Density of Expected Spending",ylab="Dollars (Millions)",xlab="Days",type="l")
    #lines(lowess(d$x,d$y), col="red") # lowess line (x,y)
    #lines(smooth.spline(d$x,d$y,df=10), col="red") # lowess line (x,y)
  })
  
  
  
  #### cumulative plot for A
  output$cumulative<-renderPlot({

    #data=getDataWNew()
    data=getData()
    formcost=c(rep.int(15,nrow(data)),rep.int(135,nrow(data)),rep.int(110,nrow(data)),rep.int(85,3*nrow(data)),rep.int(65,8*nrow(data)))
    
    datalist=array(unlist(data))
    
    negInds=which(datalist<0)
    formcost[negInds]=NA
    datalist[negInds]=NA
    notnainds=which(!is.na(datalist))
    nndatalist=abs(datalist[notnainds])
    nnformcost=formcost[notnainds]
    
    densityTable=table(nndatalist,nnformcost)
        
    densityList=densityTable%*%c(15,65,85,110,135)
    
    densityProb=densityList/sum(densityList)
    
    #d=density(densityList)
    #d=data.frame(x=as.numeric(rownames(densityList)),y=as.numeric(densityList))
    
    p=rmultinom(1, prob=as.numeric(densityProb), size=100000)
    fullP=numeric()
    for (r in 1:nrow(densityList)) {
      fullP=c(fullP,rep(x=as.numeric(rownames(densityList)[r]),times=p[r]))
    }
    
    d=density(fullP)
    
    normY=d$y/sum(d$y)
    sumY=cumsum(normY*sum(densityList))/1000000
    
    plot(d$x,sumY,main="Cumulative Expected Spending",ylab="Dollars (Millions)",xlab="Days",type='l')
    
    
  })
  
  
  
  ## quarterly plot use A+B+D
  output$quarterlyp<-renderPlot({
    # add B back in
        
    oldData=formsoutDate()
    data=getData()
    newdata=getDataWNew()
    
    fullDate=createNewDate()
    oldDataInds=which(!is.na(oldData),arr.ind=TRUE)

    if (length(oldDataInds)>0){
    data[oldDataInds]=oldData[oldDataInds]
    newdata[oldDataInds]=oldData[oldDataInds]
    }
    
    # add D back in
    outData=formsoutRange()
    outDataInds=which(!is.na(outData),arr.ind=TRUE)
    
    if (length(outDataInds)>0){
      data[outDataInds]=outData[outDataInds]
      newdata[outDataInds]=outData[outDataInds]
    }
    
   
    data[which(data<0,arr.ind=TRUE)]=0
    data[which(is.na(data),arr.ind=TRUE)]=0
    
    newdata[which(newdata<0,arr.ind=TRUE)]=0
    newdata[which(is.na(newdata),arr.ind=TRUE)]=0
    
    datepaidO=daysToDate(data,Date)
    datepaidN=daysToDate(newdata,fullDate)
    
    
    datepaidO[which(data==0,arr.ind=TRUE)]=NA
    datepaidN[which(newdata==0,arr.ind=TRUE)]=NA
    #datepaid=date+data
    
    formcost=c(15,135,110,rep.int(85,3),rep.int(65,8))
    formcost=as.data.frame(t(formcost))
    names(formcost)=names(data)
    
    mindate1=as.yearqtr(min(Date,na.rm=TRUE))
    mindate2=as.yearqtr(min(fullDate,na.rm=TRUE))
    minDate=as.yearqtr(min(c(mindate1,mindate2)))
    minYear=format(minDate,format="%Y")
    
    maxdate1=as.yearqtr(max(datepaidO[,1],na.rm=TRUE))
    maxdate2=as.yearqtr(max(datepaidN[,1],na.rm=TRUE))
    for (c in 2:ncol(data)){
    maxdate1=as.yearqtr(max(maxdate1,as.yearqtr(max(datepaidO[,c],na.rm=TRUE))))
    maxdate2=as.yearqtr(max(maxdate2,as.yearqtr(max(datepaidN[,c],na.rm=TRUE))))
    }
    
    maxdate=as.yearqtr(max(c(maxdate1,maxdate2)))
    maxYear=format(maxdate,format="%Y")
    
    yeardiff=as.numeric(maxYear)-as.numeric(minYear)+1
    
    qlist=as.yearqtr(as.numeric(minYear) + seq(0, yeardiff*4-1)/4)
    qtotalo=as.data.frame(t(numeric(length(qlist))))
    names(qtotalo)=qlist
    qtotaln=as.data.frame(t(numeric(length(qlist))))
    names(qtotaln)=qlist
    
    for (c in 1:ncol(data)){
      
      thisQuartero=as.yearqtr(datepaidO[which(data[,c]>0),c])
      thisCosto=formcost[c]
      thisTableo=table(thisQuartero)
      thisAddo=as.numeric(thisCosto)*t(as.matrix(thisTableo))
      
      for (a in 1:ncol(thisAddo)){
        thisQ=colnames(thisAddo)[a]
        thisCol=which(colnames(qtotalo)==thisQ)
        qtotalo[thisCol]=qtotalo[thisCol]+thisAddo[a]
        
      }    
      
      
      thisQuartern=as.yearqtr(datepaidN[which(newdata[,c]>0),c])
      thisCostn=formcost[c]
      thisTablen=table(thisQuartern)
      thisAddn=as.numeric(thisCostn)*t(as.matrix(thisTablen))
      
      for (a in 1:ncol(thisAddn)){
        thisQ=colnames(thisAddn)[a]
        thisCol=which(colnames(qtotaln)==thisQ)
        qtotaln[thisCol]=qtotaln[thisCol]+thisAddn[a]
        
      }
    }
    
    low=min(which(qtotaln>0))
    hi=max(which(qtotaln>0))
    qtotalo=qtotalo[low:hi]
    qtotalo=qtotalo/1000
    
    qtotaln=qtotaln[low:hi]
    qtotaln=qtotaln/1000
    
    plot(c(1:ncol(qtotaln)),as.numeric(qtotaln),ylab="Cost (Thousands)",xlab=" ",col="red",pch=16,xaxt='n')
    points(c(1:ncol(qtotalo)),as.numeric(qtotalo),pch=16,col="black")
    axis(1,at=c(1:ncol(qtotaln)),labels=names(qtotaln),las=2,cex.axis=0.8)
    
  })
  
  
  
  ## quarterly plot use A+B+D
  output$quarterlyt<-renderTable({
    
    # add B back in
    
    oldData=formsoutDate()
    data=getData()
    newdata=getDataWNew()
    
    fullDate=createNewDate()
    oldDataInds=which(!is.na(oldData),arr.ind=TRUE)
    
    if (length(oldDataInds)>0){
      data[oldDataInds]=oldData[oldDataInds]
      newdata[oldDataInds]=oldData[oldDataInds]
    }
    
    # add D back in
    outData=formsoutRange()
    outDataInds=which(!is.na(outData),arr.ind=TRUE)
    
    if (length(outDataInds)>0){
      data[outDataInds]=outData[outDataInds]
      newdata[outDataInds]=outData[outDataInds]
    }
    
    
    data[which(data<0,arr.ind=TRUE)]=0
    data[which(is.na(data),arr.ind=TRUE)]=0
    
    newdata[which(newdata<0,arr.ind=TRUE)]=0
    newdata[which(is.na(newdata),arr.ind=TRUE)]=0
    
    datepaidO=daysToDate(data,Date)
    datepaidN=daysToDate(newdata,fullDate)
    
    
    datepaidO[which(data==0,arr.ind=TRUE)]=NA
    datepaidN[which(newdata==0,arr.ind=TRUE)]=NA
    #datepaid=date+data
    
    formcost=c(15,135,110,rep.int(85,3),rep.int(65,8))
    formcost=as.data.frame(t(formcost))
    names(formcost)=names(data)
    
    mindate1=as.yearqtr(min(Date,na.rm=TRUE))
    mindate2=as.yearqtr(min(fullDate,na.rm=TRUE))
    minDate=as.yearqtr(min(c(mindate1,mindate2)))
    minYear=format(minDate,format="%Y")
    
    maxdate1=as.yearqtr(max(datepaidO[,1],na.rm=TRUE))
    maxdate2=as.yearqtr(max(datepaidN[,1],na.rm=TRUE))
    for (c in 2:ncol(data)){
      maxdate1=as.yearqtr(max(maxdate1,as.yearqtr(max(datepaidO[,c],na.rm=TRUE))))
      maxdate2=as.yearqtr(max(maxdate2,as.yearqtr(max(datepaidN[,c],na.rm=TRUE))))
    }
    
    maxdate=as.yearqtr(max(c(maxdate1,maxdate2)))
    maxYear=format(maxdate,format="%Y")
    
    yeardiff=as.numeric(maxYear)-as.numeric(minYear)+1
    
    qlist=as.yearqtr(as.numeric(minYear) + seq(0, yeardiff*4-1)/4)
    qtotalo=as.data.frame(t(numeric(length(qlist))))
    names(qtotalo)=qlist
    qtotaln=as.data.frame(t(numeric(length(qlist))))
    names(qtotaln)=qlist
    
    for (c in 1:ncol(data)){
      
      thisQuartero=as.yearqtr(datepaidO[which(data[,c]>0),c])
      thisCosto=formcost[c]
      thisTableo=table(thisQuartero)
      thisAddo=as.numeric(thisCosto)*t(as.matrix(thisTableo))
      
      for (a in 1:ncol(thisAddo)){
        thisQ=colnames(thisAddo)[a]
        thisCol=which(colnames(qtotalo)==thisQ)
        qtotalo[thisCol]=qtotalo[thisCol]+thisAddo[a]
        
      }    
      
      
      thisQuartern=as.yearqtr(datepaidN[which(newdata[,c]>0),c])
      thisCostn=formcost[c]
      thisTablen=table(thisQuartern)
      thisAddn=as.numeric(thisCostn)*t(as.matrix(thisTablen))
      
      for (a in 1:ncol(thisAddn)){
        thisQ=colnames(thisAddn)[a]
        thisCol=which(colnames(qtotaln)==thisQ)
        qtotaln[thisCol]=qtotaln[thisCol]+thisAddn[a]
        
      }
    }
    
    low=min(which(qtotaln>0))
    hi=max(which(qtotaln>0))
    qtotalo=qtotalo[low:hi]
    qtotaln=qtotaln[low:hi]
    
    qtotal=t(rbind(qtotalo,qtotaln))
        
    qtotal=as.data.frame(qtotal)
    
    names(qtotal)=c("Cost of Historic Only", "Cost of Historic and New")
    
    qtotal

    
  })
  
  
  ## Violin plots for Data C
  output$claimdays<-renderPlot({
    
    colList=c("gold","red","cyan", "green", "green", "green","purple","purple","purple","purple","purple","purple","purple","purple")
    thisData=formsinDate()    
    
    par(mai=c(0.3,0.3,0.3,0.1),mfrow=c(3,5),cex.axis=1.4)
    for (c in 1:ncol(Forms)){
      thisForm=gsub(pattern="form_",replacement="",x=names(Forms)[c])
      vioplot(thisData[which(thisData[,c]>0),c],names=thisForm,col=colList[c],ylim=c(0,max(thisData[which(thisData[,c]>0),c])+50),horizontal=TRUE)
      tmp_cmd<-paste0("abline(v=input$dayCut_",c,"[1],lty=2)")
      eval(parse(text=tmp_cmd))
      tmp_cmd<-paste0("abline(v=input$dayCut_",c,"[2],lty=2)")
      eval(parse(text=tmp_cmd))
    }
    
    
    
  })
  
  
  ## distribution plot of infusion dates
  output$datedist<-renderPlot({
    
    daylist=format(Date,format="%j")
    d=density(as.numeric(daylist))
    
    normY=d$y/sum(d$y)
    sumY=normY*length(daylist)
    monthstarts=c(0,32,61,93,122,153,183,214,245,275,306,336)
    monthlabs=c("Jan","Feb","Mar","Apr","May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
    
    
    plot(d$x,sumY,type="l", xlim=c(0,366),xaxt='n', xlab="Day of the year", ylab="Number Added", main="Distribution of Historic Infusion Dates" )
    start=as.numeric(format(input$enrollRange[1],format="%j"))
    end=as.numeric(format(input$enrollRange[2],format="%j"))
    abline(v=start,col="green",lty=2)
    abline(v=end,col="red",lty=2)
    axis(1,at=monthstarts,labels=monthlabs,las=2)
    
  })
  
  
  
  
  #create a set of new inf dates
  createNewDate <-reactive({
    
    numNew<-as.numeric(input$numNew)
    
    if (numNew==0) return (Date)
    
    data=getData()
    daylist=format(Date,format="%j")
    dayTable=table(daylist)
    
    start=input$enrollRange[1]
    startDay=format(start,format="%j")
    
    end=input$enrollRange[2]
    range=as.numeric(end-start)
    
    numLoops=range%/%366
    bonus=range%%366
    
    pullList=numeric()
    if (numLoops>0){
      for (n in 1:numLoops){
        pullList=c(pullList,as.numeric(dayTable))
      }
    }
    pullList=c(pullList,as.numeric(dayTable)[1:bonus])
    
    fullList=numeric()
    for (l in 1:range){
      fullList=c(fullList,rep(x=l,times=pullList[l]))
      
    }
    
    newdays=sample(x=fullList,size=numNew,replace=TRUE)
    newDate=as.Date(start+newdays)
    fullDate=c(Date,newDate)
    fullDate
    
  })
  
  
  ### create a set of new form data
  createNewDay<-reactive({
    
    numNew<-as.numeric(input$numNew)
    
   if (numNew==0) return (NULL)
    
    data=getData()  
    newpatients=as.data.frame(matrix(data=NA,nrow=numNew,ncol=ncol(Forms)))
    colnames(newpatients)=names(Forms)
   
    for (c in 1:ncol(newpatients)){
      
      thisColumn=newpatients[,c]
      sampleVals=data[,c]
      if (length(sampleVals)>1){
        missingVals=sample(c(sampleVals), length(thisColumn),replace=TRUE)   
        thisColumn=missingVals
        newpatients[,c]=thisColumn
      }
      
    }
    
    newpatients
    
  })
  
  
  
  
})