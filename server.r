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
  
  
  #  converts a matrix of number of days to dates.  Uses (inf)Date as start point
  daysToDate <- function(daysData){
    for (c in 1:ncol(daysData)){
      thisCol=abs(daysData[,c])
      thisDate=as.Date(Date+thisCol)
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
    summary(getData())
    
  })
  
  
#### expected plot for A
  output$expected<-renderPlot({
    data=getData()
    formcost=c(rep.int(15,nrow(data)),rep.int(135,nrow(data)),rep.int(110,nrow(data)),rep.int(85,3*nrow(data)),rep.int(65,8*nrow(data)))
    datalist=array(unlist(data))
    
    negInds=which(datalist<0)
    formcost[negInds]=0
    notnainds=which(!is.na(datalist))
    nndatalist=abs(datalist[notnainds])
    nnformcost=formcost[notnainds]
    
    
    plot(abs(nndatalist),as.numeric(nnformcost),main="Expected Spending per Person",ylab="Cost",xlab="Days")
    abline(lm(as.numeric(nnformcost)~nndatalist), col="red") # regression line (y~x)
    lines(lowess(nndatalist,as.numeric(nnformcost)), col="blue") # lowess line (x,y)
    lines(smooth.spline(nndatalist,as.numeric(nnformcost),df=10), col="green") # lowess line (x,y)
    legend("topright", legend=c("Linear Regression","Lowess","Smoothed Spline"),lty=c(1,1,1),
           col=c("red","blue","green"))
    
    
  })
  
  
  
  ## quarterly plot use A+B+D
  output$quarterly<-renderPlot({
    
    # add B back in
    oldData=formsoutDate()
    data=getData()
    oldDataInds=which(!is.na(oldData),arr.ind=TRUE)

    if (length(oldDataInds)>0){
    data[oldDataInds]=oldData[oldDataInds]
    }
    
    # add D back in
    outData=formsoutRange()
    outDataInds=which(!is.na(outData),arr.ind=TRUE)
    
    if (length(outDataInds)>0){
      data[outDataInds]=outData[outDataInds]
    }
    
   
    data[which(data<0,arr.ind=TRUE)]=0
    data[which(is.na(data),arr.ind=TRUE)]=0
    
    datepaid=daysToDate(data)
    
    datepaid[which(data==0,arr.ind=TRUE)]=NA
    #datepaid=date+data
    
    formcost=c(15,135,110,rep.int(85,3),rep.int(65,8))
    formcost=as.data.frame(t(formcost))
    names(formcost)=names(data)
    
    mindate=as.yearqtr("2004 Q1")
    maxdate=as.yearqtr("2023 Q4")
    qlist=as.yearqtr(2004 + seq(0, 79)/4)
    qtotal=as.data.frame(t(numeric(length(qlist))))
    names(qtotal)=qlist
    
    for (c in 1:ncol(data)){
      
      thisQuarter=as.yearqtr(datepaid[which(data[,c]>0),c])
      thisCost=formcost[c]
      thisTable=table(thisQuarter)
      thisAdd=as.numeric(thisCost)*t(as.matrix(thisTable))
      
      for (a in 1:ncol(thisAdd)){
        thisQ=colnames(thisAdd)[a]
        thisCol=which(colnames(qtotal)==thisQ)
        qtotal[thisCol]=qtotal[thisCol]+thisAdd[a]
        
      }
    }
    
    qtotal=qtotal[which(qtotal>0)]
    plot(c(1:ncol(qtotal)),as.numeric(qtotal),ylab="Cost",xlab=" ",pch=16,xaxt='n')
    axis(1,at=c(1:ncol(qtotal)),labels=names(qtotal),las=2,cex.axis=0.8)
    
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
  
  
})