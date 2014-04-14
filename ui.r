###############################
### Forms reimbursement - ui.R ###
###############################

library(shiny) 
#setwd("C:/SVN/sdiprima/reimbursement")
load("Forms.RData") # load the dataframe


ui_call<-list()
uicol=1
for(i in 1:ncol(Forms)){
  ui_call[[uicol]]<-h4(paste(gsub(pattern="form_",replacement="",x=names(Forms)[i]),"Form"))
  uicol=uicol+1
  
  ui_call[[uicol]]<-call("dateRangeInput","inputId"=paste("dateRange_",i,sep=""),label=paste("Infusion Date range for the", gsub(pattern="form_",replacement="",x=names(Forms)[i]),"Form"),
                         start=min(Date,na.rm=TRUE),end=max(Date,na.rm=TRUE))
  uicol=uicol+1
  
  ui_call[[uicol]]<-call("sliderInput","inputId"=paste("dayCut_",i,sep=""),label=paste("Days from infusion given to complete the", gsub(pattern="form_",replacement="",x=names(Forms)[i]),"Form"),
                         min=0,max=max(Forms[which(Forms[,i]>0),i],na.rm=TRUE),value=c(0,max(Forms[which(Forms[,i]>0),i],na.rm=TRUE)),ticks=FALSE)
  uicol=uicol+1
  
  ui_call[[uicol]]<-br()
  uicol=uicol+1
  
}



shinyUI(pageWithSidebar( 
  
  headerPanel("Forms Reimbursement"), 
  do.call(sidebarPanel,ui_call),
  
  mainPanel(
    tabsetPanel(id ="theTabs",
                tabPanel("Summary", verbatimTextOutput("s"),
                         value = "summary"),
                tabPanel("Overall Expected Cost", plotOutput("expected"),value = "plot"),
                tabPanel("Quarterly Cost", plotOutput("quarterly"),value = "plot"),
                tabPanel("Length to Claim", plotOutput("claimdays"),value = "plot")
                
    )
  )
))