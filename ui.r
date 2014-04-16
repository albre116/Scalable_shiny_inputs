###############################
### Forms reimbursement - ui.R ###
###############################

library(shiny) 
#setwd("C:/SVN/sdiprima/reimbursement")
load("Forms.RData") # load the dataframe



ui_call<-list()

ui_call[[1]]<-call("h4",paste("Choose the enrollment range"))
ui_call[[2]]<-call("dateRangeInput","inputId"="enrollRange",label=paste("Enrollment date range for new patients"),
                   start=as.Date(paste0(format(Sys.Date(),"%Y"),"-01-01")),end=as.Date(paste0(format(Sys.Date(),"%Y"),"-12-31")))
ui_call[[3]]<-call("h4",paste("Choose the number of added patients"))
ui_call[[4]]<-call("textInput","inputId"="numNew",label="Input number of patients",value=1000)

ui_call[[5]]<-call("br")
ui_call[[6]]<-call("actionButton","get", "Add Patients")



for (i in 1:6){
  ui_call[[i]]<-call("conditionalPanel",condition="input.conditionedPanels==2",ui_call[[i]])
}

uicol=7

for(i in 1:ncol(Forms)){
  ui_call[[uicol]]<-call("h4",paste(gsub(pattern="form_",replacement="",x=names(Forms)[i]),"Form"))
  uicol=uicol+1
  
  ui_call[[uicol]]<-call("dateRangeInput","inputId"=paste("dateRange_",i,sep=""),label=paste("Infusion Date range for the", gsub(pattern="form_",replacement="",x=names(Forms)[i]),"Form"),
                         start=min(Date,na.rm=TRUE),end=max(Date,na.rm=TRUE))
  uicol=uicol+1
  
  ui_call[[uicol]]<-call("sliderInput","inputId"=paste("dayCut_",i,sep=""),label=paste("Days from infusion given to complete the", gsub(pattern="form_",replacement="",x=names(Forms)[i]),"Form"),
                         min=0,max=max(Forms[which(Forms[,i]>0),i],na.rm=TRUE),value=c(0,max(Forms[which(Forms[,i]>0),i],na.rm=TRUE)),ticks=FALSE)
  uicol=uicol+1
  
  ui_call[[uicol]]<-call("br")
  uicol=uicol+1
  
}

end<-length(ui_call)
for (i in 7:end){
  ui_call[[i]]<-call("conditionalPanel",condition="input.conditionedPanels==1",ui_call[[i]])
}


shinyUI(pageWithSidebar( 
  
  headerPanel("Forms Reimbursement"),
  do.call(sidebarPanel,ui_call),
       
  
  mainPanel(
    tabsetPanel(
                
                tabPanel("Length to Claim", plotOutput("claimdays"),value = 1),
                
                #tabPanel("Summary", verbatimTextOutput("s"),value = "summary"),
                #tabPanel("OLD Individual Expected Cost", plotOutput("expected"),value = "plot"),
                tabPanel("Density of Expected Cost", plotOutput("density"),value = 1),
                tabPanel("Cumulative Expected Cost", plotOutput("cumulative"),value = 1),
                
                tabPanel("Infusion Date Distribution", plotOutput("datedist"),value = 2),
                
                tabPanel("Quarterly Cost Plot", plotOutput("quarterlyp"),value = 2),
                tabPanel("Quarterly Cost Table", tableOutput("quarterlyt"),value = 2),
                
               id = "conditionedPanels"
                
                
    )
  )
))