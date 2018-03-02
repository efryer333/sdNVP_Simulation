
#setwd("C:/Users/Gaby/Dropbox/ExplDataAnalysisR/Gabriella/Data")

SimData <- read.table("../Data/SimDF_Palmer.csv",sep=",",header=TRUE,row.names=1)
Data <- read.table("../Data/PalmerDataGroup1.csv",sep=",",header=TRUE,row.names=1)



#Subsetting Function
#This is a subset for the simdata
#Argument Example: (Dataframe, 1, "AAC")

pat_subset_Codon <- function(SimDF, pat, codon){
  
  newsub <- subset(SimDF, Pat== pat & Codon==codon,select=c(Months, SimFrequency, Cost)) 
  return(newsub)   
}

##########################################################

#This subset for the real data
pat_subset_real <- function(Data, pat, codon){
  
  newsub <- Data$Res.Mut.Precentage[which(Data$Codon==codon&Data$Pat==pat)]
  
  return(newsub)   
}
#########################################################

#This subset for the three sim frequency
simfreq_pat <- function(subpat,num){
  
  simfreq <- subpat$SimFrequency[which(subpat$Cost==num)] 
  
  return(simfreq)   
}


#Nested for loop for plots

patlist <- unique(SimData$Pat) #List of patients
codonlist <- as.character(unique(SimData$Codon)) #List of codons

#pdf("../Plots/test2.pdf",width = 10, height =10)

for(i in 1:length(patlist)){ #Going through the list of patients (1-6)
  for(j in 1:length(codonlist)){ #Going through the list of codon (3)
    pat = patlist[i] #saving the current patient in pat
    codon= codonlist[j] #saving the current codon 
    
    subpat=pat_subset_Codon(SimData,pat,codon)#This uses my subsetting function to     get the simulated months,simfreq and cost of the current patient
    
    realpat=pat_subset_real(Data,pat,codon)#This uses my subsetting function to       get the real data's freq for the current patient
    
    Mon= unique(SimData$Months[which(SimData$Pat==pat)])#this gets three months       for the current patient
    
    #This function subset the current patients simfreq based on cost (Hard Coded)
    cost10=simfreq_pat(subpat, 0.1)
    cost1=simfreq_pat(subpat, 0.01)
    cost0.1=simfreq_pat(subpat, 0.001)
    cost0.01=simfreq_pat(subpat, 0.0001)
    
    
    #This makes the plots. It first plots the real data's points and line in black.      
    plot(realpat, type= "b", col= "black", main=paste("Pat",patlist[i],"Codon",codonlist[j]), xlab="Months", ylab= "Percentage of Drug Resistance Mutants", xaxt="n",lwd = 5, pch= 12)
    axis(1,labels = Mon, at = c(1, 2, 3),las=1)
    
    for(cost in c(0.1,0.01,0.001,0.0001)){
      costdata=simfreq_pat(subpat, cost)
      
      
      lines(cost10, col="orange",lwd= 2)
      points(cost10, col="orange",lwd= 2)
      lines(cost1, col="purple", lwd=2)
      points(cost1, col= "purple", lwd=2)
      lines(cost0.1, col="red", lwd=2)
      points(cost0.1, col="red", lwd=2)
      lines(cost0.01, col="green", lwd=2)
      points(cost0.01, col="green", lwd=2)
      
      legend("bottomleft", c("Real Data", "Cost 10%","Cost 1%","Cost 0.1%", "Cost 0.01%"),pch= 16, col=c("black","orange","purple","red", "green"))
    }
    
  }
}

dev.off()

