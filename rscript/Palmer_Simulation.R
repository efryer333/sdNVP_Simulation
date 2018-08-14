#Load Data/Set up Working Directory

#getwd()
setwd("/Users/emilyfryer/Documents/SFSU/CoDELab/sdNVP_Simulation/") #Emily's WD

#setwd("C:/Users/Gaby/Documents/CoDE/sdNVP_Simulation") #Gabriella's WD
Data <- read.table("Data/PalmerDataGroup1.csv",sep=",",header=TRUE,row.names=1)
head(Data)


#Simulation Function

Sim_Fun <- function(months, startfreq, cost){ 
  #There are 3 parameters to pass into the Sim_Fun function when it is called: months, startfreq and cost
  #Months is a vector of three values
  
  No=100000  # Number of Individuals
  u=1/No     # Mutation rate
  cost= cost # fitness cost
  bm=1-cost  # mutant fitness
  bw=1       # wildtype fitness
  mut= 1000 * startfreq[1] # The start number of mutants .... looks like frequency but actually percentage .... change to mulitply by .01 instead of 1000
  wt= No-mut # number of wt
  
  #The storages usage:
  #mut stores the mut-array
  mutStorage=c() 
  #This stores the total number of wild type array
  wtStorage=c()
  #This stores new mutants
  newmutStorage= c()
  #This stores new wt
  newWTStorage= c()
  #Sim Storage for three month timepoint
  SimStorage= c()
  
  gen1= months[1]*30 #start of the month point ... HIV regeneration time is 1 day ... multiplied to get scale in days
  gen2= months[2]*30 #mid test point
  gen3= months[3]*30 #last test point
  
  for (gen in gen1:gen3){
    
    if (gen== gen1){
      month1 = (mut/No)*100 #Generation is a day ... and we're storing the number of mutants into a variable called month1
    }
    if (gen== gen2){
      month2 = (mut/No)*100
    }  
    if (gen== gen3){
      month3 = (mut/No)*100
    }  
    
    #To see if any wt changed to mutants    
    newmut=rbinom(1,wt,u) 
    newmutStorage = append(newmutStorage, newmut)
    wt=wt-newmut 
    mut= mut + newmut 
    
    #To see if any mutants changed to wt
    newwt=rbinom(1,mut,u) 
    newWTStorage= append(newWTStorage, newwt)
    wt=wt+newwt 
    mut=mut- newwt
    
    #Binomial Sampling
    #p is the expected fraction of mutants for the next generation
    P=(mut*bm)/((mut*bm) + (wt*bw))
    mut= rbinom(1,No,P)          
    wt= No-mut
    
    #Storing the new WT and Mut Values
    wtStorage= append(wtStorage, wt)
    mutStorage= append(mutStorage, mut)
  }
  
  SimStorage= append(SimStorage, month1)
  SimStorage= append(SimStorage, month2)
  SimStorage= append(SimStorage, month3)
  
  
  
  return(SimStorage)
}


#Nested Loop


costlist <- 10^seq(-2, -1, by=0.05) #List of cost//
length(costlist)

codonlist <- as.character(unique(Data$Codon)) #List of codon
patlist <- unique(Data$Pat) #List of patients

#number of rows needed for DF
numRowNeeded <- length(unique(Data$Pat))*length(costlist)*length(codonlist)*3

#Creates DF
SimDF = data.frame(Pat= rep(0,numRowNeeded), Months=0, SimFrequency = 0, ResMut= "", Cost= 0,Codon= "",stringsAsFactors = FALSE)


#This is the outer most loop (goes through patient list 1:6)
for(i in 1:length(patlist)){
  #Middle loop: goes through the codon list (3)
  for(j in 1:length(codonlist)){
    #This goes through the cost list and adds data in the DF
    for(k in 1:length(costlist)){
      pat=patlist[i] #Goes into the patlist
  
      codon = codonlist[j] #Goes into the codonlist
      #in data(df) it is looking for which pat = patlist[i] and codon[j]
      freq<- Data$Res.Mut.Precentage[which(Data$Pat==pat&Data$Codon==codon)] 
      #it is looking in months of data(df) finding which pat = pat[i]
      months<- unique(Data$Month[which(Data$Pat==pat)])
      
      cost=costlist[k]
      #Finds the start row of the empty DF row
      StartRow <- min(which(SimDF$Pat== 0))
      #fills pat data
      SimDF$Pat[StartRow:(StartRow +2)] <- rep(pat, 3)
      #fills months data
      SimDF$Months[StartRow:(StartRow +2)] <- c(months[1], months[2], months[3])
      
      #fills in the res type
      if(codon=="AAC"||codon=="AAT"){
        SimDF$ResMut[StartRow:(StartRow +2)] <- rep("K103N",3)  
      }
      if(codon=="TGT"){
        SimDF$ResMut[StartRow:(StartRow +2)] <- rep("Y181C",3)  
      }
      #fills in the sim data
      SimDF$SimFrequency[StartRow: (StartRow +2)] <-Sim_Fun(months,freq,cost)
      #fills in the cost data
      SimDF$Cost[StartRow:(StartRow +2)]<- rep(cost,3)
      #fills in the cost
      SimDF$Codon[StartRow:(StartRow +2)] <- rep(codon,3)
    }
  }
}
write.csv(SimDF, file="Data/SimDF_Palmer.csv")


