
#setwd("C:/Users/Gaby/Dropbox/ExplDataAnalysisR/Gabriella/Data")

SimData <- read.table("Data/SimDF_Palmer.csv",sep=",",header=TRUE,row.names=1)
Data <- read.table("Data/PalmerDataGroup1.csv",sep=",",header=TRUE,row.names=1)

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
  
  #newsub <- Data$Res.Mut.Precentage[which(Data$Codon==codon&Data$Pat==pat)]
  newsub <- subset(Data, Pat == pat & Codon == codon, select = c(Month, Res.Mut.Precentage))
  return(newsub)   
}
#########################################################

#This subset for the three sim frequency
simfreq_pat <- function(subpat,num){
  
  simfreq <- subpat$SimFrequency[which(subpat$Cost==num)] 
  
  return(simfreq)   
}



# Need to loop through subpat and return each SimFreq for each Cost at each month

patlist <- unique(SimData$Pat) #List of patients
codonlist <- as.character(unique(SimData$Codon)) #List of codons


#pdf("Plots/Palmer_LinePlot/EF_Plots.pdf",width = 10, height =10)
for(i in 1:length(patlist)){ #Going through the list of patients (1-6)
  for(j in 1:length(codonlist)){ #Going through the list of codon (3)
    codon = codonlist[j] #saving the current codon 
    pat = patlist[i] #saving the current patient in pat
    
    # Holding the length of costlist in a variable   
    ncost = length(costlist)  
  
    
    subpat=pat_subset_Codon(SimData,pat,codon)#This uses my subsetting function to     get the simulated months,simfreq and cost of the current patient
    simfreq = (subpat$SimFrequency) #Getting the Simulated frequency for each subsetted patient
    
    realpat=pat_subset_real(Data,pat,codon)#This uses my subsetting function to       get the real data's freq for the current patient
    
    Mon= unique(SimData$Months[which(SimData$Pat==pat)])#this gets three months       for the current patient
    
    yCosts = unique(SimData$Cost[which(SimData$Pat==pat)])
    
    #Getting the range for the x and y axis
    xrange <- range(Mon) 
    yrange <- range(simfreq)
    
    #plot settup and addition of line for real patient data
    plot(realpat, xlim = xrange, ylim = c(0,50) ,axes = FALSE, type= "b", col= "red", pch = 15, lwd = 2.5, main=paste("Pat",patlist[i],"Codon",codonlist[j]), xlab = "Time Point", ylab = "Frequency")
    axis(1,labels = Mon, at = realpat$Month)
    axis(2)
    
    # Legend setup
    legend('topright', # places a legend at the appropriate place
           legend = c('Simulated data','Real Patient Data'), # puts text in the legend
           lty=c(1,1), # gives the legend appropriate symbols (lines)
           lwd=c(2.5,2.5),col=c('grey','red')) # gives the legend lines color and width
    
    #Creating a range of colors for each fitness cost value
    #colors <- rainbow(ncost)
    
    #This loop plots all of the fitness costs associated with each codon for each patient
    for (k in 1:length(costlist)) {
      pat_cost = costlist[k]
      #print(costlist[k])
      subcost <- subset(subpat, pat_cost == Cost, select=c(Months, SimFrequency))
      lines(subcost$Months, subcost$SimFrequency, lty = 1, lwd = 1.0, col = "#cccccc")
      #axis(2, labels = costlist[k], at = subcost$SimFrequency[3], las = 2)
      
      
    }
    
    
        
    
      }
    

    #This makes the plots. It first plots the real data's points and line in black.      
    
  #plot(realpat, type= "b", col= "black", main=paste("Pat",patlist[i],"Codon",codonlist[j]), xlab="Months", ylab= "Percentage of Drug Resistance Mutants", xaxt="n",lwd = 5, pch= 12)
    
  #legend("bottomleft", c("Real Data", "Cost 10%","Cost 1%","Cost 0.1%", "Cost 0.01%"),pch= 16, col=c("black","orange","purple","red", "green"))
    }
    
  

#dev.off()

