
#This needs to reference from the Palmer_SSDPlot code


BestCost= c()# This is going to be our cost storage

BestCost= BestCost*100

Varx = rep(c(1,2,3), times = 6)

for(i in 1:length(patlist)){ #Going through the list of patients (1-6)
  for(j in 1:length(codonlist)){ #Going through the list of codon (3)
    pat = patlist[i] #saving the current patient in pat
    codon= codonlist[j] #saving the current codon 
    
    ssd_pat <- subset(EstCost_DF, Pat== pat, select = c(Pat, SSD, Cost, Codon))
    head(ssd_pat)
    
    ssd_pat_Codon <- subset(ssd_pat, Codon== codon, select = c(Pat, SSD, Cost, Codon)) 
    head(ssd_pat_Codon)
    
    index= which.min(ssd_pat_Codon$SSD) #This find the smallest ssd, the lower the ssd the, better the fit is
    BestCost= append(BestCost, ssd_pat_Codon$Cost[index])
  }
}

BestCodon = rep(c("AAC","AAT","TGT"), times = 6)

#plot(Varx, BestCost, type= "p", col= "black", main=paste("Best Estimated Guess"), xlab="Codon", ylab= "Estimated Cost", xaxt="n")
#axis(1,labels = codonlist, at = c(1, 2, 3),las=1)


library(ggplot2)
#pdf("../Plots/Palmer_SummaryPlot/patsum2.pdf",width = 10, height =10)
BestCost_plot = ggplot(data.frame(x=BestCodon, y=log(BestCost)), aes(BestCodon, BestCost, colour = BestCodon)) 
BestCost_plot + geom_point(position = position_jitter(width = .3), size = 2) + ggtitle("Estimated Fitness Costs of K103N and Y181C") +
  xlab("Codon") + ylab("Fitness Cost") 
  #dev.off()

