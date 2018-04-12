
#This needs to reference from the Palmer_SSDPlot code


StartFreq= c()# This is going to be our cost storage

Varx = rep(c(1,2,3), times = 6)

for(i in 1:length(patlist)){ #Going through the list of patients (1-6)
  for(j in 1:length(codonlist)){ #Going through the list of codon (3)
    pat = patlist[i] #saving the current patient in pat
    codon= codonlist[j] #saving the current codon 
    
    Start_freq_pat <- subset(Data, Pat== pat, select = c(Pat,Month, Res.Mut.Precentage, Codon))
    head(Start_freq_pat)
    
    SF_pat_Codon <- subset(Start_freq_pat, Codon== codon, select = c(Pat, Month, Res.Mut.Precentage, Codon)) 
    head(ssd_pat_Codon)
    
    index= SF_pat_Codon$Res.Mut.Precentage[1] 
    StartFreq= append(StartFreq, index)
    
    
  }
}

Codon = rep(c("AAC","AAT","TGT"), times = 6)

plot(Varx, StartFreq, type= "p", col= "black", main=paste("Best Estimated Guess"), xlab="Codon", ylab= "Start", xaxt="n")
axis(1,labels = codonlist, at = c(1, 2, 3),las=1)


library(ggplot2)
#pdf("../Plots/Palmer_SummaryPlot/patsum2.pdf",width = 10, height =10)
StartFreq_plot = ggplot(data.frame(x=Codon, y=StartFreq), aes(Codon, StartFreq, colour = Codon)) 
StartFreq_plot + geom_point(size = 2, position = "jitter")  + ggtitle("Starting Frequencies of K103N and Y181C") +
         xlab("Codon") + ylab("Percentage of Fitness Cost")
#dev.off()


