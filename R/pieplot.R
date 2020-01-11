
PiePlot = function(Data, Target_Column, num_perc = "num", Main_Title = "", colorpalette = "PuBuGn"){
  
  # Loading the Libraries
  library(RColorBrewer)
  
  data1 = data.frame(table(Data[,Target_Column]))
  colnames(data1) = c(Target_Column,"freq")
  # View(data1)
  
  if(num_perc == "num"){
    data1$pct <- data1$freq
    lbls <- as.vector(data1[[Target_Column]])
    slices <- as.vector(data1$freq)
    pct <- as.vector(data1$pct)
    
    lbls <- paste(lbls,"->",pct) # add percents to labels
    
  }else if(num_perc == "perc"){
    data1$pct <- format(round(((data1$freq)/sum(data1$freq)*100), 2), nsmall = 2)
    lbls <- as.vector(data1[[Target_Column]])
    slices <- as.vector(data1$freq)
    pct <- as.vector(data1$pct)
    
    lbls <- paste(lbls,"->",pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    
  }else{
    print("Invalid Input - Only 'num'(Numbers) or 'perc'(Percentage) for Bars")
  }
  
  # Available Pallettes:
  # Pallette-1:
  # Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd,
  # Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd
  # Pallette-2:
  # BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
  coul <- colorRampPalette(brewer.pal(9, colorpalette))(length(lbls))
  
  return(pie(slices,labels = lbls, col = coul,main = Main_Title))
}
