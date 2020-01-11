
Barchart2D <- function(Data, x1, x2, num_perc = "num",
                       xlabel = "", ylabel = "", main.Title = paste("BarChart of",xlabel,"&",ylabel),
                       inc.Legend = TRUE, Legend.Title = ylabel,
                       colorpalette = "PuBuGn", Axis.Flip = FALSE){
  
  # Intermediate Transformations
  # Y-Axis Indicators
  if(num_perc == "num"){
    y_axis_indicators = NULL
  }else if(num_perc == "perc"){
    y_axis_indicators = scale_y_continuous(labels = scales::percent)
  }
  
  # Y-Axis Label
  if(num_perc == "num"){
    y_axis_label = ""
  }else if(num_perc == "perc"){
    y_axis_label = ""
  }
  
  # Color Pallettes:
  # Available Pallettes:
  # Pallette-1:
  # Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd,
  # Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd
  # Pallette-2:
  # BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
  # coul = brewer.pal(9, colorpalette)
  # coul = colorRampPalette(coul)(length(unique(Data[,x2])))
  
  # Grouped-BarPlot
  if(num_perc == "num"){
    ggplot(Data, aes(x = as.factor(Data[,x1]), fill = as.factor(Data[,x2])))+
      geom_bar(aes(y = (..count..)), position="dodge", show.legend = inc.Legend)+
      geom_text(aes(y = (..count..), label=((..count..))),
                stat="count", position=position_dodge(0.9), vjust=-0.5)+
      xlab(xlabel) + ylab(y_axis_label) + ggtitle(main.Title)+ labs(fill = Legend.Title)+
      y_axis_indicators+
      scale_fill_brewer(palette = colorpalette)+
      theme_minimal()
    
  }else if(num_perc == "perc"){
    ggplot(Data, aes(x = as.factor(Data[,x1]), fill = as.factor(Data[,x2])))+
      geom_bar(aes(y = (..count.. / sum(..count..))), position="dodge", show.legend = inc.Legend)+
      geom_text(aes(y = (..count.. / sum(..count..)), label=scales::percent((..count.. / sum(..count..)))),
                stat="count", position=position_dodge(0.9), vjust=-0.5)+
      xlab(xlabel) + ylab(y_axis_label) + ggtitle(main.Title)+ labs(fill = Legend.Title)+
      y_axis_indicators+
      scale_fill_brewer(palette = colorpalette)+
      # scale_fill_manual(values=wes_palette(n = (length(unique(Data[,x2]))), name="GrandBudapest"))
      theme_minimal()
    
  }else{
    print("Invalid Input")
  }
} 
