
Barchart1D <- function(Data, Target_Column, num_perc = "num", Axis.Title_Name = "", 
                       colorpalette = "Dark2", Axis.Flip = FALSE){
  
  # Renaming the Axis.Title Name
  if(Axis.Title_Name == ""){
    Axis.Title_Name = Target_Column
  }else{
    Axis.Title_Name = Axis.Title_Name
  }
  
  # Getting the Target Column
  Target_Column = Data[,Target_Column]
  lbls = unique(Target_Column)
  
  # Flipping Arrangement
  if(Axis.Flip == FALSE){
    axis.pos = NULL
    vadjustment_value = -0.5
  }else if(Axis.Flip == TRUE){
    axis.pos = coord_flip(expand = TRUE)
    vadjustment_value = 0.5
  }
  
  # Setting the Colour Pallette for the Chart
  mycolors <- colorRampPalette(brewer.pal(8, colorpalette))(length(lbls))
  
  # Generating the Plot
  if(num_perc == "num"){
    y_axis_indicators = NULL
    
    ggplot(Data, aes(x = Target_Column, fill = Target_Column))+
      geom_bar(aes(y = (..count..)), position="dodge", show.legend = FALSE)+
      geom_text(aes(y = (..count..), label=((..count..))),
                stat="count", position=position_dodge(0.9), vjust = vadjustment_value, size = 3.5)+
      labs(x = Axis.Title_Name, y = 'Count', fill = Axis.Title_Name, 
           title = paste("BarChart of",Axis.Title_Name))+
      y_axis_indicators+
      # scale_fill_brewer(palette = colorpalette)+
      scale_fill_manual(values = mycolors)+
      axis.pos+
      theme_minimal()
    
  }else if(num_perc == "perc"){
    y_axis_indicators = scale_y_continuous(labels = scales::percent)
    
    ggplot(Data, aes(x = Target_Column, fill = Target_Column))+
      geom_bar(aes(y = (..count.. / sum(..count..))), position="dodge", show.legend = FALSE)+
      geom_text(aes(y = (..count.. / sum(..count..)), label=scales::percent((..count.. / sum(..count..)))),
                stat="count", position=position_dodge(0.9), vjust = vadjustment_value, size = 3.5)+
      labs(x = Axis.Title_Name, y = 'Percent', fill = Axis.Title_Name, 
           title = paste("BarChart of",Axis.Title_Name))+
      y_axis_indicators+
      # scale_fill_brewer(palette = colorpalette)+
      scale_fill_manual(values = mycolors)+
      axis.pos+
      theme_minimal()
    
  }else{
    print("Invalid Input")
  }
}

# Trail
# Barchart1D(Data = ref_df, Target_Column = Sector, Axis.Title_Name = "Sector",colorpalette = "Dark2",Axis.Flip = FALSE)
