## Carissa Evans 
## 7/23/24
## DREAM Challenge

## Import Dragon descriptors sheet and remove all columns except the % of molecules 
df_p<- read_csv("~/Mainland Lab Dropbox/Mainland Lab Team Folder/Projects/CarissaDream/dreaming-with/data/raw/Dragon_Descriptors.csv") 
  df_percentages <- subset(df_p, select = c("CID","H%", "C%", "N%", "O%") ) %>%
  clean_names() 

## Get a list of CIDs for each mixture
   cidList <- df_lookup$CIDs
   cidString <- as.character(cidList)
   cidString <- strsplit(cidString, ";")

   
## Function to get %X for each CID in mixture, convert to a list, then return the average %X
   find_percentage <- function(cidString, df_percentages, cid, percentColumn) {
       #search cid column in df_percentages for CIDs in each mixture, pulls corresponding %X
       filterPercent <- df_percentages [df_percentages[[cid]] %in% cidString]
       #creates a list of the numeric list of the percentages
       percentX <- filterPercent[[percentColumn]]
       as.list.numeric_version(percentX)
       #finds average %X
       averageX <- mean(percentX)
       return(averageX)
   }
   
## Get %H for each CID in mixture
   #percentH <- find_percentage(cidString, df_percentages, "cid", "h_percent")
 

## Calculate the difference in molecule percentages 
  diffMoleculePercentages <- function(percentColumn) {
      df %>%
      percentData <- df_percentages[[percentColumn]]
       # Get CIDs of of mixture 1
       left_join(
           df_lookup,
           by = c("dataset", "mixture_1" = "mixture_label")
       ) %>%
       # Get CIDs of of mixture 2
       rename(a = CIDs) %>%
       left_join(
           df_lookup,
           by = c("dataset", "mixture_2" = "mixture_label")
       ) %>%
       rename(b = CIDs) %>%
    ## Get the average percentages of each mixture
       mix1Percent <- find_percentage(a, df_percentages, "cid", percentColumn)
       mix2Percent <- find_percentage(b, df_percentages, "cid", percentColumn)
       ## Calculate the percent difference between the mixtures
       absoluteDiff <- abs(mix1Percent - mix2Percent)
       average <- (mix1Percent + mix2Percent)/2
       percentDiff <- (absoluteDiff/average) * 100
       return(percentDiff)
  }


## Hydrogen Comparison 
  hydrogenData <- diffMoleculePercentages(h_percent)
  
## Carbon Comparison 
  carbonData <- diffMoleculePercentages(c_percent)
  
## Oxygen Comparison 
  oxygenData <- diffMoleculePercentages(o_percent)
  
## Nitrogen Comparison
  nitrogenData <- diffMoleculePercentages(n_percent)
  
  
  
## Create data sheet atom percentage differences data  
    df_atomPercents <- data.frame(
        hPercentColumn <- hydrogenData, 
        cPercentColumn <- carbonData, 
        oPercentColumn <- oxygenData, 
        nPercentColumn <- nitrogenData 
)

## Combine experimental data sheet with atom percentage differences data and write to a csv
    df_carissasdata <- cbind(df, df_atomPercents)
    write.csv(df_carissasdata, file="~/Mainland Lab Dropbox/Mainland Lab Team Folder/Projects/CarissaDream/dreaming-with/data/processed/carissasdata.csv", overwrite = TRUE)


    
## Plots    

## Plotting hydrogen data    
df_carissasdata %>%
    ggplot(aes(y = hydrogenData, x = experimental_values)) +
    geom_point() +
    geom_smooth(method = "lm")+
    geom_point(color = "hotpink")+
    plot.background = element_rect(fill="lightpink")+
    labs(x="Percentage discrimination",y="Percent Difference of Hydrogen Atoms")

## Plotting carbon data 
df_carissasdata %>%
    ggplot(aes(y = carbonData, x = experimental_values)) +
    geom_point() +
    geom_smooth(method = "lm")+
    geom_point(color = "hotpink")+
    plot.background = element_rect(fill="lightpink")+
    labs(x="Percentage discrimination",y="Percent Difference of Carbon Atoms")

## Plotting oxygen data 
df_carissasdata %>%
    ggplot(aes(y = oxygenData, x = experimental_values)) +
    geom_point() +
    geom_smooth(method = "lm")+
    geom_point(color = "hotpink")+
    plot.background = element_rect(fill="lightpink")+
    labs(x="Percentage discrimination",y="Percent Difference of Oxygen Atoms")

## Plotting nitrogen data 
df_carissasdata %>%
    ggplot(aes(y = nitrogenData, x = experimental_values)) +
    geom_point() +
    geom_smooth(method = "lm")+
    geom_point(color = "hotpink")+
    plot.background = element_rect(fill="lightpink")+
    labs(x="Percentage discrimination",y="Percent Difference of Nitrogen Atoms")
