library('stringr')
library('ggplot2')
library('grid')
library('gridExtra')


# Input file looks like this
# each line will have lots of trailing whitespace
#
# "COM14",1610461592972228600,# time,samples,output,input                                                   
# "COM14",1610461592972228600,# time,samples,output,input                                                   
# "COM14",1610461592987848000,# time,samples,output,input                                                   
# "COM14",1610461592987848000,34.347825,-1,1,535.999966                                                     
# "COM14",1610461593003480300,34.356370,-1,8,535.999966                                                     
# "COM14",1610461593003480300,34.363205,-1,50,538.379860                                                    
# "COM14",1610461593019098400,34.376786,-1,1,535.999966                                                     
# "COM14",1610461593019098400,34.381852,-1,8,535.999966                                                     
# "COM14",1610461593034723200,34.388688,-1,50,536.399889
#"COM30",1610461594987849700,# time,samples,output,input                                                   
#"COM14",1610461594987849700,36.348770,-1,50,546.699953                                                    
#"COM30",1610461594987849700,# time,samples,output,input                                                   
#"COM30",1610461595003480000,# time,samples,output,input                                                   
#"COM14",1610461595003480000,36.362290,-1,1,535.999966                                                     
#"COM30",1610461595003480000,30.871994,0,1,367.999983                                                      
#"COM14",1610461595003480000,36.367478,-1,8,535.999966                                                     
#"COM30",1610461595003480000,30.875001,0,8,395.999985                                                      
#"COM14",1610461595019099500,36.374283,-1,50,547.879887                                                    
#"COM30",1610461595019099500,30.879998,0,50,378.559923                                                     
#"COM14",1610461595019099500,36.387527,-1,1,535.999966   

vref <- 3.3

datafile <- c("sampledata-run1.txt")

all_text_lines <- readLines(datafile)

### Discard the header lines which end up embedded in the data
data <- read.csv(text=all_text_lines[!grepl("#",all_text_lines)],
                 header=FALSE,
                 stringsAsFactors=FALSE,
                 col.names=c("port", "hosttime", "boardtime", "output","samples", "input"))

### REMINDER: as.numeric(as.character(factor_Variable)) is required!

data$hosttime <- as.numeric(data$hosttime)
data$relhosttime_s <- (data$hosttime - min(data$hosttime)) / 1e9
data$boardtime <- as.numeric(data$boardtime)
data$output <- as.numeric(data$output)
data$input <- as.numeric(data$input)

### Replace the -1 values indicating no output with NA
data$output[data$output < 0] <- NA


g1 <- ggplot(data[seq(308000,338000),], aes(x=relhosttime_s, color=port)) +
  ggtitle("ADC Comparison on Feather boards (A5)",
          subtitle="SAMD21 DAC (A0) providing voltage to 0.1uF ceramic capacitor") +
  theme_light(base_size=28) +
  theme(plot.title=element_text(hjust=0.5, size=38),
        plot.subtitle=element_text(hjust=0.5, size=28),
        legend.position="top", legend.title=element_blank(),
        legend.spacing.x = unit(0.5, 'char')) +
  geom_point(aes(y=input), alpha=0.5, shape=16) +
  geom_line(data=subset(data[seq(308000,338000),],!is.na(output)),
            aes(x=relhosttime_s, y=output, color="OUTPUT"), linetype="dashed") +
  labs(x="time (s)") +
  scale_y_continuous("ADC value",
                     sec.axis = sec_axis(~ . * vref / 65535.0, name = "voltage (V)")
                    ) +
  scale_color_manual("board", 
                     labels = c("COM14" = "ESP32-S2 6.1.0-rc0  ",
                                "COM18" = "nRF52840 6.0.1  ",
                                "COM30" = "SAMD21  6.0.1  ",
                                "COM31" = "SAMD51 6.0.1  ",
                                "OUTPUT" = "DAC"),
                     values = c("COM14" = "firebrick3",
                                "COM18" = "darkorchid1",
                                "COM30" = "goldenrod3",
                                "COM31" = "deepskyblue",
                                "OUTPUT" = "black")) +
  guides(colour=guide_legend(override.aes = list(size=5)))
g1


filebase <- "adc-analaysis"
filename <- sprintf("-run1-20210112-g1")
##
ggsave(paste(filebase, filename, ".png", sep=""),
       g1,
       dpi=100, height=30, width=40, units="in", limitsize = FALSE)
  