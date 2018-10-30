library('stringr')
library('ggplot2')

text <- readLines("putty.gas-sensors.test.5.log")
### Python tuple format, e.g.
### (1969.553852,1970.523834,28891.7,2313.95,7700.89,28896.1,29661.0,29566.4,29404.4,29292.5)
pattern <- "^\\((.*)\\)$"

datat3 <- read.csv(text=na.omit(str_match(text, pattern)[,2]),
                   header=F,
                   col.names=c("t1","t2","A0","A1","A2","A3","A4","A5","A6","A7"))

datat3$seconds = (datat3$t1 + datat3$t2 ) / 2.0
datat3$minutes = datat3$seconds / 60.0
datat3$A2div20 = datat3$A2 / 20.0

### ADC reference voltage
arefvolt <- 3.25
### Supplemented load resistances
rlmq3      <- 496
rlmics5524 <- 4980

### Calculate the resistance of sensor from
### pinvalue (0-65535)
### varef (adc reference voltage, around 3.3)
### vcc (voltage, around 5.00)
### rl (load resistor, possibly supplemented)
### MQ-3 rl around 500 ohm - measured as 496
### MiCS-5524 around 5k ohm - measure as 4980
calcRs <- function (pinvalue, varef, vcc, rl) {
  rs <- (vcc * 65536 * rl) / (pinvalue * varef) - rl
  rs
}

filebase <- "gas.sensor.test.DATE."

### Size values are chosen to look good on png output and
### may not render well on screen
gcols <- c("red", "green")

vcc <- 5.09
sain        <- 330
lidajar     <- 890
saoutlidoff <- 1060
sampledesc <- "red wine low ppm"
filename   <- "t15"

### Ratio is normal Ro/Rs but taking reciprocal to make it
### go up with ppm
### Ro based on values between 50 and 20 seconds before sample introduced
### This is a bit inefficient as it recalcs all values regardless of whether they are
### being plotted
datat3$A1rors <- calcRs(mean(datat3[datat3$t1 > sain-50 &
                                    datat3$t1 < sain-20,"A1"]),
                        arefvolt, vcc, rlmq3)  /
                 calcRs(datat3$A1, arefvolt, vcc, rlmq3)
datat3$A2rors <- calcRs(mean(datat3[datat3$t1 > sain-50 &
                                    datat3$t1 < sain-20,"A2"]),
                        arefvolt, vcc, rlmics5524) /
                 calcRs(datat3$A2, arefvolt, vcc, rlmics5524)

g <- ggplot(datat3, aes(x=seconds)) +
     ggtitle(paste("Sensor test -", sampledesc)) +
     theme_light(base_size=28) +
     theme(plot.title=element_text(hjust=0.5, size=34),
           axis.text.x=element_text(hjust=0.5, vjust=0.5, angle=90),
           panel.grid.minor.y=element_blank()) +
     geom_line(aes(y=A1rors, color="MiCS-5524"), alpha=0.75, size=2) +
     geom_line(aes(y=A2rors, color="MQ-3"), alpha=0.75, size=2) +
     geom_point(aes(y=A1rors), size=2.5, alpha=0.25, color=gcols[1]) +
     geom_point(aes(y=A2rors), size=2.5, alpha=0.25, color=gcols[2]) +
#     scale_y_continuous("pin.value",
#                        sec.axis = sec_axis(~ . / 65536 * arefvolt,
#                                            name = "volt")) +
     scale_y_log10("Ro/Rs") +
     annotation_logticks(sides = "lr",
                         short=unit(10,"points"),
                         mid=unit(20,"points"),
                         long=unit(40,"points")) +
     scale_color_manual(values=gcols) +
     labs(color="sensor") + 
     geom_vline(xintercept=sain, color="darkgray", linetype="dashed", size=2) +
     annotate("text", label=" IN", x=sain, y=0.8,
              vjust=1, hjust=0, size=8) +
     geom_vline(xintercept=lidajar, color="darkgray", linetype="dashed", size=2) +
     annotate("text", label=" LID AJAR", x=lidajar, y=0.8,
              vjust=1, hjust=0, size=8) +
     geom_vline(xintercept=saoutlidoff, color="darkgray", linetype="dashed", size=2) +
     annotate("text", label=" OUT\n LID OFF", x=saoutlidoff, y=0.8,
              vjust=1, hjust=0, size=8) +
     coord_cartesian(xlim=c(sain-30, saoutlidoff+100), ylim = c(0.6, 55))

### Check plot on screen (fonts render poorly)  
g

### Save as 1600x1200 (4:3) png
ggsave(paste(filebase, filename, ".png", sep=""), g,
       dpi=100, height=12, width=16, units="in")
