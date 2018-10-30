library('stringr')
library('ggplot2')

### Read data from serial console capture and filter out
### anything which looks like Python tuple style data
### for parsing into a data.frame
text <- readLines("putty.gas-sensors.test.3.log")
pattern <- "^\\((.*)\\)$"
datat3 <- read.csv(text=na.omit(str_match(text, pattern)[,2]),
                   header=F,
                   col.names=c("t1","t2","A0","A1","A2","A3","A4","A5","A6","A7"))

datat3$seconds = ( datat3$t1 + datat3$t2 ) / 2.0
datat3$minutes = datat3$seconds / 60.0

vcc <- 4.93
arefvolt <- 3.3
rlmq3 <- 500
rlmics5524 <- 5000

### Calculate the resistance of sensor from
### pinvalue (0-65535)
### varef (adc reference voltage, around 3.3)
### vcc (voltage, around 5.00)
### rl (load resistor, possibly supplemented externally)
### MQ-3 rl around 500 ohm - measured as 496
### MiCS-5524 around 5k ohm - measure as 4k98
calcRs <- function (pinvalue, varef, vcc, rl) {
  rs <- (vcc * 65536 * rl) / (pinvalue * varef) - rl
  rs
}

### Ratio is normall Ro/Rs but taking reciprocal to make it
### go up with ppm
datat3$A1rors = 24000 / calcRs(datat3$A1, arefvolt, vcc, rlmq3)
datat3$A2rors = 53000 / calcRs(datat3$A2, arefvolt, vcc, rlmics5524)

### Size values are chosen to look good on png output and
### may not render well on screen
g <- ggplot(datat3, aes(x=seconds)) +
     ggtitle("Sensor test - methylated spirit on cotton bud") +
     theme_light(base_size=28) +
     theme(plot.title=element_text(hjust=0.5, size=34),
           axis.text.x=element_text(hjust=0.5, vjust=0.5, angle=90)) +
     geom_line(aes(y=A1rors, color="MiCS-5524"),
               alpha=0.75, size=1.5) +
     geom_line(aes(y=A2rors, color="MQ-3"),
               alpha=0.75, size=1.5) +
     geom_point(aes(y=A1rors), size=2) +
     geom_point(aes(y=A2rors), size=2) +
     scale_y_log10("Ro/Rs") +
     scale_color_manual(values=c("red", "green")) +
     labs(color="sensor") +
     geom_vline(xintercept=77, color="darkgray",
                linetype="dashed", size=2) +
     annotate("text", label=" IN", x=77, y=0.8, hjust=0, size=8) +
     geom_vline(xintercept=345, color="darkgray",
                linetype="dashed", size=2) +
     annotate("text", label=" OUT", x=345, y=0.8, hjust=0, size=8) +
     coord_cartesian(xlim=c(50,130), ylim = c(0.8, 100))

### Check plot on screen
g

### Save as 1600x1200 (4:3) png
ggsave("somefile.png", g,
       dpi=100, height=12, width=16, units="in")
