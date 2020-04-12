library('plot3D')
library('data.table')

### Magnetometer data from two Adafruit CLUE boards
### discussed in https://forums.adafruit.com/viewtopic.php?f=65&t=164250

data_mag <- list("clue 1", "earth", -10.5086, -7.46857, 39.8129,
 "clue 1", "earth", -11.1371, -7.04472, 40.2952,
 "clue 1", "earth", -11.1371, -7.04472, 40.2952,

 "clue 2", "earth", -46.0976, 20.3449, 21.5288,
 "clue 2", "earth", -46.5653, 20.8272, 21.2803,
 "clue 2", "earth", -46.2146, 20.8857, 21.5873,

 "clue 1", "magnet", -92.9845, -36.1736, -171.353,
 "clue 1", "magnet", -92.6045, -36.2905, -171.485,
 "clue 1", "magnet", -94.5922, -36.9921, -172.683,

 "clue 2", "magnet", -128.047, -2.30927, -204.238,
 "clue 2", "magnet", -127.916, -2.51388, -203.727,
 "clue 2", "magnet", -129.465, -1.95849, -205.013)

columns <- 5
data_mag_df <- data.frame(device=unlist(data_mag[seq(1, length(data_mag), columns)]),
                          conditions=unlist(data_mag[seq(2, length(data_mag), columns)]),
                          x=unlist(data_mag[seq(3, length(data_mag), columns)]),
                          y=unlist(data_mag[seq(4, length(data_mag), columns)]),
                          z=unlist(data_mag[seq(5, length(data_mag), columns)])
                        )

data_mag_dt <- as.data.table(data_mag_df)
### unique() is in there because there is a same x,y,z in data!
highest_data_mag_dt <- unique(data_mag_dt[data_mag_dt[, .I[z == max(z)], by=.(device, conditions)]$V1])


### pointsize affects fonts in a good way
pngscale <- 2

filebase <- "clue-magnetometer-comparison"

for (frame in seq(1, 21)) {
  filename <- paste(filebase, sprintf("-frame%04d", frame), ".png", sep="")
  png(filename, width=1600 * pngscale, height=1200 * pngscale,
    units="px", pointsize=36 * pngscale, type="cairo")
  
  phi_angle <- frame * 0.85 + 4
  theta_angle <- 60 - frame * 1.65
  with(data_mag_df, scatter3D(x,y,z, 
                              main="Magnetometer Readings for Two CLUE boards",
                              bty="g", d=8,
                              pch="*", lwd=3, cex=3, type="h", ticktype = "detailed",
                              cex.lab=1.25,
                              clab="z",
                              colkey=list(length=0.8, width=0.6, dist=0.01),
                              theta=theta_angle, phi=phi_angle))

  ### Draw interconnecting thick lines
  with(highest_data_mag_dt[conditions=="earth"], lines3D(x,y,z,
                                                         lwd=10,
                                                         colkey=FALSE,
                                                         add=TRUE))
  with(highest_data_mag_dt[conditions=="magnet"], lines3D(x,y,z,
                                                         lwd=10,
                                                         colkey=FALSE,
                                                         add=TRUE))
  ### Not sure how to do vertical adj
  ##zoffset <- ( max(data_mag_df$z) - min(data_mag_df$z) ) * 0.06
  zoffset <- 0
  with(highest_data_mag_dt, text3D(x,y,z+zoffset,
                                   adj=-0.2,
                                   labels=paste(device, conditions, sep="\n"),
                                   add=TRUE))

  dev.off()
}
