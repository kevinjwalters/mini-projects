library(tuneR)  ### is this the one that can write wav files
library(ggplot2)
library(gridExtra)
library(grid)   ### for textGrob
library(data.table)
library(english)

### Imagemagick creation of looping animated gif
### convert -delay 25 -resize 480x360 +repage $(ls waveform-construction-sawtooth-i00*) -loop 0 ../sawtooth.test.2.gif

filebase <- "waveform-construction-"

samplelen <- 512
# small number gives smoother, longer animated wave addition
chunksize <- 64
waverange <- 1.3

### geom_step looks useful for any future reconstruction graphs
### also rep(a,each=18/length(a)) can resample array without interpolation

# Create at 1600:1200 (4:3 ratio) then scale down later for animated gif (480 x 360 ?)
gdpi <- 100
gheight <- 12
gwidth <- 16

# return fundamental or overtone in round brackets
fundover <- function(i) {
  ifelse(i==1, "(fundamental)", paste("(",ordinal(i-1)," overtone)",sep=""))
}

# return fundamental and overtone(s) summary
# need linebreak for big fonts
fundoversum <- function(i) {
  if(i==1) {
    "(fundamental only)"
  } else if(i == 2) {
    "(fundamental\n+ 1 overtone)"
  } else { 
    paste("(fundamental +\n",i-1," overtones)",sep="")
  }
}

proper <- function(s) sub("(.)", ("\\U\\1"), tolower(s), pe=TRUE)

### functions are not a vector type so must be stored in a list
### I() needs to be used to stop automatic attempt to convert to vector
### but constructed data.frame cannot be displayed, gives an error - tut
##graphs <- data.frame(name=c("sawtooth", "square"),
##                     rep=c(5, 5),
##                     func=I(list(function (n, x) { -1^(n-1) * 1/n * sin(n * x) },
##                                 function (n, x) { 1/(n*2-1) * sin((n*2-1)*x) }))
##                    )

### data.table can hold lists
graphs <- data.table(name=c("sawtooth",
                            "square"),
                     rep=c(10, 10),
                     wavefunc=list(function (n, x) { 2/pi * (-1)^(n-1) * 1/n * sin(n * (x-pi)) },
                                   function (n, x) { 4/pi * 1/(n*2-1) * sin((n*2-1)*x) } ),
                     frequencyfunc=list(function (n) { n },
                                        function (n) { n*2-1 } )
                    )

# possibly a better way to do this
blank <- textGrob("")
                    
for(gi in 1:nrow(graphs)) {
    name <- graphs$name[[gi]]
    rep  <- graphs$rep[[gi]]
    wavefunc <- graphs$wavefunc[[gi]]
    
    ### NA will stop a line being plotted
    wave <- rep(NA, samplelen)
    
    for (i in seq(1,rep)) {
        from <- seq(1, samplelen, chunksize)
        to <- tail(c(from-1, samplelen), -1)  # tail -1 removing first element
        
        wave_add <- wavefunc(i, 0:(samplelen-1) / samplelen * 2 * pi)
        
        frame <- 1
        for(ci in seq(1, length(from))) {
            lastx <- to[ci]
            
            oldwavepart <- wave[from[ci]:lastx]
            oldwavepart[is.na(oldwavepart)] <- 0
            wave[from[ci]:lastx] <- oldwavepart + wave_add[from[ci]:lastx]
            
            dat <- data.frame(x=seq(0, 511),
                              xdeg=seq(0, 511) * 360/512,
                              wave=wave,
                              wave_add=wave_add,
                              sparkle=rep(NA, 512))
            lastxdeg <- lastx*360/512

            ### assumes there are at least SIX points per chunk
            dat$sparkle[(lastx-5):lastx] <- paste0("c",1:6)
                              ### TODO change Y of line
            ptl <- ggplot(dat, aes(x=xdeg, y=wave)) +
                   theme_light(base_size=28) +
                   labs(x="degrees", y="amplitude") +
                   geom_point() +
                   scale_x_continuous(breaks=seq(0, 360, by=90)) +
                   geom_segment(aes(x=lastxdeg, y=-waverange, xend=lastxdeg, yend=wave),
                                data=dat[lastx,],
                                color="darkgray", linetype="dashed", size=2) +
                   geom_point(shape=8, size=5, na.rm = TRUE, aes(color=sparkle)) +
                   scale_colour_manual(values=rep(c("yellow","orange","red"), each=2),
                                       guide=FALSE) +
                   coord_cartesian(ylim = c(-waverange, waverange))
            ptr <- blank
            pbl <- ggplot(dat, aes(x=xdeg, y=wave_add)) +
                   theme_light(base_size=28) +
                   labs(name="Adding harmonic wave", x="degrees", y="amplitude") +
                   geom_point() +
                   scale_x_continuous(breaks=seq(0, 360, by=90)) +
                   geom_segment(aes(x=lastxdeg, y=wave_add, xend=lastxdeg, yend=waverange),
                                data=dat[lastx,],
                                color="darkgray", linetype="dashed", size=2) +
                   coord_cartesian(ylim = c(-waverange, waverange))
            pbr <- textGrob(paste(proper(name), "wave\n",
                                  ordinal(i), "harmonic\n",
                                  fundover(i)),
                            gp = gpar(fontsize=60))

            ptable <- arrangeGrob(ptl, ptr, pbl, pbr, ncol=2)

            # show on screen
            #grid.draw(ptable)

            ### Save as 1600x1200 (4:3) png
            ### TODO ADD ANOTHER SEQUENCE NUMBER IN HERE 0 PAD
            ggsave(paste(filebase, name,
                         sprintf("-i%03df%03d", i, frame), ".png", sep=""),
                   ptable,
                   dpi=gdpi, height=gheight, width=gwidth, units="in")
            frame <- frame + 1
        }

        # take the first half of magnitude fft, rest is the same thing
        fft_magn_wave <- abs(fft(wave))
        ### R is ok with log10(0), easier to plot with geom_col if positive
        power_spectrum_db <- log10(fft_magn_wave / max(fft_magn_wave)) * 20
        power_spectrum_db_pos60 <- ifelse(power_spectrum_db > -60,
                                          power_spectrum_db + 60, NA)

        dat <- data.frame(x=seq(0, 511),
                          xdeg=seq(0, 511) * 360/512,
                          wave=wave,
                          wave_add=wave_add,
                          power_spectrum_db=power_spectrum_db,
                          power_spectrum_db_pos60=power_spectrum_db_pos60
                          )

        ptl <- ggplot(dat, aes(x=xdeg, y=wave)) +
               theme_light(base_size=28) +
               labs(x="degrees", y="amplitude") +
               geom_point() +
               scale_x_continuous(breaks=seq(0, 360, by=90)) +
               coord_cartesian(ylim = c(-waverange, waverange))
        ptr <- ggplot(dat, aes(x=factor(x), y=power_spectrum_db_pos60)) +
               theme_light(base_size=28) +
               theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.text.x=element_text(hjust=0.5, vjust=0.55, angle=90, size=16)) +
               labs("Power spectrum", x="frequency/Hz", y="power/dB") +
               geom_col(color="darkgreen", fill="darkgreen", width=0.7) +
               scale_x_discrete(breaks=1:(20-1)) + 
               scale_y_continuous(breaks=seq(0, 60, by=10)) +
               coord_cartesian(xlim=c(1, 20), ylim = c(0, 60))
        pbl <- ggplot(dat, aes(x=xdeg, y=wave_add)) +
               theme_light(base_size=28) +
               labs(name="Last harmonic wave", x="degrees", y="amplitude") +
               geom_point() +
               scale_x_continuous(breaks=seq(0, 360, by=90)) +
               coord_cartesian(ylim = c(-waverange, waverange))
        pbr <- textGrob(paste(proper(name), "wave\n",
                              i, "harmonics\n",
                              fundoversum(i)),
                        gp = gpar(fontsize=60))

        ptable <- arrangeGrob(ptl, ptr, pbl, pbr, ncol=2)

        # show on screen
        #grid.draw(ptable)

        ### Save as 1600x1200 (4:3) png
        ### Generate multiple plots to duplicate frame to dwell on image longer
        ### in animation (could just run copy command here?)
        duplicates <- ifelse(i==rep, 20, 10)  # do 20 on final image
        for(rependframe in 1:duplicates) {
            ggsave(paste(filebase, name,
                         sprintf("-i%03df%03d", i, frame), ".png", sep=""),
                   ptable,
                   dpi=gdpi, height=gheight, width=gwidth, units="in")
            frame <- frame + 1
        }
    }
}
