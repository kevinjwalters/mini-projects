library(tuneR)  ### is this the one that can write wav files
library(ggplot2)
library(gridExtra)
library(grid)   ### for textGrob
library(data.table)
library(english)

filebase <- "waveform-construction-"

samplelen <- 512
chunksize <- 64
waverange <- 1.2

### TODO - make graphs look pretty
### get magnitude right
### do i want to swtich to -1.2 to 1.2 for overshoots
### get x axis right
### discard second half of FFT
### look at previous graphs for inspiration as they were pretty clean
### colour? could add could during graph anim to show it being "drawn" on final point
### some labels?
### label as fundamental, first harmonic, second harmonic?
### maybe the answer is not to repeat final image but do NON looping animated gif

### geom_step looks useful for any future reconstruction graphs
### also rep(a,each=18/length(a)) can resample array without interpolation

# Create at 1600:1200 (4:3 ratio) then scale down later for animated gif (480 x 360 ?)
gdpi <- 100
gheight <- 16
gwidth <- 12

# return fundamental or overtone in round brackets
fundover <- function(i) {
  ifelse(i==1, "(fundamental)", paste("(",ordinal(i-1)," overtone)",sep="")
}

# return fundamental and overtone(s) summary
fundoversum <- function(i) {
  if(i==1) {
    "(fundamental only)"
  } else if (i == 2) {
    "(fundamental + 1 overtone)"
  } else { 
    paste("(fundamental + ",i-1," overtones)",sep="")
  }
}

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
                     rep=c(5, 5),
                     func=list(function (n, x) { 1/pi * (-1)^n * 1/n * sin(n * x) },
                               function (n, x) { 4/pi * 1/(n*2-1) * sin((n*2-1)*x) } )
                    )

# possibly a better way to do this
blank <- textGrob("")
                    
for(gi in 1:nrow(graphs)) {
    name <- graphs$name[[gi]]
    rep  <- graphs$rep[[gi]]
    func <- graphs$func[[gi]]
    
    ### NA will stop a line being plotted
    wave <- rep(NA, samplelen)
    
    for (i in seq(1,rep)) {
        from <- seq(1, samplelen, chunksize)
        to <- tail(c(from-1, samplelen), -1)  # tail -1 removing first element
        
        wave_add <- func(i, 0:(samplelen-1) / samplelen * 2 * pi)
        
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

            ### assumes there are at least three points per chunk
            dat$sparkle[(lastx-2):lastx] <- c("c1", "c2", "c3")
                              ### TODO change Y of line
            ptl <- ggplot(dat, aes(x=xdeg, y=wave)) +
                   theme_light(base_size=28) +
                   geom_point() +
                   scale_x_continuous(breaks=seq(0, 360, by=45)) +
                   geom_segment(aes(x=lastxdeg, y=-waverange, xend=lastxdeg, yend=wave),
                                data=dat[lastx,],
                                color="darkgray", linetype="dashed", size=2) +
                   geom_point(shape=8, size=4, na.rm = TRUE, aes(color=sparkle)) +
                   scale_colour_manual(values=c("yellow","orange","red"), guide=FALSE) +
                   coord_cartesian(ylim = c(-waverange, waverange))
            ptr <- blank
            pbl <- ggplot(dat, aes(x=xdeg, y=wave_add)) +
                   theme_light(base_size=28) +
                   geom_point() + 
                   scale_x_continuous(breaks=seq(0, 360, by=45)) +
                   geom_segment(aes(x=lastxdeg, y=wave_add, xend=lastxdeg, yend=waverange),
                                data=dat[lastx,],
                                color="darkgray", linetype="dashed", size=2) +
                   coord_cartesian(ylim = c(-waverange, waverange))
            pbr <- textGrob(paste(name, "wave\n",
                                  ordinal(i), "harmonic\n",
                                  fundover(i)))
                            
            ptable <- arrangeGrob(ptl, ptr, pbl, pbr, ncol=2)

            # show on screen
            #grid.draw(ptable)

            ### Save as 1600x1200 (4:3) png
            ### TODO ADD ANOTHER SEQUENCE NUMBER IN HERE 0 PAD
            ggsave(paste(filebase, name,
                         sprintf("i%03df%03d", i, frame), ".png", sep=""),
                   ptable,
                   dpi=gdpi, height=gheight, width=gwidth, units="in")
            frame <- frame + 1
        }
        
        wave <- wave + wave_add
        
        # take the first half of magnitude fft, rest is the same thing
        fft_magn_wave <- abs(fft(wave))
        ### R is ok with log10(0), easier to plot with geom_col if positive
        power_spectrum_db <- log10(fft_magn_wave / max(fft_magn_wave)) * 20
        power_spectrum_db_pos60 <= ifelse(power_spectrum_db > -60,
                                          power_spectrum_db + 60, 0)

        dat <- data.frame(x=seq(0, 511),
                          xdeg=seq(0, 511) * 360/512,
                          wave=wave,
                          wave_add=wave_add,
                          power_spectrum_db=power_spectrum_db,
                          power_spectrum_db_pos60=power_spectrum_db_pos60
                          )

        ptl <- ggplot(dat, aes(x=xdeg, y=wave)) +
               theme_light(base_size=28) +
               geom_point() +
               scale_x_continuous(breaks=seq(0, 360, by=45)) +
               coord_cartesian(ylim = c(-waverange, waverange))
        ptr <- ggplot(dat, aes(x=factor(x), y=power_spectrum_db_pos60)) +
               theme_light(base_size=28) +
               geom_point() +
               coord_cartesian(xlim=c(1, 20), ylim = c(0, 60))
        pbl <- ggplot(dat, aes(x=xdeg, y=wave_add)) +
               theme_light(base_size=28) +
               geom_point() +
               scale_x_continuous(breaks=seq(0, 360, by=45)) +
               coord_cartesian(ylim = c(-waverange, waverange))
        pbr <- textGrob(paste(name, "wave\n",
                              i, "harmonics\n",
                              fundoversum(i)))

        ptable <- arrangeGrob(ptl, ptr, pbl, pbr, ncol=2)

        # show on screen
        #grid.draw(ptable)

        ### Save as 1600x1200 (4:3) png
        ### Generate multiple plots to duplicate frame to dwell on image longer
        ### in animation (could just run copy command here?)
        for(rependframe in 1:4) {
            ggsave(paste(filebase, name,
                         sprintf("i%03df%03d", i, frame), ".png", sep=""),
                   ptable,
                   dpi=gdpi, height=gheight, width=gwidth, units="in")
            frame <- frame + 1
        }
    }
}
