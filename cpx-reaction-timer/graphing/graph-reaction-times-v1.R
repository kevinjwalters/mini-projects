library('stringr')
library('ggplot2')

### First format
### Trial  1 : visual reaction time is  0.193359
### Trial  1 : audio reaction time is  0.130859
### Trial  2 : visual reaction time is  0.179688
### Trial  2 : audio reaction time is  0.140625

### Second format

### ('Trial 1', 'visual', 0.234375, 0.234375, 0.0)
### ('Trial 1', 'auditory', 0.25, 0.25, 0.0)
### ('Trial 2', 'visual', 0.1875, 0.210938, 0.0331456)
### ('Trial 2', 'auditory', 0.234375, 0.242188, 0.0110485)
### ('Trial 3', 'visual', 0.25, 0.223958, 0.032526)
### ('Trial 3', 'auditory', 0.171875, 0.21875, 0.0413399)

makegraph <- function (datafile) {
  
filebase <- 'reactiontime-'
filename <- gsub("\\.txt","",basename(datafile))

text <- readLines(datafile)


description <- str_match(text[1], "^# *(.*)")[,2]

# Original casual string approach from 0.5
pattern_f1 <- "Trial\\s+(\\d+)[\\s:]+(\\w+)\\s+reaction time is\\s*([\\d.]*)$"
data <- read.csv(text=apply(na.omit(str_match(text, pattern_f1)[,2:4]),
                            1,
                            function(str) { paste(str, collapse=",") }),
                 header=F,
                 col.names=c("trial", "type", "reactiontime"))
if (length(data$reactiontime) == 0) {
  # Tuple format intended for Mu from 0.7
  pattern_f2 <- "^\\((.*)\\)$"
  data <- read.csv(text=na.omit(str_match(text, pattern_f1)[,2]),
                   header=F,
                   col.names=c("trialtext", "type", "reactiontime", "r_mean", "r_sd"))
  data$trial <- as.integer(str_match(data$trialtext, "\\d+")[,2])
}

data$reactiontimems = data$reactiontime * 1000

# Renaming audio if it occurs to auditory
# using levels as this is a factor variable
levels(data$type) <- gsub("audio", "auditory", levels(data$type))

# 
stats <- c("TODO - new data.frame with stats and X y position for geom_label plot - UNFINISHED")
stats <- data.frame(type=levels(data$type),
                    x=max(data$trial),
                    y=rev(seq(20, by=30, length.out=length(levels(data$type)))),
                    label=sapply(levels(data$type), 
                                 function(x) { times <-data[data$type==x,]$reactiontimems;
                                               sprintf("%s mean=%.1fms sd=%.1fms", x, mean(times), sd(times)) }))

g <- ggplot(data) +
  ggtitle(paste("Reaction times - ", description)) +
  theme_light(base_size=28) +
  theme(plot.title=element_text(hjust=0.5, size=34),
        legend.position="top", legend.title=element_blank(),
        legend.spacing.x = unit(0.5, 'char'),
        panel.grid.minor.y=element_blank()) +
  labs(y="reaction time (ms)") +
  geom_point(aes(x=trial, y=reactiontimems, color=type, shape=type), size=8) + 
  coord_cartesian(ylim=c(0, max(data$reactiontimems))) +
  geom_text(aes(x=x, y=y, label=label), stats, hjust=1, size=10)

### Check plot on screen (fonts render poorly)
g

### Save as 1600x1200 (4:3) png
ggsave(paste(filebase, filename, ".png", sep=""), g,
       dpi=100, height=12, width=16, units="in")
}


lapply(Sys.glob("../data/*.txt"), makegraph)
