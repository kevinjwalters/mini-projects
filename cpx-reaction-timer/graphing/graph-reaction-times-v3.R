library('stringr')
library('ggplot2')
library('grid')
library('gridExtra')

### First data format
### Trial  1 : visual reaction time is  0.193359
### Trial  1 : audio reaction time is  0.130859
### Trial  2 : visual reaction time is  0.179688
### Trial  2 : audio reaction time is  0.140625

### Second data format
### ('Trial 1', 'visual', 0.234375, 0.234375, 0.0)
### ('Trial 1', 'auditory', 0.25, 0.25, 0.0)
### ('Trial 2', 'visual', 0.1875, 0.210938, 0.0331456)
### ('Trial 2', 'auditory', 0.234375, 0.242188, 0.0110485)
### ('Trial 3', 'visual', 0.25, 0.223958, 0.032526)
### ('Trial 3', 'auditory', 0.171875, 0.21875, 0.0413399)

### Make graphs with 0-100ms impossible region
### and exclude anything below this from stats
minreactiontime <- 100.0

maxgraphtime <- 450.0

makestripes <- function(x1, x2, y1, y2) {
  shift = (x2 - x1)/8
  xpos <- seq(x1 - shift * 4, x2 + shift * 4, length.out=40)
  data.frame(x=xpos-shift, y=y1, xend=xpos+shift, yend=y2)
}


makegraph <- function (datafile) {
  
filebase <- 'reactiontime-'
filename <- gsub("\\.txt","",basename(datafile))

text <- readLines(datafile)


description <- str_match(text[1], "^#\\s*(.*)")[,2]

# Original casual string approach from 0.5
pattern_f1 <- "Trial\\s+(\\d+)[\\s:]+(\\w+)\\s+reaction time is\\s*([\\d.]*)$"
data <- read.csv(text=apply(na.omit(str_match(text, pattern_f1)[,2:4]),
                            1,
                            function(str) { paste(str, collapse=",") }),
                 header=F,
                 col.names=c("trial", "type", "reactiontime"))
if (length(data$reactiontime) == 0) {
  # Tuple format intended for Mu from 0.7
  # the extra white space between parameters means it is not quite CSV
  pattern_f2 <- "^\\((.*)\\)$"
  data <- read.csv(text=na.omit(str_match(text, pattern_f2)[,2]),
                   header=F,
                   col.names=c("trialtext", "type", "reactiontime", "r_mean", "r_sd"))
  data$trial <- as.integer(str_match(data$trialtext, "(\\d+)")[,2])
}

data$reactiontimems = data$reactiontime * 1000

# Renaming audio if it occurs to auditory and then removing any single quotes
# whitespace removal is needed because of the "not quite CSV" aforementioned
# using levels() as this is a factor variable
levels(data$type) <- gsub("^audio$", "auditory", levels(data$type))
levels(data$type) <- gsub("\\s*'", "", levels(data$type))

### Make statistics which will be added to graph as text
### Extra space on the front of string is to make geom_label boxes
### look nicer :(
stats <- data.frame(type=levels(data$type),
                    x=max(data$trial),
                    y=rev(seq(10, by=30, length.out=length(levels(data$type)))),
                    label=sapply(levels(data$type), 
                                 function(x) { times <-data[data$type==x,]$reactiontimems;
                                               validtimes <- times[times >= minreactiontime];
                                               sprintf(" %s mean=%.1fms sd=%.1fms", x,
                                                      mean(validtimes),
                                                      sd(validtimes)) }))

title <- paste0("Reaction times - ", description)
  
g1 <- ggplot(data) +
  ##ggtitle(paste("Reaction times - ", description)) +
  theme_light(base_size=28) +
  theme(plot.title=element_text(hjust=0.5, size=34),
        legend.position="top", legend.title=element_blank(),
        legend.spacing.x = unit(0.5, 'char'),
        panel.grid.minor.y=element_blank()) +
  labs(y="reaction time (ms)") +
  geom_point(aes(x=trial, y=reactiontimems, shape=type), color="black", size=8, show.legend=F) +
  geom_point(aes(x=trial, y=reactiontimems, color=type, shape=type), size=6) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=0, ymax=minreactiontime, alpha=0.05, fill="pink") +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend),
               makestripes(min(data$trial) - 1, max(data$trial) + 1, 0, minreactiontime),
               inherit.aes=F,
               lineend="round", size=2, alpha=0.3, color="red") +
  geom_label(aes(x=x, y=y, label=label), stats, hjust=1, size=8, fill="white", alpha=0.6) +
  coord_cartesian(xlim=c(min(data$trial), max(data$trial)),
                  ylim=c(0, maxgraphtime))

g2 <- ggplot(data, aes(x=type, y=reactiontimems,
                      color=type, fill=type)) +
  ##ggtitle(paste("Reaction times - ", description)) +
  theme_light(base_size=28) +
  theme(plot.title=element_text(hjust=0.5, size=34),
        legend.position="top", legend.title=element_blank(),
        legend.spacing.x = unit(0.5, 'char'),
        panel.grid.minor.y=element_blank()) +
  labs(y="reaction time (ms)") +
  geom_violin() +
  geom_dotplot(binaxis='y', stackdir='center', binwidth=10, color="black") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=0, ymax=minreactiontime, alpha=0.05, fill="pink") +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend),
               makestripes(0, length(levels(data$type)) + 1, 0, minreactiontime),
               inherit.aes=F,
               lineend="round", size=2, alpha=0.3, color="red") +
  coord_cartesian(xlim=c(1, length(levels(data$type))),
                  ylim=c(0, maxgraphtime))
  #geom_histogram(alpha=0.5, binwidth=10) +
  #geom_histogram(aes(y=..density..), alpha=0.5, position="identity", binwidth=10) +
  #geom_density(alpha=.25) +
  #coord_cartesian(xlim=c(0, max(data$reactiontimems))) +
  #scale_fill_hue(l=40)

### Check plot on screen (fonts render poorly)
grid.arrange(g1, g2, ncol=2, top=title)

# 34 too big for some of the long descriptions
g <- arrangeGrob(g1, g2, ncol=2, top=textGrob(title, gp=gpar(fontsize=28)))

### Save as 1600x1200 (4:3) png
ggsave(paste(filebase, filename, ".png", sep=""),
       g,
       dpi=100, height=12, width=16, units="in")
}

lapply(Sys.glob("../data/*.txt"), makegraph)

