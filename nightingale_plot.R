##SCRIPT TO CREATE NIGHTINGALE PLOTS FROM ANGLE MEASUREMENTS##
##AUTHOR: Tamara Lechon Gomez
##ADDRESS: lechongomezt@cardiff.ac.uk
##DATE CREATED: 19 Aug 2024
##DATE LAST MODIFIED: 20 Aug 2024


#Load libraries needed for the code to work
library(dplyr)
library(ggplot2)

#Open pop up window to select input file
#Input file is a txt file with tab separated columns
#Each column will be a variable
#Default expectation is: Plate  Sample  Growth_Angle
#Growth_Angle is the only required variable
#Growth_Angle should have a + or - value (left = +, right = -)
root.df <- read.csv(file.choose(), header = T, sep = "\t")

#This line will transform the angle measurements to 360 if they
#have been measured from the vertical line
root.angle <- root.df$Root_Growth_Angle + 180

#Data will be discretised into intervals of 10 degree
value.cut <- cut(root.angle, breaks = 
                   c(0,10,30,50,70,90,110,130,150,170,190,210,230,250,
                     270,290,310,330,350,360))

#Data will be collapsed into 20 degree bins and labels changed to 
#midpoint
levels(value.cut) <- list("0"=c("(350,360]","(0,10]"),
                          "20"="(10,30]", "40"="(30,50]",
                          "60"="(50,70]", "80"="(70,90]",
                          "100"="(90,110]", "120"="(110,130]",
                          "140"="(130,150]", "160"="(150,170]",
                          "180"="(170,190]", "200"="(190,210]",
                          "220"="(210,230]", "240"="(230,250]",
                          "260"="(250,270]", "280"="(270,290]",
                          "300"="(290,310]", "320"="(310,330]",
                          "340"="(330,350]")

#Data will be summarised, ie. count the number of measurements in each bin
root.angle.table <- table(value.cut)

#Counts transformed into frequency, ie. how many times an angle
#has been measured expressed as a percentage of total measurements
root.prop.table <- prop.table(root.angle.table)*100
#and transformed into a dataframe (common R data structure)
root.angle.freq <- as.data.frame(root.prop.table)

#Variable names changed to avoid clash with global variables
colnames(root.angle.freq)[names(root.angle.freq) == "Freq"] <- "frequency"
colnames(root.angle.freq)[names(root.angle.freq) == "value.cut"] <- "angle"

#Adding a column that will round the percentage to nearest integer
#Can be used to annotate plot manually after plot generation
root.angle.freq$percentage <- round(root.angle.freq$frequency)

#The code below generates the plot. Colours can be changed using
#hexadecimal codes
p <- ggplot(root.angle.freq, aes(x=angle, y=frequency)) +  
  geom_bar(fill = "#9fd66f", stat = "identity", #bar fill colour
           linewidth=0.6, color="#61825e") + #bar line colour
  coord_polar(start = -0.175) + #this can be tweaked to align plot on any angle
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10, color = "black"),#angle labels colour
        axis.text.y.left = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(linetype = "dotted",color = "#ede8e8"),#polar graph lines type and colour
  )
p #print plot to screen

#The code below saves the plot to the computer as a PDF and PNG image
pdf(file = file.choose(), width = 5, height = 5) #choose a file name including .pdf
grid::grid.draw(p)
dev.off()
ggsave(p, filename = file.choose(), width = 5, height = 5, dpi=1200) #chose a file name including.png

#The code belows saves the table generated during data processing from
#which the plot is created
#Output should be a tab separated txt file with three variables:
#Angle: bin midpoint
#Frequency: frequency of measured angle expressed as percentage
#Percentage: frequency rounded to nearest integer
write.table(root.angle.freq, file = file.choose(), sep = "\t", #choose a file name including.txt
            col.names = T, row.names = F, quote = F)
