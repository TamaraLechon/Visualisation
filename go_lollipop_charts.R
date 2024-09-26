##SCRIPT TO CREATE LOLLIPOP CHARTS FROM GO ENRICHMENT ANALYSIS##
##AUTHOR: Tamara Lechon Gomez
##ADDRESS: lechongomezt@cardiff.ac.uk
##DATE CREATED: 30 Jun 2023
##DATE LAST MODIFIED: 26 Sep 2024

# Load Required Libraries
library(ggplot2)
library(wesanderson)

# Read data
#Expected input file format is a csv with 8 fields obtained from gprofiler, last field (driver terms) is optional
#Variables: source, term_name, termID, significance, intersection, term_size, enrichment, highlighted
gprof <- read.table(file.choose(), header = TRUE, sep = ",")
gprof$source <- factor(gprof$source)

# Highlight driver terms
#highlight <- ifelse(gprof$highlighted == "TRUE", "#C70039", "black") #optional step to highlight driver terms

# Plot with bars
p <- ggplot(gprof, aes(x=term_name, y=significance, size=(intersection/50), colour = source)) + #size can be adapted by modifying the divisor
  geom_segment( aes(x=term_name, xend=term_name, y=0, yend=significance), linewidth = 0.5) +
  scale_x_discrete(limits = gprof$term_name) +
  geom_point() +
  scale_color_manual(name = "Ontology", values = c("Cellular Component"="#C70039", "Biological Process"="#003366", "Molecular Function"="darkgoldenrod1")) +
  theme_light() +
  coord_flip() +
  #ylab("Significance") + xlab("Term") +
  #guides(size = guide_legend(title="Number of Genes", label = c(50, 100, 150, 200, 250)))+
  labs(x = "Term", y = "Significance", size = "Number of Genes") +
  scale_size_continuous(breaks = c(1, 2, 3, 4, 5), labels = c(50, 100, 150, 200, 250)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    #axis.text.y = element_text(colour = highlight)
  )
p


# Saving images
pdf(file = file.choose(), width = 10, height = 20) #choose a file name including .pdf, width and height can be modified
grid::grid.draw(p)
dev.off()
ggsave(p, filename = file.choose(), width = 10, height = 20, dpi = 1200) #choose a file name including .png, width and height can be modified

# Expected output is a lollipop chart summarising GO functional analysis saved as .png and .pdf