setwd("~/Desktop/U Georgia/Rebecca_Project")
outputs <- read.csv("results.treatment.mod.may10.csv", header = T, stringsAsFactors = F) #read in model outputs
library(ggplot2)
library(egg)

phis <- outputs[c(68:71, 73:84),c(1,2,4,5)] #survival estimates
phis$Placement <- c(rep(1,4), rep(2:4, 4)) #locations on the graph
phis$Label <- c(rep("Average",4), rep(c("Direct release", "Head-start", "Wild recruit"), 4))
phis$group <- c("Hatchlings\n", "Juveniles\n", "Subadults\n", "Adults\n",
                rep(c("Hatchlings\n", "Juveniles\n", "Subadults\n", "Adults\n"), each = 3))
phis <- phis[phis$group != "Adults\n",] #remove irrelevant graph item
phis$p <- 6

g <- ggplot(phis, aes(y = Mean, x = Placement, shape = Label))+
  facet_grid(~group)+
  geom_errorbar(aes(ymin = Lower95, ymax = Upper95), width = 0.2, size = 1.2)+
  geom_point(size = 7, fill = "black")+
  scale_shape_manual(values=c(21:24))+
  theme_light()+
  labs(y = "\nAnnual apparent survival\n")+
  ylim(0,1)+
  theme(text = element_text(size = 30, color = "black"),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size = 28),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(size=.7, color = "black"),
        strip.placement = "inside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", size = 32),
        legend.position="bottom", legend.direction="horizontal",
        legend.text = element_text(size = 32),
        panel.border = element_rect(colour = "white", fill=NA, size=.5))
g <- tag_facet(g, tag_pool = c("A Hatchlings", "B Juveniles", "C Subadults"), close = "", open = "", hjust = -0.07, cex = 8) #put the labels on top of panels
#g <- tag_facet(g, tag_pool = c("A", "B", "C"), close = "", open = "", hjust = -0.50, cex = 8) #alternate labeling
g


ggsave("Fig3_July13.jpeg", g, dpi = 600, width = 16, height = 8, device = "jpeg" ) #save figure
#ggsave("Fig3_July13_alt.jpeg", g, dpi = 600, width = 16, height = 8, device = "jpeg" ) #save alt figure 
