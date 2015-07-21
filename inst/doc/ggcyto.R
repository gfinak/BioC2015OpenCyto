## ---- echo=FALSE, results = "hide"---------------------------------------
library(knitr)
library(RColorBrewer)
opts_chunk$set(message = FALSE, error = FALSE, warning = FALSE, fig.height= 5, fig.width= 7)

## ---- echo = FALSE, results = "hide"-------------------------------------
library(ggcyto)
library(BioC2015OpenCyto)
# load data
data(tbdata)

## ---- echo = FALSE, eval=FALSE-------------------------------------------
#  #Add default FlowJo transformation since the original was not copied while downsampling.
#  fluorescence_channels <- as.vector(parameters(getCompensationMatrices(tbdata[[1]])))
#  #Create a default FlowJo transformation on all channels.
#  transList <- transformList(fluorescence_channels, flowJoTrans())
#  #Add the transformation to the dataset
#  flowWorkspace:::.addTrans(tbdata@pointer, transList)

## ------------------------------------------------------------------------
# Subset the data for a demo of visualization.
ptids <- unique(pData(tbdata)[["PID"]])[1:2] 
tbdata <- subset(tbdata, `PID` %in% ptids)
Rm("CD4",tbdata)

## ------------------------------------------------------------------------
# extract the CD3 population
fs <- getData(tbdata, "CD3")

## ------------------------------------------------------------------------
p <- ggcyto(fs, aes(x = CD4)) 
p1 <- p + geom_histogram(bin = 60) 
p1

## ------------------------------------------------------------------------
myPars <- ggcyto_par_set(limits = "instrument")
p1 + myPars

## ----results='markup'----------------------------------------------------
# print the default settings
ggcyto_par_default()

## ------------------------------------------------------------------------
p = p + geom_density() +  geom_density(fill = "black") + myPars
p

## ----results='asis'------------------------------------------------------
kable(pData(fs))

## ------------------------------------------------------------------------
#change facetting (default is facet_wrap(~name))
p + facet_grid(known_response ~ Peptide)

## ------------------------------------------------------------------------
# 2d hexbin
p <- ggcyto(fs, aes(x = CD4, y = CD8)) + geom_hex(bins = 60) + ylim(c(-100,4e3)) + xlim(c(-100,3e3))  
p

## ------------------------------------------------------------------------
p + scale_fill_gradientn(colours = brewer.pal(n=8,name="PiYG"),trans="sqrt")

## ------------------------------------------------------------------------
p + scale_fill_gradient(trans = "sqrt", low = "gray", high = "black")

## ------------------------------------------------------------------------
ggcyto(fs, aes(x = CD4, y = CD8))+ geom_hex(bins = 60)+geom_density2d(colour = "black")+ylim(c(-100,4e3)) + xlim(c(-100,3e3))  

## ------------------------------------------------------------------------
# add geom_gate layer
p <- ggcyto(fs, aes(x = CD4, y = CD8)) + geom_hex(bins = 60) + ylim(c(-100,4e3)) + xlim(c(-100,3e3))  
g <- getGate(tbdata, "CD4+")
p <- p + geom_gate(g)
p

## ------------------------------------------------------------------------
# add geom_stats
p + geom_stats()

## ---- echo=FALSE---------------------------------------------------------
### transform data (somehow it is not working)
# #transform data back to raw scale
# inverse.trans <- getTransformations(gs[[1]], inverse = T)[[" APC Cy7-A"]]

###  There is an issue in transform method for ncdfFlowSet that
## new cdf file created at the same folder as original cdf which could be probmatic for gs folder
# fs_raw <- transform(as(fs, "flowSet"), transformList("<APC Cy7-A>", inverse.trans), cdfFile =)
# p1 <- ggcyto(fs_raw, aes(x = CD4)) + geom_area(stat = "density") 
# p1 + myPars
# 
# # display data in log scale
# p1 + scale_x_flowJo_biexp() 

## ------------------------------------------------------------------------
#use customized range to overwrite the default data limits 
myPars <- ggcyto_par_set(limits = list(y = c(-100,4e3), x = c(-100,3e3)))
p <- ggcyto(tbdata, aes(x = CD4, y = CD8), subset = "CD3") 
p <- p + geom_hex(bins = 64) + myPars
p

## ------------------------------------------------------------------------
#only display marker on axis
p <- p + labs_cyto("marker")
p

## ------------------------------------------------------------------------
# add gate
p + geom_gate("CD4+CD8-")

## ------------------------------------------------------------------------
# add two gates
p <- p + geom_gate(c("CD4+CD8-","CD4-CD8-")) 
p

## ------------------------------------------------------------------------
p + geom_stats() 

## ------------------------------------------------------------------------
# add stats just for one specific gate
p + geom_stats("CD4+CD8-")

## ------------------------------------------------------------------------
# change stats type, background color and position
p + geom_stats("CD4+CD8-", type = "count", size = 6,  color = "white", fill = "black", adjust = 0.3)

## ------------------------------------------------------------------------
#'subset' is ommitted
p <- ggcyto(tbdata, aes(x = CD4, y = CD8)) + geom_hex(bins = 64) + myPars + geom_gate(c("CD4+CD8-", "CD4-CD8-"))
p

## ------------------------------------------------------------------------
Rm("CD8+",tbdata)
Rm("CD4+",tbdata)
p <- ggcyto(tbdata, aes(x = CD4, y = CD8), subset = "CD3") + geom_hex(bins = 64) + geom_gate() + geom_stats() + myPars
p 

## ------------------------------------------------------------------------
p + axis_x_inverse_trans() + axis_y_inverse_trans()

## ------------------------------------------------------------------------
class(p)
class(p$data)

