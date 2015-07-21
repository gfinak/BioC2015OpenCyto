## ----preamble, echo = FALSE, results = "hide"----------------------------
library(knitr)
opts_chunk$set(error=FALSE, message = FALSE, eval = TRUE, warning = FALSE, cache=FALSE,fig.width=7,fig.height=7)

## ----libs, echo=FALSE, results = "hide"----------------------------------
library(BioC2015OpenCyto)
library(ggcyto)
data(tbdata)
library(openCyto)

## ----clean---------------------------------------------------------------
#Delete existing gates
Rm("Singlets",tbdata)

## ----subset--------------------------------------------------------------
tbdata_subset = subset(tbdata,`PID`%in%unique(pData(tbdata)$`PID`)[1:2])

## ----singlets,warning=FALSE,error=FALSE----------------------------------
template = add_pop(
  tbdata_subset, alias = "Singlets", pop = "Singlets", parent = "root", dims = "FSC-A,FSC-H",gating_method = "singletGate",gating_args =
  "wider_gate=TRUE,subsample_pct=0.1"
  )

## ----plot_singlets-------------------------------------------------------
ggcyto(tbdata_subset,mapping = aes(x = `FSC-A`,y = `FSC-H`)) + geom_hex(bins =
                                                                          50) + geom_gate("Singlets") + xlim(c(0,2e5))

## ----plot_cd14-----------------------------------------------------------
ggcyto(tbdata_subset,mapping = aes(x = "CD14"),subset = "Singlets") + geom_histogram(binwidth =
                                                                                       100) + xlim(c(-100,4096)) + facet_wrap( ~ PID,scales = "free")

## ----cd14----------------------------------------------------------------
template = rbind(
  template,add_pop(
  tbdata_subset,alias = "CD14n", "CD14-", parent = "Singlets", dims = "CD14", gating_method =
  "mindensity",groupBy = "PID",collapseDataForGating = TRUE
  )
  )

## ----replot_cd14---------------------------------------------------------
ggcyto(tbdata_subset,mapping = aes(x = "CD14"),subset = "Singlets") + geom_histogram(binwidth =
                                                                                       100) + xlim(c(-100,4096)) + facet_wrap( ~ PID,scales = "free") + geom_gate("CD14n")

## ----nondebris-----------------------------------------------------------
template = template[1:2,]
  template = rbind(
  template,add_pop(
  tbdata_subset,alias = "nonDebris",pop = "nonDebris+",parent = "CD14n",dims = "FSC-A",gating_method =
  "boundary",collapseDataForGating = FALSE,gating_args = "min=40000,max=2.5e5"
  )
  )
  ggcyto(tbdata_subset,mapping = aes(x = "FSC-A",y = "SSC-A"),subset = "CD14n") + geom_hex(bins = 100) +
    geom_gate()

## ----lymphocytes---------------------------------------------------------
  template = rbind(
    template, add_pop(
    tbdata_subset,alias = "Lymphocytes",pop = "Lymphocytes+",parent = "nonDebris",dims = "FSC-A,SSC-A",gating_method =
    "flowClust.2d",preprocessing_method = "prior_flowClust",preprocessing_args="K=3",collapseDataForGating = FALSE,gating_args =
    "quantile=0.95,target=c(80000,50000),K=3"
    )
    )
  ggcyto(tbdata_subset,mapping = aes(x = "FSC-A",y = "SSC-A"),subset = "nonDebris") +
    geom_hex(bins = 50) + geom_gate("Lymphocytes")

## ----live----------------------------------------------------------------
  template = rbind(
    template, add_pop(
    tbdata_subset,alias = "Live",pop = "Live+",parent = "Lymphocytes",dims = "<Am Cyan-A>",gating_method = "boundary",gating_args = "min=0,max=2000",collapseDataForGating = FALSE
    )
    )
    
    ggcyto(tbdata_subset,mapping = aes(x = "AViD",y="SSC-A"),subset = "Lymphocytes") +
    geom_hex(bins = 100) + ggcyto_par_set(limits = "instrument") +
    geom_gate("Live")

## ----cd3-----------------------------------------------------------------
    template = rbind(
      template, add_pop(
      tbdata_subset,alias = "CD3",pop = "CD3+",parent = "Live",dims = "CD3",gating_method = "mindensity"
      )
      )
      ggcyto(tbdata_subset,mapping = aes(x = "CD3",y = "FSC-A"),subset = "Live") +
      geom_hex(bins=100) + ggcyto_par_set(limits = "instrument") + geom_gate("CD3")

## ----cd4cd8--------------------------------------------------------------
      template = rbind(
        template, add_pop(
        tbdata_subset,alias = "*",pop = "CD4+/-CD8+/-",dims = "CD4,CD8",gating_method =
        "mindensity2",gating_args="gate_range=c(500,4000)",parent = "CD3")
        )
        ggcyto(tbdata_subset,mapping = aes(x = "CD4",y = "CD8"),subset = "CD3") +
        geom_hex(bins=100) + ggcyto_par_set(limits = "data") + geom_gate()+xlim(0,3000)+ylim(0,4000)
        

## ----table,results='asis'------------------------------------------------
kable(pData(parameters(getData(tbdata_subset[[1]])))[,1:2],row.names=FALSE)

## ----tnf-----------------------------------------------------------------
        template = rbind(
          template, add_pop(
          tbdata_subset,alias = "TNF",pop = "TNF+",dims = "TNFa",gating_method = "tailgate",gating_args = "auto_tol=TRUE",parent =
          "CD4+CD8-",collapseDataForGating = TRUE,groupBy = "PID"
          )
          )
          ggcyto(tbdata_subset,mapping = aes(x = "TNFa",y = "SSC-A"),subset = "CD4+CD8-") +
          geom_hex(bins = 100) + ggcyto_par_set(limits = "instrument") + geom_gate() +
          facet_grid(PID ~ Peptide,scale = "free") + geom_gate()

## ----il2-----------------------------------------------------------------
          template = rbind(
            template, add_pop(
            tbdata_subset,alias = "IL2",pop = "IL2+",dims = "IL2",gating_method = "tailgate",gating_args = "auto_tol=TRUE",parent =
            "CD4+CD8-",collapseDataForGating = TRUE,groupBy = "PID"
            )
            )
          ggcyto(tbdata_subset,mapping = aes(x = "IL2",y = "SSC-A"),subset = "CD4+CD8-") +
            geom_hex(bins = 100) + ggcyto_par_set(limits = "instrument") + geom_gate() +
            facet_grid(PID ~ Peptide,scale = "free") + geom_gate()

## ----il22----------------------------------------------------------------
          template = rbind(
            template, add_pop(
            tbdata_subset,alias = "IL22",pop = "IL22+",dims = "IL22",gating_method =
            "tailgate",gating_args = "auto_tol=TRUE,adjust=2",parent = "CD4+CD8-",collapseDataForGating = TRUE,groupBy = "PID"
            )
            )
          ggcyto(tbdata_subset,mapping = aes(x = "IL22",y = "SSC-A"),subset = "CD4+CD8-") +
            geom_hex(bins = 100) + ggcyto_par_set(limits = "instrument") + geom_gate() +
            facet_grid(PID ~ Peptide,scale = "free") + geom_gate()

## ----il4-----------------------------------------------------------------
          template = rbind(
            template, add_pop(
            tbdata_subset,alias = "IL4",pop = "IL4+",dims = "IL4",gating_method = "tailgate",gating_args = "auto_tol=TRUE",parent =
            "CD4+CD8-",collapseDataForGating = TRUE,groupBy = "PID"
            )
            )
            ggcyto(tbdata_subset,mapping = aes(x = "IL4",y = "SSC-A"),subset = "CD4+CD8-") +
            geom_hex(bins = 100) + ggcyto_par_set(limits = "instrument") + geom_gate() +
            facet_grid(PID ~ Peptide,scale = "free") + geom_gate()

## ----cd154---------------------------------------------------------------
            template = rbind(
              template, add_pop(
              tbdata_subset,alias = "CD154",pop = "CD154+",dims = "CD154",gating_method =
              "tailgate",gating_args = "auto_tol=TRUE",parent = "CD4+CD8-",collapseDataForGating = TRUE,groupBy = "PID"
              )
              )
              ggcyto(tbdata_subset,mapping = aes(x = "CD154",y = "SSC-A"),subset = "CD4+CD8-") +
              geom_hex(bins = 100) + ggcyto_par_set(limits = "instrument") + geom_gate() +
              facet_grid(PID ~ Peptide,scale = "free") + geom_gate()

## ----ifng----------------------------------------------------------------
              template = rbind(
                template, add_pop(
                tbdata_subset,alias = "IFNg",pop = "IFNg+",dims = "IFNg",gating_method =
                "tailgate",gating_args = "auto_tol=TRUE",parent = "CD4+CD8-",collapseDataForGating = TRUE,groupBy = "PID"
                )
                )
                ggcyto(tbdata_subset,mapping = aes(x = "IFNg",y = "SSC-A"),subset = "CD4+CD8-") +
                geom_hex(bins = 100) + ggcyto_par_set(limits = "instrument") + geom_gate() +
                facet_grid(PID ~ Peptide,scale = "free") + geom_gate()

## ----il17a---------------------------------------------------------------
                template = rbind(
                  template, add_pop(
                  tbdata_subset,alias = "IL17a",pop = "IL17a+",dims = "IL17a",gating_method =
                  "tailgate",gating_args = "auto_tol=TRUE",parent = "CD4+CD8-",collapseDataForGating = TRUE,groupBy = "PID"
                  )
                  )
                  ggcyto(tbdata_subset,mapping = aes(x = "IL17a",y = "SSC-A"),subset = "CD4+CD8-") +
                  geom_hex(bins = 100) + ggcyto_par_set(limits = "instrument") + geom_gate() +
                  facet_grid(PID ~ Peptide,scale = "free") + geom_gate()

## ----clean2--------------------------------------------------------------
tmp = tempfile(fileext=".csv")
write.csv(template,tmp,row.names=FALSE)
gt = gatingTemplate(tmp)
data(tbdata)
Rm("Singlets",tbdata) #Remove any gates attached to the GatingSet

## ----gating--------------------------------------------------------------
library(parallel)
#clean up our gating set
gating(gt,tbdata,parallel_type="multicore",mc.cores=7)

## ----qc------------------------------------------------------------------
stats = getPopStats(tbdata) #extract stats
stats[,prop := Count/ParentCount] #compute the cell proportions
stats = merge(stats,pData(tbdata),by="name")
data.table::setDT(stats)
ggplot(stats[,.(CV = mad(prop) / median(prop)),.(Parent,Peptide,Population)]) +
  geom_bar(position = "dodge",stat = "identity") + aes(y = CV,x = Population,fill =
  Peptide) + theme(axis.text.x = element_text(angle = 90,hjust = 1)) + facet_wrap( ~
  Parent,scales = "free_x")

## ----qc2-----------------------------------------------------------------
ggplot(stats) + geom_boxplot() + aes(y = prop,x = Population) + geom_jitter() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + facet_wrap( ~
  Parent,scales = "free")

## ----qc3-----------------------------------------------------------------
nm  = stats[,.(name,rZ = (prop - median(prop)) / mad(prop)),.(Population)][,.(outlier = abs(rZ) >
                                                                                3),.(name,Population)][outlier == TRUE][order(name)][Population %in% "CD8+"]$name
                                                                                
                                                                                ggcyto(tbdata[as.character(nm)],mapping = aes(x = "CD8"),subset = "CD3") +
                                                                                geom_histogram(binwidth = 25) + geom_gate("CD8+")

## ----manual_gating-------------------------------------------------------
for(n in nm){
  mygate = getGate(tbdata[[n]],"CD8+") #extract a gates
  mygate@min["<PerCP Cy55 Blue-A>"] = 1750 #adjust the min value for CD8
  setGate(tbdata[[n]],"CD8+",mygate) #set the gate for the cell population
  recompute(tbdata[[n]]) # recompute the statistics.

  #again for the others
  mygate = getGate(tbdata[[n]],"CD4-CD8+")
  mygate@min["<PerCP Cy55 Blue-A>"]=1750
  mygate@max["<PerCP Cy55 Blue-A>"]=Inf
  setGate(tbdata[[n]],"CD4-CD8+",mygate)


  mygate = getGate(tbdata[[n]],"CD4+CD8-")
  mygate@max["<PerCP Cy55 Blue-A>"]=1750
  setGate(tbdata[[n]],"CD4+CD8-",mygate)

  mygate = getGate(tbdata[[n]],"CD4-CD8-")
  mygate@max["<PerCP Cy55 Blue-A>"]=1750
  setGate(tbdata[[n]],"CD4-CD8-",mygate)

  mygate = getGate(tbdata[[n]],"CD4+CD8+")
  mygate@min["<PerCP Cy55 Blue-A>"]=1750
  setGate(tbdata[[n]],"CD4+CD8+",mygate)

  recompute(tbdata[[n]],"CD4-CD8+")
  recompute(tbdata[[n]],"CD4+CD8-")
  recompute(tbdata[[n]],"CD4-CD8-")
  recompute(tbdata[[n]],"CD4+CD8+")
}
plotGate(tbdata[as.character(nm)],c("CD4+CD8-","CD4-CD8+","CD4-CD8-","CD4+CD8+"),
         xbin=256,margin=FALSE)

## ----empirical_diff------------------------------------------------------
library(scales)
#custom hyperbolic arcsine transformation.
asinh_trans = function (c=800) 
{
    trans_new(name = "asinh", transform = function(x) asinh(x * 
        c), inverse = function(x) sinh(x)/c)
}
stats = merge(pData(tbdata),getPopStats(tbdata),by="name")
data.table::setDT(stats)
stats = stats[Parent=="CD4+CD8-"][,prop:=Count/ParentCount]
stats=reshape2::dcast(stats,PID+Population+Parent~Peptide)


ggplot(stats) + geom_boxplot(outlier.colour = NA) + geom_jitter(aes(fill =
      Population),position = position_jitterdodge()) + aes(x = Population,y =
      pmax(`ESAT-6` - DMSO,0),col = Population) + facet_wrap( ~ Parent,scales =
      "free") + theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
      scale_y_continuous("Background Corrected Proportion",trans =
      "asinh") + 
      ggtitle("Marginal Background Corrected Proportions\nof Cytokine Producing CD4 Cells") +
                                                                      scale_x_discrete("Marginal Cytokine")

## ----compass-------------------------------------------------------------
library(COMPASS)
data = getSingleCellExpression(tbdata,nodes = c("IL2","IFNg","CD154","IL17a","IL4","TNF","IL22")) #single-cell expression of seven markers
counts = as.vector(getPopStats(tbdata,subpopulations = "CD4+CD8-",statistic =
"count")[,.(name,Count)])  #parent population cell counts
counts = as.vector(as.matrix(counts[,2,with = FALSE]))
names(counts) = names(data)
meta = pData(tbdata) #metadata

CC = COMPASSContainer(
data = data,counts = counts,meta = meta,individual_id = "PID",sample_id = "name"
)
set.seed(100)
compass_result = COMPASS(CC,treatment = Peptide == "ESAT-6",control = Peptide ==
"DMSO")

## ----heatmap-------------------------------------------------------------
p = plot(compass_result,threshold=0.01)

## ----fs------------------------------------------------------------------
ggplot(cbind(compass_result$data$meta,PFS = FunctionalityScore(compass_result))) +
  geom_boxplot(outlier.colour = NA) + geom_jitter(aes(col = known_response)) +
  aes(x = known_response,y = PFS) + theme_gray()

