# 3.02.2020


# Niklas Möhring and Leonie Vidensky
# Agricultural Economics and Policy Group, ETH Zurich







################################ R-Code for the Computation of the Pesticide Load Indicator (see Kudsk et al., 2018; Möhring et al., 2019)




# Read-in data on Substances (Fill out Table_R_substances.xlsx) ####

library(readxl)


substances <- read_excel("./Table_R_substances.xlsx")







####### Fate Load Computation ####

# Degradation (persistence)

substances$U <- substances$`SCI-Grow`/substances$`Reference.SCI-Grow`*substances$Load.Factor.SCI

# Accumulation (bioaccumulation)

substances$B <- substances$BCF/substances$Reference.BCF*substances$Load.Factor.BCF

# SCI-Grow Index

substances$P <- substances$SoilDT50/substances$Reference.SoilDT50*substances$Load.Factor.SoilDT50


# Fate Load substances

substances$Fate.Load.substances <- substances$U + substances$B + substances$P






####### Toxicity Load Computation #####

# Short term effect Birds 

substances$Fa <- ifelse(substances$Birds.Acute.LD50.mg.kg ==0,0,substances$Reference.Value.Birds/substances$Birds.Acute.LD50.mg.kg*substances$Load.Factor.Birds)

# Short term effect Mammals 

substances$Pa <- ifelse(substances$Mammals.Acute.Oral.LD50.mg.kg.BW.day==0, 0, substances$Reference.Value.Mammals/substances$Mammals.Acute.Oral.LD50.mg.kg.BW.day*substances$Load.Factor.Mammals)

# short term effect Fish 

substances$Fla <- ifelse(substances$Fish.Acute.96hr.LC50.mg.l==0,0, substances$Reference.Value.Fish/substances$Fish.Acute.96hr.LC50.mg.l*substances$Load.Factor.Fish)

# short term effect Daphnia 

substances$Da <- ifelse(substances$Aquatic.Invertebrates.Acute.48hr.EC50.mg.l==0,0, substances$Reference.Value.Aquatic.Invertebrates/substances$Aquatic.Invertebrates.Acute.48hr.EC50.mg.l*substances$Load.Factor.Aquatic.Invertebrates)

# short term effect Algae 

substances$Aa <- ifelse(substances$Algae.Acute.72hr.EC50.Growth.mg.l==0,0, substances$Reference.Value.Algae/substances$Algae.Acute.72hr.EC50.Growth.mg.l*substances$Load.Factor.Algae)

# short term effetc Aquatic plants 

substances$Vp <- ifelse(substances$Aquatic.Plants.Acute.7d.EC50.mg.l==0,0, substances$Reference.Value.Aquatic.Plants/substances$Aquatic.Plants.Acute.7d.EC50.mg.l*substances$Load.Factor.Aquatic.Plants)

# short term effect Earthworms

substances$Ra <- ifelse(substances$Earthworms.Acute.14d.LC50.mg.kg==0,0, substances$Reference.Value.Earthworms/substances$Earthworms.Acute.14d.LC50.mg.kg*substances$Load.Factor.Earthworms)

# short term effect Bees

substances$Ba <- ifelse(substances$BeesLD50==0,0, substances$Reference.Value.Bees/substances$BeesLD50*substances$Load.Factor.Bees)

# Long-term effect Fish 

substances$Degradation.Factor.Water <- ifelse(substances$water.phase.DT50.days == 0 | substances$water.phase.DT50.days == 708, 1,((1-exp((-log(2)/substances$water.phase.DT50.days )*7)/(((log(2)/substances$water.phase.DT50.days )*7)))) )
                                              
substances$Flk <- ifelse(substances$Fish.Chronic.21d.NOEC.mg.l.corrected==0,0, substances$Reference.Value.Fish.Chronic/substances$Fish.Chronic.21d.NOEC.mg.l.corrected*substances$Load.Factor.Fish.Chronic*substances$Degradation.Factor.Water)

# Long-term effect Daphnia 

substances$Dk <- ifelse(substances$Aquatic.Invertebrates.Chronic.21d.NOEC.mg.l.correted==0,0, substances$Reference.Value.Aquatic.Invertebrates.Chronic/substances$Aquatic.Invertebrates.Chronic.21d.NOEC.mg.l.correted*substances$Load.Factor.Aquatic.Invertebrates.Chronic*substances$Degradation.Factor.Water)

# Long-term effect Earthworms

substances$Degradation.Factor.Soil <- ifelse(substances$SoilDT50 == 0 | substances$SoilDT50==708, 1, ((1-exp((-log(2)/substances$SoilDT50)*180))/((log(2)/substances$SoilDT50)*180)))

substances$Rk <- ifelse(substances$Earthworms.Chronic.14d.NOEC..Reproduction.mg.kg.corrected == 0,0,substances$Reference.Value.Earthworms.Chronic/substances$Earthworms.Chronic.14d.NOEC..Reproduction.mg.kg.corrected*substances$Load.Factor.Earthworms.Chronic*substances$Degradation.Factor.Soil)


## Environmental Toxicity substances

substances$Environmental.Toxicity.Substance <- substances$Fa + substances$Pa+substances$Fla + substances$Da + substances$Aa + substances$Vp + substances$Ra + substances$Ba + substances$Flk + substances$Dk + substances$Rk










# Read-in data on Products (Fill out Table_R_substances.xlsx) ####

products <- read_excel("./Table_R_products.xlsx")







####### Health Load Computation #####

products$HL <- products$formula*(products$sum.risk.score/products$reference.sum.risk.scores)







###### Output: Load Indicator Values ######

substances$TL.products <- substances$concentration*substances$Environmental.Toxicity.Substance
substances$FL.products <- substances$concentration*substances$Fate.Load.substances

TL <- aggregate(substances$TL.products, by=list(products=substances$product), FUN=sum) 
FL <- aggregate(substances$FL.products, by=list(products=substances$product),FUN=sum)

products$TL <- TL$x[TL$products==products$product]
products$FL <- FL$x[FL$products==products$product]

products$L <- products$HL + products$TL + products$FL





###### Output: Load Index Values #######

# caculate STI-Quotient 

products$STI <- products$amount.applied/products$standard.doses

# calculate Load Index

products$LI <- products$STI * products$L

