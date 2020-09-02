# a) Calculating the human health Load using the PPDB database ####


# The sum.risk.score has to be calculated two times: One time for the EC.Risk.Classification and one time for the CLP.classification.2013

#R und H Saetze
substances$R22<-ifelse(str_detect(substances$EC.Risk.Classification, "22"),10,0)
substances$R37<-ifelse(str_detect(substances$EC.Risk.Classification, "37"),10,0)
substances$R38<-ifelse(str_detect(substances$EC.Risk.Classification, "38"),10,0)
substances$R65<-ifelse(str_detect(substances$EC.Risk.Classification, "65"),10,0)
substances$R66<-ifelse(str_detect(substances$EC.Risk.Classification, "66"),10,0)
substances$R20<-ifelse(str_detect(substances$EC.Risk.Classification, "20"),15,0)
substances$R21<-ifelse(str_detect(substances$EC.Risk.Classification, "21"),15,0)
substances$R36<-ifelse(str_detect(substances$EC.Risk.Classification, "36"),15,0)
substances$R43<-ifelse(str_detect(substances$EC.Risk.Classification, "43"),20,0)
substances$R33<-ifelse(str_detect(substances$EC.Risk.Classification, "33"),30,0)
substances$R67<-ifelse(str_detect(substances$EC.Risk.Classification, "67"),30,0)
substances$R25<-ifelse(str_detect(substances$EC.Risk.Classification, "25"),50,0)
substances$R42<-ifelse(str_detect(substances$EC.Risk.Classification, "42"),50,0)
substances$R64<-ifelse(str_detect(substances$EC.Risk.Classification, "64"),50,0)
substances$R23<-ifelse(str_detect(substances$EC.Risk.Classification, "23"),70,0)
substances$R24<-ifelse(str_detect(substances$EC.Risk.Classification, "24"),70,0)
substances$R28<-ifelse(str_detect(substances$EC.Risk.Classification, "28"),70,0)
substances$R34<-ifelse(str_detect(substances$EC.Risk.Classification, "34"),70,0)
substances$R40<-ifelse(str_detect(substances$EC.Risk.Classification, "40"),70,0)

substances$R62<-ifelse(str_detect(substances$EC.Risk.Classification, "62"),70,0)
substances$R63<-ifelse(str_detect(substances$EC.Risk.Classification, "63"),70,0)
substances$R68<-ifelse(str_detect(substances$EC.Risk.Classification, "68"),70,0)
substances$R26<-ifelse(str_detect(substances$EC.Risk.Classification, "26"),100,0)
substances$R27<-ifelse(str_detect(substances$EC.Risk.Classification, "27"),100,0)
substances$R35<-ifelse(str_detect(substances$EC.Risk.Classification, "35"),100,0)
substances$R39<-ifelse(str_detect(substances$EC.Risk.Classification, "39"),100,0)
substances$R45<-ifelse(str_detect(substances$EC.Risk.Classification, "45"),100,0)
substances$R46<-ifelse(str_detect(substances$EC.Risk.Classification, "46"),100,0)
substances$R48<-ifelse(str_detect(substances$EC.Risk.Classification, "48"),100,0)
substances$R49<-ifelse(str_detect(substances$EC.Risk.Classification, "49"),100,0)
substances$R60<-ifelse(str_detect(substances$EC.Risk.Classification, "60"),100,0)
substances$R61<-ifelse(str_detect(substances$EC.Risk.Classification, "61"),100,0)

substances$R<-substances$R20+substances$R21+substances$R22+substances$R23+substances$R24+substances$R25+substances$R26+substances$R27+substances$R28+substances$R33+substances$R34+substances$R35+substances$R36+substances$R37+substances$R38+substances$R39+substances$R40+substances$R42+substances$R43+substances$R45+substances$R46+substances$R48+substances$R49+substances$R60+substances$R61+substances$R62+substances$R63+substances$R64+substances$R65+substances$R66+substances$R67+substances$R68

#If there are two different risk points per H, the higher one has been taken 
#This happened for H300, H314, H330, H310)
substances$H302<-ifelse(str_detect(substances$CLP.classification.2013, "302"),10,0)
substances$H335<-ifelse(str_detect(substances$CLP.classification.2013, "335"),10,0)
substances$H315<-ifelse(str_detect(substances$CLP.classification.2013, "315"),10,0)
substances$H304<-ifelse(str_detect(substances$CLP.classification.2013, "304"),10,0)
substances$H066<-ifelse(str_detect(substances$CLP.classification.2013, "066"),10,0)
substances$H332<-ifelse(str_detect(substances$CLP.classification.2013, "332"),15,0)
substances$H312<-ifelse(str_detect(substances$CLP.classification.2013, "312"),15,0)
substances$H319<-ifelse(str_detect(substances$CLP.classification.2013, "319"),15,0)
substances$H317<-ifelse(str_detect(substances$CLP.classification.2013, "317"),20,0)
substances$H336<-ifelse(str_detect(substances$CLP.classification.2013, "336"),30,0)
substances$H301<-ifelse(str_detect(substances$CLP.classification.2013, "301"),50,0)
substances$H334<-ifelse(str_detect(substances$CLP.classification.2013, "334"),50,0)
substances$H362<-ifelse(str_detect(substances$CLP.classification.2013, "362"),50,0)
substances$H331<-ifelse(str_detect(substances$CLP.classification.2013, "331"),70,0)
substances$H311<-ifelse(str_detect(substances$CLP.classification.2013, "311"),70,0)
substances$H314<-ifelse(str_detect(substances$CLP.classification.2013, "314"),100,0)
substances$H351<-ifelse(str_detect(substances$CLP.classification.2013, "351"),70,0)
substances$H318<-ifelse(str_detect(substances$CLP.classification.2013, "318"),70,0)
substances$H373<-ifelse(str_detect(substances$CLP.classification.2013, "373"),70,0)
substances$H361<-ifelse(str_detect(substances$CLP.classification.2013, "361"),70,0)
substances$H371<-ifelse(str_detect(substances$CLP.classification.2013, "371"),70,0)
substances$H341<-ifelse(str_detect(substances$CLP.classification.2013, "341"),70,0)
substances$H330<-ifelse(str_detect(substances$CLP.classification.2013, "330"),100,0)
substances$H300<-ifelse(str_detect(substances$CLP.classification.2013, "300"),85,0)
substances$H310<-ifelse(str_detect(substances$CLP.classification.2013, "310"),100,0)
substances$H370<-ifelse(str_detect(substances$CLP.classification.2013, "370"),100,0)
substances$H350<-ifelse(str_detect(substances$CLP.classification.2013, "350"),100,0)
substances$H340<-ifelse(str_detect(substances$CLP.classification.2013, "340"),100,0)
substances$H372<-ifelse(str_detect(substances$CLP.classification.2013, "372"),100,0)
substances$H360<-ifelse(str_detect(substances$CLP.classification.2013, "360"),100,0)

substances$H<-substances$H066+substances$H300+substances$H301+substances$H302+substances$H304+substances$H310+substances$H311+substances$H312+substances$H314+substances$H315+substances$H317+substances$H318+substances$H319+substances$H330+substances$H331+substances$H332+substances$H334+substances$H335+substances$H336+substances$H340+substances$H341+substances$H350+substances$H351+substances$H360+substances$H361+substances$H362+substances$H370+substances$H371+substances$H372+substances$H373

substances$R[substances$R==""]<-0
substances$R[is.na(substances$R)]<-0

substances$HR<-substances$H
substances$HR[is.na(substances$HR)]<-substances$R[is.na(substances$HR)]
# Until 2012 take R Variable as sum.risk.score variable to compute the Health Load
# From 2013 take HR variable as sum.risk.score variable to compute the Health Load

# --> We need to add a Year variable in the table 






# b) Correcting text in database entries ####


substances$SoilDT50[substances$SoilDT50==""]<-0
substances$SoilDT50[is.na(substances$SoilDT50)]<-0
substances$SoilDT50[substances$SoilDT50=="#N/A"]<-0
substances$SoilDT50[substances$SoilDT50>709]<-0
substances$SoilDT50<-as.numeric(substances$SoilDT50)
substances$SoilDT50[substances$Soil.DT50.notes=="Both iron and phosphate naturally occur in soil. Degradation will be very slow"]<-0

substances$LogP[is.na(substances$LogP)]<-0

substances$BCF[is.na(substances$BCF)]<-0
substances$BCF[substances$BCF=="Low risk"]<-0

substances$SCI.GROW[substances$SCI.GROW=="Cannot be calculated"]<-0
substances$SCI.GROW[is.na(substances$SCI.GROW)]<-0                    
substances$SCI.GROW<-as.numeric(substances$SCI.GROW)
substances$SCI.GROW[substances$SCI.GROW>1000]<-0


substances$Birds...Acute.LD50.mg.kg[is.na(substances$Birds...Acute.LD50.mg.kg)]<-0 
substances$Birds...Acute.LD50.mg.kg[substances$Birds...Acute.LD50.mg.kg==""]<-0

substances$Mammals...Acute.Oral.LD50.mg.kg.BW.day[is.na(substances$Mammals...Acute.Oral.LD50.mg.kg.BW.day)]<-0
substances$Mammals...Acute.Oral.LD50.mg.kg.BW.day[substances$Mammals...Acute.Oral.LD50.mg.kg.BW.day==""]<-0  

substances$Fish...Acute.96hr.LC50.mg.l[is.na(substances$Fish...Acute.96hr.LC50.mg.l)]<-0
substances$Fish...Acute.96hr.LC50.mg.l[substances$Fish...Acute.96hr.LC50.mg.l==""]<-0  

substances$Aquatic.Invertebrates...Acute.48hr.EC50.mg.l[is.na(substances$Aquatic.Invertebrates...Acute.48hr.EC50.mg.l)]<-0
substances$Aquatic.Invertebrates...Acute.48hr.EC50.mg.l[substances$Aquatic.Invertebrates...Acute.48hr.EC50.mg.l==""]<-0

substances$Algae...Acute.72hr.EC50.Growth.mg.l[is.na(substances$Algae...Acute.72hr.EC50.Growth.mg.l)]<-0  
substances$Algae...Acute.72hr.EC50.Growth.mg.l[substances$Algae...Acute.72hr.EC50.Growth.mg.l==""]<-0 

substances$Aquatic.Plants...Acute.7d.EC50.mg.l[is.na(substances$Aquatic.Plants...Acute.7d.EC50.mg.l)]<-0  
substances$Aquatic.Plants...Acute.7d.EC50.mg.l[substances$Aquatic.Plants...Acute.7d.EC50.mg.l==""]<-0 

substances$Earthworms...Acute.14d.LC50.mg.kg[is.na(substances$Earthworms...Acute.14d.LC50.mg.kg)]<-0 
substances$Earthworms...Acute.14d.LC50.mg.kg[substances$Earthworms...Acute.14d.LC50.mg.kg==""]<-0
substances$Earthworms...Acute.14d.LC50.mg.kg[substances$Earthworms...Acute.14d.LC50.mg.kg=="Toxic"]<-1

substances$BeesLD50[substances$BeesLD50=="#N/A"]<-0
substances$BeesLD50[is.na(substances$BeesLD50)]<-0  
substances$BeesLD50[substances$BeesLD50==""]<-0 

substances$Water.phase.DT50...days[is.na(substances$Water.phase.DT50...days)]<-0 
substances$Water.phase.DT50...days[substances$Water.phase.DT50...days==""]<-0
substances$Water.phase.DT50...days[substances$Water.phase.DT50...days=="<1"]<-0.5
substances$Water.phase.DT50...days[substances$Water.phase.DT50...days==">100"]<-100
substances$Water.phase.DT50...days[substances$Water.phase.DT50...days=="Slow, DT50 25-30 days"]<-30
substances$Water.phase.DT50...days[substances$Water.phase.DT50...days=="Stable"]<-708

substances$Fish...Chronic.21d.NOEC.mg.l[is.na(substances$Fish...Chronic.21d.NOEC.mg.l)]<-0  
substances$Fish...Chronic.21d.NOEC.mg.l[(substances$Fish...Chronic.21d.NOEC.mg.l=="")]<-0

substances$Aquatic.Invertebrates...Chronic.21d.NOEC.mg.l[is.na(substances$Aquatic.Invertebrates...Chronic.21d.NOEC.mg.l)]<-0  
substances$Aquatic.Invertebrates...Chronic.21d.NOEC.mg.l[(substances$Aquatic.Invertebrates...Chronic.21d.NOEC.mg.l=="")]<-0  

substances$Earthworms...Chronic.14d.NOEC..Reproduction.mg.kg[is.na(substances$Earthworms...Chronic.14d.NOEC..Reproduction.mg.kg)]<-0  
substances$Earthworms...Chronic.14d.NOEC..Reproduction.mg.kg[(substances$Earthworms...Chronic.14d.NOEC..Reproduction.mg.kg=="")]<-0  


