library(AECPLoadIndicator)
library(stringr)


read.table <- function(mdb_file, table_name) {
    lines <- system(paste("mdb-export", mdb_file, table_name), intern=TRUE);
    table <- read.csv(text=paste(lines), header=TRUE, sep=",")
    names(table) <- make.names(names(table))
    table
}


extend.fate <- function(fate)
{
    fate$LogP[is.na(fate$LogP)] <- 0
    fate$LogP<-as.numeric(fate$LogP)
    fate$LogP[is.na(fate$LogP)] <- 0
    fate$BCF[fate$LogP >6 & fate$LogP !=0] <- 10^((-0.2*fate$LogP[fate$LogP >6 & fate$LogP !=0])+(2.74*fate$LogP[fate$LogP >6 & fate$LogP !=0])-4.72)
    fate$BCF[fate$LogP <6 & fate$LogP !=0]<-10^((0.86*fate$LogP[fate$LogP <6 & fate$LogP !=0])-0.7)
    fate$BCF[fate$Bioconcentration.Factor!="" & !(is.na(fate$Bioconcentration.Factor))]<-fate$Bioconcentration.Factor[fate$Bioconcentration.Factor!="" & !(is.na(fate$Bioconcentration.Factor))]
    fate$BCF[is.na(fate$BCF)]<-0
    fate$BCF[fate$BCF=="Low risk"]<-0
    fate$BCF<-as.numeric(fate$BCF)
    fate$BCF[fate$BCF>5100]<-5100


    x<-cbind(as.numeric(fate$Soil.DT50.lab...days),as.numeric(fate$Soil.DT50.typical...days))
    fate$SoilDT50<-rowMeans(x,na.rm=TRUE)
    fate$stable<-ifelse(str_detect(fate$Soil.DT50.notes, "Stable"),1,0)
    fate$Stable<-ifelse(str_detect(fate$Soil.DT50.notes, "stable"),1,0)
    fate$stable<-fate$stable+fate$Stable
    fate$SoilDT50[fate$stable!="0"]<-2*354


    fate$Water.phase.DT50...days[is.na(fate$Water.phase.DT50...days)]<-0
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days==""]<-0
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days=="<1"]<-0.5
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days==">100"]<-100
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days=="Slow, DT50 25-30 days"]<-30
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days=="Stable"]<-708

    fate

}


create.substances.table <- function(input_table) {

    names(input_table)<-make.names(names(input_table))

    fate <- read.table("PPDB_ETH_20-05-12.mdb", "Fate");
    fate <- extend.fate(fate);

    general <- read.table("PPDB_ETH_20-05-12.mdb", "General");
    ecotox <- read.table("PPDB_ETH_20-05-12.mdb", "Ecotox");


    col_names <- AECPLoadIndicator:::required_columns_substances

    result <- data.frame(matrix(NA, nrow = nrow(input_table), ncol = length(col_names)),
                        stringsAsFactors=FALSE)
    names(result) <- col_names

    row.count <- 0;

    for (irow in 1:nrow(input_table)) {
        row = input_table[irow,];
        CAS = row$CAS.number;
        product = row$substance;
        if (CAS == "" || product == "")
            next;

        match = general[general[,18] == CAS,];

        if (nrow(match) == 0) {
            cat(paste("no entry for CAS", CAS, "\n"))
            next;
        }
        else if (tolower(match$Active) != tolower(product)) {
            cat(paste("entry for CAS", CAS, "refers to", match$Active,
                    "which does not match given product", product, "\n"))
            next;
        }

        id <- match$ID;

        fate_row <- fate[fate$ID == id,];
        ecotox_row <- ecotox[ecotox$ID == id,];

        row = c(
                row$substance,
                row$product,
                row$concentration,

                fate_row$SCI.GROW,
                12.5,
                row$Load.Factor.SCI,

                fate_row$BCF,
                5100,
                row$Load.Factor.BCF,

                fate_row$SoilDT50,
                354,
                row$Load.Factor.SoilDT50,

                ecotox_row$Birds...Acute.LD50.mg.kg,
                10,
                row$Load.Factor.Birds,

                ecotox_row$Mammals...Acute.Oral.LD50.mg.kg.BW.day,
                8,
                row$Load.Factor.Mammals,

                ecotox_row$Fish...Acute.96hr.LC50.mg.l,
                0.00021,
                row$Load.Factor.Fish,

                ecotox_row$Aquatic.Invertebrates...Acute.48hr.EC50.mg.l,
                0.00015,
                row$Load.Factor.Aquatic.Invertebrates,

                ecotox_row$Algae...Acute.72hr.EC50.Growth.mg.l,
                0.00002,
                row$Load.Factor.Algae,

                ecotox_row$Aquatic.Plants...Acute.7d.EC50.mg.l,
                0.00035,
                row$Load.Factor.Aquatic.Plants,

                ecotox_row$Earthworms...Acute.14d.LC50.mg.kg,
                1.0,
                row$Load.Factor.Earthworms,

                (ecotox_row$Honeybees...Contact.acute.48hr.LD50.ug.per.bee +
                ecotox_row$Honeybees...Oral.Acute.48hr.LD50.ug.per.bee) / 2.0,
                0.015,
                row$Load.Factor.Bees,

                ecotox_row$Fish...Chronic.21d.NOEC.mg.l,
                3,
                row$Load.Factor.Fish.Chronic,

                fate_row$Water.phase.DT50...days,

                ecotox_row$Aquatic.Invertebrates...Chronic.21d.NOEC.mg.l,
                0.00015,
                row$Load.Factor.Aquatic.Invertebrates.Chronic,

                ecotox_row$Earthworms...Chronic.NOEC,
                0.8,
                row$Load.Factor.Earthworms.Chronic
                )

        row.count <- row.count + 1;
        result[row.count,] = row

    }
    result <- result[seq(1, row.count), ]
}

t <- create.substances.table(read.csv("substances_example.csv"))
print(t);

