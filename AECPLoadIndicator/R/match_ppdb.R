#' @importFrom readxl read_excel
#' @importFrom stringr str_detect


read.excel <- function(excel_file) {
    table <- as.data.frame(read_excel(excel_file))
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

    fate$SCI.GROW[fate$SCI.GROW=="Cannot be calculated"] <- 0
    fate$SCI.GROW[is.na(fate$SCI.GROW)] <- 0
    fate$SCI.GROW <- as.numeric(fate$SCI.GROW)
    fate$SCI.GROW[fate$SCI.GROW>1000] <- 0

    x<-cbind(as.numeric(fate$Soil.DT50.lab...days),as.numeric(fate$Soil.DT50.typical...days))
    fate$SoilDT50<-rowMeans(x,na.rm=TRUE)
    fate$stable<-ifelse(str_detect(fate$Soil.DT50.notes, "Stable"),1,0)
    fate$Stable<-ifelse(str_detect(fate$Soil.DT50.notes, "stable"),1,0)
    fate$stable<-fate$stable+fate$Stable
    fate$SoilDT50[fate$stable!="0"]<-2*354

    fate$SoilDT50[fate$SoilDT50==""] <- 0
    fate$SoilDT50[is.na(fate$SoilDT50)] <- 0
    fate$SoilDT50[fate$SoilDT50=="#N/A"] <- 0
    fate$SoilDT50[fate$SoilDT50>709] <- 0
    fate$SoilDT50 <- as.numeric(fate$SoilDT50)
    fate$SoilDT50[fate$Soil.DT50.notes=="Both iron and phosphate naturally occur in soil. Degradation will be very slow"] <- 0


    fate$Water.phase.DT50...days[is.na(fate$Water.phase.DT50...days)]<-0
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days==""]<-0
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days=="<1"]<-0.5
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days==">100"]<-100
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days=="Slow, DT50 25-30 days"]<-30
    fate$Water.phase.DT50...days[fate$Water.phase.DT50...days=="Stable"]<-708

    fate

}


compute_R <- function(human) {
    R22 <- ifelse(str_detect(human$EC.Risk.Classification, "22"), 10, 0)
    R37 <- ifelse(str_detect(human$EC.Risk.Classification, "37"), 10, 0)
    R38 <- ifelse(str_detect(human$EC.Risk.Classification, "38"), 10, 0)
    R65 <- ifelse(str_detect(human$EC.Risk.Classification, "65"), 10, 0)
    R66 <- ifelse(str_detect(human$EC.Risk.Classification, "66"), 10, 0)
    R20 <- ifelse(str_detect(human$EC.Risk.Classification, "20"), 15, 0)
    R21 <- ifelse(str_detect(human$EC.Risk.Classification, "21"), 15, 0)
    R36 <- ifelse(str_detect(human$EC.Risk.Classification, "36"), 15, 0)
    R43 <- ifelse(str_detect(human$EC.Risk.Classification, "43"), 20, 0)
    R33 <- ifelse(str_detect(human$EC.Risk.Classification, "33"), 30, 0)
    R67 <- ifelse(str_detect(human$EC.Risk.Classification, "67"), 30, 0)
    R25 <- ifelse(str_detect(human$EC.Risk.Classification, "25"), 50, 0)
    R42 <- ifelse(str_detect(human$EC.Risk.Classification, "42"), 50, 0)
    R64 <- ifelse(str_detect(human$EC.Risk.Classification, "64"), 50, 0)
    R23 <- ifelse(str_detect(human$EC.Risk.Classification, "23"), 70, 0)
    R24 <- ifelse(str_detect(human$EC.Risk.Classification, "24"), 70, 0)
    R28 <- ifelse(str_detect(human$EC.Risk.Classification, "28"), 70, 0)
    R34 <- ifelse(str_detect(human$EC.Risk.Classification, "34"), 70, 0)
    R40 <- ifelse(str_detect(human$EC.Risk.Classification, "40"), 70, 0)

    R62 <- ifelse(str_detect(human$EC.Risk.Classification, "62"), 70, 0)
    R63 <- ifelse(str_detect(human$EC.Risk.Classification, "63"), 70, 0)
    R68 <- ifelse(str_detect(human$EC.Risk.Classification, "68"), 70, 0)
    R26 <- ifelse(str_detect(human$EC.Risk.Classification, "26"), 100, 0)
    R27 <- ifelse(str_detect(human$EC.Risk.Classification, "27"), 100, 0)
    R35 <- ifelse(str_detect(human$EC.Risk.Classification, "35"), 100, 0)
    R39 <- ifelse(str_detect(human$EC.Risk.Classification, "39"), 100, 0)
    R45 <- ifelse(str_detect(human$EC.Risk.Classification, "45"), 100, 0)
    R46 <- ifelse(str_detect(human$EC.Risk.Classification, "46"), 100, 0)
    R48 <- ifelse(str_detect(human$EC.Risk.Classification, "48"), 100, 0)
    R49 <- ifelse(str_detect(human$EC.Risk.Classification, "49"), 100, 0)
    R60 <- ifelse(str_detect(human$EC.Risk.Classification, "60"), 100, 0)
    R61 <- ifelse(str_detect(human$EC.Risk.Classification, "61"), 100, 0)

    R <- (R20 + R21 + R22 + R23 + R24 + R25 + R26 + R27 + R28 + R33 + R34
        + R35 + R36 + R37 + R38 + R39 + R40 + R42 + R43 + R45 + R46
        + R48 + R49 + R60 + R61 + R62 + R63 + R64 + R65 + R66 + R67 + R68)
    R
}

compute_HR <- function(human) {
    #If there are two different risk points per H, the higher one has been taken
    #This happened for H300, H314, H330, H310)
    H302 <- ifelse(str_detect(human$CLP.classification.2013, "302"), 10, 0)
    H335 <- ifelse(str_detect(human$CLP.classification.2013, "335"), 10, 0)
    H315 <- ifelse(str_detect(human$CLP.classification.2013, "315"), 10, 0)
    H304 <- ifelse(str_detect(human$CLP.classification.2013, "304"), 10, 0)
    H066 <- ifelse(str_detect(human$CLP.classification.2013, "066"), 10, 0)
    H332 <- ifelse(str_detect(human$CLP.classification.2013, "332"), 15, 0)
    H312 <- ifelse(str_detect(human$CLP.classification.2013, "312"), 15, 0)
    H319 <- ifelse(str_detect(human$CLP.classification.2013, "319"), 15, 0)
    H317 <- ifelse(str_detect(human$CLP.classification.2013, "317"), 20, 0)
    H336 <- ifelse(str_detect(human$CLP.classification.2013, "336"), 30, 0)
    H301 <- ifelse(str_detect(human$CLP.classification.2013, "301"), 50, 0)
    H334 <- ifelse(str_detect(human$CLP.classification.2013, "334"), 50, 0)
    H362 <- ifelse(str_detect(human$CLP.classification.2013, "362"), 50, 0)
    H331 <- ifelse(str_detect(human$CLP.classification.2013, "331"), 70, 0)
    H311 <- ifelse(str_detect(human$CLP.classification.2013, "311"), 70, 0)
    H314 <- ifelse(str_detect(human$CLP.classification.2013, "314"), 100, 0)
    H351 <- ifelse(str_detect(human$CLP.classification.2013, "351"), 70, 0)
    H318 <- ifelse(str_detect(human$CLP.classification.2013, "318"), 70, 0)
    H373 <- ifelse(str_detect(human$CLP.classification.2013, "373"), 70, 0)
    H361 <- ifelse(str_detect(human$CLP.classification.2013, "361"), 70, 0)
    H371 <- ifelse(str_detect(human$CLP.classification.2013, "371"), 70, 0)
    H341 <- ifelse(str_detect(human$CLP.classification.2013, "341"), 70, 0)
    H330 <- ifelse(str_detect(human$CLP.classification.2013, "330"), 100, 0)
    H300 <- ifelse(str_detect(human$CLP.classification.2013, "300"), 85, 0)
    H310 <- ifelse(str_detect(human$CLP.classification.2013, "310"), 100, 0)
    H370 <- ifelse(str_detect(human$CLP.classification.2013, "370"), 100, 0)
    H350 <- ifelse(str_detect(human$CLP.classification.2013, "350"), 100, 0)
    H340 <- ifelse(str_detect(human$CLP.classification.2013, "340"), 100, 0)
    H372 <- ifelse(str_detect(human$CLP.classification.2013, "372"), 100, 0)
    H360 <- ifelse(str_detect(human$CLP.classification.2013, "360"), 100, 0)

    HR <- (H066 + H300 + H301 + H302 + H304 + H310 + H311 + H312 + H314 + H315
          + H317 + H318 + H319 + H330 + H331 + H332 + H334 + H335 + H336 + H340
          + H341 + H350 + H351 + H360 + H361 + H362 + H370 + H371 + H372 + H373)
    HR
}


extend.products.table <- function(products_table, substances_table, human, general)
{
    for (irow in 1:nrow(products_table)) {
        sum.risk.score <- 0.0
        products_row = products_table[irow,]
        substances_rows = substances_table[substances_table$product == products_row$product,]

        for (jrow in 1:nrow(substances_rows)) {
            substance_row = substances_rows[jrow,]
            CAS = substance_row$CAS.number
            substance = substance_row$substance
            match = general[which(general[,18] == CAS),]

            if (nrow(match) == 0) {
                cat(paste("no entry for CAS", CAS, "\n"))
                next
            }
            else if (tolower(match$Active) != tolower(substance)) {
                cat(paste("entry for CAS", CAS, "refers to", match$Active,
                        "which does not match given substance", substance, "\n"))
                next
            }

            id <- match$ID
            human_row = human[which(human$ID == id),]
            if (nrow(human_row) == 0) {
                cat(paste("no entry for human risk for id", id, "\n"))
                next
            }
            if (products_row$Year <= 2012) {
                sum.risk.score <- sum.risk.score + compute_R(human_row)
            }
            else {
                sum.risk.score <- sum.risk.score + compute_HR(human_row)
            }
        }
        products_table[irow, "sum.risk.score"] <- sum.risk.score
        products_table[irow, "reference.sum.risk.scores"] <- 350
    }
    products_table
}


create.substances.table <- function(input_table, general, fate, ecotox) {

    names(input_table)<-make.names(names(input_table))

    fate <- extend.fate(fate)

    col_names <- required_columns_substances

    result <- data.frame(matrix(NA, nrow = nrow(input_table), ncol = length(col_names)),
                        stringsAsFactors=FALSE)
    names(result) <- col_names

    row.count <- 0

    for (irow in 1:nrow(input_table)) {
        row = input_table[irow,]
        CAS = row$CAS.number
        substance = row$substance
        if (CAS == "" || substance == "")
            next

        match = general[which(general[,18] == CAS),]

        if (nrow(match) == 0) {
            cat(paste("no entry for CAS", CAS, "\n"))
            next
        }
        else if (tolower(match$Active) != tolower(substance)) {
            cat(paste("entry for CAS", CAS, "refers to", match$Active,
                    "which does not match given substance", substance, "\n"))
            next
        }

        id <- match$ID

        fate_row <- fate[which(fate$ID == id),]
        ecotox_row <- ecotox[which(ecotox$ID == id),]

        row = c(
                row$substance,
                row$product,
                as.numeric(row$concentration),

                as.numeric(fate_row$SCI.GROW),
                12.5,
                as.numeric(row$Load.Factor.SCI),

                as.numeric(fate_row$BCF),
                5100,
                as.numeric(row$Load.Factor.BCF),

                as.numeric(fate_row$SoilDT50),
                354,
                as.numeric(row$Load.Factor.SoilDT50),

                as.numeric(ecotox_row$Birds...Acute.LD50.mg.kg),
                10,
                as.numeric(row$Load.Factor.Birds),

                as.numeric(ecotox_row$Mammals...Acute.Oral.LD50.mg.kg.BW.day),
                8,
                as.numeric(row$Load.Factor.Mammals),

                as.numeric(ecotox_row$Fish...Acute.96hr.LC50.mg.l),
                0.00021,
                as.numeric(row$Load.Factor.Fish),

                as.numeric(ecotox_row$Aquatic.Invertebrates...Acute.48hr.EC50.mg.l),
                0.00015,
                as.numeric(row$Load.Factor.Aquatic.Invertebrates),

                as.numeric(ecotox_row$Algae...Acute.72hr.EC50.Growth.mg.l),
                0.00002,
                as.numeric(row$Load.Factor.Algae),

                as.numeric(ecotox_row$Aquatic.Plants...Acute.7d.EC50.mg.l),
                0.00035,
                as.numeric(row$Load.Factor.Aquatic.Plants),

                as.numeric(ecotox_row$Earthworms...Acute.14d.LC50.mg.kg),
                1.0,
                as.numeric(row$Load.Factor.Earthworms),

                (as.numeric(ecotox_row$Honeybees...Contact.acute.48hr.LD50.ug.per.bee) +
                 as.numeric(ecotox_row$Honeybees...Oral.Acute.48hr.LD50.ug.per.bee)) / 2.0,
                0.015,
                as.numeric(row$Load.Factor.Bees),

                as.numeric(ecotox_row$Fish...Chronic.21d.NOEC.mg.l),
                0.00015,
                as.numeric(row$Load.Factor.Fish.Chronic),

                as.numeric(fate_row$Water.phase.DT50...days),

                as.numeric(ecotox_row$Aquatic.Invertebrates...Chronic.21d.NOEC.mg.l),
                0.00015,
                as.numeric(row$Load.Factor.Aquatic.Invertebrates.Chronic),

                as.numeric(ecotox_row$Earthworms...Chronic.NOEC),
                0.8,
                as.numeric(row$Load.Factor.Earthworms.Chronic)
                )

        row.count <- row.count + 1
        result[row.count,] = row

    }
    result <- result[seq(1, row.count), ]
}


#' @title Extend tables by data from PPDB
#'
#' @param substances Dataframe with substance data
#' @param products Dataframe with products data
#' @param folder folder with exported xlsx files from PPDB
#' @return names lists with updated substances and products data frames
#'
#' @export

match.ppdb <- function(substances, products, folder) {

    human <- read.excel(file.path(folder, "Human.xlsx"))
    general <- read.excel(file.path(folder, "General.xlsx"))
    fate <- read.excel(file.path(folder, "Fate.xlsx"))
    ecotox <- read.excel(file.path(folder, "Ecotox.xlsx"))

    products <- extend.products.table(products, substances, human, general)
    substances <- create.substances.table(substances, general, fate, ecotox)

    return(list(products=products, substances=substances))
}
