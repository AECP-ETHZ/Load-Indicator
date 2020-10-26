# a) Calculating the human health Load using the PPDB database ####


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
    R[R==""] <- 0
    R[is.na(R)] <- 0
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
    HR[HR==""] <- 0
    HR[is.na(HR)] <- 0
    HR
}
