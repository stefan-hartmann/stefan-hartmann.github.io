###This script should be saved in the [LING-COL] folder of the GerManC Corpus, which
###is available here:

###http://ota.ahds.ac.uk/desc/2544

###This folder should be set as the working directory. (Alternatively, you can of course
###also copy&paste the LING-COL files into your working directory.)
###The documents "texts_germanc.csv", "GerManC_ung_MASTER.csv", and 
###"GerManC_NIs_MASTER.csv", which you can find in the same zip folder as this script,
###should be saved in the same folder.

###I have worked with a slightly modified version of LING-COL in which the special
###characters <ä>, <ö>, <ü>, and <ß> have been replaced by <ae>, <oe>, <ue>, and <ß>,
###respectively.

###The methods of analysis used in this script draw on simple collexeme analysis and
###distinctive collexeme analysis as proposed by Stefanowitsch & Gries (2003) and
###Gries & Stefanowitsch (2004):

###Stefanowitsch, A., & Gries, S. T. (2003). Collostructions. Investigating the 
###Interaction of Words and Constructions. International Journal of Corpus Linguistics 8, 
###209-243.

###Gries, S. T., & Stefanowitsch, A. (2004). Extending Collostructional Analysis. 
###A Corpus-Based Perspective on 'Alternations'. International Journal of Corpus 
###Linguistics 9, 97-129.



###############################################################
##########################FIRST STEPS##########################
###############################################################

###clear workspace
rm(list=ls(all=T))

####Read and edit files

###texts:
texts_germanc <- read.delim(file="texts_germanc.csv", header=T, sep=";")

###ung-corpus
ung.gc.bv <- read.csv("GerManC_ung_MASTER.csv", head=T, sep=";")
texts_germanc$Dateiname <- factor(texts_germanc$Dateiname)
texts_germanc$Jahrzehnt <- factor(texts_germanc$Jahrzehnt)

###NI Corpus 
ni.gc.bv <- read.delim("GerManC_NIs_MASTER.csv", head=T, sep=";")
ni.gc.bv[,28] <- ni.gc.bv$Lemma ###adding a column for the base verbs
colnames(ni.gc.bv)[28] <- "Basisverb"
ni.gc.bv$Basisverb <- tolower(ni.gc.bv$Basisverb) ###Since we are dealing with conversions, the base verbs are identical with the nominals apart from the fact that the nominals are capitalized.

###shortcut for the GerManC decades
decades_gc <- levels(texts_germanc$Jahrzehnt)

###shortcut for the GerManC years
texts_germanc$Jahr <- factor(texts_germanc$Jahr)
years_gc <- levels(texts_germanc$Jahr)


###all tokens in the corpus

##I admit that the following code is a bit cumbersome (I wrote it more than a year ago;
##today I would probably go for a more elegant solution). It creates a csv sheet for
##every corpus document, yielding 336 documents with a unified format (in contrast to
##the original corpus documents: here, the format of the spreadsheets varies; e.g., the
##lemmas are sometimes in the second and sometimes in the seventh column. This is why 
##I use the first if-loop, taking the position of the morphological annotation as an
##index of how the respective document is formatted). Then these individual documents
##are merged via rbind into a single document.

for(j in 1:length(texts_germanc$Dateiname)) {
  
  ###dataframe
  gc.df <- as.data.frame(matrix(ncol=6, nrow=0))
  colnames(gc.df) <- c("Token", "Lemma", "POS", "Text", "Year", "Period")
  
  text01 <- read.delim(as.character(texts_germanc$Dateiname[j]), sep="\t", head=F, quote="")
  if("NK$" %in% text01[,2] | "SB$" %in% text01[,2] | "AG$" %in% text01[,2]) {
    for(i in 1:length(text01[,7])) {
      helper <- length(na.omit(gc.df$Token));
      gc.df[(helper+1),] <- NA;
      
      token <- c()
      token <- as.character(text01[i,7])
      if(is.na(token)) {
        token <- "UNKLAR"
      } else {}
      
      gc.df$Token[(helper+1)] <- token
      
      lemma <- c()
      lemma <- as.character(text01[i,5])
      if(is.na(lemma)) {
        lemma <- "UNKLAR"
      } else {}
      
      gc.df$Lemma[(helper+1)] <- lemma
      
      pos <- c()
      pos <- as.character(text01[i,4])
      if(is.na(pos)) {
        pos <- "UNKLAR"
      } else {}
      
      gc.df$POS[(helper+1)] <- pos
      
      gc.df$Text[(helper+1)] <- as.character(texts_germanc$Dateiname[j])
      gc.df$Year[(helper+1)] <- as.character(texts_germanc$Jahr[j])
      gc.df$Period[(helper+1)] <- as.character(texts_germanc$Periode[j])
    }
  } else {
    for(i in 1:length(text01[,5])) {
      helper <- length(na.omit(gc.df$Token));
      gc.df[(helper+1),] <- NA;
      
      token <- c()
      token <- as.character(text01[i,2])
      if(is.na(token)) {
        token <- "UNKLAR"
      } else {}
      
      gc.df$Token[(helper+1)] <- token
      
      lemma <- c()
      lemma <- as.character(text01[i,5])
      if(is.na(lemma)) {
        lemma <- "UNKLAR"
      } else {}
      
      gc.df$Lemma[(helper+1)] <- lemma
      
      pos <- c()
      pos <- as.character(text01[i,4])
      if(is.na(pos)) {
        pos <- "UNKLAR"
      } else {}
      
      gc.df$POS[(helper+1)] <- pos
      
      gc.df$Text[(helper+1)] <- as.character(texts_germanc$Dateiname[j])
      gc.df$Year[(helper+1)] <- as.character(texts_germanc$Jahr[j])
      gc.df$Period[(helper+1)] <- as.character(texts_germanc$Periode[j])
    }
  }
  
  write.csv(gc.df, file=paste(as.character(texts_germanc$Dateiname[j]), "_new.csv", sep="", collapse=""),
            row.names=F)
  
}


files_gc <- list.files(pattern="new.csv")
gc_corpus <- do.call(rbind, lapply(files_gc, read.csv, sep=",", head=T, colClasses=rep("character",6)))
write.table(gc_corpus, "gc_corpus.txt", sep="\t", row.names=F, quote=F)
alltokens <- read.delim("gc_corpus.txt", sep="\t", head=T, quote="")


alltokens$Lemma <- gsub("ß", "ss", alltokens$Lemma)
alltokens$Lemma <- gsub("ä|Ä", "ae", alltokens$Lemma)
alltokens$Lemma <- gsub("ö|Ö", "oe", alltokens$Lemma)
alltokens$Lemma <- gsub("ü|Ö", "ue", alltokens$Lemma)

###all verbs in the corpus
all_verbs_gc <- alltokens[(grep("^VA.", alltokens$POS, invert=T)),][grep("^V.", alltokens[(grep("^VA.", alltokens$POS, invert=T)),]$POS),]

###all nouns in the corpus
all_nouns_gc <- alltokens[(grep("^NE", alltokens$POS, invert=T)),][grep("^N.", alltokens[(grep("^NE", alltokens$POS, invert=T)),]$POS),]

###All base verbs in one vector (for ung and NIs, respectively)
lemmas_base_verbs <- levels(ung.gc.bv$Basisverb)
base_verbs_ni <- levels(factor(ni.gc.bv$Basisverb))


##############################################################
#########################PRODUCTIVITY#########################
##############################################################

#####0) Potential Productivity of ung-nominalization

##find hapaxes
hapaxes <- droplevels(as.data.frame(table(ung.gc.bv$Lemma))[which(as.data.frame(table(ung.gc.bv$Lemma))$Freq==1),]$Var1)

##Pot. Prod.
pot_prod <- c()
for(i in 1:length(years_gc)) {
  pot_prod[i] <- length(which(ung.gc.bv$Jahr==years_gc[i] & ung.gc.bv$Lemma %in% hapaxes)) /
    length(which(ung.gc.bv$Jahr==years_gc[i]))
}


pot_prod2 <- c()
periods <- levels(ung.gc.bv$Periode)

for(i in 1:length(periods)) {
  pot_prod2[i] <- length(which(ung.gc.bv[which(ung.gc.bv$Periode==periods[i]),]$Lemma %in% hapaxes)) /
    length(ung.gc.bv[which(ung.gc.bv$Periode==periods[i]),]$Lemma)

}

##Potential Productivity - finegrained
##(problematic, as the corpus texts vary in size)
plot(years_gc, pot_prod, pch=20, cex=0.7, ylab="Potential Productivity", xlab="Year",
     col="steelblue2")
abline(lm(pot_prod~as.numeric(years_gc)), lty=2, col="darkred")
grid(col="lightgreen")
cor.test(as.numeric(years_gc), pot_prod, method="kendall")

##Potential Productivity - three-stage periodization envisaged by corpus designers
barplot(pot_prod2, space=0, names.arg=c("1650-1700", "1701-1750", "1751-1800"),
        ylab="Potential Productivity", xlab="Period", col=c("grey20", "grey40", "grey60"),
        ylim=c(0,0.098), main="ung-Nominalization, GerManC")
box()




#########################################################################
################FREQUENCIES OF DERIVATIVES AND BASE WORDS################
#########################################################################

########1) plotting the frequencies for the three periods

####1a) ung-nominals
frequencies_ung_p1 <- as.data.frame(matrix(nrow=length(lemmas_base_verbs), ncol=3))
colnames(frequencies_ung_p1) <- c("Lemma", "Freq_Lemma", "Freq_BV")

for(i in 1:length(lemmas_base_verbs)) {
  frequencies_ung_p1$Lemma[i] <- as.character(ung.gc.bv[which(ung.gc.bv$Basisverb==lemmas_base_verbs[i]),]$Lemma[1])
  frequencies_ung_p1$Freq_Lemma[i] <- length(which(ung.gc.bv[ung.gc.bv$Periode=="P1",]$Basisverb==lemmas_base_verbs[i]));
  frequencies_ung_p1$Freq_BV[i] <- length(which(all_verbs_gc[all_verbs_gc$Period=="P1",]$Lemma==lemmas_base_verbs[i]))
}


frequencies_ung_p2 <- as.data.frame(matrix(nrow=length(lemmas_base_verbs), ncol=3))
colnames(frequencies_ung_p2) <- c("Lemma", "Freq_Lemma", "Freq_BV")

for(i in 1:length(lemmas_base_verbs)) {
  frequencies_ung_p2$Lemma[i] <- as.character(ung.gc.bv[which(ung.gc.bv$Basisverb==lemmas_base_verbs[i]),]$Lemma[1])
  frequencies_ung_p2$Freq_Lemma[i] <- length(which(ung.gc.bv[ung.gc.bv$Periode=="P2",]$Basisverb==lemmas_base_verbs[i]));
  frequencies_ung_p2$Freq_BV[i] <- length(which(all_verbs_gc[all_verbs_gc$Period=="P2",]$Lemma==lemmas_base_verbs[i]))
}

frequencies_ung_p3 <- as.data.frame(matrix(nrow=length(lemmas_base_verbs), ncol=3))
colnames(frequencies_ung_p3) <- c("Lemma", "Freq_Lemma", "Freq_BV")

for(i in 1:length(lemmas_base_verbs)) {
  frequencies_ung_p3$Lemma[i] <- as.character(ung.gc.bv[which(ung.gc.bv$Basisverb==lemmas_base_verbs[i]),]$Lemma[1]);
  frequencies_ung_p3$Freq_Lemma[i] <- length(which(ung.gc.bv[ung.gc.bv$Periode=="P3",]$Basisverb==lemmas_base_verbs[i]));
  frequencies_ung_p3$Freq_BV[i] <- length(which(all_verbs_gc[all_verbs_gc$Period=="P3",]$Lemma==lemmas_base_verbs[i]))
}


plotfrequencies_ung <- function(x) {
  plot(frequencies_ung_p1$Freq_Lemma, frequencies_ung_p1$Freq_BV, type="n",
       xlab="ung-Nominal", ylab="Base Verb", xlim=c(-10, max(frequencies_ung_p1$Freq_Lemma)))
  for(i in 1:length(lemmas_base_verbs)) {
    if(x$Freq_Lemma[i]==0) {} else {
      text(x$Freq_Lemma[i], x$Freq_BV[i],
           labels=x$Lemma[i])
    }
  }
}




####1b) NIs
frequencies_ni_p1 <- as.data.frame(matrix(nrow=length(base_verbs_ni), ncol=3))
colnames(frequencies_ni_p1) <- c("Lemma", "Freq_Lemma", "Freq_BV")

for(i in 1:length(base_verbs_ni)) {
  frequencies_ni_p1$Lemma[i] <- as.character(ni.gc.bv[which(ni.gc.bv$Basisverb==base_verbs_ni[i]),]$Lemma[1])
  frequencies_ni_p1$Freq_Lemma[i] <- length(which(ni.gc.bv[ni.gc.bv$Periode=="P1",]$Basisverb==base_verbs_ni[i]));
  frequencies_ni_p1$Freq_BV[i] <- length(which(all_verbs_gc[all_verbs_gc$Period=="P1",]$Lemma==base_verbs_ni[i]))
}


frequencies_ni_p2 <- as.data.frame(matrix(nrow=length(base_verbs_ni), ncol=3))
colnames(frequencies_ni_p2) <- c("Lemma", "Freq_Lemma", "Freq_BV")

for(i in 1:length(base_verbs_ni)) {
  frequencies_ni_p2$Lemma[i] <- as.character(ni.gc.bv[which(ni.gc.bv$Basisverb==base_verbs_ni[i]),]$Lemma[1])
  frequencies_ni_p2$Freq_Lemma[i] <- length(which(ni.gc.bv[ni.gc.bv$Periode=="P2",]$Basisverb==base_verbs_ni[i]));
  frequencies_ni_p2$Freq_BV[i] <- length(which(all_verbs_gc[all_verbs_gc$Period=="P2",]$Lemma==base_verbs_ni[i]))
}

frequencies_ni_p3 <- as.data.frame(matrix(nrow=length(base_verbs_ni), ncol=3))
colnames(frequencies_ni_p3) <- c("Lemma", "Freq_Lemma", "Freq_BV")

for(i in 1:length(base_verbs_ni)) {
  frequencies_ni_p3$Lemma[i] <- as.character(ni.gc.bv[which(ni.gc.bv$Basisverb==base_verbs_ni[i]),]$Lemma[1]);
  frequencies_ni_p3$Freq_Lemma[i] <- length(which(ni.gc.bv[ni.gc.bv$Periode=="P3",]$Basisverb==base_verbs_ni[i]));
  frequencies_ni_p3$Freq_BV[i] <- length(which(all_verbs_gc[all_verbs_gc$Period=="P3",]$Lemma==base_verbs_ni[i]))
}

plotfrequencies_NI <- function(x) {
  plot(frequencies_ni_p1$Freq_Lemma, frequencies_ni_p3$Freq_BV, type="n",
       xlab="NI", ylab="Base Verb", xlim=c(-10, max(frequencies_ni_p1$Freq_Lemma)+10))
  for(i in 1:length(base_verbs_ni)) {
    if(x$Freq_Lemma[i]==0) {} else {
      text(x$Freq_Lemma[i], x$Freq_BV[i],
           labels=x$Lemma[i])
    }
  }
}



##Derivative/base verb frequency distribution for both patterns in all three periods
par(mfrow=c(2,3))
plotfrequencies_ung(frequencies_ung_p1); title(main="Period 1 (1650-1700)"); grid()
plotfrequencies_ung(frequencies_ung_p2); title(main="Period 2 (1701-1750)"); grid()
plotfrequencies_ung(frequencies_ung_p3); title(main="Period 3 (1571-1800)"); grid()
plotfrequencies_NI(frequencies_ni_p1); title(main="Period 1 (1650-1700)"); grid()
plotfrequencies_NI(frequencies_ni_p2); title(main="Period 2 (1701-1750)"); grid()
plotfrequencies_NI(frequencies_ni_p3); title(main="Period 2 (1751-1800)"); grid()
par(mfrow=c(1,1))




#########2) Derivative/Base Verb Cross-Tabulation Analysis

###2a) ung-Nominals

crosstab <- as.data.frame(matrix(nrow=length(lemmas_base_verbs), ncol=7))
colnames(crosstab) <- c("Lemma", "b_in_w", "non_b_in_w", "b_in_B", "non_b_in_B", "p_value", "expected")

for(i in 1:length(lemmas_base_verbs)) {
  crosstab$Lemma[i] <- as.character(ung.gc.bv[which(ung.gc.bv$Basisverb==lemmas_base_verbs[i]),]$Lemma[1]);
    c01 <- length(ung.gc.bv[which(ung.gc.bv$Basisverb==lemmas_base_verbs[i]),]$Lemma);
    c02 <- length(ung.gc.bv$Lemma)-c01;
    c03 <- length(which(all_verbs_gc$Lemma==lemmas_base_verbs[i]));
    c04 <- length(all_verbs_gc$Lemma)-c03;
  crosstab$b_in_w[i] <- c01; crosstab$non_b_in_w[i] <- c02; crosstab$b_in_B[i] <- c03; crosstab$non_b_in_B[i] <- c04;
  crosstab$p_value[i] <- fisher.test(data.frame(c(c01, c02), c(c03, c04)))$p.value;
  crosstab$expected[i] <-sum(data.frame(c(c01,c02), c(c03,c04))[1,])*sum(data.frame(c(c01,c02), c(c03,c04))[,1])/sum(data.frame(c(c01,c02), c(c03,c04)))
}


###2b) NIs

crosstab_ni <- as.data.frame(matrix(nrow=length(base_verbs_ni), ncol=7))
colnames(crosstab_ni) <- c("Lemma", "b_in_w", "non_b_in_w", "b_in_B", "non_b_in_B", "p_value", "expected")

for(i in 1:length(base_verbs_ni)) {
  crosstab_ni$Lemma[i] <- as.character(ni.gc.bv[which(ni.gc.bv$Basisverb==base_verbs_ni[i]),]$Lemma[1]);
  c01 <- length(ni.gc.bv[which(ni.gc.bv$Basisverb==base_verbs_ni[i]),]$Lemma);
  c02 <- length(ni.gc.bv$Lemma)-c01;
  c03 <- length(which(all_verbs_gc$Lemma==base_verbs_ni[i]));
  c04 <- length(all_verbs_gc$Lemma)-c03;
  crosstab_ni$b_in_w[i] <- c01; crosstab_ni$non_b_in_w[i] <- c02; crosstab_ni$b_in_B[i] <- c03; crosstab_ni$non_b_in_B[i] <- c04;
  crosstab_ni$p_value[i] <- fisher.test(data.frame(c(c01, c02), c(c03, c04)))$p.value;
  crosstab_ni$expected[i] <- sum(data.frame(c(c01,c02), c(c03,c04))[1,])*sum(data.frame(c(c01,c02), c(c03,c04))[,1])/sum(data.frame(c(c01,c02), c(c03,c04)))
}

##############################################
#############[P NOM] Construction#############
##############################################


####3. Collostructional strength and [PREP NOM constr]

###3a) Evaluating collostructional strength for the [PREP V-ung] constr

###find instances for PREP + bare noun
###As a proxy, I use those cases in which no ART tag occurs between APPR and N*
prep_n <- c()

find_prep <- which(alltokens$POS=="APPR")

for(i in 1:(length(find_prep)-1)) {
  current_segment <- alltokens[find_prep[i]:find_prep[i+1],]
  end_current <- grep("^N", current_segment$POS)[1]
  
  if (length(end_current)==0) {
    prep_n[i] <- 0
  } else if (is.na(end_current)) {
    prep_n[i] <- 0
  } else {
    prep_n[i] <- length(which(current_segment[1:end_current,]$POS=="ART"))
  }
  
}

length_prep_n <- length(which(prep_n==0))

preps_coll_total <- as.data.frame(matrix(nrow=length(levels(factor(ung.gc.bv$Lemma))), ncol=5))
colnames(preps_coll_total) <- c("Lemma", "Frequency", "Expected_Frequency", "Collostructional_Strength", "Association_Strength")

for(i in 1:length(levels(factor(ung.gc.bv$Lemma)))) {
  preps_coll_total$Lemma[i] <- as.character(levels(factor(ung.gc.bv$Lemma))[i]);
  c01 <- length(which(ung.gc.bv[which(ung.gc.bv$als_praep_Kompl=="JA"),]$Lemma==as.character(levels(factor(ung.gc.bv$Lemma))[i])))
  c02 <- length(which(ung.gc.bv[which(ung.gc.bv$als_praep_Kompl=="NEIN"),]$Lemma==as.character(levels(factor(ung.gc.bv$Lemma))[i])))
  c03 <- length_prep_n - c01
  c04 <- length(all_nouns_gc$Lemma)-length_prep_n-c02
  preps_coll_total$Frequency[i] <- c01
  preps_coll_total$Expected_Frequency[i] <- sum(data.frame(c(c01,c02), c(c03,c04))[1,])*sum(data.frame(c(c01,c02), c(c03,c04))[,1])/sum(data.frame(c(c01,c02), c(c03,c04)))
  preps_coll_total$Collostructional_Strength[i] <- fisher.test(data.frame(c(c01,c02),c(c03,c04)))$p.value
}




###to arrive at a continuous scale ranging from "repelled" to "attracted", items, the
###collostruction strength value is log(10)-transformed and the  sign is set to
###reflect the direction of association:
for(i in 1:length(preps_coll_total$Collostructional_Strength)) {
  if(preps_coll_total$Frequency[i]>=preps_coll_total$Expected_Frequency[i]) {
    preps_coll_total$Collostructional_Strength[i] <- abs(log(preps_coll_total$Collostructional_Strength[i], 10))
  } else {
    preps_coll_total$Collostructional_Strength[i] <- -(abs(log(preps_coll_total$Collostructional_Strength[i], 10)))
  }
}


#####3b) Modification in PREP NOM constructions
preps <- ung.gc.bv[which(ung.gc.bv$als_praep_Kompl=="JA"),]
preps[,40] <- NA; colnames(preps)[40] <- "Coll_Strength"

for(i in 1:length(preps$Lemma)) {
  if(preps$Lemma[i] %in% preps_coll_total$Lemma) {
    preps$Coll_Strength[i] <- preps_coll_total[which(preps_coll_total$Lemma==preps$Lemma[i]),]$Collostructional_Strength

  } else {}
}

preps$Material_zwischen_Prep_und_Nom <- factor(preps$Material_zwischen_Prep_und_Nom)

preps[,41] <- NA; colnames(preps)[41] <- "MOD"
preps[which(preps$Material_zwischen_Prep_und_Nom %in% c("AUFZ", "MOD", "BOTH")),41] <- "JA"
preps[which(preps$Material_zwischen_Prep_und_Nom=="KEIN"),41] <- "NEIN"
preps$MOD <- factor(preps$MOD)



####proportion of modified tokens for each type
preps2 <- as.data.frame(matrix(ncol=3, nrow=length(levels(factor(preps$Lemma)))))
colnames(preps2) <- c("Lemma", "Coll_Str", "Modification")

for(i in 1:length(levels(factor(preps$Lemma)))) {
  preps2$Lemma[i] <- as.character(preps[which(preps$Lemma==levels(factor(preps$Lemma))[i])[1],]$Lemma)
  preps2$Coll_Str[i] <- preps[which(preps$Lemma==levels(factor(preps$Lemma))[i])[1],]$Coll_Strength
  preps2$Modification[i] <- length(which(preps[which(preps$Lemma==levels(factor(preps$Lemma))[i]),]$MOD=="JA")) /
    length(preps[which(preps$Lemma==levels(factor(preps$Lemma))[i]),]$MOD)
}


plot(preps2$Coll_Str, preps2$Modification*100, pch=20, xlab="Collostruction Strength", 
     ylab="% of modified items", main="Coll. Strength and Modification")
abline(lm(preps2$Modification*100~preps2$Coll_Str))
grid()

cor.test(preps2$Coll_Str, preps2$Modification, method="kendall")
