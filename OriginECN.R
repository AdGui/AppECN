library("RCurl")
library("httr")
library("XML")
library("magrittr")
library("dplyr")

#2010
#annee <- GET("http://tice.parisdescartes.fr/resultat/classmed/consul.php", config=c(
#  referer = "http://www.cnci.univ-paris5.fr/cnci_m/ECN2010.html",
#  add_headers(Connection = "keep-alive"),
#  set_cookies(),
#  verbose()
#))

#2011
#annee <- GET("http://tice.parisdescartes.fr/resultat/classnet/consul.php", config=c(
#  referer = "http://www.cnci.univ-paris5.fr/cnci_m/resulECN11.html",
#  add_headers(Connection = "keep-alive"),
#  set_cookies(),
#  verbose()
#))

#2012
#annee <- GET("http://cnci.parisdescartes.fr/resultat/classV12/consul.php", config=c(
#  referer = "http://www.cnci.univ-paris5.fr/cnci_m/ECN2012.html",
#  add_headers(Connection = "keep-alive"),
#  set_cookies(),
#  verbose()
#))

originECN <- function(varNom, varPrenom='', varNaiss='', annee){
  require("RCurl")
  require("httr")
  require("XML")
  require("magrittr")
  # selon les années modifier url en fonction des url ci-dessous
  #2010
  if(annee == 2010){
    urlForm <- "http://tice.parisdescartes.fr/resultat/classmed/consul.php"
    urlTable <- "http://tice.parisdescartes.fr/resultat/classmed/recherche.php"
  }

  #2011
  if(annee == 2011){
    urlForm <- "http://tice.parisdescartes.fr/resultat/classnet/consul.php"
    urlTable <- "http://tice.parisdescartes.fr/resultat/classnet/recherche.php"
  }

  #2012
  if(annee == 2012){
    urlForm <- "http://cnci.parisdescartes.fr/resultat/classV12/consul.php"
    urlTable <- "http://cnci.parisdescartes.fr/resultat/classV12/recherche.php"
  }

  #2013
  if(annee == 2013){
    urlForm <- "http://cnci.parisdescartes.fr/resultat/class13N/consul.php"
    urlTable <- "http://cnci.parisdescartes.fr/resultat/class13N/recherche.php"
  }

  #2014
  if(annee == 2014){
    urlForm <- "http://cnci.parisdescartes.fr/resultat/resuCN14/consul.php"
    urlForm <- "http://cnci.parisdescartes.fr/resultat/resuCN14/recherche.php"
  }
  
  doc <- POST(urlTable, 
              config = c(
                verbose = FALSE,
                referer = urlForm,
                encoding = "gzip,deflate",
                add_headers(Connection = "keep-alive",
                  Expect="")
              ),
              body = list(Nom = varNom,
                           Prenom = varPrenom,
                           Naiss = varNaiss,
                           typ_Etud='etud'),
              user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64; rv:38.0) Gecko/20100101 Firefox/38.0")#,
              #set_cookies('PHPSESSID' = as.character(annee$cookies))
  )
  l1 <- doc$content %>% rawToChar() %>% htmlParse() %>% readHTMLTable()
  if(varPrenom == ''){
    if(length(l1[[1]]) == 4){return(l1[[5]])} else {return(NULL)}
  } else {}
}


####### 2010 #######
liste2010 <- as.list(rep(NA, times=26^2))
for(i in 1:26){
  for(j in 1:26) {
    dum <- toupper(paste0(letters[i],letters[j]))
    dfDum <- originECN(varNom = dum, annee = 2010)
    liste2010[[j+(i-1)*26]] <- dfDum
    print(dum)
  }
}
#Sauvegarde car lourd
liste2010Sauv <- liste2010

# 2 parce que AA == NA 
Origin2010 <- liste2010[[2]]
for(i in 3:length(liste2010)){
  if(is.na(liste2010[i])){} else {Origin2010 <- rbind(Origin2010,liste2010[[i]])}
}

# Fusion BDD affectations et origine
ECN2010 <- read.csv("J:/R/R.Prj/ECN_v3/data/ECN2010.csv")
dumN <- gsub('-',' ',Origin2010$Nom, ignore.case=TRUE)
dumP <- gsub('-',' ',Origin2010$Prenom, ignore.case=TRUE)
Origin2010$IDFusion <- paste0(substr(dumN,1,5), substr(dumP,1,5), substr(Origin2010$Naissance,1,2))
dumN <- gsub('é','e',ECN2010$nom, ignore.case=TRUE)
dumN <- gsub('è','e',dumN, ignore.case=TRUE)
dumN <- gsub('ç','c',dumN, ignore.case=TRUE)
dumN <- gsub('ô','o',dumN, ignore.case=TRUE)
dumN <- gsub('ë','e',dumN, ignore.case=TRUE)
dumN <- gsub('ï','i',dumN, ignore.case=TRUE)
dumN <- gsub('î','i',dumN, ignore.case=TRUE)
dumN <- gsub('-',' ',dumN, ignore.case=TRUE)
dumN <- gsub(',','',dumN, ignore.case=TRUE)

dumP <- gsub('é','e',ECN2010$prenom, ignore.case=TRUE)
dumP <- gsub('è','e',dumP, ignore.case=TRUE)
dumP <- gsub('ç','c',dumP, ignore.case=TRUE)
dumP <- gsub('ô','o',dumP, ignore.case=TRUE)
dumP <- gsub('ë','e',dumP, ignore.case=TRUE)
dumP <- gsub('ï','i',dumP, ignore.case=TRUE)
dumP <- gsub('î','i',dumP, ignore.case=TRUE)
dumP <- gsub('-',' ',dumP, ignore.case=TRUE)
dumP <- gsub(',','',dumP, ignore.case=TRUE)

dumJN <- c()
for(i in 1:length(ECN2010$ddj)){
  if(nchar(ECN2010$ddj[i])==1) {dumJN[i] <- paste0('0', as.character(ECN2010$ddj[i]))} else {dumJN[i] <- ECN2010$ddj[i]}
}
ECN2010$IDFusion <- paste0(substr(toupper(dumN),1,5),substr(toupper(dumP),1,5), substr(dumJN,1,2))
ECN2010$nni <- NA
for(i in 1:length(ECN2010$nni)){
  if(ECN2010$IDFusion[i] %in% (Origin2010$IDFusion)){ECN2010$nni[i] <- as.character(Origin2010$Nni[which(Origin2010$IDFusion == ECN2010$IDFusion[i])])}
  if(length(which(Origin2010$IDFusion == ECN2010$IDFusion[i])) > 1){print(i)} #Gestion des correspondances multiples
}
ECN2010$nni[254] <- "009950"
# 757 et 1683 correctement affecté
# ECN2010[1683,]
# which(Origin2010$IDFusion == ECN2010$IDFusion[1683])
# Origin2010[c(4281,4302),]

ECN2010[which(is.na(ECN2010$nni))[1:10],]
length(which(is.na(ECN2010$nni)))
#Fusion suffisante reste 218 nni non trouvées sur 6133
order(as.numeric(ECN2010$nni))
ECN2010[order(as.numeric(ECN2010$nni)),][650:825,]

ECN2010$nIni <- substr(ECN2010$nom, 1,1)
ECN2010 <- ECN2010[order(as.numeric(ECN2010$nni)),]
OriginECN2010 <- ECN2010[-which(is.na(ECN2010$nni)),]
vec <- c()
for(i in 2:length(OriginECN2010[,1])){
  if(which(letters == tolower(OriginECN2010$nIni[i])) < which(letters == tolower(OriginECN2010$nIni[i-1]))){
    vec[length(vec)+1] <- i
  }
}
vec2 <- c()
for(i in 1:(length(vec)-1)){
  if(vec[i+1] - vec[i] > 10){vec2[length(vec2)+1] <- vec[i] }
}
vec2[length(vec2)+1] <- length(OriginECN2010[,1])
### tombe exactement sur 28 (nombre de subdivision en France, mais probablement approximatif)
# affectation de l'origine par le nni
dfDum <- tally(group_by(ECN2010, sub))
for(i in 1:length(vec2)-1){
  vecL[i] <- vec2[i+1] - vec2[i]
}
#inefficace - à faire de façon manuelle ?

#Coupure Manuelle
listeO <- list(
  data.frame(id=OriginECN2010$IDFusion[1:2], regO='TCEM1'), 
  data.frame(id=OriginECN2010$IDFusion[3:406], regO='Ly'),
  data.frame(id=OriginECN2010$IDFusion[407:715], regO='AixM'),
  data.frame(id=OriginECN2010$IDFusion[716:809], regO='Bre'),
  data.frame(id=OriginECN2010$IDFusion[810:942], regO='Ami'),
  data.frame(id=OriginECN2010$IDFusion[943:1086], regO='Gre'),
  data.frame(id=OriginECN2010$IDFusion[1087:1199], regO='IDF1'),
  data.frame(id=OriginECN2010$IDFusion[1200:1310], regO='Lim'),
  data.frame(id=OriginECN2010$IDFusion[1311:1649], regO='Lil1'),
  data.frame(id=OriginECN2010$IDFusion[1650:1720], regO='Lil2'),
  data.frame(id=OriginECN2010$IDFusion[1721:1831], regO='Ang'),
  data.frame(id=OriginECN2010$IDFusion[1832:1832], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[1833:2213], regO='Bord'),
  data.frame(id=OriginECN2010$IDFusion[2214:2370], regO='IDF2'),
  data.frame(id=OriginECN2010$IDFusion[2371:2508], regO='CleF'),
  data.frame(id=OriginECN2010$IDFusion[2509:2635], regO='StE'),
  data.frame(id=OriginECN2010$IDFusion[2636:2771], regO='Ren'),
  data.frame(id=OriginECN2010$IDFusion[2772:2872], regO='Nic'),
  data.frame(id=OriginECN2010$IDFusion[2873:3015], regO='Poi'),
  data.frame(id=OriginECN2010$IDFusion[3016:3116], regO='IDF3'),
  data.frame(id=OriginECN2010$IDFusion[3117:3232], regO='IDF4'),
  data.frame(id=OriginECN2010$IDFusion[3233:3234], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[3235:3376], regO='Dij'),
  data.frame(id=OriginECN2010$IDFusion[3377:3380], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[3381:3464], regO='Bes'),
  data.frame(id=OriginECN2010$IDFusion[3465:3579], regO='Rei'),
  data.frame(id=OriginECN2010$IDFusion[3580:3582], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[3583:3698], regO='Toul'),
  data.frame(id=OriginECN2010$IDFusion[3699:3801], regO='Toul2'),
  data.frame(id=OriginECN2010$IDFusion[3802:3802], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[3803:3962], regO='IDF5'),
  data.frame(id=OriginECN2010$IDFusion[3963:4322], regO='IDF6'),
  data.frame(id=OriginECN2010$IDFusion[4323:4420], regO='Cae'),
  data.frame(id=OriginECN2010$IDFusion[4421:4578], regO='Nant'),
  data.frame(id=OriginECN2010$IDFusion[4579:4752], regO='Nanc'),
  data.frame(id=OriginECN2010$IDFusion[4753:4907], regO='Rou'),
  data.frame(id=OriginECN2010$IDFusion[4908:4909], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[4910:5099], regO='Stra'),
  data.frame(id=OriginECN2010$IDFusion[5100:5274], regO='Tour'),
  data.frame(id=OriginECN2010$IDFusion[5275:5276], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[5277:5457], regO='Mon'),
  data.frame(id=OriginECN2010$IDFusion[5458:5462], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[5463:5737], regO='IDF7'),
  data.frame(id=OriginECN2010$IDFusion[5738:5840], regO='IDF8'),
  data.frame(id=OriginECN2010$IDFusion[5841:5850], regO='TCEM1'),
  data.frame(id=OriginECN2010$IDFusion[5851:5907], regO='UE')
)
dfO <- c()
for(i in 1:length(listeO)){
  dfO <- rbind(dfO, listeO[[i]])
}
OriginECN2010$regO <- dfO$regO

dfRes <- summarize(group_by(OriginECN2010,regO),length(rang), median(rang), min(rang),quantile(rang,0.25),quantile(rang,0.75), max(rang))
dfRes <-dfRes[order(as.character(dfRes$regO)),]

### Outils de répartition
#OriginECN2010[5840:5907,]
#which(OriginECN2010$nom =='Godet')
#dum <- tally(group_by(OriginECN2010[5841:5907,], sub));data.frame(dum$sub, dum$n)


###### 2011 ######
liste2011 <- as.list(rep(NA, times=26^2))
for(i in 1:26){
  for(j in 1:26) {
    dum <- toupper(paste0(letters[i],letters[j]))
    dfDum <- originECN(varNom = dum, annee = 2011)
    liste2011[[j+(i-1)*26]] <- dfDum
    print(dum)
  }
}
#Sauvegarde car lourd
liste2011Sauv <- liste2011



###### 2012 ######
liste2012 <- as.list(rep(NA, times=26^2))
for(i in 1:26){
  for(j in 1:26){
    dum <- toupper(paste0(letters[i],letters[j]))
    dfDum <- originECN(varNom = dum, annee = 2012)
    liste2012[[j+(i-1)*26]] <- dfDum
    print(dum)
  }
}

#Sauvegarde car lourd
liste2012Sauv <- liste2012

# 2 parce que AA == NA 
Origin2012 <- liste2012[[2]]
for(i in 3:length(liste2012)){
  if(is.na(liste2012[i])){} else {Origin2012 <- rbind(Origin2012,liste2012[[i]])}
}

# Fusion BDD affectations et origine
ECN2012 <- read.csv("J:/R/R.Prj/ECN_v3/data/ECN2012.csv")
dumN <- gsub('-',' ',Origin2012$Nom, ignore.case=TRUE)
dumP <- gsub('-',' ',Origin2012$Prenom, ignore.case=TRUE)
Origin2012$IDFusion <- paste0(substr(dumN,1,5), substr(dumP,1,5), substr(Origin2012$Naissance,1,2))
dumN <- gsub('é','e',ECN2012$nom, ignore.case=TRUE)
dumN <- gsub('è','e',dumN, ignore.case=TRUE)
dumN <- gsub('ç','c',dumN, ignore.case=TRUE)
dumN <- gsub('ô','o',dumN, ignore.case=TRUE)
dumN <- gsub('ë','e',dumN, ignore.case=TRUE)
dumN <- gsub('ï','i',dumN, ignore.case=TRUE)
dumN <- gsub('î','i',dumN, ignore.case=TRUE)
dumN <- gsub('-',' ',dumN, ignore.case=TRUE)
dumN <- gsub(',','',dumN, ignore.case=TRUE)

dumP <- gsub('é','e',ECN2012$prenom, ignore.case=TRUE)
dumP <- gsub('è','e',dumP, ignore.case=TRUE)
dumP <- gsub('ç','c',dumP, ignore.case=TRUE)
dumP <- gsub('ô','o',dumP, ignore.case=TRUE)
dumP <- gsub('ë','e',dumP, ignore.case=TRUE)
dumP <- gsub('ï','i',dumP, ignore.case=TRUE)
dumP <- gsub('î','i',dumP, ignore.case=TRUE)
dumP <- gsub('-',' ',dumP, ignore.case=TRUE)
dumP <- gsub(',','',dumP, ignore.case=TRUE)

dumJN <- c()
for(i in 1:length(ECN2012$ddj)){
  if(nchar(ECN2012$ddj[i])==1) {dumJN[i] <- paste0('0', as.character(ECN2012$ddj[i]))} else {dumJN[i] <- ECN2012$ddj[i]}
}
ECN2012$IDFusion <- paste0(substr(toupper(dumN),1,5),substr(toupper(dumP),1,5), substr(dumJN,1,2))
ECN2012$nni <- NA
for(i in 1:length(ECN2012$nni)){
  if(ECN2012$IDFusion[i] %in% (Origin2012$IDFusion)){ECN2012$nni[i] <- as.character(Origin2012$Nni[which(Origin2012$IDFusion == ECN2012$IDFusion[i])])}
  if(length(which(Origin2012$IDFusion == ECN2012$IDFusion[i])) > 1){print(i)} #Gestion des correspondances multiples
}

# résolution des problèmes de Fusion
which(Origin2012$IDFusion == ECN2012$IDFusion[659])
Origin2012[c(5095,5096),]
Origin2012[c(4763,4764),]
ECN2012$nni[659] <- '004956'
ECN2012$nni[5711] <- '003032'

length(which(is.na(ECN2012$nni)))
head(ECN2012[which(is.na(ECN2012$nni)),],10)
#Fusion suffisante reste 278 nni non trouvées sur 7314
order(as.numeric(ECN2012$nni))
ECN2012[order(as.numeric(ECN2012$nni)),]

ECN2012$nIni <- substr(ECN2012$nom, 1,1)
ECN2012 <- ECN2012[order(as.numeric(ECN2012$nni)),]
OriginECN2012 <- ECN2012[-which(is.na(ECN2012$nni)),]
vec <- c()
for(i in 2:length(OriginECN2012[,1])){
  if(which(letters == tolower(OriginECN2012$nIni[i])) < which(letters == tolower(OriginECN2012$nIni[i-1]))){
    vec[length(vec)+1] <- i
  }
}


