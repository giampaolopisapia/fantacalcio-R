loadQuotazioni <- function(file = "quotazioniFantagazzetta1Giornata.csv") {
  quotazioni <- read.csv(paste("Dataset", file, sep="/") , sep = ";", stringsAsFactors=FALSE, 
                         header = TRUE, 
                           quote = "")
  #dftouppercase(quotazioni)
  quotazioni$Quota <- as.integer(round(as.numeric((quotazioni$Quota))))
  quotazioni$Calciatore <- toupper(quotazioni$Calciatore)
  quotazioni
}

initAsta <- function() {
  asta <- data.frame(Fantasquadra=factor(),
                     Ruolo=factor(levels = c("P", "D", "C", "A"), ordered=TRUE),
                     Calciatore=character(),
                     Squadra=character(),
                     Prezzo=integer(),
                     Quota=integer(),
                     Prog=integer(), stringsAsFactors = F)
  
  asta
}

loadAsta <- function(file = "AstaIniziale2015_16.csv") {
  asta <- read.table(paste("Asta", file, sep="/") , sep = ";", 
                     header = TRUE, quote = "", stringsAsFactors = FALSE, colClasses = c(Ruolo="factor"))
  asta$Ruolo <- factor(asta$Ruolo, levels = c('P', 'D', 'C', 'A'), ordered=TRUE)
  asta
}


saveAsta <- function(df, file = "AstaIniziale2015_16.csv") {
  ## write header
  write.table(df, file = paste("Asta", file, sep="/"), row.names=FALSE, na="",
              col.names=TRUE, quote=FALSE, sep=";")
}

bestQuote <- function(quote, ruolo, num=10, asta){
  if(!missing(asta)){
    asta$EXTRACOL <- 1 # use a column name that is not present among 
    merged <- merge(quote, asta[, c("Calciatore", "EXTRACOL")], by = "Calciatore", all=TRUE)
    
    dfdifference <- merged[is.na(merged$EXTRACOL),]
    dfdifference$EXTRACOL <- NULL
  }
  else{
    dfdifference <- quote
  }
  sub <- dfdifference[dfdifference$Ruolo %in% ruolo, ]
  if(nrow(sub) < num | num == 0){
    num = nrow(sub)
  }
  sub <- sub[with(sub, order(-Quota, Calciatore)), ]
  sub[1:num,]
}

insertRow <- function(df, newrow){
  df <- rbind(df, newrow)
  df
}

addAcquisto <- function(asta, squadra, nome, prezzo, quotazioni){
  upperNome <- toupper(nome)
  if(missing(quotazioni))
    q <- loadQuotazioni()
  else
    q <- quotazioni
  quotaNome <- q[grepl(upperNome, toupper(q$Calciatore)), ]
  if(nrow(quotaNome) != 1){
    quotaNome <- q[grepl(paste("^", upperNome, "$", sep = ""), toupper(q$Calciatore)), ]
    if(nrow(quotaNome) != 1){
      stop(paste("Nome calciatore ambiguo: ", quotaNome$Calciatore, "\n"))
    }
  }
  calciatore <- as.character(quotaNome$Calciatore)
  # check name not exist yet
  if(calciatore %in% asta$Calciatore)
  {
    stop(paste(" Calciatore venduto a ", as.character(asta[asta$Calciatore == calciatore, "Fantasquadra"])))
  }
  # calculate prog number by role
  ruolo <- ordered(quotaNome$Ruolo, levels=c("P","D","C","A"))
  if(is.na(ruolo)){
    stop(paste("Ruolo calciatore non trovato: ", quotaNome, "\n"))
  }
  prog <- as.numeric(nrow(asta[asta$Ruolo==ruolo & as.character(asta$Fantasquadra)==squadra,]))
  newrow <- data.frame(Fantasquadra=squadra, Ruolo=ruolo, 
                       Calciatore=calciatore, 
                       Squadra=as.character(quotaNome$Squadra), 
                       Prezzo=as.numeric(prezzo), 
                       Quota=quotaNome$Quota,
                       Prog=prog + 1)
  asta <- insertRow(asta, newrow)
  # check residui
  crediti <- residui(asta)[squadra]
  print(crediti)
  if(crediti < 0){
    stop(paste("Crediti insufficienti", crediti))
  }
  bkpFilename <- paste("bkp", format(Sys.time(), "%Y%m%d_%H%m%S"), sep = "_")
  saveAsta(asta, paste(bkpFilename, "csv", sep = "."))
  asta
}

residui <- function(asta){
  res <- tapply(asta$Prezzo, asta$Fantasquadra, function(x) 2000 - sum(x))
  res
}

viewTabellone <- function(asta){
  asta$newcol <- apply(asta,1,function(row) paste( row[3] , row[5], sep = " - "  ))
  asta.m <- melt(asta, id=c("Ruolo", "Prog", "Fantasquadra"), measure.vars = c("newcol") )
  asta.m <- as.data.frame(unclass(asta.m))
  tabellone <- cast(asta.m, Ruolo + Prog ~ Fantasquadra)
  tabella <- tabellone[,c(1,3:ncol(tabellone))]
  # aggregate prezzo by ruolo
  ag <- aggregate(tabella[2:ncol(tabella)], by=tabella["Ruolo"], sumprezzo, na.rm=TRUE)
  # add total
  ag[(nrow(ag) + 1), 2:ncol(tabella)] <- colSums(ag[, 2:ncol(tabella)], na.rm=TRUE)
  # cast to char for rbind
  ag[, 2:ncol(ag)] <- convert.magic(ag[, 2:ncol(ag)], "character")
  # union of frames
  tabella <- rbind(tabella, ag)
  # order
  tabella <- tabella[with(tabella, order(Ruolo)), ]
  View(tabella)
  tabella
}

viewTabellone2 <- function(asta, subtot = FALSE){
  asta$newcol <- apply(asta,1,function(row) paste( row[3] , row[5], sep = " - "  ))
  asta.m1 <- melt(asta, id=c("Ruolo", "Prog", "Fantasquadra"), measure.vars = c("newcol") )
  asta.m2 <- melt(asta, id=c("Ruolo", "Prog", "Fantasquadra"), measure.vars = c("Prezzo") )
  colnames(asta.m1) <- c("Ruolo", "Prog", "Fantasquadra", "variable2", "value2")
  asta.m <- merge(asta.m1, asta.m2)
  asta.m <- as.data.frame(unclass(asta.m))
  if(subtot){
    #c("Ruolo", "grand_row")
    tabellone <- cast(asta.m, Ruolo + Prog ~ Fantasquadra, value = "value2", fun.aggregate=sumprezzo, margins=TRUE)
  }
  else{
    tabellone <- cast(asta.m, Ruolo + Prog ~ Fantasquadra, value = "value2")
  }
  tabella <- tabellone[,c(1,3:ncol(tabellone))]
  View(tabella)
  tabella
}

sumprezzo <- function(x, na.rm=TRUE){
  if(na.rm) x <- x[!is.na(x)]
  x <- unlist(strsplit(as.character(x), " - "))[2*(1:length(x))]
  x <- as.integer(x)
  #as.character(sum(x))
  sum(x)
}

convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}

deleteAcquisto <- function(asta){
  asta <- asta[-nrow(asta),]
  asta
}

quotaSquadre <- function(df, num=25){
  appo <- df[with(df, order(-Quota, Calciatore)), ]
  appo <- tapply(appo$Quota, appo$Fantasquadra, function(x) c( sum(x[1:num], na.rm=TRUE)))
  appo
}

findCalciatore <- function(quote, nome)  {
  quote[grep(nome, quote$Calciatore),]
}