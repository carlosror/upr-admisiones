################################
# Tidying up the data
################################

library(plyr) # rename

admisiones <- read.csv("tasas.csv", encoding="UTF-8")

# Throw out column you don't intend to use
admisiones$Location.1 <- NULL

# Throw out record with Genero = "No Especificado". There's just one record
admisiones <- subset(admisiones, GENERO != "No Especificado")

# Throw out 117 records with missing GPA and 6 records with missing IGS
admisiones <- subset(admisiones, !is.na(GPA))
admisiones <- subset(admisiones, !is.na(IGS))

# Dealing with tildes
# Always a mess
genero_corrected <- "Género"
Encoding(genero_corrected) <- "UTF-8"
Bayamon_corrected <- "BAYAMÓN"
Encoding(Bayamon_corrected) <- "UTF-8"
RioPiedras_corrected <- "RÍO PIEDRAS"
Encoding(RioPiedras_corrected) <- "UTF-8"

# Rename some columns
admisiones <- rename(admisiones, replace=c("CALENDARIO" = "Calendario"))
admisiones <- rename(admisiones, replace=c("GENERO" = "Genero")) #pass on the tilde for column names
admisiones <- rename(admisiones, replace=c("INSTITUCION.DE.PROCEDENCIA" = "Institucion"))#pass on the tilde for column names
admisiones <- rename(admisiones, replace=c("CAMPUS" = "Campus"))
admisiones <- rename(admisiones, replace=c("PROGRAM" = "Programa"))

# Change "Calendario Académico 12-13" to "2012-2013" and all others as well
admisiones$Calendario <- substr(admisiones$Calendario, 22, 26) # now have just "12-13"
admisiones$Calendario <- revalue(admisiones$Calendario, c("13-14" = "2013-2014"))
admisiones$Calendario <- revalue(admisiones$Calendario, c("12-13" = "2012-2013"))
admisiones$Calendario <- revalue(admisiones$Calendario, c("11-12" = "2011-2012"))
admisiones$Calendario <- revalue(admisiones$Calendario, c("10-11" = "2010-2011"))
admisiones$Calendario <- revalue(admisiones$Calendario, c("09-10" = "2009-2010"))

# Change BAYAMON to BAYAMÓN and RIOPIEDRAS to RÍO PIEDRAS
admisiones$Campus <- revalue(admisiones$Campus, c("BAYAMON" = Bayamon_corrected))
admisiones$Campus <- revalue(admisiones$Campus, c("RIOPIEDRAS" = RioPiedras_corrected))

# Remove codes from high schools and programs
admisiones$Institucion <- substr(admisiones$Institucion, 9, length(admisiones$Institucion))
admisiones$Programa <- substr(admisiones$Programa, 10, length(admisiones$Programa))

# Create IGS buckets
# http://stackoverflow.com/questions/23316161/the-condition-has-length-1-and-only-the-first-element-will-be-used
# Not as straightforward as it seems
bucket_1 <- admisiones$IGS < 225
bucket_2 <- admisiones$IGS >= 225 & admisiones$IGS < 250
bucket_3 <- admisiones$IGS >= 250 & admisiones$IGS < 275
bucket_4 <- admisiones$IGS >= 275 & admisiones$IGS < 300
bucket_5 <- admisiones$IGS >= 300 & admisiones$IGS < 325
bucket_6 <- admisiones$IGS >= 325 & admisiones$IGS < 350
bucket_7 <- admisiones$IGS >= 350
admisiones[bucket_1, "IGS_bucket"] <- "< 225"
admisiones[bucket_2, "IGS_bucket"] <- "225 - 249"
admisiones[bucket_3, "IGS_bucket"] <- "250 - 274"
admisiones[bucket_4, "IGS_bucket"] <- "275 - 299"
admisiones[bucket_5, "IGS_bucket"] <- "300 - 324"
admisiones[bucket_6, "IGS_bucket"] <- "325 - 349"
admisiones[bucket_7, "IGS_bucket"] <- "> 350"
# admisiones$IGS_bucket <- if (admisiones$IGS >= 225 && admisiones$IGS < 249) "225 - 249" else "other"


# Write to a csv which will be put in the shiny app folder
write.table(admisiones, "admisiones.csv", sep=",", row.names=FALSE, fileEncoding = "UTF-8")