
#Script para consolidar datos de los archivos de texto de atributos (sddatt, sdeatt) y localizaciones (sddloc, sdeloc) 

#Obtener url del directorio de trabajo dek proyecto
#urlDefault = "D:/G/GitHub/Uliege/R/Tests/"
#setwd(urlDefault)
urlDefault = getwd()

#Directorios del proyecto
urlRawData = paste(urlDefault,"/rawdata/",sep = "")
urlFigures = paste(urlDefault,"/figures/",sep = "")

#Data Acumulada DIA
txtTime = "Day"
sddattDay = NULL
sddlocDay = NULL
sdeattDay = NULL
sdelocDay = NULL

#Lectura de archivos
setwd(urlFigures)
#txtNumber=0

#Repetir
for(k in 0:200){
  txtNumber=k
  #Archivo SDD
  #Atributos (att)
  txtName = paste("sddatt",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  if(file.exists(fileTxtData)){
    con <- file(fileTxtData, open="r")
    sddattLeido <- read.csv(fileTxtData, header=TRUE)
    sddattDay = rbind(sddattDay, sddattLeido)
    rm(sddattLeido)
    close(con)  
  }else{
    next()
  }
  
  #Posiciones (loc)
  txtName = paste("sddloc",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sddlocLeido <- read.csv(fileTxtData, header = FALSE)
  sddlocLeido$V1 = NULL
  names(sddlocLeido) <- c("id", "x",  "y") 
  sddlocDay = rbind(sddlocDay, sddlocLeido)
  rm(sddlocLeido)
  close(con)
  
  
  #Archivo SDE
  #Atributos (att)
  txtName = paste("sdeatt",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sdeattLeido <- read.csv(fileTxtData, header=TRUE)
  sdeattDay = rbind(sdeattDay, sdeattLeido)
  rm(sdeattLeido)
  close(con)
  
  #Posiciones (loc)
  txtName = paste("sdeloc",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sdelocLeido <- read.csv(fileTxtData, header = FALSE)
  sdelocLeido$V1 = NULL
  names(sdelocLeido) <- c("id", "x",  "y") 
  sdelocDay = rbind(sdelocDay, sdelocLeido)
  rm(sdelocLeido)
  close(con)
  
}

#Volvemos al directorio por defecto
setwd(urlDefault)





#Data Acumulada SEMANA
txtTime = "Week"
sddattWeek = NULL
sddlocWeek = NULL
sdeattWeek = NULL
sdelocWeek = NULL

#Lectura de archivos
setwd(urlFigures)
#txtNumber=0

for(k in 0:200){
  #Repetir
  txtNumber=k
  #Archivo SDD
  #Atributos (att)
  txtName = paste("sddatt",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  if(file.exists(fileTxtData)){
    con <- file(fileTxtData, open="r")
    sddattLeido <- read.csv(fileTxtData, header=TRUE)
    sddattWeek = rbind(sddattWeek, sddattLeido)
    rm(sddattLeido)
    close(con)  
  }else{
    next()
  }
  
  #Posiciones (loc)
  txtName = paste("sddloc",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sddlocLeido <- read.csv(fileTxtData, header = FALSE)
  sddlocLeido$V1 = NULL
  names(sddlocLeido) <- c("id", "x",  "y") 
  sddlocWeek = rbind(sddlocWeek, sddlocLeido)
  rm(sddlocLeido)
  close(con)
  
  
  #Archivo SDE
  #Atributos (att)
  txtName = paste("sdeatt",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sdeattLeido <- read.csv(fileTxtData, header=TRUE)
  sdeattWeek = rbind(sdeattWeek, sdeattLeido)
  rm(sdeattLeido)
  close(con)
  
  #Posiciones (loc)
  txtName = paste("sdeloc",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sdelocLeido <- read.csv(fileTxtData, header = FALSE)
  sdelocLeido$V1 = NULL
  names(sdelocLeido) <- c("id", "x",  "y") 
  sdelocWeek = rbind(sdelocWeek, sdelocLeido)
  rm(sdelocLeido)
  close(con)
  
}

#Volvemos al directorio por defecto
setwd(urlDefault)




#Data Acumulada AÑO
txtTime = "Year"
sddattYear = NULL
sddlocYear = NULL
sdeattYear = NULL
sdelocYear = NULL

#Lectura de archivos
setwd(urlFigures)
#txtNumber=0

for(k in 0:200){
  #Repetir
  txtNumber=k
  #Archivo SDD
  #Atributos (att)
  txtName = paste("sddatt",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  if(file.exists(fileTxtData)){
    con <- file(fileTxtData, open="r")
    sddattLeido <- read.csv(fileTxtData, header=TRUE)
    sddattYear = rbind(sddattYear, sddattLeido)
    rm(sddattLeido)
    close(con)  
  }else{
    next()
  }
  
  #Posiciones (loc)
  txtName = paste("sddloc",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sddlocLeido <- read.csv(fileTxtData, header = FALSE)
  sddlocLeido$V1 = NULL
  names(sddlocLeido) <- c("id", "x",  "y") 
  sddlocYear = rbind(sddlocYear, sddlocLeido)
  rm(sddlocLeido)
  close(con)
  
  
  #Archivo SDE
  #Atributos (att)
  txtName = paste("sdeatt",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sdeattLeido <- read.csv(fileTxtData, header=TRUE)
  sdeattYear = rbind(sdeattYear, sdeattLeido)
  rm(sdeattLeido)
  close(con)
  
  #Posiciones (loc)
  txtName = paste("sdeloc",txtTime,"-tld",sep = "")
  fileTxtData = paste(txtName,txtNumber,"_Output.txt",sep="")
  con <- file(fileTxtData, open="r")
  sdelocLeido <- read.csv(fileTxtData, header = FALSE)
  sdelocLeido$V1 = NULL
  names(sdelocLeido) <- c("id", "x",  "y") 
  sdelocYear = rbind(sdelocYear, sdelocLeido)
  rm(sdelocLeido)
  close(con)
  
}

#Volvemos al directorio por defecto
setwd(urlDefault)



print("Fin procesamiento... ")

