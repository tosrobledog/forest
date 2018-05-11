library(genderizeR)
library(gender)
###### Género
x<-author[,3]
split1<-strsplit(x,", ")
split2.df.Gender <- data.frame(gender=character())
options(stringsAsFactors = FALSE)
split2.df.GenderizeR <- data.frame(genderizeR=character())
options(stringsAsFactors = FALSE)
for (i in 1:length(split1))
{
  ###Se escoge el nombre del autor, y se desecha el apellido del mismo
  split2<-strsplit(split1[[i]][2]," ")  

  ###Se escoge el primer nombre, ya que varios autores tienen dos nombres, se le 
  ###aplica el gender y se convierte en dataframe para obtener el género fácilmente
  gen1_gender<-data.frame(gender(split2[[1]][1]))

  ###Se selecciona la ubicación en donde aparece el género, y con rbind se van guardando
  ###uno debajo del otro en un dataframe
  split2.df.Gender<-rbind(split2.df.Gender,gen1_gender[1,4])
  
  #
  
  tryCatch({
          gen1<-data.frame(findGivenNames(split2[[1]][1], progress=FALSE))
  }, error=function(e){})
  split2.df.GenderizeR<-rbind(split2.df.GenderizeR,gen1[1,2])
}
names(split2.df.Gender) <- "Gender"
names(split2.df.GenderizeR) <- "GenderizeR"


###Se unifica el dataframe de autores con el de género para que quede en uno solo
author<-cbind(author,split2.df.GenderizeR,split2.df.Gender)

author$Gender_R<-ifelse(is.na(author$GenderizeR), author$Gender, author$GenderizeR)

### Borramos las columnas de Gender y GenderizeR
author$Gender=NULL  
author$GenderizeR=NULL
######