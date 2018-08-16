# My packages
library(data.table)
library(readr)
library(plyr)
library(lpSolve)
library(sqldf)


# deff our csv main path
csv_path <- "C:/Users/letic/Desktop/Desafio"
setwd(csv_path)

# Store pattern into list
my_data_list<- list.files(pattern="*.csv")
data_list<- lapply(my_data_list, function(s) as.data.table(read.csv(s, sep = ",", header = TRUE)))

# Deff aux variables
i = 1
original_len<- length(data_list)

# Use original table names
for(j in 1:original_len){
  if (i<=length(data_list)){    
    nome_table<- my_data_list[[i]]
    nome_table<- gsub(".csv", "", nome_table )
    data_list[[paste(nome_table)]]<- data_list[[j]]
    i<- i + 1
  }
}

collect_var_data<- function (mytable, variable){ 
  t<- as.data.table(mytable)
  t<- t[, get(variable)]
  t<- unique(t)
  t[]
}

convert_levels<- function(level_column, metadata_level){
  tmp<- copy(metadata_level)
  i = 1
  for(i in 1:dim(tmp)[1]){
    
    pattern<- tmp[i, 1]
    to_go<- tmp[i, 2]
    level_column<- lapply(level_column, function(s) gsub(pattern, to_go, "Teens 2.1"))
    i = 1 +i
  }
}


# Create group ids 
y<- copy(data_list$tb_vol_mesmaescala)
y<- y[, maior:= ifelse(y$Código> y$Código2,y$Código, y$Código2  )]
y<- y[, menor:= ifelse(y$Código< y$Código2,y$Código, y$Código2  )]
y<- y[, key:= paste(maior, menor, sep ="|")]
# z<- y[, group:= seq_len(.N), by = 'key' ]

z<- as.data.table(unique(y$key))
z<- z[, group:= seq_len(.N)]
colnames(z)[1]<- 'key'
y<- merge(y, z, by ="key")
y<- y[, c("Código", "group")]

data_list$tb_vol_mesmaescala<- y

setnames(data_list$tb_vol_mesmaescala, 'Código', 'candidate_id')
setnames(data_list$`tb_vol_cadastro-1`, 'Código.Voluntário', 'candidate_id')
data_list$tb_vol_mesmaescala<- unique(data_list$tb_vol_mesmaescala)

up_to_date_vol<- merge(data_list$`tb_vol_cadastro-1`, data_list$tb_vol_mesmaescala, by = 'candidate_id', all.x = T)
setnames(up_to_date_vol, 'Nível.Máximo.de.Conhecimento', 'candidate_level')
setnames(data_list$tb_dm_equivalencia, 'Nivel', 'candidate_level')

up_to_date_vol_x<- up_to_date_vol
y<- convert_levels(up_to_date_vol_x$candidate_level, metadata_level)


# Free memory
gc()


