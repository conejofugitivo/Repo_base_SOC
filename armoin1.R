library(mpspline2)
library(readxl)
library(writexl)

datos_suelo <- as.data.frame(read_xlsx("03_Base_Limpia_Fisica.xlsx"))

#puntos decimales
datos_suelo$ID_Punto <- as.numeric(as.character(datos_suelo$ID_Punto))
datos_suelo$Lim_sup  <- as.numeric(gsub(",", ".", as.character(datos_suelo$Lim_sup)))
datos_suelo$Lim_inf  <- as.numeric(gsub(",", ".", as.character(datos_suelo$Lim_inf)))
datos_suelo$CO_g     <- as.numeric(gsub(",", ".", as.character(datos_suelo$CO_g)))

#Contamos cuántas capas tiene cada ID
conteos <- as.data.frame(table(datos_suelo$ID_Punto))
colnames(conteos) <- c("ID_Punto", "n_capas")
conteos$ID_Punto <- as.numeric(as.character(conteos$ID_Punto))

# Solo nos quedamos con los IDs que tienen 2 o más capas
ids_validos <- conteos$ID_Punto[conteos$n_capas >= 2]
df_input <- datos_suelo[datos_suelo$ID_Punto %in% ids_validos, c("ID_Punto", "Lim_sup", "Lim_inf", "CO_g")]

print(paste("Puntos con datos suficientes (>=2 capas):", length(ids_validos)))
print(paste("Puntos descartados (1 sola capa):", sum(conteos$n_capas == 1)))

# armonización si hay puntos válidos
if(nrow(df_input) > 0) {
  resultado <- mpspline(obj = df_input, var_name = "CO_g", d = c(0, 5, 15, 30, 60, 100), vlow = 0)
  #extracción de puntos
  lista_tablas <- list()
  for (i in seq_along(resultado)) 
  {
    val <- resultado[[i]]$est_dcm
    if (!is.null(val)) {
      df_fila <- as.data.frame(t(val))
      df_fila$ID_Punto <- as.numeric(names(resultado)[i])
      lista_tablas[[i]] <- df_fila
    }
  }
  
  matriz_final_r <- do.call(rbind, lista_tablas)
  
  # 5. UNIÓN CON GEOGRAFÍA
  coords <- unique(datos_suelo[, c("ID_Punto", "lon", "lat")])
  final_ecuador <- merge(matriz_final_r, coords, by = "ID_Punto", all.x = TRUE)
  
  write_xlsx(final_ecuador, "05_Matriz_Ecuador_Armonizada_Final.xlsx")
  print(paste("SE generó un archivo con", nrow(final_ecuador), "perfiles"))
  
} else {
  print("ERROR: capas únicas")
}

