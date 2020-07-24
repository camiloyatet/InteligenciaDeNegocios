library(dplyr);library(data.table);
# ------------------------------------------ Proceso -----------------------------------------------

Base <- readRDS("input/Consolidacion.rds") %>% select(id_persona,Nombre, NumIdPersona, Genero, Edad, Categoria, Segmento_poblacional, estado_civil,
                                                      nivel_academico, segmento_grupo_familiar,EstratoPersona,marca_afiliado_unico,id_empresa,RazonSocial,Piramide1,Piramide2 )

saveRDS(Base,"output/Base.rds")
