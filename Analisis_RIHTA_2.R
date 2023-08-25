## Patients Living with Arterial Hypertension in Mexico: First Insights of The Mexican Registry of Arterial Hypertension (RIHTA Study).
## Data Analysis: Neftali Eduardo Antonio-Villa (neftalivilla@comunidad.unam.mx) 
## Latest version of Analysis 02-August-2023
## Any question regarding analysis, please contact Neftali Eduardo Antonio-Villa 

sum(!is.na(base$laboratorio))

#####Library#####

library(tidyverse)
library(readxl)
library(dplyr)
library(mice)
library(haven)
library(epiR)
library(ggpubr)
library(ggsci)
library(rstatix)
library(rmapshaper)
library(ggthemes)
library(patchwork)
library(gtsummary)
library(data.table)
library(jtools)
library(readspss)
library(na.tools)
library(nnet)

#####Load Dataset####

setwd("/Users/nefoantonio/Library/CloudStorage/OneDrive-UNIVERSIDADNACIONALAUTÓNOMADEMÉXICO/PROYECTOS/DRA PALOMO/RITA/Articulo RITHA")
base.original <- readspss::read.sav("/Users/nefoantonio/Library/CloudStorage/OneDrive-UNIVERSIDADNACIONALAUTÓNOMADEMÉXICO/PROYECTOS/DRA PALOMO/RITA/Articulo RITHA/RIHTA_25_ABR_v1.sav",pass="H1p3rt3ns0")
base.original<-janitor::clean_names(base.original)
base<-labelled::remove_labels(base.original)
base<-lapply(base, function(x) { attributes(x) <- NULL; x })
base<-as.data.frame(base)

#####Multiple Imputation Variable####

base$talla[base$talla<0.4]<-NA
base$talla[base$talla>2.0]<-NA
base$imc[base$imc<5]<-NA
base$imc[base$imc>60]<-NA
base$cintura[base$cintura<10]<-NA
base$cintura[base$cintura>170]<-NA
base$creatinina[base$creatinina<=0]<-NA
base$creatinina[base$creatinina>20]<-NA
base$creatinina[base$creatinina<=0]<-NA
base$creatinina[base$creatinina>100]<-NA
base$colesterol[base$colesterol<=10]<-NA
base$colesterol_ldl[base$colesterol_ldl<=0]<-NA
base$colesterol_hdl[base$colesterol_hdl<=0]<-NA
base$trigliceridos[base$trigliceridos<=0]<-NA
base$hb[base$hb<=0]<-NA
base$hb[base$hb>=30]<-NA
base$hba1c[base$hba1c<=0]<-NA
base$hba1c[base$hba1c>=30]<-NA
base$glucosa[base$glucosa<=20]<-NA
base$glucosa[base$glucosa>=1000]<-NA


base<-base%>%dplyr::mutate(base, id = rownames(base))%>%dplyr::mutate(id=as.numeric(id))
base2<-base%>%dplyr::select(id,peso,talla,imc,cintura,creatinina,colesterol,colesterol_ldl,colesterol_hdl,trigliceridos,hb,glucosa)
base2_imp<-mice::mice(base2, m=5, maxit=5,seed = 123)
base2_imp_2<-complete(base2_imp,1)
base<-base%>%dplyr::select(-c(peso,talla,imc,cintura,creatinina,colesterol,colesterol_ldl,colesterol_hdl,trigliceridos,hb,glucosa))%>%
  left_join(base2_imp_2,by="id")

#mice::stripplot(base2_imp, pch = 20, cex = 1.2)#ver como quedan las imputaciones junto a cada una de las variables observadas
#mice::densityplot(base2_imp)

summary(base$creatinina)
summary(base2$creatinina)
t.test(base$creatinina,base2$creatinina)

#####Recoding Variables#####
 
#Binary Outcomes
base$fuma<-dplyr::recode(base$fuma, "2" = 1, "1" = 0, .default = NA_real_)
base$consume_alcohol<-dplyr::recode(base$consume_alcohol, "2" = 1, "1" = 0, .default = NA_real_)
base$prueba_positiva_covid_19<-dplyr::recode(base$prueba_positiva_covid_19, "2" = 1, "1" = 0, .default = NA_real_)
base$hipertenso<-dplyr::recode(base$hipertenso, "2" = 1, "1" = 0, .default = NA_real_)
base$presion_arterial_en_casa<-dplyr::recode(base$presion_arterial_en_casa, "2" = 1, "1" = 0, .default = NA_real_)
base$evidencia_organo_blanco<-dplyr::recode(base$evidencia_organo_blanco, "2" = 1, "1" = 0, .default = NA_real_)
base$servicio_medico<-dplyr::recode(base$servicio_medico, "2" = 1, "1" = 0, .default = NA_real_)
base$danio_organo_blanco<-dplyr::recode(base$danio_organo_blanco, "2" = 1, "1" = 0, .default = NA_real_)
base$calcioantagonistas<-dplyr::recode(base$calcioantagonistas, "2" = 1, "1" = 0, .default = NA_real_)
base$iecas<-dplyr::recode(base$iecas, "2" = 1, "1" = 0, .default = NA_real_)
base$aras<-dplyr::recode(base$aras, "2" = 1, "1" = 0, .default = NA_real_)
base$tiazidicos<-dplyr::recode(base$tiazidicos, "2" = 1, "1" = 0, .default = NA_real_)
base$antagonistas_aldosterona<-dplyr::recode(base$antagonistas_aldosterona, "2" = 1, "1" = 0, .default = NA_real_)
base$asas<-dplyr::recode(base$asas, "2" = 1, "1" = 0, .default = NA_real_)
base$betabloqueadores<-dplyr::recode(base$betabloqueadores, "2" = 1, "1" = 0, .default = NA_real_)
base$prazocin<-dplyr::recode(base$prazocin, "2" = 1, "1" = 0, .default = NA_real_)
base$tamsulosina<-dplyr::recode(base$tamsulosina, "2" = 1, "1" = 0, .default = NA_real_)
base$hidralazina<-dplyr::recode(base$hidralazina, "2" = 1, "1" = 0, .default = NA_real_)
base$aspirina<-dplyr::recode(base$aspirina, "2" = 1, "1" = 0, .default = NA_real_)
base$estatinas<-dplyr::recode(base$estatinas, "2" = 1, "1" = 0, .default = NA_real_)
base$ezetimibes<-dplyr::recode(base$ezetimibes, "2" = 1, "1" = 0, .default = NA_real_)
base$fibratos<-dplyr::recode(base$fibratos, "2" = 1, "1" = 0, .default = NA_real_)
base$diabetico<-dplyr::recode(base$diabetico, "2" = 1, "1" = 0, .default = NA_real_)
base$terazocina<-dplyr::recode(base$terazocina, "2" = 1, "1" = 0, .default = NA_real_)
base$alfametildopa<-dplyr::recode(base$alfametildopa, "2" = 1, "1" = 0, .default = NA_real_)
base$cardiopatia_isquemica<-dplyr::recode(base$cardiopatia_isquemica, "2" = 1, "1" = 0, .default = NA_real_)
base$enfermedad_vascular_cerebral<-dplyr::recode(base$enfermedad_vascular_cerebral, "2" = 1, "1" = 0, .default = NA_real_)
base$enfermedad_renal_cronica<-dplyr::recode(base$enfermedad_renal_cronica, "2" = 1, "1" = 0, .default = NA_real_)
base$restriccion_calorica<-dplyr::recode(base$restriccion_calorica, "2" = 1, "1" = 0, .default = NA_real_)
base$restriccion_proteica<-dplyr::recode(base$restriccion_proteica, "2" = 1, "1" = 0, .default = NA_real_)
base$restriccion_salina<-dplyr::recode(base$restriccion_salina, "2" = 1, "1" = 0, .default = NA_real_)

base$disnea<-dplyr::recode(base$disnea, "2" = 1, "1" = 0, .default = NA_real_)
base$cefalea<-dplyr::recode(base$cefalea, "2" = 1, "1" = 0, .default = NA_real_)
base$mareo<-dplyr::recode(base$mareo, "2" = 1, "1" = 0, .default = NA_real_)
base$ronquidos<-dplyr::recode(base$ronquidos, "2" = 1, "1" = 0, .default = NA_real_)
base$angina<-dplyr::recode(base$angina, "2" = 1, "1" = 0, .default = NA_real_)
base$palpitaciones<-dplyr::recode(base$palpitaciones, "2" = 1, "1" = 0, .default = NA_real_)
base$edema<-dplyr::recode(base$edema, "2" = 1, "1" = 0, .default = NA_real_)
base$sincope<-dplyr::recode(base$sincope, "2" = 1, "1" = 0, .default = NA_real_)
base$somnolencia_diurna<-dplyr::recode(base$somnolencia_diurna, "2" = 1, "1" = 0, .default = NA_real_)


# base<-data.frame(lapply(base, function(x) {gsub("No", 0, x)}))
# base<-data.frame(lapply(base, function(x) {gsub("Si", 1, x)}))
# base<-lapply(base,as.numeric)

#IMSS
base$cual_servicio_medico_REC<-NULL
base$cual_servicio_medico_REC[base$cual_servicio_medico==1]<-1
base$cual_servicio_medico_REC[base$cual_servicio_medico!=1]<-0

#Edad
base$edad_cat<-NULL
base$edad_cat[base$edad<45]<-1
base$edad_cat[base$edad>=45 & base$edad<65]<-2
base$edad_cat[base$edad>=65]<-3

#Categorias de HAS
base$TAS_CATEGORIAS<-NULL
base$TAS_CATEGORIAS[base$promedio_sistolica<130]<-1
base$TAS_CATEGORIAS[base$promedio_sistolica>=130 & base$promedio_sistolica<140]<-2
base$TAS_CATEGORIAS[base$promedio_sistolica>=140 & base$promedio_sistolica<159]<-3
base$TAS_CATEGORIAS[base$promedio_sistolica>=160]<-4

base$TAD_CATEGORIAS<-NULL
base$TAD_CATEGORIAS[base$promedio_diastolica<85]<-1
base$TAD_CATEGORIAS[base$promedio_diastolica>=85 & base$promedio_diastolica<90]<-2
base$TAD_CATEGORIAS[base$promedio_diastolica>=90 & base$promedio_diastolica<99]<-3
base$TAD_CATEGORIAS[base$promedio_diastolica>=100]<-4

#TA CONTROL 
base$TA_CONTROL<-NULL
base$TA_CONTROL[base$TAS_CATEGORIAS==1 | base$TAD_CATEGORIAS==1]<-1 #Normal
base$TA_CONTROL[base$TAS_CATEGORIAS==2 | base$TAD_CATEGORIAS==2]<-2 #PA Normal Alta
base$TA_CONTROL[base$TAS_CATEGORIAS==3 | base$TAD_CATEGORIAS==3]<-3 #HTA Grado 1
base$TA_CONTROL[base$TAS_CATEGORIAS==4 | base$TAD_CATEGORIAS==4]<-4 #HTA Grado 2 y 3

#TA CONTROL AHA
base$TA_CONTROL_AHA<-NULL
base$TA_CONTROL_AHA[base$promedio_diastolica<130 | base$promedio_diastolica<80]<-1
base$TA_CONTROL_AHA[base$promedio_diastolica>=130 | base$promedio_diastolica>=80]<-0


#TA CONTROL ESC
base$TA_CONTROL_ESC<-NULL
base$TA_CONTROL_ESC[base$promedio_diastolica<140 | base$promedio_diastolica<90]<-1
base$TA_CONTROL_ESC[base$promedio_diastolica>=140 | base$promedio_diastolica>=90]<-0


#IMC Categorias
base$imc_cat<-NULL
base$imc_cat[base$imc<25]<-1
base$imc_cat[base$imc>=25 & base$imc<30]<-2
base$imc_cat[base$imc>=30]<-3

#Sexo
base$sexo_rec<-NULL
base$sexo_rec[base$sexo==1]<-1
base$sexo_rec[base$sexo==2]<-0

#Diabetes FINAL
base$diabetes_NEAV<-NULL
base$diabetes_NEAV[base$diabetico==1 | base$glucosa>=200 | base$hba1c>=6.5]<-1
base$diabetes_NEAV[is.na(base$diabetes_NEAV)]<-0

#Cintura
base$cintura_rec<-NULL
base$cintura_rec[base$sexo_rec==1 & base$cintura>=90]<-1
base$cintura_rec[base$sexo_rec==0 & base$cintura>=80]<-1
base$cintura_rec<-na.tools::na.replace(base$cintura_rec,0)

#ICE
base$ICE<-NULL
base$ICE<-base$cintura/(base$talla*100)

#METS_IR
base$METS_IR<-((log((2*base$glucosa)+base$trigliceridos)*base$imc))/(log(base$colesterol_hdl))

#METS_VF
base$METSVF<-(4.466+0.011*(log(base$METS_IR)^3)+3.239*(log(base$ICE)^3)+0.319*(base$sexo_rec)
              +0.594*(log(base$edad)))

#Grasa Visceral
base$VAT_METS<-exp(base$METSVF)

#Hipercolesterolemia
base$hipercol<-NULL
base$hipercol[base$colesterol>=200]<-1
base$hipercol[base$colesterol<200]<-0

#METS-IR Aumentado 

base$METS_IR_AUMENTADO<-NULL
base$METS_IR_AUMENTADO[base$METS_IR>=51.13]<-1
base$METS_IR_AUMENTADO[base$METS_IR<51.13]<-0

#LDL Alto ≥100
base$ldl_alto<-NULL
base$ldl_alto[base$colesterol_ldl>=100]<-1
base$ldl_alto[base$colesterol_ldl<100]<-0

#HDL Bajo
base$hipohdl<-NULL
base$hipohdl[base$sexo_rec==1 & base$colesterol_hdl<50]<-1
base$hipohdl[base$sexo_rec==0 & base$colesterol_hdl<40]<-1
base$hipohdl[base$sexo_rec==1 & base$colesterol_hdl>=50]<-0
base$hipohdl[base$sexo_rec==0 & base$colesterol_hdl>=40]<-0

#Hipertrigliceridemia 
base$hipertrig<-NULL
base$hipertrig[base$trigliceridos>=150]<-1
base$hipertrig[base$trigliceridos<150]<-0

#Any Dyslipidemia
base$dislipidemia<-NULL
base$dislipidemia[base$hipertrig==1 | base$hipohdl==1 | base$ldl_alto==1]<-1
base$dislipidemia[is.na(base$dislipidemia)]<-0

#Hiperglucemia
base$hiperglu<-NULL
base$hiperglu[base$glucosa>=100]<-1
base$hiperglu[base$glucosa<100]<-0

#Descontrol Glucemico
base$descontrol_gluc<-NULL
base$descontrol_gluc[base$hba1c>=9.0]<-1
base$descontrol_gluc[base$hba1c<9.0]<-0

#Bioquimicos
base$colesterol_no_hdl<-base$colesterol-base$colesterol_hdl
base$TFG_CKD_EPI<-nephro::CKDEpi.creat.rf(base$creatinina, base$sexo_rec, base$edad)

#CKD Categories
base$CAT_CKD<-NULL
base$CAT_CKD[base$TFG_CKD_EPI>=90]<-1
base$CAT_CKD[base$TFG_CKD_EPI>=60 & base$TFG_CKD_EPI<90]<-2
base$CAT_CKD[base$TFG_CKD_EPI>=30 & base$TFG_CKD_EPI<60]<-3
base$CAT_CKD[base$TFG_CKD_EPI>=15 & base$TFG_CKD_EPI<30]<-4
base$CAT_CKD[base$TFG_CKD_EPI<15]<-5

#Cualquier Tratamiento
base$TRATAMIENTO_DIC<-NULL
base$TRATAMIENTO_DIC[base$tipo_tratamiento==1]<-0
base$TRATAMIENTO_DIC[base$tipo_tratamiento==2]<-1
base$TRATAMIENTO_DIC[base$tipo_tratamiento==3]<-1
base$TRATAMIENTO_DIC[is.na(base$TRATAMIENTO_DIC)]<-0

#TA ≥130/85 mmHg
base$TA_ELEVADA<-NULL
base$TA_ELEVADA[base$TA_CONTROL!=1]<-1 #Normal
base$TA_ELEVADA<-na.tools::na.replace(base$TA_ELEVADA,0)

#Categorias de HAS
base$CATEGORIA_HAS<-NULL
base$CATEGORIA_HAS[base$TRATAMIENTO_DIC==0]<-1
base$CATEGORIA_HAS[base$TRATAMIENTO_DIC==1 & base$TA_ELEVADA==1]<-2
base$CATEGORIA_HAS[base$TRATAMIENTO_DIC==1 & base$TA_ELEVADA==0]<-3

#Origen Etnico Meztizo
base$origen_etnico_rec<-NULL
base$origen_etnico_rec[base$origen_etnico==1]<-1
base$origen_etnico_rec[is.na(base$origen_etnico_rec)]<-2

#Sabe Medir HAS

base$lo_entrenaron_rec<-NULL
base$lo_entrenaron_rec[base$lo_entrenaron==1]<-1
base$lo_entrenaron_rec[base$lo_entrenaron==2]<-1
base$lo_entrenaron_rec[base$lo_entrenaron==3]<-1
base$lo_entrenaron_rec[is.na(base$lo_entrenaron)]<-0

#Antecedente de HAS en el Embarazo 
base$has_embarazo<-NULL
base$has_embarazo[base$embarazos_eclampsia==2]<-1
base$has_embarazo[base$embarazos_preeclampsia==2]<-1
base$has_embarazo[base$embarazos_hipertension==2]<-1
base$has_embarazo[base$sexo_rec==1 & is.na(base$has_embarazo)]<-0


#GLOBORISK
library(globorisk)

base$edad_dummy<-NULL
base$edad_dummy<-base$edad
base$edad_dummy[base$edad_dummy<=40]<-40
base$edad_dummy[base$edad_dummy>=80]<-80

base$colesterol_dummy<-base$colesterol*0.02586
base$colesterol_dummy[base$colesterol_dummy<=1]<-NA
base$colesterol_dummy[base$colesterol_dummy>=6]<-NA

base$year<-c("2020")

base$globorisk_officina<-globorisk(
  sex = base$sexo_rec,
  age = base$edad_dummy,
  sbp = base$promedio_sistolica,
  smk = base$fuma,
  bmi = base$imc,
  iso = rep("MEX", nrow(base)),
  year = base$year,
  version = "office",
  type = "risk",
  updated_lac = T)*100

base$globorisk_labs<-globorisk(
  sex = base$sexo_rec,
  age = base$edad_dummy,
  sbp = base$promedio_sistolica,
  tc = base$colesterol_dummy,
  dm = base$diabetes_NEAV,
  smk = base$fuma,
  iso = rep("MEX", nrow(base)),
  year = base$year,
  version = "lab",
  type = "risk"
)*100


base$globorisk_officina_elevado<-NULL
base$globorisk_officina_elevado[base$globorisk_officina>=10]<-1
base$globorisk_officina_elevado[base$globorisk_officina<10]<-0



#Mujeres Rec
base$embarazada[base$sexo_rec==0 & !is.na(base$embarazada)]<-NA
base$anticoncepcion_hormonal[base$sexo_rec==0 & !is.na(base$anticoncepcion_hormonal)]<-NA
base$tratamiento_de_reemplazo_hormonal[base$sexo_rec==0 & !is.na(base$tratamiento_de_reemplazo_hormonal)]<-NA
base$falla_ovarica_prematura[base$sexo_rec==0 & !is.na(base$falla_ovarica_prematura)]<-NA
base$embarazos_eclampsia[base$sexo_rec==0 & !is.na(base$embarazos_eclampsia)]<-NA
base$embarazos_preeclampsia[base$sexo_rec==0 & !is.na(base$embarazos_preeclampsia)]<-NA
base$embarazos_hipertension[base$sexo_rec==0 & !is.na(base$embarazos_hipertension)]<-NA

#Microalbuminuria 
base$Microalbuminuria_NEAV<-NULL
base$Microalbuminuria_NEAV[base$albuminuria_proteinuria>=2]<-1
base$Microalbuminuria_NEAV[base$tipo_evidencia_organo_blanco==1]<-1
base$Microalbuminuria_NEAV<-na.tools::na.replace(base$Microalbuminuria_NEAV,0)

#ERC con TFG <60 ml/min
base$ERC_NEAV<-NULL
base$ERC_NEAV[base$TFG_CKD_EPI<=60]<-1
base$ERC_NEAV[base$tipo_evidencia_organo_blanco==2]<-1
base$ERC_NEAV<-na.tools::na.replace(base$ERC_NEAV,0)

#Hipertrofia Ventricular y/o Crecimiento de Cavidades
base$HVI_CREC_CARD_NEAV<-NULL
base$HVI_CREC_CARD_NEAV[base$crecimiento_cavidades==2]<-1
base$HVI_CREC_CARD_NEAV[base$tipo_evidencia_organo_blanco==3]<-1
base$HVI_CREC_CARD_NEAV<-na.tools::na.replace(base$HVI_CREC_CARD_NEAV,0)

#Retinopatia HAS
base$RETINO_NEAV<-NULL
base$RETINO_NEAV[base$tipo_evidencia_organo_blanco==4]<-1
base$RETINO_NEAV<-na.tools::na.replace(base$RETINO_NEAV,0)

#Indice Tobillo Brazo Aumentado
base$INDICE_TO.BRA_NEAV<-NULL
base$INDICE_TO.BRA_NEAV[base$tipo_evidencia_organo_blanco==5]<-1
base$INDICE_TO.BRA_NEAV<-na.tools::na.replace(base$INDICE_TO.BRA_NEAV,0)

#Presion de Pulso ≥60 mmHg
base$PRESION_PULSO_60_NEAV<-NULL
base$PRESION_PULSO_60_NEAV[base$presion_de_pulso>=60]<-1
base$PRESION_PULSO_60_NEAV[base$tipo_evidencia_organo_blanco==6]<-1
base$PRESION_PULSO_60_NEAV<-na.tools::na.replace(base$PRESION_PULSO_60_NEAV,0)


#Daño Organo Blanco
base$DOB_REC_NEAV<-NULL
base$DOB_REC_NEAV[base$evidencia_organo_blanco==1]<-1
base$DOB_REC_NEAV[base$Microalbuminuria_NEAV==1]<-1
base$DOB_REC_NEAV[base$ERC_NEAV==1]<-1
base$DOB_REC_NEAV[base$HVI_CREC_CARD_NEAV==1]<-1
base$DOB_REC_NEAV[base$RETINO_NEAV==1]<-1
base$DOB_REC_NEAV[base$INDICE_TO.BRA_NEAV==1]<-1
base$DOB_REC_NEAV[base$PRESION_PULSO_60_NEAV==1]<-1
base$DOB_REC_NEAV<-na.tools::na.replace(base$DOB_REC_NEAV,0)

#DOB Temprano
base$DOB_REC_NEAV_45<-NULL
base$DOB_REC_NEAV_45[base$DOB_REC_NEAV==1 & base$edad<=45]<-1
base$DOB_REC_NEAV_45[is.na(base$DOB_REC_NEAV_45)]<-0

#Numero de Daño de Organo Blanco
base$NUM_DOB<-as.numeric(base$PRESION_PULSO_60_NEAV==1)+
as.numeric(base$INDICE_TO.BRA_NEAV==1)+
as.numeric(base$RETINO_NEAV==1)+
as.numeric(base$HVI_CREC_CARD_NEAV==1)+
as.numeric(base$ERC_NEAV==1)+
as.numeric(base$Microalbuminuria_NEAV==1)+
as.numeric(base$evidencia_organo_blanco==1)


#Numero de Comobilidades

base$NUM_COMORB<-as.numeric(base$diabetes_NEAV==1)+
as.numeric(base$cardiopatia_isquemica==1)+
as.numeric(base$enfermedad_vascular_cerebral==1)+
as.numeric(base$enfermedad_renal_cronica==1)+
as.numeric(base$fuma==1)

#Sin Comorbilidades
base$sin_comorb<-NULL
base$sin_comorb[base$NUM_COMORB==0]<-1
base$sin_comorb[base$NUM_COMORB>=1]<-0

#Typos

base$fuma[is.na(base$fuma)]<-0
base$consume_alcohol[is.na(base$consume_alcohol)]<-0
base$prueba_positiva_covid_19[is.na(base$prueba_positiva_covid_19)]<-0
base$hipertenso[is.na(base$hipertenso)]<-0
base$presion_arterial_en_casa[is.na(base$presion_arterial_en_casa)]<-0
base$evidencia_organo_blanco[is.na(base$evidencia_organo_blanco)]<-0
base$servicio_medico[is.na(base$servicio_medico)]<-0
base$anios_educacion[is.na(base$anios_educacion)]<-0
base$danio_organo_blanco[is.na(base$danio_organo_blanco)]<-0
base$lo_entrenaron[base$presion_arterial_en_casa==0 & !is.na(base$lo_entrenaron)]<-NA
base$anios_educacion[base$anios_educacion==0]<-1

base$calcioantagonistas<-na.tools::na.replace(base$calcioantagonistas,0)
base$calcioantagonistas[base$calcioantagonista>=1]<-1
base$calcioantagonista[base$calcioantagonistas==1 & is.na(base$calcioantagonista)]<-7


base$iecas<-na.tools::na.replace(base$iecas,0)
base$iecas[base$ieca>=1]<-1
base$ieca[base$iecas==1 & is.na(base$ieca)]<-9

base$aras<-na.tools::na.replace(base$aras,0)
base$aras[base$ara>=1]<-1
base$ara[base$aras==1 & is.na(base$ara)]<-10

base$tiazidicos<-na.tools::na.replace(base$tiazidicos,0)
base$tiazidicos[base$tiazidico>=1]<-1
base$tiazidico[base$tiazidicos==1 & is.na(base$tiazidico)]<-4

base$antagonistas_aldosterona<-na.tools::na.replace(base$antagonistas_aldosterona,0)
base$antagonistas_aldosterona[base$antagonista_aldosterona>=1]<-1
base$antagonista_aldosterona[base$antagonistas_aldosterona==1 & is.na(base$antagonista_aldosterona)]<-2

base$asas<-na.tools::na.replace(base$asas,0)
base$asas[base$asa>=1]<-1
base$asa[base$asas==1 & is.na(base$asa)]<-3


base$betabloqueadores<-na.tools::na.replace(base$betabloqueadores,0)
base$betabloqueadores[base$betabloqueador>=1]<-1
base$betabloqueador[base$betabloqueadores==1 & is.na(base$betabloqueador)]<-8

base$prazocin<-na.tools::na.replace(base$prazocin,0)
base$tamsulosina<-na.tools::na.replace(base$tamsulosina,0)
base$hidralazina<-na.tools::na.replace(base$hidralazina,0)
base$aspirina<-na.tools::na.replace(base$aspirina,0)

base$estatinas<-na.tools::na.replace(base$estatinas,0)
base$estatinas[base$estatina>=1]<-1
base$estatina[base$estatinas==1 & is.na(base$estatina)]<-5

base$ezetimibes<-na.tools::na.replace(base$ezetimibes,0)
base$ezetimibes[base$ezetimibe>=1]<-1
base$ezetimibe[base$ezetimibes==1 & is.na(base$ezetimibe)]<-3

base$fibratos<-na.tools::na.replace(base$fibratos,0)
base$fibratos[base$fibrato>=1]<-1
base$fibrato[base$fibratos==1 & is.na(base$fibrato)]<-4



#Recoding NA
base$tratamiento<-forcats::fct_explicit_na(factor(base$tratamiento))
base$tipo_tratamiento<-forcats::fct_explicit_na(factor(base$tipo_tratamiento))
base$esquema_tratamiento_farmacologico<-forcats::fct_explicit_na(factor(base$esquema_tratamiento_farmacologico))
# base$hipercol<-forcats::fct_explicit_na(factor(base$hipercol))
# base$ldl_alto<-forcats::fct_explicit_na(factor(base$ldl_alto))
# base$hipohdl<-forcats::fct_explicit_na(factor(base$hipohdl))
# base$hipertrig<-forcats::fct_explicit_na(factor(base$hipertrig))
# base$hiperglu<-forcats::fct_explicit_na(factor(base$hiperglu))

#####Setting Atributes####

#Correcting typos
#Factor
base$sexo_rec<-factor(base$sexo_rec,labels = c("Female","Male"))
base$anios_educacion<-factor(base$anios_educacion,labels = c("0 Years","1-6 Years","7-12 Years","≥12 Years"))
base$origen_etnico_rec<-factor(base$origen_etnico_rec,labels = c("Meztizo","Other"))
base$lo_entrenaron<-factor(base$lo_entrenaron,labels = c("Learn Alone","A Relative/Friend Taught Him","Taught by Medic"))
base$ultima_vez_presion_arterial<-factor(base$ultima_vez_presion_arterial,labels = c("Never","<1 Year",">1 Year"))
base$tratamiento<-factor(base$tratamiento,labels = c("Begin with","Modification","W/o Changes","Unknown"))
base$tipo_tratamiento<-factor(base$tipo_tratamiento,labels = c("Non-pharmacological\nOnly","Pharmacological\nOnly","Both","Uncoded"))
base$esquema_tratamiento_farmacologico<-factor(base$esquema_tratamiento_farmacologico,labels = c("Monotherapy","Double therapy","Triple therapy","+4 Antihypertensives","Uncoded"))
base$CATEGORIA_HAS<-factor(base$CATEGORIA_HAS,labels = c("W/o Hypertensive Treatment",
                                                         "In Treatment and With Uncontrolled Hypertension",
                                                         "In Treatment and With Controlled Hypertension"))

base$cual_servicio_medico<-factor(base$cual_servicio_medico,
                                  labels = c("IMSS","ISSSTE","SSA","Marina","Otro"))

base$imc_cat<-factor(base$imc_cat,labels = c("Normal-weight","Over-weight","Obesity"))
base$TA_CONTROL<-factor(base$TA_CONTROL,labels = c("Normal\n(<130/85 mmHg)",
                                                   "Normal-Alta\n(130-139/85-89 mmHg)",
                                                   "Grado 1\n(140-159/90-99 mmHg)",
                                                   "Grado 2 y 3\n(>160/100 mmHg)"))
base$CAT_CKD<-factor(base$CAT_CKD,labels = c("Stage 1","Stage 2","Stage 3","Stage 4","Stage 5"))
base$vacuna_covid_19<-factor(base$vacuna_covid_19,labels = c("No","Incomplete","Complete"))
base$cuanto_ejercicio<-factor(base$cuanto_ejercicio,labels = c("No-Exercise","< 150 min","≥ 150 min"))
base$edad_cat<-factor(base$edad_cat,labels = c("<45","45-65","≥65"))
base$NUM_DOB<-factor(base$NUM_DOB,labels = c("0","1","2","3","4"))

base$calcioantagonista<-factor(base$calcioantagonista,labels = c("Amlodipine","Diltiazem","Felodipine","Nicardipine","Niferdipine","Verapamil","Unknown"))
base$ieca<-factor(base$ieca,labels = c("Captopril","Enalapril","Fosinorpil","Lisinopril","Perindopril","Ramipril","Zofenopril","Unknown"))
base$ara<-factor(base$ara,labels = c("Azilsartan","Candersartan","Eporsartan","Irbesartan","Losartan","Olmesartan","Telmisartan","Valsartan","Fimasartan","Unknown"))
base$antagonista_aldosterona<-factor(base$antagonista_aldosterona,labels = c("Spironolactone","Unknown"))
base$asa<-factor(base$asa,labels = c("Furosemide","Bumethamide","Unknown"))
base$tiazidico<-factor(base$tiazidico,labels = c("Clortalidona","Hydrochlorothiazide","Indapamide","Unknown"))
base$betabloqueador<-factor(base$betabloqueador,labels = c("Atenolol","Bisoprolol","Metoprolol Succinate","Metoprolol Tartate","Nebivolol","Propanolol","Carvedidol","Unknown"))
base$estatina<-factor(base$estatina,labels = c("Rosuvastatin","Atorvastatin","Pravastatin","Unknown"))
base$fibrato<-factor(base$fibrato,labels = c("Fenofibrate","Bezafibrate","Cipofibrate","Unknown"))
base$ezetimibe<-factor(base$ezetimibe,labels = c("Ezetimibe","Ezetimibe/Simvastatin","Unknown"))
base$ansioso_ultimas_semanas<-factor(base$ansioso_ultimas_semanas,labels = c("No","Little","More-or-Less","Very","Very-Much"))
base$triste_ultimo_mes<-factor(base$triste_ultimo_mes,labels = c("No","Little","More-or-Less","Very","Very-Much"))




###Labels##
#Sociodemograficos
setattr(base$sexo_rec, "label", "Sex, (%)")
setattr(base$edad, "label", "Age, (Years)")
setattr(base$anios_educacion, "label", "Education, (Years)")
setattr(base$servicio_medico, "label", "Has a medical insurance, (%)")
setattr(base$cual_servicio_medico_REC, "label", "Type of medical insurance, (%)")
setattr(base$origen_etnico_rec, "label", "Ethnic origin, (%)")

#Ginecobstetrico
setattr(base$embarazada, "label", "Current pregnancy, (%)")
setattr(base$anticoncepcion_hormonal, "label", "Hormonal contraceptives, (%)")
setattr(base$tratamiento_de_reemplazo_hormonal, "label", "Hormone replacement treatment, (%)")
setattr(base$falla_ovarica_prematura, "label", "Premature ovarian failure, (%)")
setattr(base$embarazos_eclampsia, "label", "Previous eclampsia, (%)")
setattr(base$embarazos_preeclampsia, "label", "Previous preclampsia, (%)")
setattr(base$embarazos_hipertension, "label", "Hypertension during pregnancy, (%)")
setattr(base$has_embarazo, "label", "Hypertensive disorder in pregnancy, (%)")

#Antropometria
setattr(base$peso, "label", "Weight, (Kg)")
setattr(base$talla, "label", "Height, (Mts)")
setattr(base$imc, "label", "BMI, (Kg/mts2)")
setattr(base$cintura, "label", "Waist circumference, (Cms)")
setattr(base$promedio_sistolica, "label", "SBP, (mmHg)")
setattr(base$promedio_diastolica, "label", "DBP, (mmHg)")
setattr(base$promedio_pulso, "label", "HR, (bpm)")

#Clinicos

setattr(base$sin_comorb, "label", "Withouth comorbidities, (%)")
setattr(base$fuma, "label", "Previous smoking, (%)")
setattr(base$consume_alcohol, "label", "Alcoholism, (%)")
setattr(base$diabetes_NEAV, "label", "Diabetes, (%)")
setattr(base$cardiopatia_isquemica, "label", "Previous CVD, (%)")
setattr(base$enfermedad_vascular_cerebral, "label", "Stroke, (%)")
setattr(base$enfermedad_renal_cronica, "label", "CKD, (%)")
setattr(base$prueba_positiva_covid_19, "label", "Previous COVID-19, (%)")

#Evaluación de HAS
setattr(base$hipertenso, "label", "Agree to live with hypertension, (%)")
setattr(base$ultima_vez_presion_arterial, "label", "Last BP measurement, (%)")
setattr(base$clases_medicamentos_presion_arterial, "label", "Types of antihypertensives, (%)")
setattr(base$presion_arterial_en_casa, "label", "BP is taken at home, (%)")
setattr(base$lo_entrenaron, "label", "Where did you learn to take BP, (%)")
setattr(base$DOB_REC_NEAV, "label", "End organ damage, (%)")
setattr(base$DOB_REC_NEAV_45, "label", "Premature EOD, (%)")
setattr(base$tipo_evidencia_organo_blanco, "label", "Type of EOD, (%)")

#Tratamiento
setattr(base$tratamiento, "label", "Modification in treatment, (%)")
setattr(base$tipo_tratamiento, "label", "Type of treatment, (%)")
setattr(base$esquema_tratamiento_farmacologico, "label", "Regime of Treatment, (%)")
setattr(base$metas_de_tratamiento, "label", "Goals of treatment, (%)")
setattr(base$puntos_afec, "label", "AFEC, (pts)")
setattr(base$resultado_test_afec, "label", "AFEC results, (%)")
setattr(base$metas_de_tratamiento, "label", "Goals of treatment, (%)")
setattr(base$TA_ELEVADA, "label", "Uncontrolled Hypertension (TA ≥140/90 mmHg), (%)")
setattr(base$CATEGORIA_HAS, "label", "Clinical managment, (%)")
setattr(base$imc_cat, "label", "BMI categories, (%)")


#Tipos de tratamiento
setattr(base$calcioantagonistas, "label", "Calcium antagonists, (%)")
setattr(base$iecas, "label", "ACEIs, (%)")
setattr(base$aras, "label", "ARBs, (%)")
setattr(base$tiazidicos, "label", "Thiazide Diuretics, (%)")
setattr(base$antagonistas_aldosterona, "label", "Aldosterone Antagonists, (%)")
setattr(base$asas, "label", "Loop diuretics, (%)")
setattr(base$betabloqueadores, "label", "Beta−blockers, (%)")
setattr(base$prazocin, "label", "Prazocin, (%)")
setattr(base$terazocina, "label", "Terazocine, (%)")
setattr(base$tamsulosina, "label", "Tamsulosin, (%)")
setattr(base$hidralazina, "label", "Hydralazine, (%)")
setattr(base$alfametildopa, "label", "Alphamethyldopa, (%)")

setattr(base$estatinas, "label", "Statins, (%)")
setattr(base$ezetimibes, "label", "Ezetimibe, (%)")
setattr(base$fibratos, "label", "Fibrates, (%)")
setattr(base$aspirina, "label", "Aspirin, (%)")

#Laboratorios
setattr(base$creatinina, "label", "Serum creatinine, (mg/dl)")
setattr(base$colesterol, "label", "Total cholesteroll, (mg/dl)")
setattr(base$colesterol_ldl, "label", "LDL-C, (mg/dl)")
setattr(base$colesterol_hdl, "label", "HDL-C, (mg/dl)")
setattr(base$colesterol_no_hdl, "label", "Non-HDL-C, (mg/dl)")
setattr(base$trigliceridos, "label", "Triglycerides, (mg/dl)")
setattr(base$hb, "label", "Hemoglobin, (mg/dl)")
setattr(base$ht, "label", "Hematocrit, (mg/dl)")
setattr(base$glucosa, "label", "Glucose, (mg/dl)")
setattr(base$hba1c, "label", "HbA1c, (mg/dl)")
setattr(base$k, "label", "Potassium, (mg/dl)")
setattr(base$na, "label", "Sodium, (mg/dl)")
setattr(base$TFG_CKD_EPI, "label", "eGFR-CKD-EPI, (ml/min/1.73 m²)")
setattr(base$globorisk_officina, "label", "Office CVD-Globorisk, (%)")
setattr(base$globorisk_officina_elevado, "label", "Increased CVD-Risk, (%)")
setattr(base$globorisk_labs, "label", "Laboratory CVD-Globorisk, (%)")
setattr(base$riesgo_cardiovascular, "label", "CVD-SCORE, (%)")

setattr(base$descontrol_gluc, "label", "A1C ≥9.0, (%)")
setattr(base$hipercol, "label", "Hypercholesterolemia, (%)")
setattr(base$ldl_alto, "label", "LDL-C ≥100 mg/dl, (%)")
setattr(base$hipohdl, "label", "Hypoalphaproteinemia, (%)")
setattr(base$hipertrig, "label", "Hypertriglyceridemia, (%)")

setattr(base$hiperglu, "label", "Hyperglucemia, (%)")
setattr(base$CAT_CKD, "label", "CKD Stages, (%)")
setattr(base$esquema_tratamiento_farmacologico, "label", "Farmacological Treatment, (%)")
setattr(base$tipo_tratamiento, "label", "Type of Treatment, (%)")

setattr(base$Microalbuminuria_NEAV, "label", "ACR >30 mg/g, (%)")
setattr(base$ERC_NEAV, "label", "eFGR <60 ml/min/1.73 m2SC, (%)")
setattr(base$HVI_CREC_CARD_NEAV, "label", "Left ventricular hypertrophy, (%)")
setattr(base$RETINO_NEAV, "label", "Hypertensive retinopathy, (%)")
setattr(base$INDICE_TO.BRA_NEAV, "label", "Ankle-Brachial Index <0.9, (%)")
setattr(base$PRESION_PULSO_60_NEAV, "label", "Pulse pressure ≥60 mmHg, (%)")

setattr(base$vacuna_covid_19, "label", "COVID-19 Vaccine Scheme, (%)")
setattr(base$cuanto_ejercicio, "label", "Time of Excercise, (%)")

setattr(base$disnea, "label", "Dyspnea, (%)")
setattr(base$cefalea, "label", "Headache, (%)")
setattr(base$mareo, "label", "Dizziness, (%)")
setattr(base$ronquidos, "label", "Snoring, (%)")
setattr(base$angina, "label", "Angina, (%)")
setattr(base$palpitaciones, "label", "Palpitations, (%)")
setattr(base$edema, "label", "Edema, (%)")
setattr(base$sincope, "label", "Syncope, (%)")
setattr(base$somnolencia_diurna, "label", "Daytime sleepiness, (%)")

setattr(base$calcioantagonista, "label", "Type of calcium antagonists, (%)")
setattr(base$ieca, "label", "Type of ACEIs, (%)")
setattr(base$ara, "label", "Type of ARBs, (%)")
setattr(base$tiazidico, "label", "Type of thiazide diuretics, (%)")
setattr(base$antagonista_aldosterona, "label", "Type of aldosterone antagonists, (%)")
setattr(base$asa, "label", "Type of loop diuretics, (%)")
setattr(base$betabloqueador, "label", "Type of beta−blockers, (%)")
setattr(base$estatina, "label", "Type of statin, (%)")
setattr(base$ezetimibe, "label", "Type of ezetimibe, (%)")
setattr(base$fibrato, "label", "Type of fibrates, (%)")
setattr(base$edad_cat, "label", "Groups of age, (%)")

setattr(base$ansioso_ultimas_semanas, "label", "Anxiety last month, (%)")
setattr(base$triste_ultimo_mes, "label", "Sadness last month, (%)")
setattr(base$cintura_rec, "label", "Abdominal obesity, (%)")

setattr(base$restriccion_calorica, "label", "Caloric restriction, (%)")
setattr(base$restriccion_salina, "label", "Saline restriction, (%)")
setattr(base$restriccion_proteica, "label", "Proteic restriction, (%)")

setattr(base$dislipidemia, "label", "Dyslipidemia, (%)")
setattr(base$VAT_METS, "label", "Abdominal Obesity [METS-VF], (%)")


#####Caracteristicas Descriptivas Estratificadas por Control Hipertensivo (Tabla 1)#####

base %>% 
  dplyr::select(edad,edad_cat,sexo_rec,anios_educacion,origen_etnico_rec,servicio_medico,cual_servicio_medico_REC, #1
                diabetes_NEAV,cardiopatia_isquemica,enfermedad_vascular_cerebral,enfermedad_renal_cronica,prueba_positiva_covid_19,vacuna_covid_19,has_embarazo, #2
                disnea,cefalea,mareo,ronquidos,angina,palpitaciones,edema,sincope,somnolencia_diurna, #3
                fuma,consume_alcohol,cuanto_ejercicio,restriccion_calorica,restriccion_proteica,restriccion_salina, #4
                hipertenso,ultima_vez_presion_arterial,presion_arterial_en_casa,lo_entrenaron,ansioso_ultimas_semanas,triste_ultimo_mes, #5
                imc,cintura,cintura_rec,VAT_METS,promedio_sistolica,promedio_diastolica,promedio_pulso,  #7
                hb,creatinina,TFG_CKD_EPI,glucosa,hba1c,colesterol,colesterol_ldl,colesterol_hdl,trigliceridos, #8
                CAT_CKD,globorisk_officina)%>% 
  tbl_summary(missing = "ifany")%>%
  bold_labels()%>%
  as_flex_table()%>%
  flextable::save_as_docx(path="Submission/Version 2/Table_1.docx")

#####Evaluación de Tratamientos Farmacologicos (Figura 2)####

#Esquema de Tratamiento

Figure1A<-ggplot(base, aes(x= esquema_tratamiento_farmacologico,
                           fill=esquema_tratamiento_farmacologico)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),color="black") + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= ((..count..)/sum(..count..))), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="esquema_tratamiento_farmacologico") +
  scale_y_continuous(labels = scales::percent,limits = c(0,0.75))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("")+
  ylab("Percentage, (%)")+
  scale_fill_grey(start = 0.2,end = 0.8)+
  theme_minimal()+
  labs(fill="",
       title ="Regime of Treatment",
       subtitle = "",
       caption = "")+
  guides(fill="none")


#Tipo de Tratamiento

Figure1B<-ggplot(base, aes(x= factor(tipo_tratamiento),
                           fill=factor(tipo_tratamiento))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),color="black") + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= ((..count..)/sum(..count..))), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="tipo_tratamiento") +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.75))+
  xlab("")+
  ylab("Percentage, (%)")+
  scale_fill_grey(start = 0.2,end = 0.8)+
  theme_minimal()+
  labs(fill="",
       title ="Type of Treatment",
       subtitle = "",
       caption = "")+
  guides(fill="none")

#Tipo de Medicamento

Figure1C<-base %>% 
  dplyr::select(calcioantagonistas,iecas,aras,tiazidicos,antagonistas_aldosterona,asas,betabloqueadores,
                prazocin,terazocina,tamsulosina,hidralazina,alfametildopa)%>%
  dplyr::summarise(dplyr::across(everything(), list(sum),na.rm=T))%>%
  tidyr::gather(group, value)%>%
  mutate(perc.num=value / nrow(base)*100,
         perc.lab = paste(paste0("",round(value / nrow(base)*100,2),"%","")))%>%
  ggplot(aes(reorder(group, -perc.num), perc.num,fill=group)) +
  geom_col(color="black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()+
  scale_x_discrete(labels= c("ARBs",
                             "Thiazide Diuretics",
                             "ACEIs",
                             "Calcium antagonists",
                             "Beta-blockers",
                             "Loop diuretics",
                             "Aldosterone Antagonists",
                             "Tamsulosin",
                             "Prazocin",
                             "Hydralazine",
                             "Alphamethyldopa",
                             "Terazocine"))+
  xlab("")+
  ylab("Percentage, (%)")+
  theme_minimal()+
  labs(fill="",
       title ="Type of Antihypertensive",
       subtitle = "",
       caption = "")+
  theme(axis.text.y = element_text(size = 13),title = element_text(size = 10))+
  geom_text(aes(label=perc.lab), vjust = 0.5,hjust = -0.2, color="black", size=4)+
  scale_y_continuous(limits = c(0, 110))+
  guides(fill="none")+
  scale_fill_grey(start = 0.2,end = 0.8)

#Esquema de Tratamiento Adicionales

Figure1D<-base %>% 
  dplyr::select(estatinas,ezetimibes,fibratos,aspirina)%>%
  dplyr::summarise(dplyr::across(everything(), list(sum),na.rm=T))%>%
  tidyr::gather(group, value)%>%
  mutate(perc.num=value / nrow(base)*100,
         perc.lab = paste(paste0("",round(value / nrow(base)*100,2),"%","")))%>%
  ggplot(aes(reorder(group, -perc.num), perc.num,fill=group)) +
  geom_col(color="black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()+
  scale_x_discrete(labels= c("Aspirin",
                             "Statins",
                             "Fibrates",
                             "Ezetimibe"))+
  ylab("Percentage, (%)")+
  xlab("")+
  theme_minimal()+
  scale_fill_lancet()+
  labs(fill="",
       title ="Additional Pharmacological Treatment",
       subtitle = "",
       caption = "")+
  theme(axis.text.y = element_text(size = 13),title = element_text(size = 10))+
  geom_text(aes(label=perc.lab), vjust = 0.5,hjust = -0.2, color="black", size=4)+
  scale_y_continuous(limits = c(0, 110))+
  guides(fill="none")+
  scale_fill_grey(start = 0.2,end = 0.8)

Figure1<-ggarrange(Figure1A,Figure1B,Figure1C,Figure1D,ncol = 2,nrow = 2,labels = LETTERS[1:4])

ggsave(Figure1,
       filename = "Submission/Version 2/Figure2.png", 
       bg="white",
       width = 28, 
       height = 20,
       units=c("cm"),
       dpi = 450,
       limitsize = FALSE)


#####Evaluación de la Descontrol Hipertensivo (Figura 3)#####

#AHA 
ncas <- table(base$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.1.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

ncas <- table(base$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.1.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.1.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

ncas <- table(base$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.1.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)


Fig1.df.1<-rbind(Fig1.df.1.1,Fig1.df.1.2,Fig1.df.1.3,Fig1.df.1.4)
Fig1.df.1$group<-c(1:4)
Fig1.df.1$group<-factor(Fig1.df.1$group,
                        labels = rep(c("Controlled",
                                       "Uncontrolled"),2))
Fig1.df.1$subtype<-c(1:4)
Fig1.df.1$subtype<-factor(Fig1.df.1$subtype,
                          labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                     "ACC/AHA 2017\n(<130/80 mmHg)",
                                     "ESC/ESH 2018\n(<140/90 mmHg)",
                                     "ESC/ESH 2018\n(<140/90 mmHg)"))


#Estratificacion por Sexo
#Hombre
#AHA 
ncas <- table(base[base$sexo_rec=="Male",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$sexo_rec=="Male",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.2.1.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

ncas <- table(base[base$sexo_rec=="Male",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$sexo_rec=="Male",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.2.1.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$sexo_rec=="Male",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$sexo_rec=="Male",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.2.1.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

ncas <- table(base[base$sexo_rec=="Male",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$sexo_rec=="Male",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.2.1.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)


Fig1.df.2.1<-rbind(Fig1.df.2.1.1,Fig1.df.2.1.2,Fig1.df.2.1.3,Fig1.df.2.1.4)
Fig1.df.2.1$group<-c(1:4)
Fig1.df.2.1$group<-factor(Fig1.df.2.1$group,
                        labels = rep(c("Controlled",
                                       "Uncontrolled"),2))
Fig1.df.2.1$subtype<-c(1:4)
Fig1.df.2.1$subtype<-factor(Fig1.df.2.1$subtype,
                          labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                     "ACC/AHA 2017\n(<130/80 mmHg)",
                                     "ESC/ESH 2018\n(<140/90 mmHg)",
                                     "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.2.1$class<-c("Men")

#Mujeres
#AHA 
ncas <- table(base[base$sexo_rec=="Female",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$sexo_rec=="Female",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.2.2.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$sexo_rec=="Female",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$sexo_rec=="Female",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.2.2.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$sexo_rec=="Female",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$sexo_rec=="Female",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.2.2.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$sexo_rec=="Female",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$sexo_rec=="Female",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.2.2.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Fig1.df.2.2<-rbind(Fig1.df.2.2.1,Fig1.df.2.2.2,Fig1.df.2.2.3,Fig1.df.2.2.4)
Fig1.df.2.2$group<-c(1:4)
Fig1.df.2.2$group<-factor(Fig1.df.2.2$group,
                          labels = rep(c("Controlled",
                                         "Uncontrolled"),2))
Fig1.df.2.2$subtype<-c(1:4)
Fig1.df.2.2$subtype<-factor(Fig1.df.2.2$subtype,
                            labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.2.2$class<-c("Women")
Fig1.df.2<-rbind(Fig1.df.2.1,Fig1.df.2.2)

#Estratificacion por Edad
#<45 Años
#AHA 
ncas <- table(base[base$edad_cat=="<45",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$edad_cat=="<45",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.1.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$edad_cat=="<45",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$edad_cat=="<45",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.1.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$edad_cat=="<45",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$edad_cat=="<45",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.1.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$edad_cat=="<45",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$edad_cat=="<45",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.1.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Fig1.df.3.1<-rbind(Fig1.df.3.1.1,Fig1.df.3.1.2,Fig1.df.3.1.3,Fig1.df.3.1.4)
Fig1.df.3.1$group<-c(1:4)
Fig1.df.3.1$group<-factor(Fig1.df.3.1$group,
                          labels = rep(c("Controlled",
                                         "Uncontrolled"),2))
Fig1.df.3.1$subtype<-c(1:4)
Fig1.df.3.1$subtype<-factor(Fig1.df.3.1$subtype,
                            labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.3.1$class<-c("<45 years")

#45-65
#AHA 
ncas <- table(base[base$edad_cat=="45-65",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$edad_cat=="45-65",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.2.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$edad_cat=="45-65",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$edad_cat=="45-65",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.2.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$edad_cat=="45-65",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$edad_cat=="45-65",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.2.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$edad_cat=="45-65",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$edad_cat=="45-65",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.2.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Fig1.df.3.2<-rbind(Fig1.df.3.2.1,Fig1.df.3.2.2,Fig1.df.3.2.3,Fig1.df.3.2.4)
Fig1.df.3.2$group<-c(1:4)
Fig1.df.3.2$group<-factor(Fig1.df.3.2$group,
                          labels = rep(c("Controlled",
                                         "Uncontrolled"),2))
Fig1.df.3.2$subtype<-c(1:4)
Fig1.df.3.2$subtype<-factor(Fig1.df.3.2$subtype,
                            labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.3.2$class<-c("45-65 years")

#>65 Años
#AHA 
ncas <- table(base[base$edad_cat=="≥65",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$edad_cat=="≥65",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.3.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$edad_cat=="≥65",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$edad_cat=="≥65",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.3.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$edad_cat=="≥65",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$edad_cat=="≥65",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.3.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$edad_cat=="≥65",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$edad_cat=="≥65",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.3.3.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Fig1.df.3.3<-rbind(Fig1.df.3.3.1,Fig1.df.3.3.2,Fig1.df.3.3.3,Fig1.df.3.3.4)
Fig1.df.3.3$group<-c(1:4)
Fig1.df.3.3$group<-factor(Fig1.df.3.3$group,
                          labels = rep(c("Controlled",
                                         "Uncontrolled"),2))
Fig1.df.3.3$subtype<-c(1:4)
Fig1.df.3.3$subtype<-factor(Fig1.df.3.3$subtype,
                            labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.3.3$class<-c("≥65 years")
Fig1.df.3<-rbind(Fig1.df.3.1,Fig1.df.3.2,Fig1.df.3.3)
Fig1.df.3$class<-factor(Fig1.df.3$class,levels = c("<45 years","45-65 years","≥65 years"))

#Por Educacion
#0 Años
#AHA 
ncas <- table(base[base$anios_educacion=="0 Years",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$anios_educacion=="0 Years",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.1.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$anios_educacion=="0 Years",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$anios_educacion=="0 Years",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.1.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$anios_educacion=="0 Years",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$anios_educacion=="0 Years",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.1.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$anios_educacion=="0 Years",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$anios_educacion=="0 Years",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.1.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Fig1.df.4.1<-rbind(Fig1.df.4.1.1,Fig1.df.4.1.2,Fig1.df.4.1.3,Fig1.df.4.1.4)
Fig1.df.4.1$group<-c(1:4)
Fig1.df.4.1$group<-factor(Fig1.df.4.1$group,
                          labels = rep(c("Controlled",
                                         "Uncontrolled"),2))
Fig1.df.4.1$subtype<-c(1:4)
Fig1.df.4.1$subtype<-factor(Fig1.df.4.1$subtype,
                            labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.4.1$class<-c("None-Education")


#1-6 Años
#AHA 
ncas <- table(base[base$anios_educacion=="1-6 Years",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$anios_educacion=="1-6 Years",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.2.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$anios_educacion=="1-6 Years",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$anios_educacion=="1-6 Years",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.2.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$anios_educacion=="1-6 Years",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$anios_educacion=="1-6 Years",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.2.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$anios_educacion=="1-6 Years",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$anios_educacion=="1-6 Years",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.2.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Fig1.df.4.2<-rbind(Fig1.df.4.2.1,Fig1.df.4.2.2,Fig1.df.4.2.3,Fig1.df.4.2.4)
Fig1.df.4.2$group<-c(1:4)
Fig1.df.4.2$group<-factor(Fig1.df.4.2$group,
                          labels = rep(c("Controlled",
                                         "Uncontrolled"),2))
Fig1.df.4.2$subtype<-c(1:4)
Fig1.df.4.2$subtype<-factor(Fig1.df.4.2$subtype,
                            labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.4.2$class<-c("Elementary")

#7-12 Años 
#AHA 
ncas <- table(base[base$anios_educacion=="7-12 Years",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$anios_educacion=="7-12 Years",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.3.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$anios_educacion=="7-12 Years",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$anios_educacion=="7-12 Years",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.3.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$anios_educacion=="7-12 Years",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$anios_educacion=="7-12 Years",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.3.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$anios_educacion=="7-12 Years",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$anios_educacion=="7-12 Years",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.3.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Fig1.df.4.3<-rbind(Fig1.df.4.3.1,Fig1.df.4.3.2,Fig1.df.4.3.3,Fig1.df.4.3.4)
Fig1.df.4.3$group<-c(1:4)
Fig1.df.4.3$group<-factor(Fig1.df.4.3$group,
                          labels = rep(c("Controlled",
                                         "Uncontrolled"),2))
Fig1.df.4.3$subtype<-c(1:4)
Fig1.df.4.3$subtype<-factor(Fig1.df.4.3$subtype,
                            labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.4.3$class<-c("High-School")

#≥12 Años Años
#AHA 
ncas <- table(base[base$anios_educacion=="≥12 Years",]$TA_CONTROL_AHA==1)[2]; npop <- sum(!is.na(base[base$anios_educacion=="≥12 Years",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.4.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$anios_educacion=="≥12 Years",]$TA_CONTROL_AHA==0)[2]; npop <- sum(!is.na(base[base$anios_educacion=="≥12 Years",]$TA_CONTROL_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.4.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#ESC
ncas <- table(base[base$anios_educacion=="≥12 Years",]$TA_CONTROL_ESC==1)[2]; npop <- sum(!is.na(base[base$anios_educacion=="≥12 Years",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.4.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)

ncas <- table(base[base$anios_educacion=="≥12 Years",]$TA_CONTROL_ESC==0)[2]; npop <- sum(!is.na(base[base$anios_educacion=="≥12 Years",]$TA_CONTROL_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig1.df.4.4.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Fig1.df.4.4<-rbind(Fig1.df.4.4.1,Fig1.df.4.4.2,Fig1.df.4.4.3,Fig1.df.4.4.4)
Fig1.df.4.4$group<-c(1:4)
Fig1.df.4.4$group<-factor(Fig1.df.4.4$group,
                          labels = rep(c("Controlled",
                                         "Uncontrolled"),2))
Fig1.df.4.4$subtype<-c(1:4)
Fig1.df.4.4$subtype<-factor(Fig1.df.4.4$subtype,
                            labels = c("ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ACC/AHA 2017\n(<130/80 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)",
                                       "ESC/ESH 2018\n(<140/90 mmHg)"))
Fig1.df.4.4$class<-c("College or Higher")

Fig1.df.4<-rbind(Fig1.df.4.1,Fig1.df.4.2,Fig1.df.4.3,Fig1.df.4.4)
Fig1.df.4$class<-factor(Fig1.df.4$class,levels = c("None-Education","Elementary","High-School","College or Higher"))


Fig1.A<-ggplot(Fig1.df.1,aes(subtype,est,fill=group))+
  geom_bar(position = "fill",stat='identity')+
  scale_y_continuous(labels = scales::percent)+
  geom_text(
    aes(label = paste0(round(est,1),"%","\n","(",round(lower,1),"-",round(upper,1),")"), y = est/100),
    position =  position_fill(vjust = 0.5),
    vjust = 0.5,size=5,color="white")+
  theme_minimal()+
  xlab("")+
  ylab("Prevalence, (%)")+
  labs(fill="")+
  scale_fill_grey(start = 0.2,end = 0.8)+
  theme(legend.position = "top")+
  ggtitle("Overall Sample")


Fig1.B<-ggplot(Fig1.df.2,aes(class,est,fill=group))+
  geom_bar(position = "fill",stat='identity')+
  scale_y_continuous(labels = scales::percent)+
  geom_text(
    aes(label = paste0(round(est,1),"%","\n","(",round(lower,1),"-",round(upper,1),")"), y = est/100),
    position =  position_fill(vjust = 0.5),
    vjust = 0.5,size=3,color="white")+
  theme_minimal()+
  xlab("")+
  ylab("Prevalence, (%)")+
  labs(fill="")+
  scale_fill_grey(start = 0.2,end = 0.8)+
  theme(legend.position = "top")+
  facet_wrap(~subtype)+
  ggtitle("Sex")

Fig1.C<-ggplot(Fig1.df.3,aes(class,est,fill=group))+
  geom_bar(position = "fill",stat='identity')+
  scale_y_continuous(labels = scales::percent)+
  geom_text(
    aes(label = paste0(round(est,1),"%","\n","(",round(lower,1),"-",round(upper,1),")"), y = est/100),
    position =  position_fill(vjust = 0.5),
    vjust = 0.5,size=3,color="white")+
  theme_minimal()+
  xlab("")+
  ylab("Prevalence, (%)")+
  labs(fill="")+
  scale_fill_grey(start = 0.2,end = 0.8)+
  theme(legend.position = "top")+
  facet_wrap(~subtype)+
  ggtitle("Age Categories")

Fig1.D<-ggplot(Fig1.df.4,aes(class,est,fill=group))+
  geom_bar(position = "fill",stat='identity')+
  scale_y_continuous(labels = scales::percent)+
  geom_text(
    aes(label = paste0(round(est,1),"%","\n","(",round(lower,1),"-",round(upper,1),")"), y = est/100),
    position =  position_fill(vjust = 0.5),
    vjust = 0.5,size=3,color="white")+
  theme_minimal()+
  xlab("")+
  ylab("Prevalence, (%)")+
  labs(fill="")+
  scale_fill_grey(start = 0.2,end = 0.8)+
  theme(legend.position = "top")+
  facet_wrap(~subtype)+
  ggtitle("Educational Attainments")

Fig2<-ggarrange(Fig1.A,ggarrange(Fig1.B,Fig1.C,Fig1.D,common.legend = T,legend = "none",ncol = 1,nrow = 3,labels = LETTERS[2:4]),ncol = 2,nrow = 1,labels = c("A",""),common.legend = T)

ggsave(Fig2,
       filename = "Submission/Version 2/Figure3.png", 
       bg="white",
       width = 40, 
       height = 25,
       units=c("cm"),
       dpi = 450,
       limitsize = FALSE)


#####Cardiometabolic risk factors (Figura 4)#####

#Numero de Cardiometabolic
base$NUM_CARDIOMETS<-as.numeric(base$imc_cat=="Over-weight")+
  as.numeric(base$imc_cat=="Obesity")+
  as.numeric(base$cintura_rec==1)+
  as.numeric(base$hipercol==1)+
  as.numeric(base$hipertrig==1)+
  as.numeric(base$ldl_alto==1)+
  as.numeric(base$hipohdl==1)+
  as.numeric(base$diabetes_NEAV==1)+
  as.numeric(base$METS_IR_AUMENTADO==1)+
  as.numeric(base$globorisk_officina_elevado==1)+
  as.numeric(base$TFG_CKD_EPI<=60)


base$NUM_CARDIOMETS_CAT<-NULL
base$NUM_CARDIOMETS_CAT[base$NUM_CARDIOMETS==0]<-1
base$NUM_CARDIOMETS_CAT[base$NUM_CARDIOMETS>=1 & base$NUM_CARDIOMETS<=2]<-2
base$NUM_CARDIOMETS_CAT[base$NUM_CARDIOMETS>=3 & base$NUM_CARDIOMETS<=4]<-3
base$NUM_CARDIOMETS_CAT[base$NUM_CARDIOMETS>=5 & base$NUM_CARDIOMETS<=6]<-4
base$NUM_CARDIOMETS_CAT[base$NUM_CARDIOMETS>=7]<-5
base$NUM_CARDIOMETS_CAT<-factor(base$NUM_CARDIOMETS_CAT,labels = c("0","1-2","3-4","5-6","≥7"))
  
#Overweight
ncas <- table(base$imc_cat=="Over-weight")[2]; npop <- sum(!is.na(base$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)


#Obesity
ncas <- table(base$imc_cat=="Obesity")[2]; npop <- sum(!is.na(base$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

#Abdominal Obesity
ncas <- table(base$cintura_rec==1)[2]; npop <- sum(!is.na(base$cintura_rec))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)


#Hipercolesterolemia
ncas <- table(base$hipercol==1)[2]; npop <- sum(!is.na(base$hipercol))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#Hipertrigliceridemia
ncas <- table(base$hipertrig==1)[2]; npop <- sum(!is.na(base$hipertrig))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.5<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#LDL ≥100
ncas <- table(base$ldl_alto==1)[2]; npop <- sum(!is.na(base$ldl_alto))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.6<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#HDL <40/50
ncas <- table(base$hipohdl==1)[2]; npop <- sum(!is.na(base$hipohdl))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.7<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)


#Diabetes
ncas <- table(base$diabetes_NEAV==1)[2]; npop <- sum(!is.na(base$diabetes_NEAV))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.8<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#IR
ncas <- table(base$METS_IR_AUMENTADO==1)[2]; npop <- sum(!is.na(base$METS_IR_AUMENTADO))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.9<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)


#CVD ≥10
ncas <- table(base$globorisk_officina_elevado==1)[2]; npop <- sum(!is.na(base$globorisk_officina_elevado))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.10<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)


#eGFR <60
ncas <- table(base$TFG_CKD_EPI<=60)[2]; npop <- sum(!is.na(base$TFG_CKD_EPI))
tmp <- as.matrix(cbind(ncas, npop))
Fig3.df.11<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                           conf.level = 0.95) * 100,2)


Fig3.df<-rbind(Fig3.df.1,Fig3.df.2,Fig3.df.3,Fig3.df.4,Fig3.df.5,Fig3.df.6,Fig3.df.7,Fig3.df.8,Fig3.df.9,Fig3.df.10,Fig3.df.11)
Fig3.df$group<-c("Overweight","Obesity","Abdominal\nObesity","High-Cholesterol",
                   "High-Triglycerides","High-LDL-C","Low-HDL-C","Diabetes",
                   "Insulin\nResistance","High\nCDV-Risk","Low-eGFR")

Fig3.df$group<-factor(Fig3.df$group,levels = c("Abdominal\nObesity",
                                               "High-LDL-C",
                                               "High\nCDV-Risk",
                                               "High-Triglycerides",
                                               "Low-HDL-C",
                                               "Overweight",
                                               "Obesity",
                                               "Diabetes",
                                               "High-Cholesterol",
                                               "Insulin\nResistance",
                                               "Low-eGFR"))



#Chart Pie
Fig3A.df <- base %>% 
  filter(NUM_CARDIOMETS_CAT != "NA") %>% 
  group_by(NUM_CARDIOMETS_CAT) %>% 
  count() %>% 
  mutate(NUM_CARDIOMETS_CAT=factor(NUM_CARDIOMETS_CAT))%>%
  ungroup()%>% 
  arrange(desc(NUM_CARDIOMETS_CAT)) %>%
  mutate(percentage = round(n/sum(n),4)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

Fig3A<-ggplot(data = Fig3A.df, 
       aes(x = "", y = percentage, fill = NUM_CARDIOMETS_CAT))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void()+
  labs(fill="")+
  scale_fill_grey(start = 0.8,end = 0.2)+
  theme(legend.position = "right")+
  ggtitle("")


Fig3B<-ggplot(Fig3.df,aes(x=reorder(group,-est),y=est,fill=group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_minimal()+
  scale_fill_grey(start = 0.2,end = 0.8)+
  xlab("")+
  ylab("Prevalence, (%)")+
  geom_text(
    aes(label = paste0(round(est,1),"%","\n","(",round(lower,1),"-",round(upper,1),")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0,size=5)+
  scale_y_continuous(limits = c(-.1,100))+
  ggtitle("Prevalence of Cardiometabolic Risk Factors")+
  theme(legend.position = "none")+
  theme(axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),
        legend.text= element_text(size=14),
        axis.title.y = element_text(size=14),text = element_text(size=14))+ 
  annotation_custom(
    ggplotGrob(Fig3A), 
    xmin = 13, xmax = 7, ymin = 45, ymax = 110)

ggsave(Fig3B,
       filename = "Submission/Version 2/Figure4.png", 
       bg="white",
       width = 45, 
       height = 20,
       units=c("cm"),
       dpi = 450,
       limitsize = FALSE)

#####Stratification of Cardiometabolic Risk Factors by Hypertension Control (Supplementary Figure 1)######

#ACC/AHA 2017\n(<130/80 mmHg)

Sup.Fig1A.df <- base %>% 
  filter(NUM_CARDIOMETS_CAT != "NA") %>% 
  group_by(NUM_CARDIOMETS_CAT,TA_CONTROL_AHA) %>% 
  count() %>% 
  mutate(NUM_CARDIOMETS_CAT=factor(NUM_CARDIOMETS_CAT))%>%
  group_by(TA_CONTROL_AHA)%>%
  arrange(desc(NUM_CARDIOMETS_CAT)) %>%
  mutate(percentage = round(n/sum(n),4)*100)%>%
  mutate(TA_CONTROL_AHA=factor(TA_CONTROL_AHA))

round(prop.table(table(base$NUM_CARDIOMETS_CAT,base$TA_CONTROL_AHA),2)*100,2)

Sup.Fig1A.df$TA_CONTROL_AHA<-factor(Sup.Fig1A.df$TA_CONTROL_AHA,labels = c("Uncontrolled","Controlled"))

#ESC/ESH 2018\n(<140/90 mmHg)

Sup.Fig1B.df <- base %>% 
  filter(NUM_CARDIOMETS_CAT != "NA") %>% 
  group_by(NUM_CARDIOMETS_CAT,TA_CONTROL_ESC) %>% 
  count() %>% 
  mutate(NUM_CARDIOMETS_CAT=factor(NUM_CARDIOMETS_CAT))%>%
  group_by(TA_CONTROL_ESC)%>%
  arrange(desc(NUM_CARDIOMETS_CAT)) %>%
  mutate(percentage = round(n/sum(n),4)*100,
         lab.pos = cumsum(percentage)-.5*percentage)%>%
  mutate(TA_CONTROL_ESC=factor(TA_CONTROL_ESC))

Sup.Fig1B.df$TA_CONTROL_ESC<-factor(Sup.Fig1B.df$TA_CONTROL_ESC,labels = c("Uncontrolled","Controlled"))

#Stratification by AHA/ACC
#Controlled
#Overweight
ncas <- table(base[base$TA_CONTROL_AHA==1,]$imc_cat=="Over-weight")[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)


#Obesity
ncas <- table(base[base$TA_CONTROL_AHA==1,]$imc_cat=="Obesity")[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#Abdominal Obesity
ncas <- table(base[base$TA_CONTROL_AHA==1,]$cintura_rec==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$cintura_rec))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)


#Hipercolesterolemia
ncas <- table(base[base$TA_CONTROL_AHA==1,]$hipercol==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$hipercol))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#Hipertrigliceridemia
ncas <- table(base[base$TA_CONTROL_AHA==1,]$hipertrig==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$hipertrig))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.5<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#LDL ≥100
ncas <- table(base[base$TA_CONTROL_AHA==1,]$ldl_alto==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$ldl_alto))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.6<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#HDL <40/50
ncas <- table(base[base$TA_CONTROL_AHA==1,]$hipohdl==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$hipohdl))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.7<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)


#Diabetes
ncas <- table(base[base$TA_CONTROL_AHA==1,]$diabetes_NEAV==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$diabetes_NEAV))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.8<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)

#IR
ncas <- table(base[base$TA_CONTROL_AHA==1,]$METS_IR_AUMENTADO==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$METS_IR_AUMENTADO))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.9<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                          conf.level = 0.95) * 100,2)


#CVD ≥10
ncas <- table(base[base$TA_CONTROL_AHA==1,]$globorisk_officina_elevado==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$globorisk_officina_elevado))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.10<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                           conf.level = 0.95) * 100,2)


#eGFR <60
ncas <- table(base[base$TA_CONTROL_AHA==1,]$TFG_CKD_EPI<=60)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==1,]$TFG_CKD_EPI))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.11<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                           conf.level = 0.95) * 100,2)


Sup.Fig2.1.df<-rbind(Sup.Fig2.1.1,Sup.Fig2.1.2,Sup.Fig2.1.3,Sup.Fig2.1.4,Sup.Fig2.1.5,Sup.Fig2.1.6,Sup.Fig2.1.7,Sup.Fig2.1.8,Sup.Fig2.1.9,Sup.Fig2.1.10,Sup.Fig2.1.11)
Sup.Fig2.1.df$group<-c("Overweight","Obesity","Abdominal\nObesity","High-Cholesterol",
                 "High-Triglycerides","High-LDL-C","Low-HDL-C","Diabetes",
                 "Insulin\nResistance","High\nCDV-Risk","Low-eGFR")

Sup.Fig2.1.df$group<-factor(Sup.Fig2.1.df$group,levels = c("Abdominal\nObesity",
                                               "High-LDL-C",
                                               "High\nCDV-Risk",
                                               "High-Triglycerides",
                                               "Low-HDL-C",
                                               "Overweight",
                                               "Obesity",
                                               "Diabetes",
                                               "High-Cholesterol",
                                               "Insulin\nResistance",
                                               "Low-eGFR"))

Sup.Fig2.1.df$class<-c("Controlled")

#Uncontrolled
#Overweight
ncas <- table(base[base$TA_CONTROL_AHA==0,]$imc_cat=="Over-weight")[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Obesity
ncas <- table(base[base$TA_CONTROL_AHA==0,]$imc_cat=="Obesity")[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#Abdominal Obesity
ncas <- table(base[base$TA_CONTROL_AHA==0,]$cintura_rec==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$cintura_rec))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Hipercolesterolemia
ncas <- table(base[base$TA_CONTROL_AHA==0,]$hipercol==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$hipercol))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#Hipertrigliceridemia
ncas <- table(base[base$TA_CONTROL_AHA==0,]$hipertrig==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$hipertrig))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.5<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#LDL ≥100
ncas <- table(base[base$TA_CONTROL_AHA==0,]$ldl_alto==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$ldl_alto))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.6<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#HDL <40/50
ncas <- table(base[base$TA_CONTROL_AHA==0,]$hipohdl==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$hipohdl))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.7<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Diabetes
ncas <- table(base[base$TA_CONTROL_AHA==0,]$diabetes_NEAV==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$diabetes_NEAV))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.8<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#IR
ncas <- table(base[base$TA_CONTROL_AHA==0,]$METS_IR_AUMENTADO==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$METS_IR_AUMENTADO))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.9<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#CVD ≥10
ncas <- table(base[base$TA_CONTROL_AHA==0,]$globorisk_officina_elevado==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$globorisk_officina_elevado))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.10<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#eGFR <60
ncas <- table(base[base$TA_CONTROL_AHA==0,]$TFG_CKD_EPI<=60)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_AHA==0,]$TFG_CKD_EPI))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.11<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Sup.Fig2.2.df<-rbind(Sup.Fig2.2.1,Sup.Fig2.2.2,Sup.Fig2.2.3,Sup.Fig2.2.4,Sup.Fig2.2.5,Sup.Fig2.2.6,Sup.Fig2.2.7,Sup.Fig2.2.8,Sup.Fig2.2.9,Sup.Fig2.2.10,Sup.Fig2.2.11)
Sup.Fig2.2.df$group<-c("Overweight","Obesity","Abdominal\nObesity","High-Cholesterol",
                       "High-Triglycerides","High-LDL-C","Low-HDL-C","Diabetes",
                       "Insulin\nResistance","High\nCDV-Risk","Low-eGFR")

Sup.Fig2.2.df$group<-factor(Sup.Fig2.2.df$group,levels = c("Abdominal\nObesity",
                                                           "High-LDL-C",
                                                           "High\nCDV-Risk",
                                                           "High-Triglycerides",
                                                           "Low-HDL-C",
                                                           "Overweight",
                                                           "Obesity",
                                                           "Diabetes",
                                                           "High-Cholesterol",
                                                           "Insulin\nResistance",
                                                           "Low-eGFR"))

Sup.Fig2.2.df$class<-c("Uncontrolled")
Sup.Fig2.df.C<-rbind(Sup.Fig2.1.df,Sup.Fig2.2.df)

#ESC/ESH 
#Controlled
#Overweight
ncas <- table(base[base$TA_CONTROL_ESC==1,]$imc_cat=="Over-weight")[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Obesity
ncas <- table(base[base$TA_CONTROL_ESC==1,]$imc_cat=="Obesity")[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#Abdominal Obesity
ncas <- table(base[base$TA_CONTROL_ESC==1,]$cintura_rec==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$cintura_rec))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Hipercolesterolemia
ncas <- table(base[base$TA_CONTROL_ESC==1,]$hipercol==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$hipercol))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#Hipertrigliceridemia
ncas <- table(base[base$TA_CONTROL_ESC==1,]$hipertrig==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$hipertrig))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.5<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#LDL ≥100
ncas <- table(base[base$TA_CONTROL_ESC==1,]$ldl_alto==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$ldl_alto))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.6<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#HDL <40/50
ncas <- table(base[base$TA_CONTROL_ESC==1,]$hipohdl==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$hipohdl))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.7<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Diabetes
ncas <- table(base[base$TA_CONTROL_ESC==1,]$diabetes_NEAV==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$diabetes_NEAV))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.8<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#IR
ncas <- table(base[base$TA_CONTROL_ESC==1,]$METS_IR_AUMENTADO==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$METS_IR_AUMENTADO))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.9<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#CVD ≥10
ncas <- table(base[base$TA_CONTROL_ESC==1,]$globorisk_officina_elevado==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$globorisk_officina_elevado))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.10<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#eGFR <60
ncas <- table(base[base$TA_CONTROL_ESC==1,]$TFG_CKD_EPI<=60)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==1,]$TFG_CKD_EPI))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.1.11<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Sup.Fig2.1.df<-rbind(Sup.Fig2.1.1,Sup.Fig2.1.2,Sup.Fig2.1.3,Sup.Fig2.1.4,Sup.Fig2.1.5,Sup.Fig2.1.6,Sup.Fig2.1.7,Sup.Fig2.1.8,Sup.Fig2.1.9,Sup.Fig2.1.10,Sup.Fig2.1.11)
Sup.Fig2.1.df$group<-c("Overweight","Obesity","Abdominal\nObesity","High-Cholesterol",
                       "High-Triglycerides","High-LDL-C","Low-HDL-C","Diabetes",
                       "Insulin\nResistance","High\nCDV-Risk","Low-eGFR")

Sup.Fig2.1.df$group<-factor(Sup.Fig2.1.df$group,levels = c("Abdominal\nObesity",
                                                           "High-LDL-C",
                                                           "High\nCDV-Risk",
                                                           "High-Triglycerides",
                                                           "Low-HDL-C",
                                                           "Overweight",
                                                           "Obesity",
                                                           "Diabetes",
                                                           "High-Cholesterol",
                                                           "Insulin\nResistance",
                                                           "Low-eGFR"))

Sup.Fig2.1.df$class<-c("Controlled")

#Uncontrolled
#Overweight
ncas <- table(base[base$TA_CONTROL_ESC==0,]$imc_cat=="Over-weight")[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Obesity
ncas <- table(base[base$TA_CONTROL_ESC==0,]$imc_cat=="Obesity")[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$imc_cat))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#Abdominal Obesity
ncas <- table(base[base$TA_CONTROL_ESC==0,]$cintura_rec==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$cintura_rec))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Hipercolesterolemia
ncas <- table(base[base$TA_CONTROL_ESC==0,]$hipercol==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$hipercol))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#Hipertrigliceridemia
ncas <- table(base[base$TA_CONTROL_ESC==0,]$hipertrig==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$hipertrig))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.5<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#LDL ≥100
ncas <- table(base[base$TA_CONTROL_ESC==0,]$ldl_alto==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$ldl_alto))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.6<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#HDL <40/50
ncas <- table(base[base$TA_CONTROL_ESC==0,]$hipohdl==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$hipohdl))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.7<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#Diabetes
ncas <- table(base[base$TA_CONTROL_ESC==0,]$diabetes_NEAV==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$diabetes_NEAV))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.8<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)

#IR
ncas <- table(base[base$TA_CONTROL_ESC==0,]$METS_IR_AUMENTADO==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$METS_IR_AUMENTADO))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.9<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                             conf.level = 0.95) * 100,2)


#CVD ≥10
ncas <- table(base[base$TA_CONTROL_ESC==0,]$globorisk_officina_elevado==1)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$globorisk_officina_elevado))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.10<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


#eGFR <60
ncas <- table(base[base$TA_CONTROL_ESC==0,]$TFG_CKD_EPI<=60)[2]; npop <- sum(!is.na(base[base$TA_CONTROL_ESC==0,]$TFG_CKD_EPI))
tmp <- as.matrix(cbind(ncas, npop))
Sup.Fig2.2.11<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                              conf.level = 0.95) * 100,2)


Sup.Fig2.2.df<-rbind(Sup.Fig2.2.1,Sup.Fig2.2.2,Sup.Fig2.2.3,Sup.Fig2.2.4,Sup.Fig2.2.5,Sup.Fig2.2.6,Sup.Fig2.2.7,Sup.Fig2.2.8,Sup.Fig2.2.9,Sup.Fig2.2.10,Sup.Fig2.2.11)
Sup.Fig2.2.df$group<-c("Overweight","Obesity","Abdominal\nObesity","High-Cholesterol",
                       "High-Triglycerides","High-LDL-C","Low-HDL-C","Diabetes",
                       "Insulin\nResistance","High\nCDV-Risk","Low-eGFR")

Sup.Fig2.2.df$group<-factor(Sup.Fig2.2.df$group,levels = c("Abdominal\nObesity",
                                                           "High-LDL-C",
                                                           "High\nCDV-Risk",
                                                           "High-Triglycerides",
                                                           "Low-HDL-C",
                                                           "Overweight",
                                                           "Obesity",
                                                           "Diabetes",
                                                           "High-Cholesterol",
                                                           "Insulin\nResistance",
                                                           "Low-eGFR"))

Sup.Fig2.2.df$class<-c("Uncontrolled")
Sup.Fig2.df.D<-rbind(Sup.Fig2.1.df,Sup.Fig2.2.df)


#Figure Merging

Sup.Fig1A<-ggplot(Sup.Fig1A.df,aes(TA_CONTROL_AHA,percentage,fill=NUM_CARDIOMETS_CAT))+
  geom_bar(position = "fill",stat='identity')+
  scale_y_continuous(labels = scales::percent)+
  geom_text(
    aes(label = percentage, y = percentage),
    position =  position_fill(vjust = 0.5),
    vjust = 0.5,size=5,color="white")+
  theme_minimal()+
  xlab("")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_grey(start = 0.8,end = 0.2)+
  theme(legend.position = "top")+
  ggtitle("ACC/AHA 2017\n(<130/80 mmHg)")


Sup.Fig1B<-ggplot(Sup.Fig1B.df,aes(TA_CONTROL_ESC,percentage,fill=NUM_CARDIOMETS_CAT))+
  geom_bar(position = "fill",stat='identity')+
  scale_y_continuous(labels = scales::percent)+
  geom_text(
    aes(label = percentage, y = percentage),
    position =  position_fill(vjust = 0.5),
    vjust = 0.5,size=5,color="white")+
  theme_minimal()+
  xlab("")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_grey(start = 0.8,end = 0.2)+
  theme(legend.position = "top")+
  ggtitle("ESC/ESH 2018\n(<140/90 mmHg)")

Sup.Fig1.C<-ggplot(Sup.Fig2.df.C,aes(x=reorder(group,-est),y=est,fill=class)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_minimal()+
  scale_fill_grey(start = 0.2,end = 0.8)+
  xlab("")+
  ylab("Prevalence, (%)")+
  geom_text(
    aes(label = paste0(round(est,1),"%"," ","(",round(lower,1),"-",round(upper,1),")"), y = est + 9.0),
    position = position_dodge(1),size=5)+
  scale_y_continuous(limits = c(-.1,100))+
  ggtitle("")+
  theme(legend.position = "top")+
  theme(axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),
        legend.text= element_text(size=14),
        axis.title.y = element_text(size=14),text = element_text(size=14))+
  coord_flip()+
  labs(fill="")

Sup.Fig1.D<-ggplot(Sup.Fig2.df.D,aes(x=reorder(group,-est),y=est,fill=class)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_minimal()+
  scale_fill_grey(start = 0.2,end = 0.8)+
  xlab("")+
  ylab("Prevalence, (%)")+
  geom_text(
    aes(label = paste0(round(est,1),"%"," ","(",round(lower,1),"-",round(upper,1),")"), y = est + 9.0),
    position = position_dodge(1),size=5)+
  scale_y_continuous(limits = c(-.1,100))+
  ggtitle("")+
  theme(legend.position = "top")+
  theme(axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),
        legend.text= element_text(size=14),
        axis.title.y = element_text(size=14),text = element_text(size=14))+
  coord_flip()+
  labs(fill="")


Sup.Fig1<-ggarrange(Sup.Fig1A,Sup.Fig1B,Sup.Fig1.C,Sup.Fig1.D,ncol = 2,nrow = 2,labels = LETTERS[1:4])

ggsave(Sup.Fig1,
       filename = "Submission/Version 2/Supplementary_Figure_1.png", 
       bg="white",
       width = 55, 
       height = 45,
       units=c("cm"),
       dpi = 450,
       limitsize = FALSE)


#####Evaluacion de Sintomas y Farmacos Estratificados por Control Hipertensivo (Tabla Suplementaria 1)#####

base %>% 
  dplyr::select(calcioantagonistas,calcioantagonista,
                iecas,ieca,
                aras,ara,
                tiazidicos,tiazidico,
                antagonistas_aldosterona,antagonista_aldosterona,
                asas,asa,
                betabloqueadores,betabloqueador,
                prazocin,tamsulosina,hidralazina,
                aspirina,
                estatinas,estatina,
                ezetimibes,ezetimibe,
                fibratos,fibrato)%>%
  tbl_summary(missing = "no")%>%
  bold_labels()%>%
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "Unknown",
                   label))%>%
  as_flex_table()%>%
  flextable::save_as_docx(path="Submission/Version 2/Supplementary_Table_2.docx")

#####Missing Data Evaluation (Supplementary Figure 1)#####

base.sf1<-base.original
base.sf1<-lapply(base.sf1, function(x) { attributes(x) <- NULL; x })
base.sf1<-as.data.frame(base.sf1)

base.sf1$talla[base.sf1$talla<0.4]<-NA
base.sf1$talla[base.sf1$talla>2.0]<-NA
base.sf1$imc[base.sf1$imc<5]<-NA
base.sf1$imc[base.sf1$imc>60]<-NA
base.sf1$cintura[base.sf1$cintura<10]<-NA
base.sf1$cintura[base.sf1$cintura>170]<-NA
base.sf1$creatinina[base.sf1$creatinina<=0]<-NA
base.sf1$creatinina[base.sf1$creatinina>20]<-NA
base.sf1$creatinina[base.sf1$creatinina<=0]<-NA
base.sf1$creatinina[base.sf1$creatinina>100]<-NA
base.sf1$colesterol[base.sf1$colesterol<=10]<-NA
base.sf1$colesterol_ldl[base.sf1$colesterol_ldl<=0]<-NA
base.sf1$colesterol_hdl[base.sf1$colesterol_hdl<=0]<-NA
base.sf1$trigliceridos[base.sf1$trigliceridos<=0]<-NA
base.sf1$hb[base.sf1$hb<=0]<-NA
base.sf1$hb[base.sf1$hb>=30]<-NA
base.sf1$hba1c[base.sf1$hba1c<=0]<-NA
base.sf1$hba1c[base.sf1$hba1c>=30]<-NA
base.sf1$glucosa[base.sf1$glucosa<=20]<-NA
base.sf1$glucosa[base.sf1$glucosa>=1000]<-NA


#Missing Values by Percentages

missing.values <- base.sf1 %>%
  dplyr::select(peso,talla,imc,cintura,creatinina,colesterol,colesterol_ldl,colesterol_hdl,trigliceridos,hb,glucosa)%>%
  dplyr::rename("Weight (kg)"=peso,
                "Height (mts)"=talla,
                "BMI (kg/mt2)"=imc,
                "Waist Circunference (cm)"=cintura,
                "Creatinine (gr/dl)"=creatinina,
                "Glucose (gr/dl)"=glucosa,
                "Cholesterol (gr/dl)"=colesterol,
                "LDL-C (gr/dl)"=colesterol_ldl,
                "HDL-C (gr/dl)"=colesterol_hdl,
                "Tryglicerides (gr/dl)"=trigliceridos,
                "Hemoglobin (gr/ml)"=hb)%>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <- (missing.values  %>% filter(isna == T) %>%     
             arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), 
                    labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", 
       x = 'Variable', y = "% of missing values")


ggsave(percentage.plot,
       filename = "Submission/Percentage_plot.png", bg = "white",
       width = 15, 
       height = 15,
       units=c("cm"),
       dpi = 350,
       limitsize = FALSE) 


base.imp<-base.sf1%>%dplyr::select(peso,talla,imc,cintura,creatinina,colesterol,colesterol_ldl,colesterol_hdl,trigliceridos,hb,glucosa)
base4_imp<-mice::mice(base.imp, m=5, maxit=5,seed = 123)
base4_imp_2<-complete(base4_imp,1)
base4_imp_2<-base4_imp_2

mice::densityplot(base4_imp)

summary(base.sf1%>%dplyr::select(peso,talla,imc,cintura,creatinina,colesterol,colesterol_ldl,colesterol_hdl,trigliceridos,hb,glucosa)%>%
          dplyr::rename("Weight (kg)"=peso,
                        "Height (mts)"=talla,
                        "BMI (kg/mt2)"=imc,
                        "Waist Circunference (cm)"=cintura,
                        "Creatinine (gr/dl)"=creatinina,
                        "Glucose (gr/dl)"=glucosa,
                        "Cholesterol (gr/dl)"=colesterol,
                        "LDL-C (gr/dl)"=colesterol_ldl,
                        "HDL-C (gr/dl)"=colesterol_hdl,
                        "Tryglicerides (gr/dl)"=trigliceridos,
                        "Hemoglobin (gr/ml)"=hb))

summary(base4_imp_2$CD4_ACTUAL)
summary(base4_imp_2$CD4_NADIR)


summary(base1 %>%dplyr::select(HEMOGLOB,GLUCOSA,ALBUMINA,EIDVHI_IHDS,TALLA,IMC,ESCOL.AÑOS,CD4_ACTUAL,CD4_NADIR)%>%
          dplyr::rename("Hemoglobin (grs/ml)"=HEMOGLOB,
                        "Glucose (mg/ml)"=GLUCOSA,
                        "Albumin (mg/ml)"=ALBUMINA,
                        "VIH Dementia Scale, (pts)"=EIDVHI_IHDS,
                        "Height (cms)"=TALLA,
                        "BMI (Kg/mts^2)"=IMC,
                        "Education (Years) "=ESCOL.AÑOS,
                        "Current CD4+ T-lymphocytes (cells/ml)"=CD4_ACTUAL,
                        "Nadir CD4+ T-lymphocytes (cells/ml)"=CD4_NADIR))







table(base$ldl_alto==1 | base$hipertrig==1 | base$hipohdl==1)
prop.table(table(base$ldl_alto==1 | base$hipertrig==1 | base$hipohdl==1))*100

ncas <- table(base$ldl_alto==1 | base$hipertrig==1 | base$hipohdl==1)[2]; npop <- sum(!is.na(base$hipohdl))
tmp <- as.matrix(cbind(ncas, npop))
round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)
