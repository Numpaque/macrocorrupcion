# Efecto de la centralidad de los contratistas en la red de contratación pública sobre el riesgo de corrupción a nivel municipal

# Juan Sebastián Numpaque Cano
# Julio 18 de 2020

# Documentación:
# Documento: https://www.overleaf.com/read/hxcfpmmytymg  
# Presentación: https://www.overleaf.com/read/hqxtjzmbgcdk

# Referencias de interés:
# https://www.fedesarrollo.org.co/sites/default/files/fedesarrollo_cpbd.pdf
# https://royalsocietypublishing.org/doi/10.1098/rsos.182103
# https://repository.urosario.edu.co/handle/10336/18525?locale-attribute=pt

# 1. Configuración del entorno ====

# 1.1 Instalación de paquetes necesarios ----
lista_de_paquetes <- c("rstudioapi", "data.table", "dplyr", "bit64", "readxl",
                       "jsonlite", "lmtest", "sandwich", "car")
nuevos_paquetes <- lista_de_paquetes[!(lista_de_paquetes %in% installed.packages()[,"Package"])]
if(length(nuevos_paquetes)) install.packages(nuevos_paquetes)

# 1.2 Carga de los paquetes necesarios ----
library(rstudioapi)
library(data.table)
library(dplyr)
library(bit64)
library(readxl)
library(jsonlite)
library(lmtest)
library(sandwich)
library(car)

# 1.3 Lectura de los datos ----
setwd(dirname(getActiveDocumentContext()$path))
casos_corrupcion <- read_excel("insumos/reporte_casos_20190429.xlsx")
secop_2_procesos <- fread("insumos/SECOP_II_-_Procesos_de_Contrataci_n.csv", encoding = "UTF-8", )

# 2. Estructuración y limpieza de datos ====

# 2.1 Se crean y/o estructuran campos de interés ----
secop_2_procesos[, `Valor Total Adjudicacion` := as.numeric(`Valor Total Adjudicacion`)]

# Duración de los procesos
secop_2_procesos[, fecha_apertura := as.Date(`Fecha de Apertura Efectiva`, format = "%m/%d/%Y")]
secop_2_procesos[, fecha_adjudicacion := as.Date(`Fecha Adjudicacion`, format = "%m/%d/%Y")]
secop_2_procesos[, duracion_seleccion := as.numeric(fecha_adjudicacion-fecha_apertura+1)]
secop_2_procesos[, anio_adjudicacion := as.numeric(format(fecha_adjudicacion, "%Y"))]

# Procesos adjudicados por modalidades competitivas y no competitivas
modalidades_competitivas <- c("Mínima cuantía", "Selección Abreviada de Menor Cuantía", "Selección abreviada subasta inversa", "Concurso de méritos abierto", "Licitación pública", "Licitación pública Obra Publica", "Licitación Pública Acuerdo Marco de Precios", "Seleccion Abreviada Menor Cuantia Sin Manifestacion Interes")
contratacion_directa <- c("Contratación directa", "Contratación Directa (con ofertas)")
regimen_especial <- c("Contratación régimen especial", "Contratación régimen especial (con ofertas)")
modalidades_no_competitivas <- c(contratacion_directa, regimen_especial)
licitacion_publica <- c("Licitación pública", "Licitación Pública Acuerdo Marco de Precios", "Licitación pública Obra Publica")

secop_2_procesos[, es_regimen_especial := `Modalidad de Contratacion` %in% regimen_especial]
secop_2_procesos[, es_licitacion := `Modalidad de Contratacion` %in% licitacion_publica]
secop_2_procesos[, competitivo := `Modalidad de Contratacion` %in% modalidades_competitivas]
secop_2_procesos[, no_competitivo := `Modalidad de Contratacion` %in% modalidades_no_competitivas]
secop_2_procesos[, monto_no_competitivo := ifelse(no_competitivo, `Valor Total Adjudicacion`, 0)]

# Banderas rojas (indicadores a nivel de proceso)
secop_2_procesos[, oferente_unico := competitivo & `Proveedores Unicos con Respuestas` == 1]
secop_2_procesos[, regimen_especial_no_competitivo := es_regimen_especial & duracion_seleccion <= 5] 
secop_2_procesos[, licitacion_rapida := `Modalidad de Contratacion` %in% licitacion_publica & duracion_seleccion < 90]  

# 3. Cálculo de indicadores de riesgo de corrupción a nivel municipal
riesgo_municipal <- secop_2_procesos %>% 
  filter(!is.na(`Ciudad de la Unidad de Contratación` != "No definida")) %>%
  group_by(`Ciudad de la Unidad de Contratación`) %>%
  summarise(oferente_unico = sum(oferente_unico),
            total_procesos = n(), # (Control)
            porcentaje_oferente_unico = sum(oferente_unico)/n(),
            procesos_con_oferentes = sum(!is.na(`Proveedores Unicos con Respuestas`)),
            promedio_oferentes = mean(`Proveedores Unicos con Respuestas`, na.rm = T),
            empresas_adjudicatarias = sum(CodigoProveedor != "No Adjudicado" & !duplicated(CodigoProveedor)),
            procesos_adjudicados = sum(CodigoProveedor != "No Adjudicado"),
            proxi_centralidad = #procesos_adjudicados/empresas_adjudicatarias,
            1 - empresas_adjudicatarias/procesos_adjudicados,
            no_competitivos = sum(no_competitivo),
            porcentaje_no_competitivos = sum(no_competitivo)/n(),
            montos_no_competitivos = sum(monto_no_competitivo),
            porcentaje_montos_no_competitivos = sum(montos_no_competitivos)/sum(`Valor Total Adjudicacion`),
            regimen_especial = sum(es_regimen_especial, na.rm = T),
            regimen_especial_no_competitivo = sum(regimen_especial_no_competitivo, na.rm = T),
            porcentaje_regimen_especial_no_competitivo = sum(regimen_especial_no_competitivo, na.rm = T)/sum(es_regimen_especial, na.rm = T),
            licitaciones = sum(es_licitacion, na.rm = T),
            licitacion_rapida = sum(licitacion_rapida, na.rm = T),
            porcentaje_licitacion_rapida = sum(licitacion_rapida, na.rm = T)/sum(es_licitacion, na.rm = T))

riesgo_municipal$proxi_centralidad[is.na(riesgo_municipal$proxi_centralidad)] <- 0
riesgo_municipal$porcentaje_montos_no_competitivos[is.na(riesgo_municipal$porcentaje_montos_no_competitivos)] <- 0
riesgo_municipal$porcentaje_regimen_especial_no_competitivo[is.na(riesgo_municipal$porcentaje_regimen_especial_no_competitivo)] <- 0
riesgo_municipal$porcentaje_licitacion_rapida[is.na(riesgo_municipal$porcentaje_licitacion_rapida)] <- 0

riesgo_municipal_final <- riesgo_municipal %>%
  mutate(CRI = (porcentaje_oferente_unico +
                  porcentaje_montos_no_competitivos +
                  porcentaje_regimen_especial_no_competitivo +
                  porcentaje_licitacion_rapida)/4) %>%
  select(municipio = `Ciudad de la Unidad de Contratación`,
         total_procesos, CRI, proxi_centralidad)

# 4. Variables de control ====
valor_agregado_2018 <- read_excel("insumos/Valor_agregado.xlsx") %>%
  select(Divipola, Municipio, Departamento, `Valor agregado`)
poblacion <- read_excel("insumos/poblacion_mpios_2018_a_2020.xlsx") %>%
  rename(Divipola = Municipio)
ipm <- read_excel("insumos/CNPV-2018_PobrezaMultidimensional_Municipal.xlsx") %>%
  filter(Dominio == "Total municipal") %>%
  select(Divipola = `Código Municipio`,
         desempleo = `Desempleo de larga duración`,
         empleo_informal = `Trabajo informal`,
         analfabetismo = Analfabetismo,
         bajo_logro = `Bajo logro educativo`)
load("insumos/poblacion_piramides.RData")
poblacion_piramides_60 <- poblacion_piramides %>%
  filter(`Grupos de edad` %in% c("60-69", "70-79", "80-89", "90-99", "100 o más")) %>%
  group_by(Divipola = Codigo) %>%
  summarise(mayores_de_60 = (sum(Hombres)+sum(Mujeres)))

poblacion_total <- poblacion_piramides %>%
  group_by(Divipola = Codigo) %>%
  summarise(total = (sum(Hombres)+sum(Mujeres)))

poblacion_piramides <- left_join(poblacion_total, poblacion_piramides_60, by = "Divipola") %>%
  mutate(tasa_60 = mayores_de_60/total) %>%
  select(Divipola, mayores_de_60 = tasa_60)

votaciones <- read_excel("insumos/Registraduria_con_divipola.xlsx") %>%
  select(COD_RNEC, DIVIPOLA)
votaciones$margen <- NA

# Descargar y estructurar margenes de ganancia en las votaciones con webscraping
for(i in 1:nrow(votaciones)){
  if(i%%50 == 0) print(i)
  datos_json <- read_json(paste0("https://resultados2019.registraduria.gov.co/json/ACT/AL/",votaciones$COD_RNEC[i],".json"))
  porcentajes <- sapply(datos_json$partotabla, function(x){
    as.numeric(gsub(",",".",gsub("%", "", x$act$cantotabla[[1]]$pvot)))
  })
  diferencias <- diff(porcentajes)
  votaciones$margen[i] <- ifelse(length(diferencias)>0, diferencias[1], -porcentajes)
}

votaciones <- votaciones %>% rename(Divipola = DIVIPOLA)

# save(votaciones, file = "insumos/backup_margenes.RData")
# load(file = "insumos/backup_margenes.RData")

control_todas <- valor_agregado_2018 %>% 
  left_join(poblacion) %>%
  left_join(ipm) %>%
  left_join(poblacion_piramides) %>%
  left_join(votaciones)

control_final <- control_todas %>%
  mutate(valor_agregado_per_capita = `Valor agregado`/Poblacion_2018,
         log_poblacion = log(Poblacion_2018)) %>%
  select(Divipola, 
         Municipio,
         valor_agregado_per_capita,
         log_poblacion, 
         margen,
         analfabetismo,
         bajo_logro,
         desempleo,
         empleo_informal,
         mayores_de_60)
  
# 5. Cruce de las bases ====
riesgo_municipal_final$municipio_llave <- riesgo_municipal_final$municipio %>%
  iconv(., from="UTF-8", to="ASCII//TRANSLIT") %>%
  tolower

control_final$municipio_llave <- control_final$Municipio %>%
  iconv(., from="UTF-8", to="ASCII//TRANSLIT") %>%
  tolower

municipios_ambiguos <- control_final$municipio_llave[duplicated(control_final$municipio_llave)]

riesgo_municipal_final <- riesgo_municipal_final %>% 
  filter(!municipio_llave %in% municipios_ambiguos)

base_cruzada <- left_join(riesgo_municipal_final, control_final, by = "municipio_llave") %>%
  mutate(log_n_procesos = log(total_procesos)) %>%
  filter(total_procesos >= 5) %>%
  select(-total_procesos) %>%
  select(-municipio) %>%
  select(-Municipio) %>%
  select(-municipio_llave) %>%
  # filter(substr(Divipola, 3, 5) != "001") %>%
  filter(!Divipola %in% c("11001", "05001", "76001")) %>%
  select(-Divipola) %>%
  filter(!is.na(valor_agregado_per_capita))

modelo_sin_centralidad <- lm(CRI~.-proxi_centralidad, data = base_cruzada)
summary(modelo_sin_centralidad)

modelo_con_centralidad <- lm(CRI~., data = base_cruzada)
summary(modelo_con_centralidad)

vif(modelo_sin_centralidad)
vif(modelo_con_centralidad)

bptest(modelo_con_centralidad)

# Errores robustos, por si hubiera un error tipo II en la prueba de heterocedasticidad
coeftest(modelo_con_centralidad, vcov = vcovHC(modelo_con_centralidad, "HC1"))
coeftest(modelo_con_centralidad, vcov = vcovHC(modelo_con_centralidad, "HC2"))
coeftest(modelo_con_centralidad, vcov = vcovHC(modelo_con_centralidad, "HC3"))
coeftest(modelo_con_centralidad, vcov = vcovHC(modelo_con_centralidad, "HC4"))

# GLS, por si hubiera un error tipo II en la prueba de heterocedasticidad
modelo_gls <- nlme::gls(CRI~., data = base_cruzada)
summary(modelo_gls)

