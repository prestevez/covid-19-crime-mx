## A closer look at crime categories in the carpetas data set

require(tidyverse)
require(magrittr)
require(lubridate)
Sys.setenv("TZ"="Europe/London")
require(tsibble)

covid_fecha <- ymd("2020-02-29")

carpetas <- read_csv("data/carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico-2020-07-03.csv", 
                     col_types = cols(
                       ao_hechos = col_double(),
                       mes_hechos = col_character(),
                       fecha_hechos = col_character(),
                       delito = col_character(),
                       categoria_delito = col_character(),
                       fiscalia = col_character(),
                       agencia = col_character(),
                       unidad_investigacion = col_character(),
                       alcaldia_hechos = col_character(),
                       colonia_hechos = col_character(),
                       ao_inicio = col_double(),
                       mes_inicio = col_character(),
                       fecha_inicio = col_datetime(format = ""),
                       calle_hechos = col_character(),
                       calle_hechos2 = col_character(),
                       longitud = col_double(),
                       latitud = col_double(),
                       geopoint = col_character()
                     )) %>% 
  mutate(fecha_hechos_parsed = parse_date_time(fecha_hechos,
                                               # orders = c("ymd_HMS", "ymd_HM", "dmy HMS", "dmy_HM")),
                                               orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", 
                                                          "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M")),
         fecha_inicio_parsed = parse_date_time(fecha_inicio,
                                               # orders = c("ymd_HMS", "ymd_HM", "dmy HMS", "dmy_HM"))
                                               orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", 
                                                          "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M"))
  )


carpetas %>% 
  filter(ao_hechos > 2016) -> carpetas_2016 

carpetas_2016 %$% delito %>% unique() -> delitos_vctr

carpetas_2016 %$% categoria_delito %>% unique() -> categoria_delitos_vctr

delitos_vctr
categoria_delitos_vctr

## Robberies

carpetas_2016 %>%
  filter(str_detect(categoria_delito, "ROBO")) %$%
  delito %>%
  unique 

carpetas_2016 %>%
  filter(!str_detect(categoria_delito, "ROBO")) %>%
  filter(str_detect(delito, "ROBO")) %$%
  delito %>%
  unique 

carpetas_2016 %>%
  filter(str_detect(delito, "ROBO")) %>%
  filter(!str_detect(delito, "SIN VIOLENCIA") & 
              !str_detect(delito, "S/V")) %>%
  filter(str_detect(delito, "CASA", negate = TRUE)) %>%
  filter(!str_detect(delito, "INFANTE") & 
           !str_detect(delito, "FLUIDO") &
           !str_detect(delito, "ESCUELA") &
           !str_detect(delito, "ANIMAL") &
           !str_detect(delito, "ALHAJA")) -> carpetas_2016_robos_violentos

carpetas_2016_robos_violentos %$% delito %>% unique -> delitos_tmp

carpetas_2016_robos_violentos %$% categoria_delito %>% unique 

delitos_vctr[which(!delitos_vctr %in% delitos_tmp)] -> delitos_vctr_not_in_robos

delitos_vctr_not_in_robos %>% 
  str_detect("ROBO") %>%
  which %>%
  delitos_vctr_not_in_robos[.] -> sv_cats

carpetas_2016 %>%
  filter(delito %in% sv_cats) %>%
  filter(str_detect(delito, "CASA", negate = TRUE)) %>%
  filter(str_detect(delito, "INFANTE", negate = TRUE))-> carpetas_2016_robos_sin_violencia

carpetas_2016_robos_sin_violencia %$% delito %>% unique

## Violent crimes

carpetas_2016 %>%
  filter(str_detect(delito, "HOMICIDIO") |
          str_detect(delito, "FEMINICIDIO") |
           str_detect(delito, "LESIONES") |
           str_detect(delito, "SECUESTRO") |
           str_detect(delito, "PLAGIO") |
           str_detect(delito, "EXTORSION") |
           str_detect(delito, "PRIVACION")) %>%
  filter(str_detect(delito, "CULPOS", negate = TRUE)) -> carpetas_2016_violentos

carpetas_2016_violentos %$%
  delito %>% unique 

carpetas_2016_violentos %$%
  categoria_delito %>% unique 
  
## Sex and gender violence crimes

carpetas_2016 %>%
  filter(str_detect(delito, "SEX") |
           str_detect(delito, "VIOLAC") |
           str_detect(delito, "ESTUPRO") |
           str_detect(delito, "INCEST")) %>%
  filter(str_detect(delito, "CORRESPONDENCIA", negate = TRUE)) -> carpetas_2016_sexuales

### domesticos

carpetas_2016 %>%
  filter(str_detect(delito, "FAMILIAR")) -> carpetas_2016_domestic

## robos a casa habitacion?

carpetas_2016 %>%
  filter(str_detect(delito, "CASA") & 
           str_detect(delito, "DAÑO", negate = TRUE)) -> carpetas_2016_robos_casa


### Prepare time series

base_time_series <- tibble(
  fecha = seq(ymd("2017-01-01"), ymd("2020-05-31"), 1),
  año = factor(year(fecha)),
  mes = fct_inorder(months(fecha)),
  diasem = fct_inorder(weekdays(fecha))
)

## ALL crimes

carpetas_2016 %>%
 mutate(fecha = date(fecha_hechos_parsed)) %>%
  count(fecha, name = "all_crimes") %>%
  right_join(base_time_series) -> daily_all

## violent robbery

carpetas_2016_robos_violentos %>%
  mutate(fecha = date(fecha_hechos_parsed)) %>%
  count(fecha, name = "violent_robbery") %>%
  right_join(base_time_series) -> daily_robos_violentos

## non-violent robbery

carpetas_2016_robos_sin_violencia %>%
  mutate(fecha = date(fecha_hechos_parsed)) %>%
  count(fecha, name = "non_violent_robbery") %>%
  right_join(base_time_series) -> daily_robos_no_violentos

## burglary

carpetas_2016_robos_casa %>%
  mutate(fecha = date(fecha_hechos_parsed)) %>%
  count(fecha, name = "burglary") %>%
  right_join(base_time_series) -> daily_robos_casa

## violent crime (non-property, non-sex)

carpetas_2016_violentos  %>%
  mutate(fecha = date(fecha_hechos_parsed)) %>%
  count(fecha, name = "violent_crime") %>%
  right_join(base_time_series) -> daily_violentos

## sex crimes

carpetas_2016_sexuales %>%
  mutate(fecha = date(fecha_hechos_parsed)) %>%
  count(fecha, name = "sex_crime") %>%
  right_join(base_time_series) -> daily_sex

## domestic violence

carpetas_2016_domestic  %>%
  mutate(fecha = date(fecha_hechos_parsed)) %>%
  count(fecha, name = "domestic_violence") %>%
  right_join(base_time_series) -> daily_domestic

# calls violence against women

mujeres <- read_csv("data/linea-mujeres_2020-07-03.csv")

mujeres %$% SERVICIO %>% unique

mujeres %>%
  pivot_longer(names(mujeres)[str_detect(names(mujeres), "TEMA")],
               values_to = "tema") %>%
  drop_na(tema) -> mujeres_long

mujeres_long %$%
  tema %>% unique %>% .[1001:length(.)]

## Keywords

mujeres_long %>%
  filter(str_detect(tema, "VIOLEN") | 
           str_detect(tema, "VIOLAC") |
           str_detect(tema, "AMENAZ") |
           str_detect(tema, "LESIO") |
           str_detect(tema, "FEMINICI") |
           str_detect(tema, "ABUSO") |
           str_detect(tema, "LIBERTAD") |
           str_detect(tema, "PRIVAC") |
           str_detect(tema, "HOMICI") |
           str_detect(tema, "SECUES") |
           str_detect(tema, "RIÑA") |
           str_detect(tema, "FORZADA") |
           str_detect(tema, "ACOSO") |
           str_detect(tema, "ARMA") |
           str_detect(tema, "INCEST") |
           str_detect(tema, "PAIDO") |
           str_detect(tema, "HOSTIG") |
           str_detect(tema, "INTEGRIDAD") |
           str_detect(tema, "DIGNIDAD")) %>%
  filter(str_detect(tema, "VIOLACIÓN [ADE]", negate = TRUE) &
           str_detect(tema, "ABUSO DE", negate = TRUE) &
           str_detect(tema, "TRÁNSITO", negate = TRUE) &
           str_detect(tema, "LABORAL", negate = TRUE) &
           str_detect(tema, "PERCUTI", negate = TRUE) &
           str_detect(tema, "DISPAROS", negate = TRUE)) %$% 
  FOLIO %>% 
  unique -> folios_vaw

folios_vaw %>% length
  
mujeres %>%
  filter(FOLIO %in% folios_vaw) %>%
  count(FECHA_HORA_ALTA) %>%
  ggplot(aes(FECHA_HORA_ALTA, n)) + 
  geom_line(alpha = 0.2) +
  geom_smooth() +
  geom_vline(xintercept = covid_fecha)

mujeres %>%
  filter(FOLIO %in% folios_vaw) %>%
  filter(FECHA_HORA_ALTA >= "2017-01-01") %>%
  count(FECHA_HORA_ALTA, name = "calls_vaw") %>%
  select(fecha = FECHA_HORA_ALTA, calls_vaw) %>%
  right_join(base_time_series) -> daily_calls_vaw


#### join all crime types

daily_all %>%
  left_join(daily_robos_violentos) %>%
  left_join(daily_robos_no_violentos) %>%
  left_join(daily_robos_casa) %>%
  left_join(daily_violentos) %>%
  left_join(daily_sex) %>%
  left_join(daily_domestic) %>%
  left_join(daily_calls_vaw) %>%
  as_tsibble() -> daily_all_ts

daily_all_ts %>%
  pivot_longer(c(2, 6:12), 
               names_to = "crime_type", 
               values_to = "incidence") -> daily_all_ts_tidy


daily_all_ts_tidy %>%
  # filter(fecha < covid_fecha) %>%
  ggplot(aes(fecha, incidence)) +
  geom_line(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ crime_type, scales = "free_y")

daily_all_ts_tidy[!complete.cases(daily_all_ts_tidy),]

## monthly patterns
daily_all_ts_tidy %>%
  as_tibble() %>%
  filter(fecha < covid_fecha) %>%
  group_by(mes, crime_type) %>%
  summarise(incidence = mean(incidence, na.rm = TRUE)) %>%
  ggplot(aes(mes, incidence)) +
  geom_col() +
  facet_wrap(~ crime_type, scales = "free_y")


daily_all_ts_tidy %>%
  as_tibble() %>%
  filter(fecha < covid_fecha) %>%
  group_by(diasem, crime_type) %>%
  summarise(incidence = mean(incidence, na.rm = TRUE)) %>%
  ggplot(aes(diasem, incidence)) +
  geom_col() +
  facet_wrap(~ crime_type, scales = "free_y")

### mobility


metrobus <- read_csv("data/afluencia-diaria-de-metrobus-cdmx_20200703.csv")
metro <- read_csv("data/afluencia-diaria-del-metro-cdmx_20200703.csv")
preliminar <- read_csv("data/afluencia-preliminar-en-transporte-publico_20200703.csv")

metrobus %>% summary
metro %>% summary
preliminar %>% summary


metrobus %>%
  filter(Fecha >= "2017-01-01" &
           Linea != "Linea 7") %>%
  ggplot(aes(Fecha, Afluencia, colour = Linea)) + 
  geom_point(alpha = .2) +
  geom_line(alpha = .2) +
  geom_smooth(se = FALSE)

metrobus %>%
  filter(Fecha >= "2017-01-01" &
           Linea != "Linea 7") %>%
  summary

metrobus %>%
  filter(Fecha >= "2017-01-01" &
           Linea != "Linea 7") %>%
  group_by(Fecha) %>%
  summarise(Afluencia = sum(Afluencia, na.rm = TRUE)) %>%
  mutate(fecha = Fecha, tipo = "metrobus") %>%
  select(-Fecha)-> metrobus_total

metrobus_total %>% 
  ggplot(aes(fecha, Afluencia)) + 
  geom_point(alpha = .2) +
  geom_line(alpha = .2) +
  geom_smooth(se = FALSE)


metro %>%
  group_by(Fecha, Linea) %>%
  filter(!Linea %in% c("Línea 12", "Linea 12")) %>%
  summarise(Afluencia = sum(Afluencia)) %>%
  ggplot(aes(Fecha, Afluencia, colour = Linea)) +
  geom_point(alpha = .2) +
  geom_line(alpha = .2) +
  geom_smooth(se = FALSE)

metro %>%
  filter(!Linea %in% c("Línea 12", "Linea 12")) %>%
  filter(Fecha >= "2017-01-01") %>%
  group_by(Fecha) %>%
  summarise(Afluencia = sum(Afluencia, na.rm = TRUE)) %>%
  mutate(fecha = Fecha,
         tipo = "metro") %>%
  select(-Fecha) -> metro_total

metrobus_total %>%
  bind_rows(metro_total) %>%
  ggplot(aes(fecha, Afluencia, color = tipo)) +
  geom_line(alpha = 0.2) +
  geom_smooth()

preliminar %>%
  filter(ORGANISMO == "STC") %>%
  group_by(FECHA) %>%
  summarise(Afluencia = sum(`AFLUENCIA TOTAL
(cifras preliminares)`, na.rm = TRUE)) %>%
  mutate(fecha = FECHA, tipo = "metro") %>%
  select(-FECHA) -> metro_prelim

metro_total %>%
  bind_rows(mutate(metro_prelim, tipo = "premlim")) %>%
  filter(fecha >= "2020-03-01") %>%
  ggplot(aes(fecha, Afluencia, color = tipo)) + 
  geom_point(alpha = 0.2) +
  geom_line(alpha = 0.2) +
  geom_smooth()


metro_total %>%
  bind_rows(filter(metro_prelim, fecha > max(.$fecha))) -> metro_total_prelim

metro_total_prelim %>%
  filter(fecha >= "2020-03-01") %>%
  ggplot(aes(fecha, Afluencia, color = tipo)) + 
  geom_point(alpha = 0.2) +
  geom_line(alpha = 0.2) +
  geom_smooth()


metrobus_total %>%
  bind_rows(metro_total_prelim) %>%
  mutate(Afluencia = na_if(x = Afluencia, y = 0)) %>%
  pivot_wider(names_from = tipo, values_from = Afluencia) %>%
  mutate(pas_total = metrobus + metro) -> total_pasajeros

total_pasajeros %>%
  ggplot(aes(fecha, pas_total)) + 
  geom_point() + 
  geom_smooth()

total_pasajeros %>%
  summary

total_pasajeros %>%
  filter(is.na(pas_total)) %>%
  data.frame()

### join passenger numbers to crime ts

daily_all_ts %>%
  left_join(total_pasajeros) -> daily_crime_mobility

daily_crime_mobility[!complete.cases(daily_crime_mobility),] %$%
  fecha -> no_calls_vaw

mujeres_long %>%
  filter(FECHA_HORA_ALTA %in% no_calls_vaw) %$%
  tema %>% unique

daily_crime_mobility %>%
  write_csv("data/daily_crime_mobility-2020-07-13.csv")

