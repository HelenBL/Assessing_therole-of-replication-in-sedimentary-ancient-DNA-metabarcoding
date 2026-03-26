
##### XRF Correlation between Cores

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Ruta al archivo
xrf_path <- "XRF_Cores_Rep_3Reps_sto50.xlsx"

# Nombres de las hojas (p.ej. "CCO1", "CCO2", "CLR1", "STO4"...)
xrf_sheets <- excel_sheets(xrf_path)

# Función para leer y limpiar UNA hoja
read_xrf_sheet <- function(sh) {
  df_raw <- read_excel(xrf_path, sheet = sh, col_types = "text")
  
  df <- df_raw %>%
    # Cambiar coma decimal por punto y <LOD> a NA
    mutate(across(
      everything(),
      ~ gsub(",", ".", .x, fixed = TRUE)
    )) %>%
    mutate(across(
      everything(),
      ~ na_if(.x, "<LOD>")
    )) %>%
    mutate(across(
      everything(),
      as.numeric
    ))
  
  # Extraer CoreID (letras) y Core_Rep (número) del nombre de la hoja
  core_id  <- gsub("[0-9]", "", sh)        # CCO, CLR, STO...
  core_rep <- as.numeric(gsub("[^0-9]", "", sh))  # 1, 2, 3, 4
  
  df$CoreID   <- core_id
  df$Core_Rep <- core_rep
  
  df
}

# Leer todas las hojas en un solo data frame
xrf_all <- map_dfr(xrf_sheets, read_xrf_sheet)

str(xrf_all)

elements <- c("Ti", "Rb", "Zr", "Fe")

# Normalizar por core (z-score)
xrf_scaled <- xrf_all %>%
  group_by(CoreID, Core_Rep) %>%
  mutate(across(all_of(elements),
                ~ as.numeric(scale(.x)),
                .names = "{.col}_z")) %>%
  ungroup()

# Formato largo
xrf_long <- xrf_scaled %>%
  pivot_longer(
    cols = ends_with("_z"),
    names_to = "Element",
    values_to = "Value_z"
  ) %>%
  mutate(
    Element  = sub("_z$", "", Element),
    Core_Rep = factor(Core_Rep)
  )


colos_rep_lines <- c(
  "1" = "#8B5062",   # cobre claro / terracota suave
  "2" = "#7E9CAB",   # gris azulado mineral (no es "azul")
  "3" = "#C97C5D"    # rosa antiguo
)



# Perfil por sitio y elemento, líneas de cada core
ggplot(xrf_long,
       aes(x = Value_z, y = Depth, group = Core_Rep)) +
  
  geom_path(aes(colour = Core_Rep), size = 1.1, alpha = 0.5) +
  geom_point(aes(colour = Core_Rep), size = 1) +
  
  scale_colour_manual(values = colos_rep_lines, name = "Biological replicate") +
  
  scale_y_reverse() +
  
  facet_grid(CoreID ~ Element, scales = "free_y") +
  
  labs(
    x = "Standardised XRF (z-score)",
    y = "Depth (cm)"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    axis.text      = element_text(size = 15),
    axis.title     = element_text(size = 18, face = "bold"),
    strip.text     = element_text(size = 15, face = "bold"),
    legend.text    = element_text(size = 18),
    legend.title   = element_text(size = 15, face = "bold"),
    panel.border   = element_rect(fill = NA)
  )


# Tamaño de bin de profundidad
bin_size <- 0.5   # cm (cámbialo si hace falta)

# 1) Bins de profundidad + media de cada elemento en cada bin
xrf_binned <- xrf_all %>%
  mutate(depth_bin = round(Depth / bin_size) * bin_size) %>%
  group_by(CoreID, Core_Rep, depth_bin) %>%
  summarise(
    across(all_of(elements), mean, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Formato largo por elemento
xrf_b_long <- xrf_binned %>%
  pivot_longer(
    cols = all_of(elements),
    names_to  = "Element",
    values_to = "Value"
  )

compute_corrs_site_element <- function(df) {
  cores <- sort(unique(df$Core_Rep))
  if (length(cores) < 2) return(NULL)
  
  combs <- t(combn(cores, 2))
  
  map_dfr(seq_len(nrow(combs)), function(i) {
    c1 <- combs[i, 1]
    c2 <- combs[i, 2]
    
    wide <- df %>%
      filter(Core_Rep %in% c(c1, c2)) %>%
      select(Core_Rep, depth_bin, Value) %>%
      pivot_wider(
        names_from  = Core_Rep,
        values_from = Value,
        names_prefix = "Core_"
      )
    
    col1 <- paste0("Core_", c1)
    col2 <- paste0("Core_", c2)
    
    # Por si acaso faltara alguna columna
    if (!(col1 %in% names(wide) && col2 %in% names(wide))) {
      r <- NA_real_
    } else {
      # solo filas con datos completos
      valid <- complete.cases(wide[[col1]], wide[[col2]])
      if (sum(valid) < 2) {
        # no hay suficientes puntos para correlación
        r <- NA_real_
      } else {
        r <- cor(wide[[col1]][valid], wide[[col2]][valid])
      }
    }
    
    tibble(
      CoreID  = df$CoreID[1],
      Element = df$Element[1],
      Core1   = c1,
      Core2   = c2,
      r       = r
    )
  })
}


# 3) Aplicar por sitio y elemento
corrs <- xrf_b_long %>%
  group_by(CoreID, Element) %>%
  group_split() %>%
  map_dfr(compute_corrs_site_element)

corrs


Table_S3 <- corrs %>%
  mutate(
    Comparison = case_when(
      Core1 == 1 & Core2 == 2 ~ "Core1 vs Core2",
      Core1 == 1 & Core2 == 3 ~ "Core1 vs Core3",
      Core1 == 2 & Core2 == 3 ~ "Core2 vs Core3"
    )
  ) %>%
  
  select(CoreID, Element, Comparison, r) %>%
  
  pivot_wider(names_from = Comparison, values_from = r) %>%
  
  rowwise() %>%
  mutate(
    `Median r` = median(c(`Core1 vs Core2`, `Core1 vs Core3`, `Core2 vs Core3`), na.rm = TRUE),
    `Min r`    = min(c(`Core1 vs Core2`, `Core1 vs Core3`, `Core2 vs Core3`), na.rm = TRUE),
    `Max r`    = max(c(`Core1 vs Core2`, `Core1 vs Core3`, `Core2 vs Core3`), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  mutate(
    Correlation = case_when(
      is.na(`Median r`) ~ NA_character_,
      `Median r` < 0.30 ~ "Weak",
      `Median r` < 0.60 ~ "Moderate",
      TRUE              ~ "Strong"
    )
  ) %>%
  
  rename(Site = CoreID) %>%
  
  select(
    Site,
    Element,
    `Core1 vs Core2`,
    `Core1 vs Core3`,
    `Core2 vs Core3`,
    `Median r`,
    `Min r`,
    `Max r`,
    Correlation
  )

Table_S3

write.xlsx(Table_S3, "Correlation_XRF.xlsx")

corr_summary <- corrs %>%
  group_by(CoreID, Element) %>%
  summarise(
    r_median = median(r, na.rm = TRUE),
    r_min    = min(r, na.rm = TRUE),
    r_max    = max(r, na.rm = TRUE),
    .groups = "drop"
  )

corr_summary

#write.xlsx(corrs, "correlaciones.xlsx")


corrs2 <- read_excel("correlaciones.xlsx")
corr_summary2 <- corrs2 %>%
  group_by(CoreID, Element) %>%
  summarise(
    r_median = median(r, na.rm = TRUE),
    r_min    = min(r, na.rm = TRUE),
    r_max    = max(r, na.rm = TRUE),
    .groups = "drop"
  )

corr_summary2
#write.xlsx(corr_summary2, "corrs_summary_OK.xlsx")


