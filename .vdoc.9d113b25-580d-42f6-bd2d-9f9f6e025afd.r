#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: Limpar ambiente
#| code-summary: Limpar ambiente
rm(list = ls())
#
#
#
#| label: Diretório
#| code-summary: Diretório
setwd("C:/Users/amand/Amanda/GitHub/inseguranca_alimentar")
#
#
#
#| label: Bibliotecas
#| code-summary: Bibliotecas
pacman::p_load(
  readr,
  dplyr,
  car,
  tidyr,
  purrr,
  broom,
  DescTools,
  reshape2,
  ggplot2,
  survey,
  gt,
  gtsummary
)
options(survey.lonely.psu = "adjust")
#
#
#
#| label: Dados
#| code-summary: Dados
df <- read.csv("ELSI Portugues (3a onda).csv")
#
#
#
#
#
#
#
#
#
#
#
#
#| label: Descritiva apetite
#| code-summary: Descritiva apetite
#| results: hide
table(df$n73_2, useNA = "ifany")
df <- df |>
  mutate(
    apetite = case_when(
      n73_2 == 1 ~ "Muito ruim",
      n73_2 == 2 ~ "Ruim",
      n73_2 == 3 ~ "Razoável",
      n73_2 == 4 ~ "Bom",
      n73_2 == 5 ~ "Muito bom",
      n73_2 %in% c(8, 9) ~ NA_character_
    ),
    apetite = factor(
      apetite,
      levels = c("Muito ruim", "Ruim", "Razoável", "Bom", "Muito bom")
    )
  )
df |>
  count(apetite) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#| label: Descritiva gosto da comida
#| code-summary: Descritiva gosto da comida
#| results: hide
table(df$n73_3, useNA = "ifany")
df <- df |>
  mutate(
    gosto_comida = case_when(
      n73_3 == 1 ~ "Muito ruim",
      n73_3 == 2 ~ "Ruim",
      n73_3 == 3 ~ "Razoável",
      n73_3 == 4 ~ "Bom",
      n73_3 == 5 ~ "Muito bom",
      n73_3 %in% c(8, 9) ~ NA_character_
    ),
    gosto_comida = factor(
      gosto_comida,
      levels = c("Muito ruim", "Ruim", "Razoável", "Bom", "Muito bom")
    )
  )
df |>
  count(gosto_comida) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#| label: Descritiva satisfeito
#| code-summary: Descritiva satisfeito
#| results: hide
table(df$n73_4, useNA = "ifany")
df <- df |>
  mutate(
    satisfeito = case_when(
      n73_4 == 1 ~ "Se sente cheio/satisfeito depois de comer apenas alguns bocados",
      n73_4 == 2 ~ "Se sente cheio/satisfeito depois de comer cerca de um terço de uma refeição",
      n73_4 == 3 ~ "Se sente cheio/satisfeito depois de comer mais da metade da refeição",
      n73_4 == 4 ~ "Se sente cheio/satisfeito depois de comer a maior parte da refeição",
      n73_4 == 5 ~ "Quase nunca se sente cheio/satisfeito",
      n73_4 %in% c(8, 9) ~ NA_character_
    ),
    satisfeito = factor(
      satisfeito,
      levels = c("Muito ruim", "Ruim", "Razoável", "Bom", "Muito bom")
    )
  )
df |>
  count(satisfeito) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#
#
#
#
#
#| label: Descritiva anorexia
#| code-summary: Descritiva IA
#| results: hide
table(df$n73_6, useNA = "ifany")
#
#
#
#| label: Remover não sabe/não respondeu (n = 28)
#| code-summary: Remover não sabe/não respondeu (n = 28)
df <- df |>
  filter(is.na(n73_6) | n73_6 != 9)
#
#
#
#| label: Rótulos IA
#| code-summary: Rótulos IA
df <- df |>
  mutate(
    n73_6 = factor(
      n73_6,
      levels = c(0, 1),
      labels = c("Não", "Sim")
    )
  )
#
#
#
#| label: Descritiva IA com rótulos
#| code-summary: Descritiva IA com rótulos
df |>
  count(n73_6) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#
#
#
#| label: Descritiva idade
#| code-summary: Descritiva idade
#| results: hide
summary(df$idade)

sum(df$idade < 60, na.rm = TRUE)
#
#
#
#| label: Remover menor que 60 anos (n = 2197)
#| code-summary: Remover menor que 60 anos (n = 2197)
df <- df |>
  filter(idade >= 60)
#
#
#
#| label: Categorizar idade
#| code-summary: Categorizar idade
df <- df |>
  mutate(
    faixa_etaria = cut(
      idade,
      breaks = c(60, 70, 80, Inf),
      right = FALSE,
      labels = c("60–69", "70–79", "≥80")
    )
  )
#
#
#
#| label: Descritiva faixa etária
#| code-summary: Descritiva faixa etária
df |>
  count(faixa_etaria) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva sexo
#| code-summary: Descritiva sexo
#| results: hide
table(df$sexo, useNA = "ifany")
#
#
#
#| label: Rótulos sexo
#| code-summary: Rótulos sexo
df <- df |>
  mutate(
    sexo = factor(
      sexo,
      levels = c(0, 1),
      labels = c("Feminino", "Masculino")
    )
  )
#
#
#
#| label: Descritiva sexo com rótulos
#| code-summary: Descritiva sexo com rótulos
df |>
  count(sexo) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva cor/raça
#| code-summary: Descritiva cor/raça
#| results: hide
table(df$e9, useNA = "ifany")
#
#
#
#| label: Categorizar cor/raça
#| code-summary: Categorizar cor/raça
df <- df |>
  mutate(
    raca_cor = case_when(
      e9 == 1 ~ "Branca",
      e9 == 2 ~ "Preta",
      e9 == 3 ~ "Parda",
      e9 %in% c(4, 5, 9) ~ NA_character_
    ),
    raca_cor = factor(
      raca_cor,
      levels = c("Branca", "Preta", "Parda")
    )
  )
#
#
#
#| label: Descritiva cor/raça categorizada
#| code-summary: Descritiva cor/raça categorizada
df |>
  count(raca_cor) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva escolaridade
#| code-summary: Descritiva escolaridade
#| results: hide
table(df$e22, useNA = "ifany")
#
#
#
#| label: Categorizar escolaridade
#| code-summary: Categorizar escolaridade
df <- df |>
  mutate(
    escolaridade = case_when(
      e22 %in% 1:5 ~ "0–4 anos",
      e22 %in% 6:9 ~ "5–8 anos",
      e22 %in% 10:18 ~ "≥9 anos",
      e22 == 99 ~ NA_character_
    ),
    escolaridade = factor(
      escolaridade,
      levels = c("0–4 anos", "5–8 anos", "≥9 anos")
    )
  )
#
#
#
#| label: Descritiva escolaridade categorizada
#| code-summary: Descritiva escolaridade categorizada
df |>
  count(escolaridade) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva estado civil
#| code-summary: Descritiva estado civil
#| results: hide
table(df$e7, useNA = "ifany")
#
#
#
#| label: Categorizar estado civil
#| code-summary: Categorizar estado civil
df <- df |>
  mutate(
    estado_civil = factor(
      case_when(
        e7 == 2 ~ "Casado ou união estável",
        e7 %in% c(1, 3, 4) ~ "Solteiro, divorciado ou viúvo"
      ),
      levels = c(
        "Casado ou união estável",
        "Solteiro, divorciado ou viúvo"
      )
    )
  )
#
#
#
#| label: Descritiva estado civil categorizado
#| code-summary: Descritiva estado civil categorizado
df |>
  count(estado_civil) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva aposentadoria
#| code-summary: Descritiva aposentadoria
#| results: hide
table(df$i0_32, useNA = "ifany")
#
#
#
#| label: Categorizar aposentadoria
#| code-summary: Categorizar aposentadoria
df <- df |>
  mutate(
    aposentadoria = factor(
      case_when(
        i0_32 == 0 ~ "Não",
        i0_32 == 1 ~ "Sim",
        i0_32 == 9 ~ NA_character_
      ),
      levels = c(
        "Não",
        "Sim"
      )
    )
  )
#
#
#
#| label: Descritiva aposentadoria categorizada
#| code-summary: Descritiva aposentadoria categorizada
df |>
  count(aposentadoria) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva moradores
#| code-summary: Descritiva moradores
#| results: hide
table(df$nmoradores, useNA = "ifany")
#
#
#
#| label: Categorizar moradores
#| code-summary: Categorizar moradores
df <- df |>
  mutate(
    moradores = factor(
      case_when(
        nmoradores == 1 ~ "1",
        nmoradores >= 2 ~ "≥2"
      ),
      levels = c("1", "≥2")
    )
  )
#
#
#
#| label: Descritiva moradores categorizado
#| code-summary: Descritiva moradores categorizado
df |>
  count(moradores) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva renda
#| code-summary: Descritiva renda
#| results: hide
summary(df$rendadompc)
#
#
#
#| label: Categorizar renda
#| code-summary: Categorizar renda
cortes <- quantile(
  df$rendadompc,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE
)

df <- df |>
  mutate(
    renda_tercil = cut(
      rendadompc,
      breaks = c(-Inf, cortes, Inf),
      labels = c(
        "1º tercil: ≤ R$ 1.168,89",
        "2º tercil: > R$ 1.168,89 e ≤ R$ 1.500,00",
        "3º tercil: > R$ 1.500,00"
      ),
      include.lowest = TRUE
    )
  )
#
#
#
#| label: Descritiva renda categorizada
#| code-summary: Descritiva renda categorizada
df |>
  count(renda_tercil) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva benefício
#| code-summary: Descritiva benefício
#| results: hide
table(df$d7, useNA = "ifany")
#
#
#
#| label: Categorizar benefício
#| code-summary: Categorizar benefício
df <- df |>
  mutate(
    beneficio = case_when(
      d7 == 0 ~ "Não",
      d7 == 1 ~ "Sim",
      d7 == 9 ~ NA_character_
    ),
    beneficio = factor(
      beneficio,
      levels = c("Não", "Sim")
    )
  )
#
#
#
#| label: Descritiva benefício categorizado
#| code-summary: Descritiva benefício categorizado
df |>
  count(beneficio) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva domicílio
#| code-summary: Descritiva domicílio
#| results: hide
table(df$b1, useNA = "ifany")
#
#
#
#| label: Categorizar domicílio
#| code-summary: Categorizar domicílio
df <- df |>
  mutate(
    domicilio = factor(
      case_when(
        b1 == 0 ~ "Próprio e quitado",
        b1 %in% c(1, 2) ~ "Próprio e não quitado ou alugado",
        b1 %in% c(3, 4) ~ "Cedido",
        b1 == 5 ~ NA_character_
      ),
      levels = c(
        "Próprio e quitado",
        "Próprio e não quitado ou alugado",
        "Cedido"
      )
    )
  )
#
#
#
#| label: Descritiva domicílio categorizado
#| code-summary: Descritiva domicílio categorizado
df |>
  count(domicilio) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva área
#| code-summary: Descritiva área
#| results: hide
table(df$f1, useNA = "ifany")
#
#
#
#| label: Categorizar área
#| code-summary: Categorizar área
df <- df |>
  mutate(
    area = factor(
      case_when(
        f1 == 0 ~ "Rural",
        f1 == 1 ~ "Urbana"
      ),
      levels = c(
        "Rural",
        "Urbana"
      )
    )
  )
#
#
#
#| label: Descritiva área categorizada
#| code-summary: Descritiva área categorizada
df |>
  count(area) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#| label: Descritiva região
#| code-summary: Descritiva região
#| results: hide
table(df$regiao, useNA = "ifany")
#
#
#
#| label: Rótulos região
#| code-summary: Rótulos região
df <- df |>
  mutate(
    regiao = factor(
      regiao,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
    )
  )
#
#
#
#| label: Descritiva região com rótulos
#| code-summary: Descritiva região com rótulos
df |>
  count(regiao) |>
  mutate(percentual = round(100 * n / sum(n), 1))
#
#
#
#
#
#
#
#| label: Desenho amostral
#| code-summary: Desenho amostral
options(survey.lonely.psu = "adjust")
desenho <- svydesign(
  ids = ~upa,
  strata = ~estrato,
  weights = ~peso_calibrado,
  data = df,
  nest = TRUE
)
#
#
#
#
#
#
#
#
#
#| label: Prevalência IA
#| code-summary: Prevalência IA
prev_ia <- svyciprop(
  ~ I(n73_6 == "Sim"),
  desenho,
  method = "logit"
)
prev <- coef(prev_ia)[1] * 100
ic <- confint(prev_ia) * 100
tibble(
  Indicador = "Insegurança alimentar",
  `Prevalência (%)` = round(prev, 1),
  `IC95%` = sprintf(
    "%.1f–%.1f",
    ic[1],
    ic[2]
  )
)
#
#
#
#
#
#
#
#| label: Tabela com dados ausentes
#| code-summary: Tabela com dados ausentes
df |>
  select(
    n73_6,
    faixa_etaria,
    sexo,
    raca_cor,
    escolaridade,
    estado_civil,
    aposentadoria,
    moradores,
    renda_tercil,
    beneficio,
    domicilio,
    area,
    regiao,
  ) |>
  tbl_summary(
    label = list(
      n73_6 ~ "Insegurança alimentar",
      faixa_etaria ~ "Faixa etária",
      sexo ~ "Sexo",
      raca_cor ~ "Raça/cor",
      escolaridade ~ "Escolaridade",
      estado_civil ~ "Estado civil",
      aposentadoria ~ "Aposentadoria",
      moradores ~ "Número de moradores no domicílio",
      renda_tercil ~ "Renda domiciliar per capita (tercis)",
      beneficio ~ "Recebimento de benefício",
      domicilio ~ "Tipo de domicílio",
      area ~ "Área de residência",
      regiao ~ "Macrorregião geográfica"
    ),
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ c(0, 1),
    missing = "always",
    missing_text = "Ausente"
  ) |>
  bold_labels() |>
  modify_header(
    label ~ glue::glue(
      "**Características (n = {format(nrow(df), big.mark = '.', decimal.mark = ',')})**"
    ),
    stat_0 ~ "**n (%)**"
  ) |>
  modify_footnote(everything() ~ NA) |>
  modify_caption(
    "**Tabela 1. Características da amostra total e dados ausentes**"
  )
#
#
#
#
#
#
#
#| label: Tabela estratificada
#| code-summary: Tabela estratificada
options(survey.lonely.psu = "adjust")
tbl_svysummary(
  desenho,
  by = n73_6,
  include = c(
    faixa_etaria,
    sexo,
    raca_cor,
    escolaridade,
    estado_civil,
    aposentadoria,
    moradores,
    renda_tercil,
    beneficio,
    domicilio,
    area,
    regiao
  ),
  label = list(
    faixa_etaria ~ "Faixa etária",
    sexo ~ "Sexo",
    raca_cor ~ "Raça/cor",
    escolaridade ~ "Escolaridade",
    estado_civil ~ "Estado civil",
    aposentadoria ~ "Aposentadoria",
    moradores ~ "Número de moradores no domicílio",
    renda_tercil ~ "Renda domiciliar per capita (tercis)",
    beneficio ~ "Recebimento de benefício",
    domicilio ~ "Tipo de domicílio",
    area ~ "Área de residência",
    regiao ~ "Macrorregião geográfica"
  ),
  statistic = all_categorical() ~ "{n_unweighted} ({p}%)",
  digits = all_categorical() ~ c(0, 1),
  missing = "no"
) |>
  add_overall(last = FALSE) |>
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  bold_labels() |>
  modify_header(
    label ~ glue::glue(
      "**Características (n = {format(nrow(df), big.mark = '.', decimal.mark = ',')})**"
    ),
    stat_0 ~ "**Total**",
    stat_1 ~ "**Não**",
    stat_2 ~ "**Sim**",
    p.value ~ "**p-valor**"
  ) |>
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Insegurança alimentar**"
  ) |>
  modify_footnote(
    stat_0 ~ "n não ponderado (% ponderada)",
    stat_1 ~ "n não ponderado (% ponderada)",
    stat_2 ~ "n não ponderado (% ponderada)"
  ) |>
  modify_footnote(
    p.value ~ "Teste de Rao-Scott"
  ) |>
  modify_caption(
    "**Tabela 2. Características da amostra total e segundo insegurança alimentar**"
  )
#
#
#
#
#
#
#
#| label: Tabela prevalência
#| code-summary: Tabela prevalência
options(survey.lonely.psu = "adjust")
calc_prev <- function(var, label, desenho) {
  prev <- svyby(
    ~ I(n73_6 == "Sim"),
    as.formula(paste0("~", var)),
    desenho,
    svyciprop,
    vartype = "ci",
    method = "logit",
    na.rm = TRUE
  )
  p_val <- svychisq(
    as.formula(paste0("~n73_6 + ", var)),
    desenho
  )$p.value
  categoria <- as.character(prev[[var]])
  tibble(
    Caracteristica = c(label, paste0("  ", categoria)),
    `Prevalência (%)` = c(
      "",
      sprintf("%.1f", 100 * prev[[2]])
    ),
    `IC95%` = c(
      "",
      sprintf(
        "%.1f–%.1f",
        100 * prev$ci_l,
        100 * prev$ci_u
      )
    ),
    `p-valor` = c(
      format.pval(
        p_val,
        digits = 3,
        eps = 0.001
      ),
      rep("", length(categoria))
    ),
    grupo = c(TRUE, rep(FALSE, length(categoria)))
  )
}
vars <- tribble(
  ~var            , ~label                                 ,
  "faixa_etaria"  , "Faixa etária"                         ,
  "sexo"          , "Sexo"                                 ,
  "raca_cor"      , "Raça/cor"                             ,
  "escolaridade"  , "Escolaridade"                         ,
  "estado_civil"  , "Estado civil"                         ,
  "aposentadoria" , "Aposentadoria"                        ,
  "moradores"     , "Número de moradores no domicílio"     ,
  "renda_tercil"  , "Renda domiciliar per capita (tercis)" ,
  "beneficio"     , "Recebimento de benefício"             ,
  "domicilio"     , "Tipo de domicílio"                    ,
  "area"          , "Área de residência"                   ,
  "regiao"        , "Macrorregião geográfica"
)
tab_prev <- map2_dfr(
  vars$var,
  vars$label,
  ~ calc_prev(.x, .y, desenho)
)
tab_prev |>
  gt() |>
  tab_header(
    title = md(
      "**Tabela 3. Prevalência de insegurança alimentar segundo características sociodemográficas**"
    )
  ) |>
  cols_label(
    Caracteristica = md("**Características**"),
    `Prevalência (%)` = md("**Prevalência (%)**"),
    `IC95%` = md("**IC95%**"),
    `p-valor` = md("**p-valor**")
  ) |>
  cols_align(
    align = "center",
    columns = `Prevalência (%)`
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Caracteristica,
      rows = grupo
    )
  ) |>
  cols_hide(
    columns = grupo
  )
#
#
#
#
#
#
#
#| label: Tabela univariada
#| code-summary: Tabela univariada
options(survey.lonely.psu = "adjust")
vars <- c(
  "faixa_etaria",
  "sexo",
  "raca_cor",
  "escolaridade",
  "estado_civil",
  "aposentadoria",
  "moradores",
  "renda_tercil",
  "beneficio",
  "domicilio",
  "area",
  "regiao"
)
labels <- c(
  faixa_etaria = "Faixa etária",
  sexo = "Sexo",
  raca_cor = "Raça/cor",
  escolaridade = "Escolaridade",
  estado_civil = "Estado civil",
  aposentadoria = "Aposentadoria",
  moradores = "Número de moradores no domicílio",
  renda_tercil = "Renda domiciliar per capita (tercis)",
  beneficio = "Recebimento de benefício",
  domicilio = "Tipo de domicílio",
  area = "Área de residência",
  regiao = "Macrorregião geográfica"
)
resultado_uni <- map_dfr(vars, function(v) {
  modelo <- svyglm(
    as.formula(
      paste0("n73_6 ~ ", v)
    ),
    design = desenho,
    family = quasibinomial()
  )
  p_global <- regTermTest(
    modelo,
    as.formula(
      paste0("~", v)
    )
  )$p
  res <- tidy(
    modelo,
    conf.int = TRUE,
    exponentiate = TRUE
  ) |>
    filter(term != "(Intercept)")
  ref <- levels(df[[v]])[1]
  bind_rows(
    tibble(
      Caracteristica = labels[[v]],
      OR_IC95 = "",
      p_valor = ifelse(
        p_global < 0.001,
        "<0.001",
        sprintf("%.3f", p_global)
      ),
      destaque = p_global < 0.20
    ),
    tibble(
      Caracteristica = paste0("  ", ref, " (ref.)"),
      OR_IC95 = "1.00",
      p_valor = "",
      destaque = FALSE
    ),
    tibble(
      Caracteristica = paste0(
        "  ",
        gsub(
          paste0("^", v),
          "",
          res$term
        )
      ),
      OR_IC95 = sprintf(
        "%.2f (%.2f–%.2f)",
        res$estimate,
        res$conf.low,
        res$conf.high
      ),
      p_valor = "",
      destaque = FALSE
    )
  )
})
resultado_uni |>
  gt() |>
  tab_header(
    title = md(
      "**Tabela 4. Regressão logística univariada para insegurança alimentar**"
    )
  ) |>
  cols_label(
    Caracteristica = md("**Características**"),
    OR_IC95 = md("**OR bruta (IC95%)**"),
    p_valor = md("**p-valor**")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Caracteristica,
      rows = OR_IC95 == ""
    )
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = p_valor,
      rows = destaque
    )
  ) |>
  cols_hide(
    columns = destaque
  )
#
#
#
#
#
#
#
#
#
#
#| label: Modelo 1 - Demográfico
#| code-summary: Modelo 1 - Demográfico
mod1 <- svyglm(
  n73_6 ~
    faixa_etaria +
    raca_cor +
    escolaridade +
    estado_civil +
    aposentadoria,
  design = desenho,
  family = quasibinomial()
)
summary(mod1)
#
#
#
#
#
#| label: Modelo 2 - Demográfico e socioeconômico
#| code-summary: Modelo 2 - Demográfico e socioeconômico
mod2 <- svyglm(
  n73_6 ~
    faixa_etaria +
    raca_cor +
    escolaridade +
    estado_civil +
    aposentadoria +
    renda_tercil +
    beneficio,
  design = desenho,
  family = quasibinomial()
)
summary(mod2)
#
#
#
#
#
#| label: Modelo 3 - Demográfico, socioeconômico, domiciliar e geográfico
#| code-summary: Modelo 3 - Demográfico, socioeconômico, domiciliar e geográfico
mod3 <- svyglm(
  n73_6 ~
    faixa_etaria +
    raca_cor +
    escolaridade +
    estado_civil +
    aposentadoria +
    renda_tercil +
    beneficio +
    domicilio +
    regiao,
  design = desenho,
  family = quasibinomial()
)
summary(mod3)
#
#
#
#
#
#| label: Tabela multivariada
#| code-summary: Tabela multivariada
options(survey.lonely.psu = "adjust")
tbl_mod1 <- tbl_regression(
  mod1,
  exponentiate = TRUE,
  add_estimate_to_reference_rows = TRUE,
  label = list(
    faixa_etaria ~ "Faixa etária",
    raca_cor ~ "Raça/cor",
    escolaridade ~ "Escolaridade",
    estado_civil ~ "Estado civil",
    aposentadoria ~ "Aposentadoria"
  ),
  estimate_fun = ~ style_number(.x, digits = 2),
  pvalue_fun = ~ style_pvalue(.x, digits = 3)
)
tbl_mod2 <- tbl_regression(
  mod2,
  exponentiate = TRUE,
  add_estimate_to_reference_rows = TRUE,
  label = list(
    faixa_etaria ~ "Faixa etária",
    raca_cor ~ "Raça/cor",
    escolaridade ~ "Escolaridade",
    estado_civil ~ "Estado civil",
    aposentadoria ~ "Aposentadoria",
    renda_tercil ~ "Renda domiciliar per capita (tercis)",
    beneficio ~ "Recebimento de benefício"
  ),
  estimate_fun = ~ style_number(.x, digits = 2),
  pvalue_fun = ~ style_pvalue(.x, digits = 3)
)
tbl_mod3 <- tbl_regression(
  mod3,
  exponentiate = TRUE,
  add_estimate_to_reference_rows = TRUE,
  label = list(
    faixa_etaria ~ "Faixa etária",
    raca_cor ~ "Raça/cor",
    escolaridade ~ "Escolaridade",
    estado_civil ~ "Estado civil",
    aposentadoria ~ "Aposentadoria",
    renda_tercil ~ "Renda domiciliar per capita (tercis)",
    beneficio ~ "Recebimento de benefício",
    domicilio ~ "Tipo de domicílio",
    regiao ~ "Macrorregião geográfica"
  ),
  estimate_fun = ~ style_number(.x, digits = 2),
  pvalue_fun = ~ style_pvalue(.x, digits = 3)
)
tbl_merge(
  tbls = list(
    tbl_mod1,
    tbl_mod2,
    tbl_mod3
  ),
  tab_spanner = c(
    "**Modelo 1**",
    "**Modelo 2**",
    "**Modelo 3**"
  )
) |>
  modify_header(
    label ~ glue::glue(
      "**Características (n = {format(nrow(df), big.mark = '.', decimal.mark = ',')})**"
    ),
    estimate_1 ~ "**OR ajustada**",
    estimate_2 ~ "**OR ajustada**",
    estimate_3 ~ "**OR ajustada**"
  ) |>
  modify_caption(
    "**Tabela 5. Associação entre características sociodemográficas e insegurança alimentar segundo modelos hierárquicos**"
  ) |>
  modify_footnote(
    everything() ~ NA
  ) |>
  bold_labels() |>
  as_gt() |>
  gt::tab_source_note(
    gt::md(
      "Modelo 1: faixa etária, raça/cor, escolaridade, estado civil e aposentadoria.  
      Modelo 2: Modelo 1 + renda domiciliar per capita e recebimento de benefício.  
      Modelo 3: Modelo 2 + tipo de domicílio e macrorregião geográfica."
    )
  )
#
#
#
#
#
#| label: Modelo completo
#| code-summary: Modelo completo
mod_final <- svyglm(
  n73_6 ~
    faixa_etaria +
    sexo +
    raca_cor +
    escolaridade +
    estado_civil +
    aposentadoria +
    moradores +
    renda_tercil +
    beneficio +
    domicilio +
    area +
    regiao,
  design = desenho,
  family = quasibinomial()
)
summary(mod_final)
vif(mod_final)
#
#
#
#
#
#| label: Tabela multivariada com todas variáveis
#| code-summary: Tabela multivariada com todas variáveis
# Modelo ajustado por todas as variáveis
options(survey.lonely.psu = "adjust")
mod_final <- svyglm(
  n73_6 ~
    faixa_etaria +
    sexo +
    raca_cor +
    escolaridade +
    estado_civil +
    aposentadoria +
    moradores +
    renda_tercil +
    beneficio +
    domicilio +
    area +
    regiao,
  design = desenho,
  family = quasibinomial()
)

tbl_regression(
  mod_final,
  exponentiate = TRUE,
  add_estimate_to_reference_rows = TRUE,
  label = list(
    faixa_etaria ~ "Faixa etária",
    sexo ~ "Sexo",
    raca_cor ~ "Raça/cor",
    escolaridade ~ "Escolaridade",
    estado_civil ~ "Estado civil",
    aposentadoria ~ "Aposentadoria",
    moradores ~ "Número de moradores no domicílio",
    renda_tercil ~ "Renda domiciliar per capita (tercis)",
    beneficio ~ "Recebimento de benefício",
    domicilio ~ "Tipo de domicílio",
    area ~ "Área de residência",
    regiao ~ "Macrorregião geográfica"
  ),
  estimate_fun = ~ style_number(.x, digits = 2),
  pvalue_fun = ~ style_pvalue(.x, digits = 3)
) |>
  modify_header(
    label ~ glue::glue(
      "**Características (n = {format(nrow(df), big.mark = '.', decimal.mark = ',')})**"
    ),
    estimate ~ "**OR ajustada**",
    p.value ~ "**p-valor**"
  ) |>
  modify_caption(
    "**Tabela 5. Associação entre características sociodemográficas e insegurança alimentar**"
  ) |>
  modify_footnote(
    everything() ~ NA
  ) |>
  bold_labels() |>
  as_gt() |>
  gt::tab_source_note(
    gt::md(
      "OR ajustada obtida por regressão logística considerando simultaneamente faixa etária, sexo, raça/cor, escolaridade, estado civil, aposentadoria, número de moradores no domicílio, renda domiciliar per capita, recebimento de benefício, tipo de domicílio, área de residência e macrorregião geográfica."
    )
  )
#
#
#
