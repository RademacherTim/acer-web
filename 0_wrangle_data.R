#===============================================================================
# script to download and wrangle sugar season data sap data
#-------------------------------------------------------------------------------

# Données à ajouter ------------------------------------------------------------
# TR - Add data from Yvon Grenier, if I can get my hands on it.
# TR - Add data from DRF one day
# TR - Additional data from other ACERnet sites (still waiting to hear from Bill)
# TR - Ajouter les données de Harvard Forest de 2024

# Maîtrise Michaël :
# 1) DHP
# 1) DHP annuelle et coulée totale annuelle
# 2) DHP annuelle et taux de sucre moyenne
# 3) Croissance annuelle (largeur de cerne et incrément de surface terrière) 
#    et coulée totale annuelle :
#       - L'année de croissance avant la coulée et la coulée totale
#       - 5, 10, 15, 20, 30, 40 et 50 ans de croissance et la coulée totale
#
# 4) MSCR :
# 4.1) Est-ce que le MSCR est un bon proxy pour la vigueur mesuré comme 
# croissance (largeur de cerne et incrément de surface terrière)?
#       - 5, 10, 15, 20, 30, 40 et 50 ans de croissance et la coulée totale
# 4.2) Est-ce que les catégories du MCSR sur (a) la coulée totale et (b) le 
# taux de sucre moyenne?
# 4.3) S'il y a un impact de certains défaults, est-ce que la distance du 
# défault module l'effet du défault? 
#
# 5) Climat et rendement annuelle et taux de sucre :
# 5.1) Nombre évènement de gel-dégel et volume de sève totale de la saison
# 5.2) Somme d'écart de température pour les évènement de gel-dégel et volume de 
# sève totale de la saison
# 5.3) Température minimale, moyenne et maximale de Décembre-Janvier-Février et 
# taux de sucre moyenne de la saison
# 5.4) Dégrée-jours et fin de la saison (i.e., date de dernière coulée)
# 5.5) Température mensuelle moyenne de Février, Mars et Avril et volume de sève mensuelle et taux de sucre mensuelle
# 5.6) Température et précipitation pendant la saison de croissance (Mai à Octobre) et taux de sucre moyennes
# 5.7) Est-ce qu'il y a une relation entre rendements annuelle et taux de sucre annuelle?

# load dependencies ------------------------------------------------------------
if (!existsFunction("%>%")) library ("tidyverse")
if (!existsFunction("yday")) library ("lubridate")
if (!existsFunction("read_excel")) library ("readxl")
if (!existsFunction("add.alpha")) library ("prettyGraphs")

# fait une liste de tous les fichiers du réseau acer-web -----------------------
# Site #1 : L'Assomption
# Site #2 : Vallée-Jonction
# Site #3 : Érable de Norvège dans le Sud-Ouest de Montréal
noms_fichiers <- list.files("./données/acer-web/coulée/",
                            pattern = "^Fiche de donn.*\\.xlsx$")

# coéfficients pour calculer la densité spécifique en fonction du brix d'après 
# Allard (1999) ----------------------------------------------------------------
a_coef <- 0.999992956631726
b_coef <- -0.00925262369550272
c_coef <- -0.00539288034998694
d_coef <- 0.0000222308169457791

# boucle pour lire les fichiers du réseau acer-web -----------------------------
for (s in 1:length(noms_fichiers)) {
  
  # extraire le site et l'année ------------------------------------------------
  site <- as.numeric(substr(strsplit(noms_fichiers[s], split = " ")[[1]][6], 2, 2))
  année <- as.numeric(substr(strsplit(noms_fichiers[s], split = " ")[[1]][7], 1, 4))
  
  # nom du fichier -------------------------------------------------------------
  fichier <- paste0("./données/acer-web/coulée/", noms_fichiers[s])
  
  # détermine le nombre de colonne, leurs noms et leur types -------------------
  if ((site == 1 & année %in% 2022:2023) | (site == 3)) {
    noms <- c("ar", "e", "date", "heure", "coulée", "poids_p", "poids_v", 
              "glace", "brix_chau1", "brix_chau2", "brix_chau3", "brix_chal1", 
              "brix_chal2", "brix_chal3", "commentaires", "vol_s")
    col_types_c <- c(
        ar = "numeric",
        e = "text",
        date = "date",
        heure = "date",
        coulée = "logical",
        poids_p = "numeric",
        poids_v = "numeric",
        glace = "guess",
        brix_chau1 = "numeric",
        brix_chau2 = "numeric",
        brix_chau3 = "numeric",
        brix_chal1 = "numeric",
        brix_chal2 = "numeric",
        brix_chal3 = "numeric",
        commentaires = "text",
        vol_s = "numeric"
      )
  } else if (site == 2 & année %in% 2022:2024) {
    noms <- c("ar", "e", "date", "heure", "coulée", "poids_p", "poids_v", 
              "glace", "brix_chau1", "brix_chau2", "brix_chau3", "brix_chal1", 
              "brix_chal2", "brix_chal3", "commentaires", "brix")
    col_types_c <- c(
      ar = "numeric",
      e = "text",
      date = "date",
      heure = "date",
      coulée = "logical",
      poids_p = "numeric",
      poids_v = "numeric",
      glace = "guess",
      brix_chau1 = "numeric",
      brix_chau2 = "numeric",
      brix_chau3 = "numeric",
      brix_chal1 = "numeric",
      brix_chal2 = "numeric",
      brix_chal3 = "numeric",
      commentaires = "text",
      brix = "numeric"
    )
  } else {
    noms <- c("ar", "e", "date", "heure", "coulée", "poids_p", "poids_v", 
              "glace", "brix_chau1", "brix_chau2", "brix_chau3", "brix_chal1", 
              "brix_chal2", "brix_chal3", "commentaires")
    col_types_c <- c(
      ar = "numeric",
      e = "text",
      date = "date",
      heure = "date",
      coulée = "logical",
      poids_p = "numeric",
      poids_v = "numeric",
      glace = "guess",
      brix_chau1 = "numeric",
      brix_chau2 = "numeric",
      brix_chau3 = "numeric",
      brix_chal1 = "numeric",
      brix_chal2 = "numeric",
      brix_chal3 = "numeric",
      commentaires = "text"
    )
  }
  
  # lire les données de coulée ----
  tmp_c <- read_excel(path = fichier, sheet = "01_coulée",
                      col_names = noms,
                      col_types = col_types_c,
                      na = "NA",
                      skip = 2) %>% 
    # combine l'heure et la date ----
    mutate(date = paste0(substr(date, 1, 10), 
                         ifelse(!is.na(heure), 
                                substr(heure, 11, 16), 
                                " 17:00"))) %>% 
    # TR - Michaël dit qu'il était au site entre 16h et 18h en général. J'ai mis 
    # 17h ici. 
    select(-heure)
  
  # calcule poids de la sève (i.e., différence de poids avec et sans sève) ----
  tmp_c <- tmp_c %>% 
    mutate(poids = poids_p - poids_v)
  
  # calcule le brix moyen dans la chaudière et au chalumeau ----
  tmp_c <- tmp_c %>% rowwise() %>%
    mutate(brix_chal = mean(c_across(brix_chal1:brix_chal3), na.rm = TRUE),
           brix_chau = mean(c_across(brix_chau1:brix_chau3), na.rm = TRUE))
  
  # détermine le brix pour estimer la densité ---
  if (!(site == 2 & année %in% 2022:2023)){
    if (!(site == 1 & année == 2022)) { # Pas de mesure de la glace pour le site 1 en 2022
      tmp_c <- tmp_c %>% 
        mutate(brix = ifelse (coulée, brix_chal, brix_chau * (1 - glace)))
    } else {
      tmp_c <- tmp_c %>% 
        mutate(brix = ifelse (coulée, brix_chal, brix_chau))
    } # TR - Je devrais développer une estimation empirique du brix en fonction de la glace
  }
  
  # estime le volume pour les années et sites où on a mesuré le poids ----------
  if((site == 1 & année == 2024) |
     (site == 2)){
    
    # densité en fonction du taux de saccharose (°Brix) d'après l'info-fiche 
    # d'Allard (1999) ----------------------------------------------------------
    tmp_c <- tmp_c %>%
      mutate(rho_s = (a_coef + (c_coef * brix)) / 
                     (1 + b_coef * brix + d_coef * brix**2),
             vol_s = poids * rho_s * 1e3) %>% 
      select(-rho_s)
  }
  
  # enlève des valeurs érronées ------------------------------------------------
  if(site == 1 & année == 2022) {
    
    # enlève taux de saccharose trop élevé le 2022-03-12 due au haut taux de glace 
    # dans les chaudières et elles ne sont pas répresentatives -------------------
    tmp_c$brix[tmp_c$date == as_date("2022-03-12")] <- NA
    
    # enlève des valeurs éronnées due à l'infiltration de précipitation dans les 
    # chaudières  --------------------
    tmp_c$brix[tmp_c$date == as_date("2022-03-07") & 
                 tmp_c$ar == 27] <- NA
    tmp_c$vol_s[tmp_c$date == as_date("2022-03-07") & 
                 tmp_c$ar == 27] <- NA
    tmp_c$brix[tmp_c$date == as_date("2022-03-18") & 
                 tmp_c$ar %in% c(16, 25, 27)] <- NA
    tmp_c$vol_s[tmp_c$date == as_date("2022-03-18") & 
                 tmp_c$ar %in% c(16, 25, 27)] <- NA
    tmp_c$brix[tmp_c$date == as_date("2022-04-18") & 
                 tmp_c$ar %in% c(15, 16, 27)] <- NA
    tmp_c$vol_s[tmp_c$date == as_date("2022-04-18") & 
                 tmp_c$ar %in% c(15, 16, 27)] <- NA
    tmp_c$brix[tmp_c$date == as_date("2022-04-19") & 
                 tmp_c$ar %in% c(1:3, 5:8, 14, 25, 32, 33)] <- NA
    tmp_c$vol_s[tmp_c$date == as_date("2022-04-19") & 
                  tmp_c$ar %in% c(1:3, 5:8, 14, 25, 32, 33)] <- NA
    tmp_c$brix[tmp_c$date == as_date("2022-04-30") & 
                 tmp_c$ar == 15] <- NA
    # TR- Je devrais éventuellement imputer le volume de sève 
      
    # chaudière à terre
    tmp_c$brix[tmp_c$date == as_date("2022-04-19") & 
                 tmp_c$ar %in% c(10, 31)] <- NA
    tmp_c$vol_s[tmp_c$date == as_date("2022-04-19") & 
                  tmp_c$ar %in% c(10, 31)] <- NA
  }
  
  # ajoute le site et l'année aux données et rénomme les commentaires ----------
  tmp_c <- tmp_c %>% mutate(site = factor(site), année = factor(année),
                            ar = factor(ar),
                            e = factor(e)) %>%
    rename(commentaires_c = commentaires)
  
  # ré-arrange les colonnes pour colliger les données --------------------------
  tmp_c <- tmp_c %>% 
    relocate(site, année, ar, e, date, coulée, glace, brix_chau1, brix_chau2, 
             brix_chau3, brix_chal1, brix_chal2, brix_chal3, commentaires_c, 
             vol_s, poids, brix_chal, brix_chau, brix)
  
  # collige tous les données des coulées du réseau acer-web --------------------
  if(s == 1){
    AW_c <- tmp_c
  } else {
    AW_c <- rbind(AW_c, tmp_c)
  }
  
  # lire les données de l'arbre ------------------------------------------------
  tmp_a <- read_excel(path = fichier, sheet = "02_arbres",
                      col_names = c("ar", "e", "nombre_e", "poids_c", "espèce",
                                    "dhp", "dhe", "hauteur_s", "orientation", 
                                    "profondeur", "écorce", "d_e", 
                                    "commentaires", "pastille", "espèce_latin", 
                                    "espèce_abbr", "hauteur_t"),
                      col_types =  c(
                        ar = "numeric",
                        e = "text",
                        nombre_e = "numeric",
                        poids_c = "numeric",
                        espèce = "text",
                        dhp = "numeric",
                        dhe = "numeric",
                        hauteur_s = "numeric",
                        orientation = "numeric",
                        profondeur = "numeric",
                        écorce = "numeric",
                        d_e = "numeric",
                        commentaires = "text",
                        pastille = "text",
                        espèce_latin = "text",
                        espèce_abbr = "text",
                        hauteur_t = "numeric"),
                      na = "NA",
                      skip = 2) 
  
  # ajoute l'année et le site aux données et renomme les commentaires ----------
  tmp_a <- tmp_a %>% mutate(site = factor(site), année = factor(année),
                            ar = factor(ar),
                            e = factor(e)) %>% 
    rename(commentaires_a = commentaires)
  
  # collige tous les données des arbres du réseau acer-web ---------------------
  if(s == 1){
    AW_a <- tmp_a
  } else {
    AW_a <- rbind(AW_a, tmp_a)
  }
  
  # lire les données du site ---------------------------------------------------
  tmp_s <- read_excel(path = fichier, sheet = "03_site",
                      col_names = c("site", "site_nom", "responsable", 
                                    "courriel", "mobile", "lat", "lon", "alt", 
                                    "date_entaillage", "heure_entaillage", 
                                    "date_désentaillage", "nombre_arbre", 
                                    "commentaires"),
                      col_types =  c(
                        site = "text",
                        site_nom = "text",
                        responsable = "text",
                        courriel = "text",
                        mobile = "text",
                        lat = "numeric",
                        lon = "numeric",
                        alt = "numeric",
                        date_entaillage = "date",
                        heure_entaillage = "date",
                        date_désentaillage = "date",
                        nombre_arbre = "numeric",
                        commentaires = "text"),
                      na = "NA",
                      skip = 2) %>%
    select(-responsable, -courriel, -mobile)
  
  # combiner la date et l'heure d'entaillage -----------------------------------
  tmp_s <- tmp_s %>% 
    mutate(date_entaillage = as_datetime(
      paste(substr(date_entaillage, 1, 10), 
            ifelse(!is.na(heure_entaillage),
                   substr(heure_entaillage, 12, 16),
                   " 12:00"), # Midi si on ne connait pas l'heure d'entaillage
            sep = " "), 
      format = "%Y-%m-%d %H:%M")) %>%
    select(-heure_entaillage, -site_nom, -nombre_arbre) %>% 
    rename(commentaires_s = commentaires)
  
  # ajoute l'année aux données -------------------------------------------------
  tmp_s <- tmp_s %>% mutate(site = factor(site), année = factor(année)) 
  
  # collige tous les données des sites du réseau acer-web ----
  if(s == 1){
    AW_s <- tmp_s
  } else {
    AW_s <- rbind(AW_s, tmp_s)
  }
}

# ajoute une colonne du jour de l'année (jda) au données acer-web --------------
AW_c <- AW_c %>% 
  add_column(jda = yday(as_datetime(AW_c$date, format = "%Y-%m-%d %H:%M")))

# combiner les données des sites, des arbres et des coulées --------------------
AW <- dplyr::left_join(AW_c, AW_a, by = c("ar", "e", "année", "site")) %>%
  dplyr::select(-brix_chal1, -brix_chal2, -brix_chal3, -brix_chau1, -brix_chau2, 
                -brix_chau3, -poids_p, -poids_v, -pastille, -espèce_latin, 
                -espèce) %>%
  dplyr::left_join(AW_s, by = c("année","site"))

# re-arrange les données pour faciliter la comparaison -------------------------
AW <- AW %>% 
  dplyr::select(-poids_c) %>%
  dplyr::relocate(site, ar, e, date, année, jda, lat, lon, 
                  alt, espèce_abbr, coulée, glace, vol_s, poids, brix_chal, 
                  brix_chau, brix, nombre_e, orientation, hauteur_s, hauteur_t, 
                  profondeur, d_e, dhp, dhe, écorce, date_entaillage, 
                  date_désentaillage, commentaires_c, commentaires_a, 
                  commentaires_s) %>%
  mutate(date_entaillage = as_date(date_entaillage),
         date_désentaillage = as_date(date_désentaillage))

# charger les données d'Harvard Forest -----------------------------------------
#HF_data_t1 <- read_csv("./données/HF/hf285-01-maple-tap_2.csv", col_types = cols())
#HF_data_s1 <- read_csv("./data/HF/hf285-02-maple-sap.csv", col_types = cols())
# utilise les lien ci-dessus une fois que les archives ont été mise à jour
HF_a <- readr::read_csv("./données/HF/HFmaple.tapping.2012_2022.csv",
                        col_types = readr::cols()) %>% 
  dplyr::mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))
HF_c <- readr::read_csv("./données/HF/HFmaple.sap.2012_2022.csv", 
                        col_types = readr::cols()) %>% 
  dplyr::mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))

# changer "HFR", qui signifie "Harvard Forest red maple", pour "AR", qui 
# signifiew "Acer rurbrum", pour assurer la consitence des jeux de données -----
HF_a <- HF_a %>% dplyr::mutate(
  ar = ifelse(substr(tree, 1, 3) == "HFR", 
              paste0("AR", substr(tree, 4, nchar(tree))),
              tree))

# renomme les colonnes ---------------------------------------------------------
HF_a <- HF_a %>% rename(e = tap, espèce_abbr = species, dhp = dbh, 
                        orientation = tap.bearing, hauteur_s = tap.height) %>% 
  select(-tree)
HF_c <- HF_c %>% rename(e = tap, ar = tree, heure = time, espèce_abbr = species, 
                        brix_chal = sugar, brix = sugar, poids = sap.wt) 

# ajoute des lignes pour les données de 2015, qui manquait ---------------------
# D'après les données de flux de sève, les arbres AR1, AR2, AR3, AR4, AR6, AR7, 
# AR9, AR10, HF35, et HF40 ont seulement une entaille --------------------------
HF_a <- HF_a %>% dplyr::add_row(
  date = as_date("2015-03-09"), # date d'entaillage en 2015 d'après Josh
  ar = c(rep(c("HF1", "HF4", "HF5", "HF6", "HF7", "HF9", "HF10", "HF12", "HF13", 
               "HF16", "HF21", "HF22", "HF23", "HF33",  "HF38",  "HF41", "HF43", 
               "AR5", "AR8"), each = 2), # ont deux entailles d'après les données de flux de sève
         c("HF35","HF40","AR1", "AR2", "AR3", "AR4","AR6", "AR7","AR9", 
           "AR10")), # ont une entaille d'après les données de flux de sève
  e = c(rep(c("A", "B"), 19), rep("A", 10)),
  espèce_abbr = c(rep("ACSA", 17*2), rep("ACRU", 2*2), rep("ACSA", 2), 
                  rep("ACRU", 8)),
  dhp = NA,
  orientation = NA,
  hauteur_s = NA)

# ajoute l'année de mesures aux les données spécifique à l'arbre ---------------
HF_a <- HF_a %>% dplyr::mutate(année = factor(lubridate::year(date)))

# ajoute une colonne avec le nombre d'entaille de l'arbre ----------------------
HF_a <- HF_a %>% dplyr::group_by(ar, année) %>% 
  dplyr::mutate(nombre_e = dplyr::case_when(
  "C" %in% e ~ 3,
  "B" %in% e ~ 2,
  "A" %in% e ~ 1,
)) %>% dplyr::ungroup()

# interpolation du diamètre à hauteur de poitrine avec regression linéaire -----
PLOT <- TRUE
for (ar in unique(HF_a$ar)) {
  con <- HF_a$ar == ar
  if (PLOT){
    par(mar = c(5, 5, 5, 1))
    plot(x = HF_a$date[con], 
         y = HF_a$dhp[con],
         xlab = "date", ylab = "dhp (cm)", 
         xlim = c(as_date("2012-01-01"), as_date("2023-01-01")), 
         ylim = c(min(HF_a$dhp[con], na.rm = TRUE) - 10, 
                  max(HF_a$dhp[con], na.rm = TRUE) + 10),
         axes = FALSE, pch = 19, col = "#91b9a4", main = ar)
    axis(side = 1, 
         at = c(as_date("2012-01-01"), as_date("2013-01-01"), as_date("2014-01-01"), 
                as_date("2015-01-01"), as_date("2016-01-01"), as_date("2017-01-01"), 
                as_date("2018-01-01"), as_date("2019-01-01"), as_date("2020-01-01"), 
                as_date("2021-01-01"), as_date("2022-01-01"), as_date("2023-01-01")),
         labels = 2012:2023)
    axis(side = 2, las = 1)
  }
  if (length(HF_a$date[con]) > 1) {
    fit <- lm(dhp ~ date, data = HF_a[con, ])
    if (PLOT) abline(fit, lty = 2, col = "#91b9a4")
  }
  # print coefficients ---------------------------------------------------------
  #print (fit$coefficients)
  
  # extrait les années pour lesquelles nous avons des données pour l'arbre -----
  années <- unique(lubridate::year(HF_c$date[HF_c$ar == ar]))
  
  # enlève les années pour lesquelles nous avons des mesures du dhp ------------
  années_dupli <- HF_a %>% 
    dplyr::filter(ar == ar & !is.na(dhp)) %>% 
    dplyr::select(année) %>% unlist() %>% unique()
  années_dupli <- as.numeric(levels(années_dupli))[années_dupli] # extraire les indices des années dupliquées
  années_dupli <- années_dupli[années_dupli %in% années] # enlève les années pour lesquelles nous n'avons pas de flux de sève
  années <- c(années, années_dupli) # enchaîne les années et les années dupliquées
  années <- années[!(années %in% années[duplicated(années)])]
  
  # création des dates pour l'interpolation du DHP -----------------------------
  if (length(années) > 0) {
    dates <- tibble::tibble(date = as_date(paste(années,"-03-01"))) 
    # disons le premier mars, car le DHP ne devrait pas croître en hiver.
  
    # interpoler le DHP à ces dates -----------------------------------------------
    for (a in années) {
      HF_a$dhp[con & HF_a$année == a] <- 
        predict(fit, dates[lubridate::year(dates$date) == a, ])  
      # plot the points to check that this works
      if (PLOT) {
        points(x = HF_a$date[con & HF_a$année == a],
               y = HF_a$dhp[con & HF_a$année == a], 
               pch = 1 , lwd = 1 , col = "#91b9a4")
      }
    }
  }
}

# TR - Je dois remesurer le DHP des arbres en 2024 pour assurer que les 
#      regressions linéaires ont de l'allure. Pour certains arbres, nous avons 
#      seulement deux dhp et la relation est negative ou trop à pic.
# supprimer les variables temporaires ------------------------------------------
rm(dates, fit, années_dupli)

# ajouter la date et le temps --------------------------------------------------
HF_c <- HF_c %>%  
  dplyr::mutate(date = ifelse(is.na(heure), 
                              paste(date, "16:00:00"),
                              paste(date, heure)))

# ajoute des colonnes pour le jour de l'année (jda) et le volume de sève (vol_s)
# calcul la densité de la sève en fonction du brix pour convertir le poids 
# en volume. J'utilise l'équation de l'info-fiche de Allard (1999) -------------
HF_c <- HF_c %>% 
  dplyr::mutate(
    jda = lubridate::yday(date),
    année = factor(lubridate::year(date)),
    rho_s = (a_coef + (c_coef * brix)) / 
      (1 + b_coef * brix + d_coef * brix**2),
    vol_s = poids * rho_s * 1e3,
    site = "HF")

# combiner les deux jeux de données (i.e., arbre et coulée) --------------------
HF <- dplyr::left_join(HF_c, HF_a, by = c("ar", "e", "année", "espèce_abbr")) %>% 
  dplyr::rename(date = "date.x",
         date_entaillage = "date.y") %>% 
  tibble::add_column(
    profondeur = 5.08, # 2 inches
    d_e = 0.79375, # 5/16" drill bit
    lat = 42.53321,
    lon = -72.19090,
    alt = 338)

# d'après Josh la dernière journée de collecte de données était également la 
# date de désentaillage --------------------------------------------------------
temp <- HF %>% dplyr::group_by(ar, e, année) %>% 
  dplyr::summarise(date_désentaillage = max(date), .groups = "drop") %>%
  dplyr::mutate(date_désentaillage = as_date(date_désentaillage))

# ajouter la date de désentaillage au données ----------------------------------
HF <- dplyr::left_join(HF, temp, by = c("ar", "e", "année"))
rm(temp)

# Corriger des fautes d'orthographe dans les données (i.e., 22 to 2.2) ---------
HF$brix[which(HF$ar == "HF33" & 
              HF$date == as_datetime("2018-03-24 16:00:00 UTC") &
              HF$e == "A")] <- 2.2

# Ajoute les colonnes manquantes -----------------------------------------------
HF <- HF %>% mutate(coulée = NA, glace = NA, brix_chal = brix, brix_chau = NA,
                    hauteur_t = NA, dhe = NA, écorce = NA, commentaires_c = NA, 
                    commentaires_a = NA, commentaires_s = NA)

# re-arranger les données de HF pour faciliter la comparaison ------------------
HF <- HF %>% 
  dplyr::relocate(site, ar, e, date, année, jda, lat, lon, alt, espèce_abbr, 
                  coulée, glace, vol_s, poids, brix_chal, brix_chau, brix, 
                  nombre_e, orientation, hauteur_s, hauteur_t, profondeur, d_e, 
                  dhp, dhe, écorce, date_entaillage, date_désentaillage, 
                  commentaires_c, commentaires_a, commentaires_s) %>% 
  dplyr::select(-heure)

# compile different data sets---------------------------------------------------
d <- dplyr::full_join(AW, HF, 
                      by = c("site", "ar", "e", "date", "année", "jda", "lat", 
                             "lon", "alt", "espèce_abbr", "coulée", "glace", 
                             "vol_s", "poids", "brix_chal", "brix_chau", "brix", 
                             "nombre_e", "orientation", "hauteur_s", 
                             "hauteur_t", "profondeur", "d_e", "dhp", "dhe", 
                             "écorce", "date_entaillage", "date_désentaillage", 
                             "commentaires_c", "commentaires_a", "commentaires_s")) 

# read AcerNet data ------------------------------------------------------------
# N.B.: This data does not include tree sizes or any metadata. It is only sap 
# flow data --------------------------------------------------------------------
# 
# Site PIs:
# Harvard Forest                   - Joshua Rapp (jrapp@massaudubon.org)
# Dartmouth Organic Farm           - David A. Lutz (david.a.lutz@dartmouth.edu)
# Quebec                           - Boris Dufour (Boris_Dufour@uqar.ca)
# Divide Ridge                     - Ryan Huish (rdh5b@uvawise.edu)
# Indiana Dunes National Lakeshore - Wendy Smith (wendy_w_smith@nps.gov)
# Southernmost Maple               - Ryan Huish (rdh5b@uvawise.edu)
# Composition : Selena Ahmed (selena.ahmed@montana.edu)
# ------------------------------------------------------------------------------ 
AN_data <- readr::read_csv("./data/AcerNet/ACERnet_sap_2012_2017_ID.csv", 
                    col_types = readr::cols()) %>% 
  dplyr::mutate(date = as_date(Date, format = "%m/%d/%Y"),
         doy = lubridate::yday(date),
         sap_volume = Sap.Wt * 1000, # see note on conversion in HF data
         sap_brix = Sugar,
         site = Site.ID,
         tap = Tap,
         tree = Tree,
         spp = Species,
         year = factor(Year)) %>%
  dplyr::select(-Sugar, -Sap.Wt, -Date, -Site, -Species, -Tree, -Tap, -Year, -Site.ID, 
         -Tree.ID, -Tree.Record.ID, -Tap.Record.ID)

# filter out Harvard Forest data, which was obtained independently -------------
AN_data <- AN_data %>% dplyr::filter(site != "HF")

# add datetime, time, and dbh columns ------------------------------------------
AN_data <- AN_data %>% tibble::add_column(datetime = NA, time = NA, dbh = NA)

# add column with number of taps -----------------------------------------------
AN_data <- AN_data %>% dplyr::group_by(tree, year) %>% 
  dplyr::mutate(n_taps = dplyr::case_when(
    "C" %in% tap ~ 3,
    "B" %in% tap ~ 2,
    "A" %in% tap ~ 1,
)) %>% dplyr::ungroup()

# add column with additional information from publication (Rapp et al., 2019) --
AN_data <- AN_data %>% 
  tibble::add_column(
    tap_date = NA,    # N.B.: TR - I still hope to get these from Josh
    tap_removal = NA, # N.B.: TR - I still hope to get these from Josh
    tap_depth = 5.08, # 2" 
    tap_width = 0.79375, # 5/16" drill bit and spile
    tap_bearing = NA,
    tap_height = NA)    

# add latitude and longitude of sites and estimated altitude -------------------
AN_data <- AN_data %>% 
  dplyr::mutate(
    lat = dplyr::case_when(
      site == "DOF"  ~ 43.734,  # Dartmouth Organic Farm
      site == "QC"   ~ 48.431,  # Quebec
      site == "DR"   ~ 37.011,  # Divide Ridge
      site == "INDU" ~ 41.625,  # Indiana Dunes National Lakeshore
      site == "SMM"  ~ 38.231), # Southernmost Maple
    lon = dplyr::case_when(
      site == "DOF"  ~ -72.249,  # Dartmouth Organic Farm
      site == "QC"   ~ -70.688,  # Quebec
      site == "DR"   ~ -82.676,  # Divide Ridge
      site == "INDU" ~ -87.081,  # Indiana Dunes National Lakeshore
      site == "SMM"  ~ -79.658), # Southernmost Maple
    alti = dplyr::case_when( # from elevation finder
      site == "DOF"  ~ 271.0,  # Dartmouth Organic Farm
      site == "QC"   ~ 243.0,  # Quebec
      site == "DR"   ~ 634.5,  # Divide Ridge
      site == "INDU" ~ 198.0,  # Indiana Dunes National Lakeshore
      site == "SMM"  ~ 837.5)) # Southernmost Maple

# re-arrange HF data for ease of comparison ------------------------------------
AN_data <- AN_data %>% 
  dplyr::relocate(site, tree, tap, date, time, datetime, year, doy, lat, lon, 
                  alti, spp, sap_volume, sap_brix, n_taps, tap_bearing, 
                  tap_height, tap_depth, tap_width)

# combine Acer Net data with other data ----------------------------------------
sap_data <- dplyr::full_join(
  sap_data, AN_data, by = c("site", "tree", "tap", "date", "time", "datetime",
                             "year", "doy", "lat", "lon", "alti", "spp", 
                             "sap_volume", "sap_brix", "n_taps", "tap_bearing", 
                             "tap_height", "tap_depth", "tap_width","tap_date", 
                             "tap_removal","dbh"))

# read raw data from Mont Valin rain gauges for two trees ----------------------
MV_data <- readr::read_csv2(file = "./data/MontValin/monts_valin_data_cleaned.csv",
                    col_types = c("cccdddD")) %>% 
  dplyr::select(-rain_inc, -event_counts)

# add datetime to enable interval association ----------------------------------
MV_data <- MV_data %>% dplyr::mutate(
  datetime = as_datetime (
    paste(date, time), 
    format = "%Y-%m-%d %H:%M", 
    tz = "EST"))

# create column with days since 2022-01-01 5:00, which I should use as daily 
# interval (e.g., 5:00 to 4:59) to separate individual thaw event-related flow 
# histogram shows that the least likely time for sapflow is at 05:00 am so I 
# used that as separator for "daily" flow 
MV_data <- MV_data %>% 
  dplyr::mutate(
    day_intervals = floor(datetime - as_datetime("2022-01-01 05:00:00", 
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "EST")))

# calculate "daily" (between 5am and 4:59am) flow ------------------------------
MV_data <- MV_data %>% dplyr::group_by(site, tree, tap, day_intervals) %>% 
  dplyr::summarise(sap_volume = sum(sap_volume_inc, na.rm = TRUE),
                   .groups = "drop") %>% 
  dplyr::mutate(doy = as.integer(day_intervals)) %>% 
  dplyr::select(-day_intervals)

# add year and the dbh for each tree -------------------------------------------
MV_data <- MV_data %>% 
  dplyr::mutate(date = lubridate::as_date("2022-01-01") + doy,
                time = "05:00",
                datetime = as_datetime(paste(date, time), 
                                       format = "%Y-%m-%d %H:%M", 
                                       tz = "EST"),
                year = factor(lubridate::year(date)),
                doy = lubridate::yday(date), 
                dbh = dplyr::case_when(tree == 1 ~ 22,
                                       tree == 2 ~ 16))

# add required columns based on information from Sara --------------------------
MV_data <- MV_data %>% tibble::add_column (
  lat = 48.63011129292363, 
  lon = -70.8875769500555, 
  alti = 220, 
  tap_date = as_date("2022-03-17"), 
  tap_removal = as_date("2022-06-01"), 
  sap_brix = NA, 
  n_taps = 1, 
  spp = "ACSA", 
  tap_bearing = NA, 
  tap_depth = 5, # between 5 and 6cm according to Sara
  tap_height = NA, # they were tapped at breast height (supposedly 1.3m)
  tap_width = 0.79375) # 5/16" spout driilled with 19/64 drill bit

# re-arrange order to match the sap_data tibble --------------------------------
MV_data <- MV_data %>% 
  dplyr::relocate(site, tree, tap, date, time, datetime, year, doy, lat, lon, 
                  alti, spp, sap_volume, sap_brix, n_taps, tap_bearing, 
                  tap_height, tap_depth, tap_width, tap_date, tap_removal, dbh)

# combine Monts Valin data with other data sets --------------------------------
sap_data <- dplyr::full_join(
  sap_data, MV_data, by = c("site", "tree", "tap", "date", "time", "datetime", 
                            "year", "doy", "lat", "lon", "alti", "spp", 
                            "sap_volume", "sap_brix", "n_taps", "tap_bearing", 
                            "tap_height", "tap_depth", "tap_width", "tap_date", 
                            "tap_removal", "dbh"))

# read Élise raw data for Outaouais --------------------------------------------
OU_data_t <- readr::read_delim(
  file = "./data/Outaouais/Erables.txt", 
  delim = "\t", 
  col_types = readr::cols())
OU_data_s2020 <- readr::read_delim(
  file = "./data/Outaouais/CH2020_data.txt", 
  delim = "\t", 
  col_types = readr::cols()) %>% 
  tibble::add_column(Comments = NA)
OU_data_s2021 <- readr::read_delim(
  file = "./data/Outaouais/CH2021_data.txt", 
  delim = "\t", 
  col_types = readr::cols())

# combine data sets for 2020 and 2021 and add year column ----------------------
OU_data_s <- rbind(OU_data_s2020, OU_data_s2021) %>% 
  dplyr::mutate(year = factor(lubridate::year(Date)))

# add dbh column ---------------------------------------------------------------
OU_data <- dplyr::left_join(OU_data_s, OU_data_t, by = "ID")

# rename columns for consistency -----------------------------------------------
OU_data <- OU_data %>% dplyr::rename(
  sap_brix = Sugar,
  sap_volume = Volume,
  date = Date,
  tree = ID,
  dbh = `DBH `) %>% 
  dplyr::filter(is.na(Comments)) %>% 
  dplyr::select(-Comments)

# add additional columns with site info ----------------------------------------
OU_data <- OU_data %>% tibble::add_column(
  site = "OU",
  tap = "A",
  time = "19:00", # these are aggregated from 19:00 of the previous 
                  # day to 19:00h of the current day
  lat = 45.954444,
  lon = -74.863611,
  alti = 233, # according to Christian Messier
  n_taps = 1, 
  spp = "ACSA",
  tap_depth = 5.0, # or 2"
  tap_height = 140, # Approximately at breast height
  tap_width = 0.79375 # or 5/16"
)

# Change species for incorrectly-identified tree (51) according to Élise -------
OU_data$spp[OU_data$tree == 51] <- "ACRU"

# add columns for datetime, year, tap_date, tap_removal, doy, tap_bearing ------
OU_data <- OU_data %>% 
  dplyr::mutate(
    tree = factor(tree),
    date = as_date(date),
    year = factor(lubridate::year(date)),
    tap_date = as_date(ifelse(year == 2020,
                              "2020-02-29", # Possibly already started 2020-02-29 for some 
                              "2021-03-06")),
    tap_removal = as_date(ifelse(year == 2020,
                                 "2020-04-17", # Might also have been 2020-04-16 
                                 "2021-04-27")),
    datetime = as_datetime(paste(date, time), 
                           format = "%Y-%m-%d %H:%M", 
                           tz = "EST"),
    doy = lubridate::yday(date),
    tap_bearing = ifelse(year == 2020, 90, # East
                         180)) # South

# set two values for sap brix to NA, as they are high, but come from 5 and 1 ml 
# sample respectively, thus are probably measurement errors --------------------
OU_data$sap_brix[which(OU_data$tree %in% c("7","15") & 
                         OU_data$date == as_date("2020-03-05"))] <- NA

# re-arrange order to match the sap_data tibble --------------------------------
OU_data <- OU_data %>% 
  dplyr::relocate(site, tree, tap, date, time, datetime, year, doy, lat, lon, 
                  alti, spp, sap_volume, sap_brix, n_taps, tap_bearing, 
                  tap_height, tap_depth, tap_width, tap_date, tap_removal, dbh)

# combine Monts Valin data with other data sets --------------------------------
sap_data <- dplyr::full_join(
  sap_data, OU_data, by = c("site", "tree", "tap", "date", "time", "datetime", 
                            "year", "doy", "lat", "lon", "alti", "spp", 
                            "sap_volume", "sap_brix", "n_taps","tap_bearing", 
                            "tap_height", "tap_depth", "tap_width", "tap_date", 
                            "tap_removal", "dbh"))


# add days since tapping column to data ----------------------------------------
sap_data <- sap_data %>% dplyr::mutate(
  days_since_tapping = as.integer(difftime(date, tap_date, units = "days"))
)

# make sure all trees and taps have unique IDs ---------------------------------
sap_data <- sap_data %>% 
  dplyr::mutate(tree = paste(site, tree, sep = "_"),
                tap = paste(site, tree, tap, sep = "_"))

# create a seasonal summary for each tap ---------------------------------------
seasonal_data <- sap_data %>% 
  dplyr::group_by(site, tree, tap, year) %>%
  dplyr::summarise(
    lat = mean(lat, na.rm = TRUE),
    spp = unique(spp),
    sap_volume = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
    sap_brix = mean(sap_brix, na.rm = TRUE),
    tap_depth = mean(tap_depth, na.rm = TRUE),
    tap_width = mean(tap_width, na.rm = TRUE),
    n_taps = as.integer(mean(n_taps, na.rm = TRUE)),
    tap_bearing = mean(tap_bearing, na.rm = TRUE),
    tap_height = mean(tap_height, na.rm = TRUE),
    dbh = mean(dbh, na.rm = TRUE),
    .groups = "drop")

# remove the data for taps that did have no sap flow at all --------------------
seasonal_data <- seasonal_data %>% dplyr::filter(sap_volume > 0)

# create log_yield variable to avoid fitting a log-normal-----------------------
seasonal_data$log_yield <- log(seasonal_data$sap_volume)

# remove single data point from Norway maple -----------------------------------
seasonal_data <- seasonal_data %>% filter(spp != "ACPL")

# group by sites and year to get sao run dates for each location ---------------
mid_season <- sap_data %>% 
  dplyr::group_by(site, year) %>% 
  dplyr::filter(sap_volume > 0) %>% 
  dplyr::select(site, year, doy) %>% 
  dplyr::distinct() %>% 
  dplyr::summarise(median_doy = floor(median(doy)), .groups = "drop")

# add median doy for sugaring season to sap_data -------------------------------
sap_data <- dplyr::left_join(sap_data, mid_season, by = c("site", "year"))

# aggregate early-season data --------------------------------------------------
early_data <- sap_data %>% 
  dplyr::filter(doy <= median_doy) %>% # only days before or on the median sap run day
  dplyr::group_by(site, tree, tap, year) %>%
  dplyr::summarise(sap_volume_e = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
                   sap_brix_e = mean(sap_brix, na.rm = TRUE),
                   .groups = "drop")
  
# aggregate late-season data ---------------------------------------------------
late_data <- sap_data %>% 
  dplyr::filter(doy > median_doy) %>% # only days after the median sap run day
  dplyr::group_by(site, tree, tap, year) %>%
  dplyr::summarise(sap_volume_l = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
                   sap_brix_l = mean(sap_brix, na.rm = TRUE),
                   .groups = "drop")
  
# add early- and late-season data to seasonal_data -----------------------------
seasonal_data <- seasonal_data %>% 
  dplyr::left_join(early_data, by = c("site", "tree", "tap", "year")) %>%
  dplyr::left_join(late_data, by = c("site", "tree", "tap", "year"))

# plot some general charcteristics to make sure all works fine -----------------
PLOT <- FALSE
if(PLOT){
  # histogram of sap volume ----------------------------------------------------
  par(mar = c(5, 5, 1, 1))
  hist(sap_data %>% select(sap_volume) %>% unlist(),
       xlab = "Sap volume (ml)", main = "", col = "#CC724066")
  
  # histogram of sap brix ------------------------------------------------------
  hist(sap_data %>% select(sap_brix) %>% unlist(), 
       breaks = seq(0, 2000, by = 0.2), xlim = c(0, 8), col = "#CC724066",
       xlab = expression(paste("Sap succrose concentration (",degree,"Brix)", sep = "")),
       main = "", lty = 1)
  abline(v = median(sap_data$sap_brix, na.rm = TRUE), col = "#94452E", lwd = 2)
  
  # plot early- versus late-season sap yield -----------------------------------
  plot(x = seasonal_data$sap_volume_e,
       y = seasonal_data$sap_volume_l, pch = 19, col = "#91b9a466",
       xlab = "Early-season sap yield (L)", 
       ylab = "Late-season sap yield (L)", 
       axes = FALSE)
  axis(side = 1)
  axis(side = 2, las = 1)
  abline(b = 1, a = 0, col = "#999999", lty = 2, lwd = 2)
  # TR - Is whether they fall above or below the line determined by the site latitude?
  
  # plot early- versus late-season sap brix ------------------------------------
  plot(x = seasonal_data$sap_brix_e,
       y = seasonal_data$sap_brix_l, pch = 19, col = "#91b9a466",
       xlab = expression(paste("Early-season sugar content (",degree,"Brix)", sep = "")), 
       ylab = expression(paste("Late-season sugar content (",degree,"Brix)", sep = "")), 
       axes = FALSE, xlim = c(0, 8), ylim = c(0, 8))
  axis(side = 1)
  axis(side = 2, las = 1)
  abline(b = 1, a = 0, col = "#999999", lty = 2, lwd = 2)
}

# remove Norway maple for now --------------------------------------------------
sap_data <- sap_data %>% filter(spp != "ACPL")

# get some basic stats for intro -----------------------------------------------
sap_data %>% dplyr::filter(sap_volume > 0 & !is.na (sap_volume)) %>% dplyr::count() # number of daily sap volume measurements
sap_data %>% dplyr::filter(!is.na (sap_brix)) %>% dplyr::count() # number of daily sugar content measurements
sap_data %>% dplyr::group_by(site, tree, tap, year) %>% dplyr::n_groups() # number of taps, as taps differ by year 
sap_data %>% dplyr::group_by(site, tree, year) %>% dplyr::n_groups() # number of tree years
sap_data %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of different tree years
sap_data %>% dplyr::group_by(site) %>% dplyr::n_groups() # number of sites
sap_data %>% dplyr::group_by(year) %>% dplyr::n_groups() # number of years
sap_data %>% dplyr::filter(spp == "ACSA") %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of sugar maples
sap_data %>% dplyr::filter(spp == "ACRU") %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of red maples
sap_data %>% dplyr::filter(spp == "ACPL") %>% dplyr::group_by(site, tree) %>% dplyr::n_groups() # number of Norway maples

# clean working space ----------------------------------------------------------
rm(con, PLOT, sheet_url, t, y, yrs, AN_data, AW_data_s, AW_data_t, 
   AW_data_w, AW_site_data, HF_data, HF_data_s, HF_data_t, mid_season, MV_data,
   OU_data, OU_data_s, OU_data_s2020, OU_data_s2021, OU_data_t)

#===============================================================================

