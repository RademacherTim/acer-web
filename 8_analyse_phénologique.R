if (!existsFunction("read_excel")) library ("readxl")
if (!existsFunction("as_datetime")) library ("lubridate")

data_p <- read_excel(path = "./données/acer-web/coulée/Fiche de données acer-web site #1 2023.xlsx",
                     sheet = "05_phénologie_automne", skip = 2,
                     col_names = c("a", "date", "heure", "cc", "col", "lf"))

# plot leaf colour change
plot(x = data_p$date[data_p$a == 1],
     y = data_p$cc[data_p$a == 1], typ = "l", col = "white",
     axes = F, 
     xlim = c(as_datetime("2023-09-01"), as_datetime("2023-11-01")),
     ylim = c(0, 1),
     xlab = "date", ylab = "Colour change (%)")
axis(side = 1, at = c(as_datetime("2023-09-01"), 
                      as_datetime("2023-10-01"), 
                      as_datetime("2023-11-01")), 
     labels = c("Sep", "Oct", "Nov"))
axis(side = 2, las = 1)
for (a in 36:45){
     #45){
  lines(x = data_p$date[data_p$a == a],
        y = data_p$cc[data_p$a == a])
}

# plot leaf fall ----
plot(x = data_p$date[data_p$a == 1],
     y = data_p$lf[data_p$a == 1], typ = "l", col = "white",
     axes = F, 
     xlim = c(as_datetime("2023-09-01"), as_datetime("2023-11-01")),
     ylim = c(0, 1),
     xlab = "date", ylab = "Colour change (%)")
axis(side = 1, at = c(as_datetime("2023-09-01"), 
                      as_datetime("2023-10-01"), 
                      as_datetime("2023-11-01")), 
     labels = c("Sep", "Oct", "Nov"))
axis(side = 2, las = 1)
for (a in 1:45){
  #45){
  lines(x = data_p$date[data_p$a == a],
        y = data_p$cc[data_p$a == a])
}
