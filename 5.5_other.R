# how variable are individual trees
range(ranef(mod5.1.1)$tree [, , "Intercept"][, 1])

# how variable are individual years
range(ranef(mod5.1.1)$year [, , "Intercept"][, 1])

# extract site and tree IDs ----------------------------------------------------
coef <- ranef(mod5.1.1)$tree [, , "Intercept"]
s_and_t <- strsplit(rownames(ranef(mod5.1.1)$tree [, , "Intercept"]), split = "_")
s <- map(s_and_t, pluck, 1) %>% unlist()
t <- map(s_and_t, pluck, 2) %>% unlist()

# look at range of coefficients within a site ----------------------------------
add_column(.data = as_tibble(coef), site = s, tree = t) %>% 
  group_by(site) %>%
  summarise(range = range(Estimate, na.rm = TRUE),
            .groups = "keep")
