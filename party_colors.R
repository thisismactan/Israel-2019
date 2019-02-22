## Create palette of party colors
party_list <- c("kadima", "labor", "shas", "likud", "likud_yb", "yisrael_beiteinu", "ual_taal", "taal", "hadash", "balad", 
                "jewish_home", "national_union", "utj", "meretz", "yesh_atid", "hatnuah", "kulanu", "joint_list", "gesher", 
                "hosen_yisrael", "new_right", "zionist_union", "arab_parties", "zehut", "taal_hadash", "balad_raam", "blue_white",
                "urwp")
party_labels <- c("Kadima", "Labor", "Shas", "Likud", "Likud Beiteinu", "Yisrael Beiteinu", "United Arab List", "Ta'al", "Hadash", "Balad", 
                  "Jewish Home", "National Union", "United Torah Judaism", "Meretz", "Yesh Atid", "Hatnuah", "Kulanu", "Joint List", "Gesher", 
                  "Hosen L'Yisrael", "New Right", "Zionist Union", "Arab parties", "Zehut", "Ta'al / Hadash", "Balad / Ra'am", "Blue and White",
                  "URWP")

names(party_labels) <- party_list

party_palette <- c("indianred1", "red", "deepskyblue", "dodgerblue4", "dodgerblue4", "darkslateblue", "darkgreen", "yellow", "darkred", "darkorange2",
                   "darkblue", "blue", "black", "greenyellow", "cyan1", "indianred1", "darkturquoise", "darkgreen", "lightsteelblue",
                   "olivedrab", "blue", "red", "darkgreen", "darkorange1", "yellow", "green4", "#2865FC", "darkblue")

names(party_palette) <- party_list

## Party data frame
parties_df <- tibble(party = party_list,
                     party_full = party_labels,
                     party_color = party_palette)

## Parties running each year
parties2009 <- c(1:4, 6, 11:14, 23)
parties2013 <- c(2, 3, 5, 7, 11, 13:16, 23)
parties2015 <- c(3, 4, 6, 11, 13:15, 17, 18, 22)
parties2019 <- c(2:4, 6, 11, 13:14, 17, 19, 21, 24:28)

## Facet wrapper labels
party_facet_labels <- c("taal_hadash" = "Ta'al / Hadash",
                        "balad_raam" = "Balad / Ra'am",
                        "meretz" = "Meretz",
                        "hatnuah" = "Hatnuah",
                        "labor" = "Labor",
                        "blue_white" = "Blue and White",
                        "kulanu" = "Kulanu",
                        "gesher" = "Gesher",
                        "yisrael_beiteinu" = "Yisrael Beiteinu",
                        "likud" = "Likud",
                        "shas" = "Shas",
                        "utj" = "United Torah Judaism",
                        "zehut" = "Zehut",
                        "new_right" = "New Right",
                        "urwp" = "Union of Right-Wing Parties")

bloc_labels <- c("left" = "Left / Center-left",
                 "center" = "Center",
                 "center_right" = "Center-right",
                 "right" = "Right",
                 "ultraorthodox" = "Haredi / Ultra-Orthodox",
                 "arab" = "Arab parties")
