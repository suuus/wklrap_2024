library(HHSKwkl)
library(tidyverse)
library(leaflet)
library(glue)

theme_set(hhskthema())  

ws_grens <- sf::st_read("data/ws_grens.gpkg", crs = 28992) %>% sf::st_transform(crs = 4326)

fys_chem <- readRDS("data/fys_chem.rds")
meetpunten <- import_meetpunten("data/meetpunten.csv")
parameters <- import_parameters("data/parameters.csv")
pfas_rpf <- readxl::read_excel("data/rpf_pfas.xlsx")

risicogrenzen_rivm <- readxl::read_excel("data/overige_pfasnormen.xlsx") %>% 
  filter(normtype == "voorstel_RIVM") %>% 
  mutate(aquo_parcode = case_when(
    aquo_parcode == "PFOS"    ~ "slinvertPFOS",
    aquo_parcode == "PFOA"    ~ "slinvertPFOA",
    aquo_parcode == "L_PFHxS" ~ "slinverPFHxS",
    TRUE ~ aquo_parcode
  ))

pfas <- 
  fys_chem %>% 
  add_jaar_maand() %>% 
  filter(jaar == 2023) %>% 
  inner_join(filter(parameters, cluster == "PFAS")) %>% 
  mutate(par = str_replace(par, "FRD-903", "GenX"))

pfas_eq <-
  pfas %>% 
  left_join(pfas_rpf, by = "aquo_parcode") %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  summarise(pfoa_equivalent = sum(pfoa_equivalent, na.rm = TRUE), .by = c(mp, datum)) %>%
  summarise(pfoa_equivalent = mean(pfoa_equivalent, na.rm = TRUE), .by = mp) %>% 
  mutate(pfoa_eq_radius = case_when(
    pfoa_equivalent < 50 ~ 7,
    pfoa_equivalent < 100 ~ 7,
    pfoa_equivalent < 200 ~ 9,
    pfoa_equivalent < 1000 ~ 9,
    pfoa_equivalent < 2000 ~ 9,
    pfoa_equivalent < 10000 ~ 9,
  )) %>% 
  left_join(meetpunten)

pal <- colorBin(RColorBrewer::brewer.pal(8, "Reds")[3:8], bins = c(20,50,100,200,1000,2000,10000))

kaart_pfoa_eq <-
  pfas_eq %>% 
  arrange(pfoa_equivalent) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  sf::st_transform(crs = 4326) %>% 
  basiskaart(type = "cartolight") %>%
  addPolylines(data = ws_grens, color = "#616161", weight = 3, label = ~"waterschapsgrens") %>%
  addCircleMarkers(label = ~glue("{signif(pfoa_equivalent, digits = 2)} ng PEQ/l"), 
                   radius = ~pfoa_eq_radius, weight = 1, 
                   fillOpacity = 1, fillColor = ~pal(pfoa_equivalent), 
                   opacity = 1, color = "#616161") %>% 
  addLegend(pal = pal, values = ~pfoa_equivalent, opacity = 1, title = "PFOA-equivalenten",
            labFormat = labelFormat(suffix = " ng PEQ/l", big.mark = ".")) %>% 
  leaflet.extras::addFullscreenControl()

plot_grenswaarden <- 
  pfas %>% 
  summarise(waarde = mean(waarde), .by = c(mp, aquo_parcode)) %>% 
  inner_join(risicogrenzen_rivm, by = "aquo_parcode") %>% 
  summarise(risico_tot = sum(waarde / normwaarde), 
            .by = mp) %>%
  ggplot(aes(x = risico_tot)) + 
  geom_dotplot(dotsize = 1, binwidth = 0.05, binaxis='x', fill = oranje) +
  scale_x_log10(labels = scales::label_number(big.mark = ".", suffix = ""), breaks = scales::breaks_log(12), limits = c(500, NA)) +
  labs(title = "PFAS t.o.v. de nieuwe risicogrenzen",
       x = "Aantal keer de risicogrenswaarde (logaritmisch)") +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") 

zwem_namen <- tibble::tribble(
  ~mp,                        ~Zwemlocatie,
  "S_0040",              "Zevenhuizerplas",
  "S_0067",               "Kralingse Plas",
  "S_0125",             "Bleiswijkse Zoom",
  "S_0144",              "Eendragtspolder",
  "S_1120",             "'t Zwarte Plasje",
  "S_1124",             "Kralings Zwembad",
  "K_1102",                "Krimpenerhout"
) %>% 
  maak_opzoeker()

plot_zwem_pfas <- 
  pfas_eq %>% 
  mutate(Zwemlocatie = zwem_namen(mp)) %>% 
  filter(!is.na(Zwemlocatie)) %>% 
  select(Zwemlocatie, pfoa_equivalent) %>% 
  mutate(frac_tdi = pfoa_equivalent / 849) %>% 
  mutate(Zwemlocatie = fct_reorder(Zwemlocatie, frac_tdi, .fun = max)) %>% 
  ggplot(aes(frac_tdi, Zwemlocatie)) + 
  geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), 
                     labels = scales::label_percent(), position = "top",
                     breaks = scales::pretty_breaks(7),
                     sec.axis = sec_axis(trans = \(waarde) waarde * 849, name = "PFOA-equivalenten (ng/l)")) +
  labs(title = "Risico van PFAS voor zwemmen",
       x = "Percentage van de toelaatbare dagelijkse inname",
       y = "",
       caption = "Volgens het scenario van een kind van 15,7 kg dat 25 keer per jaar zwemt en per keer 0,17 l binnenkrijgt.") +
  theme(panel.spacing.x = unit(15, "points"),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())
