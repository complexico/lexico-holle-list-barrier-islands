library(tidyverse)
library(readxl)
library(stringi)

# read the holle list database =====
holle_tb <- read_tsv("https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/data/digitised-holle-list-in-stokhof-1980.tsv")
holle_1904_tb <- read_tsv("https://raw.githubusercontent.com/engganolang/digitised-holle-list/refs/heads/main/data/digitised-holle-list-in-stokhof-1980-add-1904_1911.tsv")
holle_1931_tb <- read_tsv("https://raw.githubusercontent.com/engganolang/digitised-holle-list/refs/heads/main/data/digitised-holle-list-in-stokhof-1980-add-1931.tsv")
## add a tibble containing the ordered factor of Holle's ID to re-order the word list
holle_index <- tibble(ID = c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index),
                      ORDERS = factor(c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index),
                                      levels = c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index)))

# generate the holle list gloss database across all versions =====
holle_gloss <- holle_tb |> 
  select(Index, Dutch, English, Indonesian) |> 
  mutate(cats = "NBL") |> 
  bind_rows(holle_1904_tb |> 
              select(Index, Dutch, English, Indonesian) |> 
              mutate(cats = "1904",
                     Index = as.character(Index))) |> 
  bind_rows(holle_1931_tb |> 
              select(Index, Dutch, English, Indonesian) |> 
              mutate(cats = "1931",
                     Index = as.character(Index))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(holle_match = str_c(str_trim(Index, "both"),
                             str_trim(English, "both"),
                             str_trim(Indonesian, "both"),
                             sep = "_"),
         holle_match = str_to_lower(holle_match),
         Index = factor(Index, levels = Index))
  

# list the file
files <- dir(pattern = ".+\\.txt|xlsx", recursive = TRUE)
# files
# write_lines(files, "files_list.txt")

# Nias 1905 ====
nias1905note <- files |> 
  str_subset("Nias 1905") |> 
  str_subset("\\/Nias 1905-notes")
nias1905note_tb <- readxl::read_xlsx(nias1905note) |> 
  mutate(NOTE_ID = as.character(NOTE_ID)) |> 
  rename(nt = NOTE_ID)

nias1905files <- files |> 
  str_subset("Nias 1905") |> 
  str_subset("[nN]otes?", negate = TRUE)
nias1905 <- map(nias1905files, read_lines) |> 
  unlist()

### check the marker present ====
nias1905 |> 
  str_extract("^\\\\[^ ]+?(?=\\s)") |> 
  unlist() |> 
  unique()

nias1905[which(!nzchar(nias1905))] <- "<separator>"
nias1905a <- nias1905 |> 
  str_c(collapse = "__") |> 
  str_split("\\<separator\\>") |> 
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace_all("(^__|__$)", "") |> 
  # fix entries
  str_replace("(1080)\\/(1081)", "\\1-\\2")

nias1905a <- if_else(str_detect(nias1905a, "\\\\sn\\s[1-9]"),
        str_replace_all(nias1905a, "__(\\\\(ps|de|dv))", "***\\1"),
        nias1905a)
nias1905a <- if_else(str_detect(nias1905a, "\\\\sn\\s[1-9]"),
        str_replace_all(nias1905a, "__(\\\\sn [2-9])", "; \\1"),
        nias1905a)
## handle anomaly in this data as the first six IDs are duplicated across the three students
### they could be remnant of one student's work provide example mode for the other twos who then forgot to delete these models
nias1905a |> length()
nias1905a |> unique() |> length()
nias1905a <- unique(nias1905a)
### yet still duplicated IDs
nias1905_still_duplicated_id <- nias1905a |> 
  str_extract("ID_en \\d+_") |> 
  table() |> 
  sort() |> 
  rev() |> 
  (\(x) x[x>1])()
nias1905a_tb <- tibble(mc = nias1905a) |> 
  separate_wider_delim(mc, delim = "__", names_sep = "", too_few = "debug") |> 
  select(-mcmc) |> 
  mutate(across(where(is.character), ~str_replace(., "^\\\\", ""))) |> 
  # handle the multiple senses given by the students
  separate_longer_delim(mc3, delim = "; \\")
  
## check the distribution of column pieces
nias1905a_tb |> 
  count(mcmcpieces)

### in which column "ID" appear? =====
nias1905a_tb |> select(where(~any(grepl("^ID", x = .))))
# A tibble: 932 × 2
# mc1                  mc2
nias1905a_tb <- nias1905a_tb |> 
  mutate(ID = if_else(str_detect(mc1, "^ID"), mc1, NA),
         ID = if_else(str_detect(mc2, "^ID") & is.na(ID), mc2, ID))

### in which column "lx" appear? =====
nias1905a_tb |> select(where(~any(grepl("^lx", x = .))))
# mc1
nias1905a_tb <- nias1905a_tb |> 
  mutate(lx = if_else(str_detect(mc1, "^lx"), mc1, NA))

### in which column "note" appear? =====
nias1905a_tb |> select(where(~any(grepl("^Notes", x = .))))
# A tibble: 956 × 2
# mc2       mc3    
nias1905a_tb <- nias1905a_tb |> 
  mutate(nt = if_else(str_detect(mc2, "^Notes"), mc2, NA),
         nt = if_else(str_detect(mc3, "^Notes") & is.na(nt), mc3, nt))

### in which column "ps" appear? =====
nias1905a_tb |> select(where(~any(grepl("^ps", x = .))))
# A tibble: 956 × 2
# mc3     mc4 
nias1905a_tb <- nias1905a_tb |> 
  mutate(ps = if_else(str_detect(mc3, "^ps"), mc3, NA),
         ps = if_else(str_detect(mc4, "^ps") & is.na(ps), mc4, ps),
         ps = if_else(str_detect(mc3, "\\\\ps") & is.na(ps), str_extract(mc3, "(?<=\\\\)ps[^*]+?(?=[*]{3}\\\\de)"), ps))

### in which column "de" appear? =====
nias1905a_tb |> select(where(~any(grepl("^de", x = .))))
# A tibble: 944 × 3
# mc3           mc4                   mc5 
nias1905a_tb <- nias1905a_tb |> 
  mutate(de = if_else(str_detect(mc3, "^de"), mc3, NA),
         de = if_else(str_detect(mc4, "^de") & is.na(de), mc4, de),
         de = if_else(str_detect(mc5, "^de") & is.na(de), mc5, de),
         de = if_else(str_detect(mc3, "\\\\de") & is.na(de), str_extract(mc3, "(?<=\\\\)de[^*]+?(?=[*]{3}\\\\dv)"), de))

### in which column "dv" appear? =====
nias1905a_tb |> select(where(~any(grepl("^dv", x = .))))
# A tibble: 944 × 3
# mc4                   mc5                    mc6       
nias1905a_tb <- nias1905a_tb |> 
  mutate(dv = if_else(str_detect(mc4, "^dv"), mc4, NA),
         dv = if_else(str_detect(mc5, "^dv") & is.na(dv), mc5, dv),
         dv = if_else(str_detect(mc6, "^dv") & is.na(dv), mc6, dv),
         dv = if_else(str_detect(mc3, "\\\\dv") & is.na(dv), str_extract(mc3, "(?<=\\\\)dv[^*]+?(?=[*]{3})"), dv))

### in which column "dt" appear? =====
nias1905a_tb |> select(where(~any(grepl("^dt", x = .))))
# A tibble: 944 × 5
# mc1                  mc4                   mc5                    mc6              mc7  
nias1905a_tb <- nias1905a_tb |> 
  mutate(dt = if_else(str_detect(mc1, "^dt"), mc1, NA),
         dt = if_else(str_detect(mc4, "^dt") & is.na(dt), mc4, dt),
         dt = if_else(str_detect(mc5, "^dt") & is.na(dt), mc5, dt),
         dt = if_else(str_detect(mc6, "^dt") & is.na(dt), mc6, dt),
         dt = if_else(str_detect(mc7, "^dt") & is.na(dt), mc7, dt))

#### cleaning up =====
nias1905a_tb <- nias1905a_tb |> 
  select(!matches("^mc")) |> 
  mutate(across(where(is.character), ~str_replace_all(., "^[^ ]+?\\s", ""))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(ID = str_replace_all(ID, "\\s\\-\\s", "-"))
##### removing note in lx to nt ====
nias1905a_tb <- nias1905a_tb |> 
  mutate(nt = if_else(str_detect(lx, "\\<[0-9]+\\>"), str_extract(lx, "\\<[0-9]+\\>"), nt),
         lx = if_else(str_detect(lx, "\\<[0-9]+\\>"), str_replace(lx, "\\s+\\<[0-9]+\\>", ""), lx)) |> 
  ### remove the <...> in note ID
  mutate(nt = str_replace_all(nt, "(\\<|\\(|\\)|\\>)", ""))

##### add note types/categories ====
nias1905a_tb <- nias1905a_tb |> 
  mutate(cats = if_else(str_detect(ID, "^15[0-9]{2}"), "no. 102", "the Nias 1905 list"))
##### split the comma (of multiple items in a cell) into separate rows ====
# nias1905a_tb <- nias1905a_tb |> 
#   separate_longer_delim(cols = lx, delim = ", ")

##### edit punctuation that is originally "-" but turned into "/" =====
nias1905a_tb <- nias1905a_tb |> 
  mutate(ID = replace(ID, ID == "281/286", "281-286"),
         ID = replace(ID, ID == "291/294", "291-294"),
         ID = replace(ID, ID == "1236-1237", "1236/1237"))

###### check the missing note =====
setdiff(unique(nias1905note_tb$nt), unique(nias1905a_tb$nt[nias1905a_tb$nt != ""]))
# [1] "18" "31" "41" "50" "54" "58" "65" "66"
# 310/314  ...  ...  828  ...  893  1025 1026-1028
# note ID 31, 41, and 54 are not in the word list but appear in the NOTES!

setdiff(unique(nias1905a_tb$nt[nias1905a_tb$nt != ""]), unique(nias1905note_tb$nt))
# character(0) - all notes ID in the notes file appear in the notes ID in the word list

##### add missing rows ====
## these are rows missed out by the student detected due to the missing note ID checked above
nias1905a_tb <- nias1905a_tb |> 
  add_row(tibble_row(ID = "1026-1028", nt = "66", cats = "the Nias 1905 list")) |> 
  add_row(tibble_row(ID = "310/314", nt = "18", cats = "the Nias 1905 list")) |> 
  add_row(tibble_row(ID = "1025", nt = "65", cats = "the Nias 1905 list")) |> 
  add_row(tibble_row(ID = "828", nt = "50", cats = "the Nias 1905 list")) |> 
  add_row(tibble_row(ID = "893", lx = "hambaé, kalimango", nt = "58",
                     ps = "Noun", de = "crab", dv = "kepiting", 
                     cats = "the Nias 1905 list"))

setdiff(unique(nias1905note_tb$nt), unique(nias1905a_tb$nt[nias1905a_tb$nt != ""]))
# note ID 31, 41, and 54 are not in the word list but appear in the NOTES!

### COMBINE main table with the notes =====
nias1905main <- nias1905a_tb |> 
  left_join(nias1905note_tb) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  filter(ID != "")
# Warning message:
#   In left_join(nias1905a_tb, nias1905note_tb) :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 18 of `x` matches multiple rows in `y`.
# ℹ Row 36 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence
# this warning.
## The warning above IS EXPECTED!

### CHECK ID in the list with the main holle list =====
setdiff(nias1905main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))

### COMBINE THE RE-ORDERING ID in the list with the main holle list =====
nias1905main <- nias1905main |> 
  left_join(holle_index, by = join_by(ID)) |> 
  arrange(ORDERS) |> 
  select(-ORDERS) |> 
  rename(nt_form = `WORD/EXPRESSION`,
         nt_eng = ENGLISH,
         nt_idn = INDONESIAN,
         nt_comment = NOTES,
         nt_pos = PART_OF_SPEECH,
         nt_pc = IMAGE) |> 
  #### additional editing
  mutate(lx = if_else(ID == "920" & str_detect(lx, "\\/mbanoea"),
                      str_replace(lx, "\\/(?=mbanoe)", "'"),
                      lx)) |> 
  #### split forms separated by "/" into their own entries
  mutate(lx = str_split(lx, "\\/")) |> 
  unnest_longer(lx) |> 
  mutate(lx = if_else(ID == "893" & str_detect(lx, ",") & nt_form == "hambae",
                      str_replace(lx, "\\,\\skalimango$", ""),
                      lx),
         lx = if_else(ID == "893" & str_detect(lx, ",") & nt_form == "kali mango",
                      str_replace(lx, "^.+?\\,\\s", ""),
                      lx),
         lx = if_else(ID == "893" & str_detect(lx, ",") & nt_eng == "lobster",
                      str_replace(lx, "^.+?\\,\\s", ""),
                      lx),
         nt_form = replace(nt_form, ID == "304" & lx == "ama tiri",
                           "ama tiri"),
         nt_eng = replace(nt_eng, ID == "304" & lx == "ama tiri",
                           "father"),
         nt_idn = replace(nt_idn, ID == "304" & lx == "ama tiri",
                          "ayah"),
         nt_form = replace(nt_form, ID == "304" & lx == "ina tiri",
                           "ina tiri"),
         nt_eng = replace(nt_eng, ID == "304" & lx == "ina tiri",
                          "mother"),
         nt_idn = replace(nt_idn, ID == "304" & lx == "ina tiri",
                          "ibu"),
         nt_form = str_split(nt_form, "\\/")) |> 
  unnest_longer(nt_form)

#### split forms separated by "," into their own entries
nias1905main <- nias1905main |> 
  mutate(commasep = if_else(str_detect(lx, "\\,"), TRUE, FALSE)) |> 
  # filter(str_detect(lx, "\\,")) |>
  # select(ID, lx,
         # de, dv, 
         # nt, nt_form, nt_eng, nt_idn, nt_comment) |>
  separate_longer_delim(lx, ",") |> 
  mutate(lx = str_trim(lx, "both")) |> 
  mutate(remove = if_else(lx == "aloecha" & nt_form == "sola", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "abao" & nt_form == "sabao" & nt == "23", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "sabao" & nt_form == "abao" & nt == "23", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "raga-raga" & nt_form == "daga" & nt == "29", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfc("ésōlō") & nt_eng == "animal" & nt == "68", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfc("atabě") & nt_eng == "person" & nt == "68", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "daga" & nt_form == "raga-raga" & nt == "29", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "badoe" & nt_form == "mamadoe", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "mamadoe" & nt_form == "badoe", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "moroi ba" & str_detect(nt_form, "^awa.+") & nt_comment == "adverb", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "awai" & str_detect(nt_form, "^moro.+ba$") & str_detect(nt_comment, "^preposit.+"), TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "sola" & nt_form == "aloecha", TRUE, FALSE)) |> 
  filter(!remove) |> 
  mutate(nt_form = if_else(lx == "ama tiri",
                           str_replace(nt_form, "\\;\\sina.+$", ""),
                           nt_form),
         nt_form = if_else(lx == "ina tiri",
                          str_replace(nt_form, "^ama.+\\;\\s", ""),
                          nt_form),
         nt_eng = if_else(lx == "ama tiri",
                           str_replace(nt_eng, "\\;\\smoth.+$", ""),
                           nt_eng),
         nt_eng = if_else(lx == "ina tiri",
                          str_replace(nt_eng, "^fath.+\\;\\s", ""),
                          nt_eng),
         nt_idn = if_else(lx == "ama tiri",
                          str_replace(nt_idn, "\\;\\s.+$", ""),
                          nt_idn),
         nt_idn = if_else(lx == "ina tiri",
                          str_replace(nt_idn, "^.+\\;\\s", ""),
                          nt_idn)) |> 
  distinct()

### ADD EMPTY LX that has nt_form and save =====
nias1905main <- nias1905main |> 
  mutate(lx_all = if_else(lx == "" & nt_form != "",
                          nt_form,
                          lx)) 
nias1905main |> 
  write_rds("output/nias1905_tb.rds")
  # write_rds("C:/Users/GRajeg/OneDrive - Nexus365/Documents/Research/barrier-island-Holle-list-2023-05-24/data-output/nias1905_tb.rds")





# Nias 1911 ====
nias1911files <- files |> 
  str_subset("Nias_1911") |> 
  str_subset("[nN]otes?", negate = TRUE)
nias1911 <- map(nias1911files, read_lines) |> 
  unlist()
nias1911_notes <- files |> 
  str_subset("\\/NIAS_1911_NOTE") |> 
  read_xlsx() |> 
  select(-ID) |> 
  distinct() |> 
  rename(nt = `NOTE ID`) |> 
  mutate(nt = as.character(nt)) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

### check the marker present ====
nias1911 |> 
  str_extract("^\\\\[^ ]+?(?=\\s)") |> 
  unlist() |> 
  unique()

nias1911[which(!nzchar(nias1911))] <- "<separator>"
nias1911a <- nias1911 |> 
  str_c(collapse = "__") |> 
  str_split("\\<separator\\>") |> 
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace_all("(^__|__$)", "")

nias1911a <- if_else(str_detect(nias1911a, "\\\\sn\\s[1-9]"),
                     str_replace_all(nias1911a, "__(\\\\(ps|de|dv))", "***\\1"),
                     nias1911a)
nias1911a <- if_else(str_detect(nias1911a, "\\\\sn\\s[1-9]"),
                     str_replace_all(nias1911a, "__(\\\\sn [2-9])", "; \\1"),
                     nias1911a)
## handle anomaly in this data as the first six IDs are duplicated across the three students
### they could be remnant of one student's work provide example mode for the other twos who then forgot to delete these models
nias1911a |> length()
nias1911a |> unique() |> length()
nias1911a <- unique(nias1911a)
### yet still duplicated IDs
nias1911_still_duplicated_id <- nias1911a |> 
  str_extract("ID_en \\d+_") |> 
  table() |> 
  sort() |> 
  rev() |> 
  (\(x) x[x>1])()

nias1911a_tb <- tibble(mc = nias1911a) |> 
  separate_wider_delim(mc, delim = "__", names_sep = "", too_few = "debug") |> 
  select(-mcmc) |> 
  mutate(across(where(is.character), ~str_replace(., "^\\\\", "")))
## check the distribution of column pieces
nias1911a_tb |> 
  count(mcmcpieces)

### in which column "ID" appear? =====
nias1911a_tb |> select(where(~any(grepl("^ID", x = .))))
# # A tibble: 956 × 2
# mc1                 mc2
nias1911a_tb <- nias1911a_tb |> 
  mutate(ID = if_else(str_detect(mc1, "^ID"), mc1, NA),
         ID = if_else(str_detect(mc2, "^ID") & is.na(ID), mc2, ID))

### in which column "lx" appear? =====
nias1911a_tb |> select(where(~any(grepl("^lx", x = .))))
# mc1
nias1911a_tb <- nias1911a_tb |> 
  mutate(lx = if_else(str_detect(mc1, "^lx"), mc1, NA))

### in which column "note" appear? =====
nias1911a_tb |> select(where(~any(grepl("^nt", x = .))))
# A tibble: 956 × 2
# mc2       mc3    
nias1911a_tb <- nias1911a_tb |> 
  mutate(nt = if_else(str_detect(mc2, "^nt"), mc2, NA),
         nt = if_else(str_detect(mc3, "^nt") & is.na(nt), mc3, nt))

### in which column "ps" appear? =====
nias1911a_tb |> select(where(~any(grepl("^ps", x = .))))
# A tibble: 956 × 2
# mc3     mc4 
nias1911a_tb <- nias1911a_tb |> 
  mutate(ps = if_else(str_detect(mc3, "^ps"), mc3, NA),
         ps = if_else(str_detect(mc4, "^ps") & is.na(ps), mc4, ps))

### in which column "de" appear? =====
nias1911a_tb |> select(where(~any(grepl("^de", x = .))))
# # A tibble: 956 × 3
# mc3     mc4                  mc5   
nias1911a_tb <- nias1911a_tb |> 
  mutate(de = if_else(str_detect(mc3, "^de"), mc3, NA),
         de = if_else(str_detect(mc4, "^de") & is.na(de), mc4, de),
         de = if_else(str_detect(mc5, "^de") & is.na(de), mc5, de))

### in which column "dv" appear? =====
nias1911a_tb |> select(where(~any(grepl("^dv", x = .))))
# A tibble: 956 × 3
# mc4                  mc5                mc6      
nias1911a_tb <- nias1911a_tb |> 
  mutate(dv = if_else(str_detect(mc4, "^dv"), mc4, NA),
         dv = if_else(str_detect(mc5, "^dv") & is.na(dv), mc5, dv),
         dv = if_else(str_detect(mc6, "^dv") & is.na(dv), mc6, dv))

### in which column "dt" appear? =====
nias1911a_tb |> select(where(~any(grepl("^dt", x = .))))
# A tibble: 956 × 5
# mc1                 mc3     mc5                mc6            mc7  
nias1911a_tb <- nias1911a_tb |> 
  mutate(dt = if_else(str_detect(mc1, "^dt"), mc1, NA),
         dt = if_else(str_detect(mc3, "^dt") & is.na(dt), mc3, dt),
         dt = if_else(str_detect(mc5, "^dt") & is.na(dt), mc5, dt),
         dt = if_else(str_detect(mc6, "^dt") & is.na(dt), mc6, dt),
         dt = if_else(str_detect(mc7, "^dt") & is.na(dt), mc7, dt))

#### cleaning up =====
nias1911a_tb <- nias1911a_tb |> 
  select(!matches("^mc")) |> 
  mutate(across(where(is.character), ~str_replace_all(., "^[^ ]+?\\s", ""))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  ### remove the <...> in note ID
  mutate(nt = str_replace_all(nt, "(\\<|\\>)", ""))

##### add note types/categories ====
nias1911a_tb <- nias1911a_tb |> 
  mutate(cats = if_else(str_detect(ID, "^15[0-9]{2}"), "no. 249", "the Nias 1911 list"))

###### check the missing note =====
setdiff(unique(nias1911_notes$nt), unique(nias1911a_tb$nt[nias1911a_tb$nt != ""]))
# [1] "111"

setdiff(unique(nias1911a_tb$nt[nias1911a_tb$nt != ""]), unique(nias1911_notes$nt))
# [1] "(111)"

nias1911a_tb <- nias1911a_tb |> # fix the note (111)
  mutate(nt = replace(nt, nt == "(111)", "111"))
setdiff(unique(nias1911_notes$nt), unique(nias1911a_tb$nt[nias1911a_tb$nt != ""]))
# character(0)

### COMBINE main table with the notes =====
nias1911main <- nias1911a_tb |> 
  left_join(nias1911_notes) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))

### CHECK ID in the list with the main holle list =====
setdiff(nias1911main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# [1] ""          "1080/1081" "1924" 
## fix the error
nias1911main <- nias1911main |> 
  filter(ID != "") |> 
  mutate(ID = replace(ID, ID == "1924", "1294"),
         ID = replace(ID, ID == "1080/1081", "1080-1081"))
setdiff(nias1911main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# character(0)

### COMBINE THE RE-ORDERING ID in the list with the main holle list =====
nias1911main <- nias1911main |> 
  left_join(holle_index, by = join_by(ID)) |> 
  arrange(ORDERS) |> 
  select(-ORDERS) |> 
  rename(nt_form = `WORD/EXPRESSION`,
         nt_eng = `ENGLISH`,
         nt_pc = PICTURE)
#### handle/split multiple forms into their own entries =====
nias1911main <- nias1911main |> 
  mutate(commasep = if_else(str_detect(lx, "\\,"), TRUE, FALSE)) |> 
  # filter(str_detect(lx, "\\,")) |>
  # select(ID, lx,
  # de, dv, 
  # nt, nt_form, nt_eng, nt_comment) |>
  separate_longer_delim(lx, ",") |> 
  mutate(lx = str_trim(lx, "both")) |> 
  mutate(remove = if_else(lx == "aoeri" & nt == "13" & nt_form == "fa'aoeri",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "fa'aoeri" & nt == "13" & nt_form == "aoeri",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "baloese" & nt == "48" & nt_form == "dange",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "dange" & nt == "48" & nt_form == "baloese",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "tosake" & nt == "78" & nt_form == "aboto",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "aboto nowo" & nt == "78" & nt_form == "tosake",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "aefa" & nt == "82" & nt_form == "aboecho",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "aboecho" & nt == "82" & nt_form == "aefa",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(nt_form = if_else(lx == "aloecha" & nt == "76" & str_detect(nt_form, "^aloecha.+"),
                           str_replace(nt_form, "\\,\\s.+$", ""),
                           nt_form),
         nt_eng = if_else(lx == "aloecha" & nt == "76" & str_detect(nt_form, "^aloecha"),
                          str_replace(nt_eng, "\\,\\s.+$", ""),
                          nt_eng),
         nt_form = if_else(lx == "sola" & nt == "76" & str_detect(nt_form, "^aloecha.+"),
                           str_replace(nt_form, "^.+\\s", ""),
                           nt_form),
         nt_eng = if_else(lx == "sola" & nt == "76" & str_detect(nt_form, "^sol.+"),
                          str_replace(nt_eng, "^.+\\s", ""),
                          nt_eng),
         nt_comment = if_else(str_detect(nt_comment, "^\\(.+\\)$"),
                              str_replace_all(nt_comment, "(^\\(|\\)$)", ""),
                              nt_comment)) |> 
  mutate(commasepnote = if_else(str_detect(nt_form, ","),
                                TRUE,
                                FALSE)) |> 
  separate_longer_delim(cols = nt_form, ",") |> 
  mutate(nt_form = str_trim(nt_form, "both")) |> 
  mutate(nt_eng = if_else(nt_eng == "2 small kinds of ant",
                          str_replace_all(nt_eng, "(^2 |s(?=\\sof\\sant$))", ""),
                          nt_eng))

### ADD empty lx with nt_form =====
nias1911main <- nias1911main |> 
  mutate(lx_all = if_else(lx == "" & nt_form != "",
                          nt_form,
                          lx)) 
nias1911main |> 
  write_rds("output/nias1911_tb.rds")
  # write_rds("C:/Users/GRajeg/OneDrive - Nexus365/Documents/Research/barrier-island-Holle-list-2023-05-24/data-output/nias1911_tb.rds")
  





# Salang & Sigule ====
salangsigulefiles <- files |> 
  str_subset("SALANG_AND_SIGULE") |> 
  str_subset("\\.txt$")

salangsigule <- map(salangsigulefiles, read_lines) |> 
  unlist()

salangsigule_notes <- files |> 
  str_subset("SALANG_AND_SIGULE") |> 
  str_subset("C03_NOTES\\.xlsx") |> 
  read_xlsx() |> 
  rename(nt = NOTE_ID) |> 
  mutate(nt = as.character(nt))
salangsigule_notes <- salangsigule_notes |> 
  mutate(across(where(is.logical), ~as.character(.))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(INDONESIAN = replace(INDONESIAN, nt == "16" & `WORD/EXPRESSION` == "andeufa", "1 depah"),
         LÊKON = replace(LÊKON, nt == "16" & `WORD/EXPRESSION` == "andeufa", ""))

### check the marker present ====
salangsigule |> 
  str_extract("^\\\\[^ ]+?(?=\\s)") |> 
  unlist() |> 
  unique() |> 
  (\(x) x[!is.na(x)])()

salangsigule[which(!nzchar(salangsigule))] <- "<separator>"

salangsigule_a <- salangsigule |> 
  str_c(collapse = "__") |> 
  str_split("\\<separator\\>") |> 
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace_all("(^__|__$)", "")

salangsigule_a <- if_else(str_detect(salangsigule_a, "\\\\sn\\s[1-9]"),
                     str_replace_all(salangsigule_a, "__(\\\\(ps|de|dv))", "***\\1"),
                     salangsigule_a)
salangsigule_a <- if_else(str_detect(salangsigule_a, "\\\\sn\\s[1-9]"),
                     str_replace_all(salangsigule_a, "__(\\\\sn [2-9])", "; \\1"),
                     salangsigule_a)
## handle anomaly in this data as the first six IDs are duplicated across the three students
### they could be remnant of one student's work provide example mode for the other twos who then forgot to delete these models
salangsigule_a |> length()
salangsigule_a |> unique() |> length()
salangsigule_a <- unique(salangsigule_a)
### yet still duplicated IDs
salangsigule_still_duplicated_id <- salangsigule_a |> 
  str_extract("ID_en \\d+_") |> 
  table() |> 
  sort() |> 
  rev() |> 
  (\(x) x[x>1])()

salangsigule_a_tb <- tibble(mc = salangsigule_a) |> 
  separate_wider_delim(mc, delim = "__", names_sep = "", too_few = "debug") |> 
  select(-mcmc) |> 
  mutate(across(where(is.character), ~str_replace(., "^\\\\", "")))
## check the distribution of column pieces
salangsigule_a_tb |> 
  count(mcmcpieces)
# # A tibble: 5 × 2
# mcmcpieces     n
# <int> <int>
#   1          1     1
# 2          3     1
# 3          5    10
# 4          6   797
# 5          7     8

### in which column "ID" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^ID", x = .))))
# A tibble: 817 × 3
# mc1                mc2       mc3
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(ID = if_else(str_detect(mc1, "^ID"), mc1, NA),
         ID = if_else(str_detect(mc2, "^ID") & is.na(ID), mc2, ID),
         ID = if_else(str_detect(mc3, "^ID") & is.na(ID), mc3, ID))

### in which column "lx" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^lx", x = .))))
# mc1
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(lx = if_else(str_detect(mc1, "^lx"), mc1, NA))

### in which column "note" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^nt", x = .))))
# A tibble: 817 × 2
# mc2       mc3    
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(nt = if_else(str_detect(mc2, "^nt"), mc2, NA),
         nt = if_else(str_detect(mc3, "^nt") & is.na(nt), mc3, nt))

### in which column "ps" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^ps", x = .))))
# A tibble: 817 × 3
# mc2       mc3     mc4 
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(ps = if_else(str_detect(mc2, "^ps"), mc2, NA),
         ps = if_else(str_detect(mc3, "^ps") & is.na(ps), mc3, ps),
         ps = if_else(str_detect(mc4, "\\\\ps") & is.na(ps), str_extract(mc4, "(?<=\\\\)ps[^*]+?(?=[*]{3}\\\\de)"), ps))

### in which column "de" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^de", x = .))))
# # A tibble: 817 × 3
# mc3     mc4                  mc5   
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(de = if_else(str_detect(mc3, "^de"), mc3, NA),
         de = if_else(str_detect(mc4, "^de") & is.na(de), mc4, de),
         de = if_else(str_detect(mc5, "^de") & is.na(de), mc5, de))

### in which column "dv" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^dv", x = .))))
# A tibble: 817 × 3
# mc4                    mc5              mc6      
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(dv = if_else(str_detect(mc4, "^dv"), mc4, NA),
         dv = if_else(str_detect(mc5, "^dv") & is.na(dv), mc5, dv),
         dv = if_else(str_detect(mc6, "^dv") & is.na(dv), mc6, dv))

### in which column "dt" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^dt", x = .))))
# A tibble: 827 × 5
# mc1                 mc3     mc5                mc6            mc7  
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(dt = if_else(str_detect(mc1, "^dt"), mc1, NA),
         dt = if_else(str_detect(mc3, "^dt") & is.na(dt), mc3, dt),
         dt = if_else(str_detect(mc5, "^dt") & is.na(dt), mc5, dt),
         dt = if_else(str_detect(mc6, "^dt") & is.na(dt), mc6, dt),
         dt = if_else(str_detect(mc7, "^dt") & is.na(dt), mc7, dt))

### in which column "hm" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^hm", x = .))))
# A tibble: 827 × 5
# mc2 
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(hm = if_else(str_detect(mc2, "^hm"), mc2, NA))


#### cleaning up =====
salangsigule_a_tb <- salangsigule_a_tb |> 
  select(!matches("^mc")) |> 
  mutate(across(where(is.character), ~str_replace_all(., "^[^ ]+?\\s", ""))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  ### remove the <...> in note ID
  mutate(nt = str_replace_all(nt, "(\\<|\\>)", "")) |> 
  ### remove 122/123 as it does not exist in the original list
  filter(ID != "122/123") |>
  ### transferring note ID that appears in "lx" into "nt"
  mutate(nt = if_else(str_detect(lx, "\\<[0-9]+\\>"),
                      str_extract(lx, "(?<=\\<)[0-9]+(?=\\>)"),
                      nt)) |>
  mutate(lx = if_else(str_detect(lx, "\\<[0-9]+\\>"),
                      str_replace_all(lx, "\\s\\<[0-9]+\\>", ""),
                      lx))
  
##### add note types/categories ====
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(cats = if_else(str_detect(ID, "^15[0-9]{2}"), "no. 249", "the Salang-Sigule list"))

###### check the missing note =====
setdiff(unique(salangsigule_notes$nt), unique(salangsigule_a_tb$nt[salangsigule_a_tb$nt != ""]))
# [1]       "8"  "9"  "10" "11" "12"   "19" "21" "22" "23" "25" "26" "27" "28" "29" "30"
#  word ID   541  555  824  854  877

setdiff(unique(salangsigule_a_tb$nt[salangsigule_a_tb$nt != ""]), unique(salangsigule_notes$nt))
# [1] "45" <- this is a mistake; needs to be removed; done: 45 no longer exists -> character(0) as the output for this code line

# add the missing notes
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(nt = replace(nt, ID == "541", "8"),
         nt = replace(nt, ID == "555", "9"),
         nt = replace(nt, ID == "824", "10"),
         nt = replace(nt, ID == "854", "11"),
         nt = replace(nt, ID == "877", "12"),
         nt = replace(nt, ID == "1284", "19"),
         nt = replace(nt, ID == "1294", "21"),
         nt = replace(nt, ID == "1295", "22"),
         nt = replace(nt, ID == "1296", "23"),
         nt = replace(nt, ID == "1298", "25"),
         nt = replace(nt, ID == "1299", "26"),
         nt = replace(nt, ID == "1300", "27"),
         nt = replace(nt, ID == "1301", "28"),
         nt = replace(nt, ID == "1302", "29"),
         nt = replace(nt, ID == "1303", "30")) |> 
  filter(nt != "45")

### COMBINE main table with the notes =====
salangsigule_main <- salangsigule_a_tb |> 
  left_join(salangsigule_notes, by = join_by(nt)) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))

### CHECK ID in the list with the main holle list =====
setdiff(salangsigule_main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# [1] "1072/1074" "1026-2028" ""          "1080/1081" "1104/1104"
## fix the error
salangsigule_main <- salangsigule_main |> 
  filter(ID != "") |> 
  mutate(ID = replace(ID, ID == "1072/1074", "1072-1074"),
         ID = replace(ID, ID == "1026-2028", "1026-1028"),
         ID = replace(ID, ID == "1080/1081", "1080-1081"),
         ID = replace(ID, ID == "1104/1104", "1104/1105"))
setdiff(salangsigule_main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# character(0)

### COMBINE THE RE-ORDERING ID in the list with the main holle list =====
salangsigule_main <- salangsigule_main |> 
  left_join(holle_index, by = join_by(ID)) |> 
  arrange(ORDERS) |> 
  select(-ORDERS) |> 
  rename(nt_form = `WORD/EXPRESSION`,
         nt_eng = ENGLISH,
         nt_idn = INDONESIAN,
         nt_tapah = TAPAH,
         nt_lekon = LÊKON,
         nt_simalur = SIMALUR)

#### handle/split multiple forms into their own entries =====
salangsigule_main <-  salangsigule_main |> 
  mutate(commasep = if_else(str_detect(lx, "\\,"), TRUE, FALSE)) |> 
  # filter(str_detect(lx, "\\,")) |>
  # select(ID, lx,
  # de, dv,
  # matches("^nt")) |>
  separate_longer_delim(lx, ",") |> 
  mutate(lx = str_trim(lx, "both")) |> 
  
  # filter(nt == "") |> # to be commented after finish
  
  mutate(remove = if_else(lx == stri_trans_nfc("matoe-a goe abêleu") & nt_form == stri_trans_nfc("matoe-a goe alélé"),
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfc("matoe-a goe alélé") & nt_form == stri_trans_nfc("matoe-a goe abeleû"),
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfc("indeu") & nt_form == stri_trans_nfc("êkoe"),
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfc("êkoe") & nt_form == stri_trans_nfc("indeu"),
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfc("boelêm aiteu") & nt_form == stri_trans_nfc("boelêm afoe-i"),
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfc("boelêm afoe-i") & nt_form == stri_trans_nfc("boelêm aiteu"),
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfd("boelêm afoe-i") & nt_form == stri_trans_nfc("boelêm aiteu"),
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfd("géló-ah") & nt_form == "liang",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "liang" & nt_form == stri_trans_nfc("géló-ah"),
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == "biloe" & nt_form == "nofoe",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  mutate(remove = if_else(lx == stri_trans_nfd("nófoe") & nt_form == "biloe",
                          TRUE,
                          FALSE)) |> 
  filter(!remove) |> 
  separate_longer_delim(nt_form, ",") |> 
  mutate(nt_form = str_trim(nt_form, "both"))
### ADD empty lx with nt_form =====
salangsigule_main <- salangsigule_main |> 
  mutate(lx_all = if_else(lx == "" & nt_form != "",
                          nt_form,
                          lx))
salangsigule_main |> 
  write_rds("output/salangsigule_tb.rds")





# Mentawai =====
mentawaifiles_all <- files |> 
  str_subset("Mentawai")

mentawaifiles <- files |> 
  str_subset("Mentawai_Pagai") |> 
  str_subset("\\.txt$")

mentawai_xlsx <- files |> 
  str_subset("Mentawai_Pagai") |> 
  str_subset("\\.xlsx$")

## Handling the .xlsx submission =====
mentawai_b_xlsx <- read_xlsx(mentawai_xlsx) |> 
  mutate(ID = str_replace_all(ID, "\\.[0]+$", "")) |> 
  rename(lx = WORD,
         de = ENGLISH,
         dv = INDONESIA,
         ps = PART_OF_SPEECH,
         nt = NOTE_ID) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(nt = as.character(nt),
         nt = replace_na(nt, ""),
         ID = str_replace_all(ID, "\\s(?=\\-)", ""),
         ID = str_replace_all(ID, "(?<=\\-)\\s", "")) |> 
  mutate(lx = str_replace(lx, "' wo '", "'wo'")) |> 
  mutate(ps = str_to_sentence(ps)) |> 
  select(ID, lx, nt, ps, de, dv)

## Handling the .txt submission =====

mentawai <- map(mentawaifiles, read_lines) |> 
  unlist()

mentawai_notes <- files |> 
  str_subset("Mentawai\\-notes\\-and\\-additional") |> 
  read_xlsx() |> 
  rename(nt = NOTE_ID) |> 
  mutate(nt = as.character(nt)) |> 
  mutate(nt_cats = "nt",
         nt_cats = if_else(`WORD/EXPRESSION` %in% c("lakopa", "njoang lêleu", "njoang", "terengangang"),
                           "additional_data",
                           nt_cats)) |> 
  select(-ID)
mentawai_additional_data <- mentawai_notes |> 
  filter(nt_cats == "additional_data") |> 
  select(-nt_cats) |> 
  rename(lx = `WORD/EXPRESSION`,
         de = ENGLISH,
         dv = INDONESIAN,
         ps = PART_OF_SPEECH) |> 
  mutate(dv = if_else(de == "manggis",
                      de,
                      dv),
         dv = if_else(de == "scorpion",
                      "kalajengking",
                      dv),
         dv = if_else(de == "asthma",
                      "asma",
                      dv),
         dv = if_else(de == "cricket",
                      "jangkrik",
                      dv),
         de = if_else(de == "manggis",
                      "mangosteen",
                      de)) |> 
  mutate(ps = "Noun",
         ID = str_c("add_", nt, sep = "")) |> 
  select(-nt)
mentawai_notes <- mentawai_notes |> 
  filter(nt_cats == "nt") |> 
  select(-nt_cats)

mentawai_notes <- mentawai_notes |> 
  mutate(across(where(is.logical), ~as.character(.))) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

### check the marker present ====
mentawai |> 
  str_extract("^\\\\[^ ]+?(?=\\s)") |> 
  unlist() |> 
  unique() |> 
  (\(x) x[!is.na(x)])()

mentawai[which(!nzchar(mentawai))] <- "<separator>"

mentawai_a <- mentawai |> 
  str_c(collapse = "__") |> 
  str_split("\\<separator\\>") |> 
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace_all("(^__|__$)", "")

mentawai_a <- if_else(str_detect(mentawai_a, "\\\\sn\\s[1-9]"),
                          str_replace_all(mentawai_a, "__(\\\\(ps|de|dv))", "***\\1"),
                          mentawai_a)
mentawai_a <- if_else(str_detect(mentawai_a, "\\\\sn\\s[1-9]"),
                          str_replace_all(mentawai_a, "__(\\\\sn [2-9])", "; \\1"),
                          mentawai_a)
## handle anomaly in this data as the first six IDs are duplicated across the three students
### they could be remnant of one student's work provide example mode for the other twos who then forgot to delete these models
mentawai_a |> length()
mentawai_a |> unique() |> length()
mentawai_a <- unique(mentawai_a)
### yet still duplicated IDs
mentawai_still_duplicated_id <- mentawai_a |> 
  str_extract("ID_en \\d+_") |> 
  table() |> 
  sort() |> 
  rev() |> 
  (\(x) x[x>1])() 
# ID_en 377_  <- this is OK as in the original; two occurrences of 377 with two different lexical entry forms
# 2 
# RUN THE CODE BELOW TO CHECK!:
# mentawai_a |> str_subset("ID_en 377_")

mentawai_a_tb <- tibble(mc = mentawai_a) |> 
  separate_wider_delim(mc, delim = "__", names_sep = "", too_few = "debug") |> 
  select(-mcmc) |> 
  mutate(across(where(is.character), ~str_replace(., "^\\\\", "")))
## check the distribution of column pieces
mentawai_a_tb |> 
  count(mcmcpieces)
# # A tibble: 4 × 2
# mcmcpieces     n
# <int> <int>
#   1          1     1
# 2          5    31
# 3          6   445
# 4          7    35

### in which column "ID" appear? =====
mentawai_a_tb |> select(where(~any(grepl("^ID", x = .))))
# A tibble: 512 × 3
# mc1                mc2       mc3
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(ID = if_else(str_detect(mc1, "^ID"), mc1, NA),
         ID = if_else(str_detect(mc2, "^ID") & is.na(ID), mc2, ID),
         ID = if_else(str_detect(mc3, "^ID") & is.na(ID), mc3, ID))

### in which column "lx" appear? =====
mentawai_a_tb |> select(where(~any(grepl("^lx", x = .))))
# mc1
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(lx = if_else(str_detect(mc1, "^lx"), mc1, NA))

### in which column "note" appear? =====
mentawai_a_tb |> select(where(~any(grepl("^nt", x = .))))
# A tibble: 512 × 2
# mc2       mc3    
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(nt = if_else(str_detect(mc2, "^nt"), mc2, NA),
         nt = if_else(str_detect(mc3, "^nt") & is.na(nt), mc3, nt))

### in which column "ps" appear? =====
mentawai_a_tb |> select(where(~any(grepl("^ps", x = .))))
# A tibble: 512 × 3
# mc2       mc3     mc4 
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(ps = if_else(str_detect(mc2, "^ps"), mc2, NA),
         ps = if_else(str_detect(mc3, "^ps") & is.na(ps), mc3, ps),
         ps = if_else(str_detect(mc4, "\\\\ps") & is.na(ps), str_extract(mc4, "(?<=\\\\)ps[^*]+?(?=[*]{3}\\\\de)"), ps))

### in which column "de" appear? =====
mentawai_a_tb |> select(where(~any(grepl("^de", x = .))))
# # A tibble: 512 × 3
# mc3     mc4                  mc5   
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(de = if_else(str_detect(mc3, "^de"), mc3, NA),
         de = if_else(str_detect(mc4, "^de") & is.na(de), mc4, de),
         de = if_else(str_detect(mc5, "^de") & is.na(de), mc5, de))

### in which column "dv" appear? =====
mentawai_a_tb |> select(where(~any(grepl("^dv", x = .))))
# A tibble: 512 × 3
# mc4                    mc5              mc6      
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(dv = if_else(str_detect(mc4, "^dv"), mc4, NA),
         dv = if_else(str_detect(mc5, "^dv") & is.na(dv), mc5, dv),
         dv = if_else(str_detect(mc6, "^dv") & is.na(dv), mc6, dv))

### in which column "dt" appear? =====
mentawai_a_tb |> select(where(~any(grepl("^dt", x = .))))
# A tibble: 512 × 5
# mc1     mc5                mc6            mc7  
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(dt = if_else(str_detect(mc1, "^dt"), mc1, NA),
         # dt = if_else(str_detect(mc3, "^dt") & is.na(dt), mc3, dt),
         dt = if_else(str_detect(mc5, "^dt") & is.na(dt), mc5, dt),
         dt = if_else(str_detect(mc6, "^dt") & is.na(dt), mc6, dt),
         dt = if_else(str_detect(mc7, "^dt") & is.na(dt), mc7, dt))

### in which column "hm" appear? =====
mentawai_a_tb |> select(where(~any(grepl("^hm", x = .))))
# A tibble: 512 × 5
# mc2 
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(hm = if_else(str_detect(mc2, "^hm"), mc2, NA))

#### cleaning up =====
mentawai_a_tb <- mentawai_a_tb |> 
  select(!matches("^mc")) |> 
  mutate(across(where(is.character), ~str_replace_all(., "^[^ ]+?\\s", ""))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  ### remove the <...> in note ID
  mutate(nt = str_replace_all(nt, "(\\<|\\>)", ""))

##### join the .sfm data and the .xlsx data, and the Additional data =====
mentawai_a_tb <- mentawai_a_tb |> 
  bind_rows(mentawai_b_xlsx) |> 
  bind_rows(mentawai_additional_data) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

###### check the missing note =====
setdiff(unique(mentawai_notes$nt), unique(mentawai_a_tb$nt[mentawai_a_tb$nt != ""]))
# [1] "5", "44" (after manual checking into the PDF, this note ID is for word ID 255 and ID 987)

setdiff(unique(mentawai_a_tb$nt[mentawai_a_tb$nt != ""]), unique(mentawai_notes$nt))
# character(0) - all notes ID in the notes file appear in the notes ID in the word list

mentawai_a_tb <- mentawai_a_tb |> 
  ## add the missing notes into the main table
  mutate(nt = replace(nt, ID == "255", "5"),
         nt = replace(nt, ID == "987", "44"))

##### add note types/categories ====
mentawai_a_tb <- mentawai_a_tb |> 
  mutate(cats = if_else(str_detect(ID, "^15[0-9]{2}"), "no. 235", "the Mentawai (Sipora & Pagai) list"))

### COMBINE main table with the notes =====
mentawai_main <- mentawai_a_tb |> 
  left_join(mentawai_notes, relationship = "many-to-many", by = join_by(nt)) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))

### CHECK ID in the list with the main holle list =====
setdiff(mentawai_main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# [1] ""           "294/296"    "1041 /1042" "1080/1081"  "1106/1007"  "add_1"      "add_2"      "add_3"      "add_4" 
# the `add_...` is additional data not having ID like the main HL
# fix the error
mentawai_main <- mentawai_main |> 
  mutate(ID = replace(ID, ps == "Noun" & ID == "" & de == "cotton" & dv == "kapas",
                      "705")) |> 
  filter(ID != "") |> 
  mutate(ID = replace(ID, ID == "294/296", "291-294/296"),
         # comment: 294/296 in the main Holle List is listed as 291-294/296, 
         # but in the Mentawai list, it is only 294-296, 
         # so this is not a mistake 
         # but still being fixed to adjust to match the original main Holle List
         ID = replace(ID, ID == "1041 /1042", "1041/1042"), 
         ID = replace(ID, ID == "1080/1081", "1080-1081"),
         ID = replace(ID, ID == "1106/1007", "1106/1107"))
setdiff(mentawai_main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# [1] "add_1" "add_2" "add_3" "add_4"

### COMBINE THE RE-ORDERING ID in the list with the main holle list =====
mentawai_main <- mentawai_main |> 
  left_join(holle_index, by = join_by(ID)) |> 
  arrange(ORDERS) |> 
  select(-ORDERS) |> 
  rename(nt_form = `WORD/EXPRESSION`,
         nt_eng = ENGLISH,
         nt_idn = INDONESIAN,
         nt_pos = PART_OF_SPEECH)
#### handle/split multiple forms into their own entries =====
mentawai_main <- mentawai_main |> 
  
  mutate(lx = if_else(nt_form == "lalaplap" & nt == "44" & lx == "lalaplap, totoktok",
                      str_replace(lx, "\\,\\stotoktok", ""),
                      lx)) |> 
  mutate(lx = if_else(nt_form == "totoktok" & nt == "44" & lx == "lalaplap, totoktok",
                      str_replace(lx, "^lalap.+\\,\\s", ""),
                      lx)) |> 
  mutate(lx = if_else(nt_form == "silaloe" & nt == "29" & lx == "silaloe, sikalila",
                      str_replace(lx, "\\,\\ssika.+", ""),
                      lx)) |> 
  mutate(lx = if_else(nt_form == "sikaila" & nt == "29" & lx == "silaloe, sikalila",
                      str_replace(lx, "^silalo.+\\,\\s", ""),
                      lx)) |> 
  mutate(lx = if_else(nt_form == "manosa" & nt == "28" & lx == "manosa, sitoei",
                      str_replace(lx, "\\,\\ssito.+", ""),
                      lx)) |> 
  mutate(lx = if_else(nt_form == "sitoei" & nt == "28" & lx == "manosa, sitoei",
                      str_replace(lx, "^manos.+\\,\\s", ""),
                      lx)) |> 
  mutate(lx = if_else(ID == "1539" & str_detect(lx, "\\,\\spasi taptap"),
                      str_replace(lx, "\\,(?=\\spasi\\staptap)", ";"),
                      lx)) |> 
  
  mutate(commasep = if_else(str_detect(lx, "\\,"), TRUE, FALSE),
         slashsep = if_else(str_detect(lx, "\\/"), TRUE, FALSE),
         commasep_nt_form = if_else(str_detect(nt_form, "\\,"), TRUE, FALSE)) |> 
  # filter(str_detect(lx, "\\,")) |>
  select(ID, lx,
         de, dv,
         matches("^(nt|comma|slash)")) |> 
  
  separate_longer_delim(lx, ",") |> 
  mutate(lx = str_trim(lx, "both")) |> 
  
  mutate(nt_form = if_else(ID == "1426" & nt == "59",
                           str_replace(nt_form, "\\,", ";"),
                           nt_form),
         nt_eng = if_else(ID == "1426" & nt == "59",
                           str_replace(nt_eng, "\\,", ";"),
                           nt_eng)) |> 
  
  separate_longer_delim(nt_form, ",") |> 
  mutate(nt_form = str_trim(nt_form, "both")) |> 
  
  mutate(nt_form = if_else(ID == "1426" & nt == "59",
                           str_replace(nt_form, "\\;", ","),
                           nt_form),
         nt_eng = if_else(ID == "1426" & nt == "59",
                          str_replace(nt_eng, "\\;", ","),
                          nt_eng)) |> 
  mutate(lx = if_else(ID %in% c("1539", "69"),
                      str_replace(lx, "\\/", "OR"),
                      lx)) |> 
  
  separate_longer_delim(lx, "/") |> 
  mutate(lx = str_trim(lx, "both")) |> 
  
  mutate(lx = if_else(ID %in% c("1539", "69"),
                      str_replace(lx, "OR", "/"),
                      lx)) |> 
  mutate(lx = if_else(ID == "1539" & str_detect(lx, "\\;\\spasi taptap"),
                      str_replace(lx, "\\;(?=\\spasi\\staptap)", ","),
                      lx)) # |> 
  
  # filter(slashsep) |> print(n=Inf) # to be commented after finish
### ADD empty lx with nt_form =====
mentawai_main <- mentawai_main |> 
  mutate(lx_all = if_else(lx == "" & nt_form != "",
                          nt_form,
                          lx))
mentawai_main |> 
  write_rds("output/mentawai_tb.rds")





# Seumalur =====
semalurfiles_all <- files |> 
  str_subset("SEUMALUR")

semalurfiles <- semalurfiles_all |> 
  str_subset("SEUMALUR") |> 
  str_subset("\\.txt$")

semalur_xlsx <- semalurfiles_all |> 
  str_subset("SEUMALUR") |> 
  str_subset("Seumalur\\.xlsx$")

semalur_notes_file <- semalurfiles_all |> 
  str_subset("SEUMALUR") |> 
  str_subset("SEUMALUR-notes.*\\.xlsx$")

## Handling the .xlsx submission =====
semalur_b_xlsx <- semalur_xlsx |> 
  map(read_xlsx) |> 
  map(select, 1:6) |> 
  list_rbind() |> 
  mutate(ID = str_replace_all(ID, "\\.[0]+$", "")) |>
  rename(lx = WORD,
         de = ENGLISH,
         dv = INDONESIA,
         ps = PART_OF_SPEECH,
         nt = NOTE_ID) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(nt = as.character(nt),
         nt = replace_na(nt, "")
         ) |> 
  # mutate(lx = str_replace(lx, "' wo '", "'wo'")) |> 
  mutate(ps = str_to_sentence(ps)) |> 
  select(ID, lx, nt, ps, de, dv)

## Handling the .txt submission =====

semalur <- map(semalurfiles, read_lines) |> 
  unlist()

semalur_additional_data <- semalur_notes_file |> 
  read_xlsx(sheet = "ADDITIONAL DATA") |> 
  rename(lx = WORD,
         de = ENGLISH,
         dv = INDONESIAN,
         ps = `PART OF SPEECH`,
         nt = NOTE_ID) |> 
  mutate(nt = as.character(nt)) |> 
  mutate(nt = replace_na(nt, ""),
         ID = str_c("add_", row_number(), sep = ""))

semalur_notes <- semalur_notes_file |> 
  read_xlsx(sheet = "NOTES") |> 
  rename(nt = NOTE_ID) |> 
  mutate(nt = as.character(nt))

semalur_notes <- semalur_notes |> 
  mutate(across(where(is.logical), ~as.character(.))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(REMARKS = replace(REMARKS, ENGLISH == "L language", "L"),
         ENGLISH = replace(ENGLISH, ENGLISH == "L language", "language"),
         REMARKS = if_else(str_detect(ENGLISH, "^[A-Z]{1,2}\\s(?=to\\s)"),
                           str_replace(ENGLISH, "\\sto .*", ""),
                           REMARKS),
         ENGLISH = if_else(str_detect(ENGLISH, "^[A-Z]{1,2}\\s(?=to\\s)"),
                           str_replace(ENGLISH, "^(OS|L)\\s", ""),
                           ENGLISH),
         REMARKS = if_else(str_detect(ENGLISH, "\\b(L|OS)\\b"),
                           ENGLISH,
                           REMARKS),
         ENGLISH = if_else(str_detect(ENGLISH, "\\b(L|OS)\\b"),
                           "",
                           ENGLISH),
         REMARKS = if_else(str_detect(REMARKS, "\\b(L)\\b"),
                           str_replace(REMARKS, "\\b(L)\\b", "\\1akun (L)"),
                           REMARKS),
         REMARKS = if_else(str_detect(REMARKS, "\\b(OS)\\b"),
                           str_replace(REMARKS, "\\b(OS)\\b", "East <?> Seumalur (OS)"),
                           REMARKS)
         )

### check the marker present ====

semalur |> 
  str_extract("^\\\\[^ ]+?(?=\\s)") |> 
  unlist() |> 
  unique() |> 
  (\(x) x[!is.na(x)])()

semalur[which(!nzchar(semalur))] <- "<separator>"

semalur_a <- semalur |> 
  str_c(collapse = "__") |> 
  str_split("\\<separator\\>") |> 
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace_all("(^__|__$)", "")

semalur_a <- if_else(str_detect(semalur_a, "\\\\sn\\s[1-9]"),
                          str_replace_all(semalur_a, "__(\\\\(ps|de|dv))", "***\\1"),
                          semalur_a)
semalur_a <- if_else(str_detect(semalur_a, "\\\\sn\\s[1-9]"),
                          str_replace_all(semalur_a, "__(\\\\sn [2-9])", "; \\1"),
                          semalur_a)
## handle anomaly in this data as the first six IDs are duplicated across the three students
### they could be remnant of one student's work provide example mode for the other twos who then forgot to delete these models
semalur_a |> length()
semalur_a |> unique() |> length()
semalur_a <- unique(semalur_a)
### yet still duplicated IDs
semalur_still_duplicated_id <- semalur_a |> 
  str_extract("ID_en \\d+_") |> 
  table() |> 
  sort() |> 
  rev() |> 
  (\(x) x[x>1])() 
# ID_en 1292_  <- This error has been edited in the original sfm file and tracked with Git
# 2 
# > semalur_a |> str_subset("ID_en 1292_")
# [1] "\\ID_en 1292__\\nt <77>__\\ps Verb__\\de to take revenge__\\dv membalas dendam__\\dt 14/Mar/2024"      
# [2] "\\lx_skh oevalè__\\ID_en 1292__\\ps Verb__\\de to take revenge__\\dv membalas dendam__\\dt 13/Mar/2024"
## The first output above is potentially typo in original because
## the note <77> (about language) does not suit the gloss for 1292 (revenge)
## potential correct ID is 1202

semalur_a_tb <- tibble(mc = semalur_a) |> 
  separate_wider_delim(mc, delim = "__", names_sep = "", too_few = "debug") |> 
  select(-mcmc) |> 
  mutate(across(where(is.character), ~str_replace(., "^\\\\", "")))
## check the distribution of column pieces
semalur_a_tb |> 
  count(mcmcpieces)
# A tibble: 3 × 2
# mcmcpieces     n
# <int> <int>
#   1          5    24
# 2          6   447
# 3          7    32

### in which column "ID" appear? =====
semalur_a_tb |> select(where(~any(grepl("^ID", x = .))))
# A tibble: 503 × 2
# mc1                  mc2          
# <chr>                <chr>  
semalur_a_tb <- semalur_a_tb |> 
  mutate(ID = if_else(str_detect(mc1, "^ID"), mc1, NA),
         ID = if_else(str_detect(mc2, "^ID") & is.na(ID), mc2, ID))

### in which column "lx" appear? =====
semalur_a_tb |> select(where(~any(grepl("^lx", x = .))))
# mc1
semalur_a_tb <- semalur_a_tb |> 
  mutate(lx = if_else(str_detect(mc1, "^lx"), mc1, NA))

### in which column "note" appear? =====
semalur_a_tb |> select(where(~any(grepl("^nt", x = .))))
# A tibble: 503 × 2
# mc2       mc3    
semalur_a_tb <- semalur_a_tb |> 
  mutate(nt = if_else(str_detect(mc2, "^nt"), mc2, NA),
         nt = if_else(str_detect(mc3, "^nt") & is.na(nt), mc3, nt))

### in which column "ps" appear? =====
semalur_a_tb |> select(where(~any(grepl("^ps", x = .))))
# A tibble: 503 × 2
# mc3     mc4 
semalur_a_tb <- semalur_a_tb |> 
  mutate(ps = if_else(str_detect(mc3, "^ps"), mc3, NA),
         ps = if_else(str_detect(mc4, "^ps") & is.na(ps), mc4, ps))

### in which column "de" appear? =====
semalur_a_tb |> select(where(~any(grepl("^de", x = .))))
# # A tibble: 503 × 3
# mc3     mc4                  mc5   
semalur_a_tb <- semalur_a_tb |> 
  mutate(de = if_else(str_detect(mc3, "^de"), mc3, NA),
         de = if_else(str_detect(mc4, "^de") & is.na(de), mc4, de),
         de = if_else(str_detect(mc5, "^de") & is.na(de), mc5, de))

### in which column "dv" appear? =====
semalur_a_tb |> select(where(~any(grepl("^dv", x = .))))
# A tibble: 503 × 3
# mc4                    mc5              mc6      
semalur_a_tb <- semalur_a_tb |> 
  mutate(dv = if_else(str_detect(mc4, "^dv"), mc4, NA),
         dv = if_else(str_detect(mc5, "^dv") & is.na(dv), mc5, dv),
         dv = if_else(str_detect(mc6, "^dv") & is.na(dv), mc6, dv))

### in which column "dt" appear? =====
semalur_a_tb |> select(where(~any(grepl("^dt", x = .))))
# A tibble: 503 × 3
# mc5                mc6            mc7  
semalur_a_tb <- semalur_a_tb |> 
  mutate(# dt = if_else(str_detect(mc1, "^dt"), mc1, NA),
         # dt = if_else(str_detect(mc3, "^dt") & is.na(dt), mc3, dt),
         dt = if_else(str_detect(mc5, "^dt"), mc5, NA),
         dt = if_else(str_detect(mc6, "^dt") & is.na(dt), mc6, dt),
         dt = if_else(str_detect(mc7, "^dt") & is.na(dt), mc7, dt))

### in which column "hm" appear? =====
semalur_a_tb |> select(where(~any(grepl("^hm", x = .))))
# A tibble: 503 × 0

#### cleaning up =====
semalur_a_tb <- semalur_a_tb |> 
  select(!matches("^mc")) |> 
  mutate(across(where(is.character), ~str_replace_all(., "^[^ ]+?\\s", ""))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  ### remove the <...> in note ID
  mutate(nt = str_replace_all(nt, "(\\<|\\>)", ""))

##### join the .sfm data and the .xlsx data, and the Additional data =====
semalur_a_tb <- semalur_a_tb |> 
  bind_rows(semalur_b_xlsx) |> 
  bind_rows(semalur_additional_data) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

###### check the missing note =====
setdiff(unique(semalur_notes$nt), unique(semalur_a_tb$nt[semalur_a_tb$nt != ""]))
# [1] "30" (after manual checking into the PDF, this note ID is for word ID 351)

setdiff(unique(semalur_a_tb$nt[semalur_a_tb$nt != ""]), unique(semalur_notes$nt))
# character(0) - all notes ID in the notes file appear in the notes ID in the word list

semalur_a_tb <- semalur_a_tb |> 
  ## add the missing notes into the main table
  mutate(nt = replace(nt, ID == "351", "30"))

### COMBINE main table with the notes =====
semalur_main <- semalur_a_tb |> 
  left_join(semalur_notes, relationship = "many-to-many", by = join_by("nt")) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))

### CHECK ID in the list with the main holle list =====
setdiff(semalur_main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# many of the differrent ID are additional data, which is ignored
# fix error
semalur_main <- semalur_main |> 
  mutate(ID = replace(ID, ID == "1080/1081", "1080-1081"))

### COMBINE THE RE-ORDERING ID in the list with the main holle list =====
semalur_main <- semalur_main |> 
  left_join(holle_index, by = join_by(ID)) |> 
  arrange(ORDERS) |> 
  select(-ORDERS) |> 
  rename(nt_form = `WORD/EXPRESSION`,
         nt_eng = ENGLISH,
         nt_idn = INDONESIAN,
         nt_pc = IMAGE,
         nt_comment = REMARKS) |> 
  mutate(cats = "the Seumalur 1912 list")
### ADD empty lx with nt_form =====
semalur_main <- semalur_main |> 
  mutate(lx_all = if_else(lx == "" & nt_form != "",
                          nt_form,
                          lx))
#### TO-DO: handle/split multiple forms into their own entries =====
# semalur_main <-  semalur_main |> 
#   mutate(commasep = if_else(str_detect(lx, "\\,"), TRUE, FALSE)) |> 
#   # filter(str_detect(lx, "\\,")) |>
#   # select(ID, lx,
#   # de, dv,
#   # matches("^nt")) |>
#   separate_longer_delim(lx, ",") |> 
#   mutate(lx = str_trim(lx, "both"))


semalur_main |> 
  write_rds("output/semalur_tb.rds")





# Sigule & Salang ====
## IMPORTANT: missing contents for page 5-7 as the assigned student never showed up =====
### run the tesseract OCR for the PDFs for page 5-7 ======
#### ==== the following codes have been executed ==== ####
# pdf <- dir("C-grp-06-HolleList-01_28387_assignsubmission_file/", 
#             full.names = TRUE, pattern = ".pdf")
# pdf
# pngs <- pdftools::pdf_convert(pdf,
#                               pages = NULL,
#                               dpi = 600)
# txts <- tesseract::ocr(pngs)
# unlink(dir(pattern = "sigulei\\-and\\-salang.+\\.png$"))
dir_where_data_missing <- "C-grp-06-HolleList-01_28387_assignsubmission_file/C6_SIGULEI AND SALANG/Sigulei and Salang_2101541071_Samuel Gabriel Kadiman/"
# saveRDS(txts, file = str_c(dir_where_data_missing, "/sigulei-salang-5-7.rds"))
#### ==== the preceding codes have been executed ==== ####
txts <- read_rds(str_c(dir_where_data_missing, "/sigulei-salang-5-7.rds"))
##### processing page 5 =====
txt1 <- txts[[1]]
txt2 <- txts[[2]]
txt3 <- txts[[3]]
txt1_df <- txt1 |> 
  str_replace(".*SIGULEI AND SALANG.*\\n+", "") |> 
  str_split("\\n") |> 
  map(~str_replace_all(., "^fee(?=\\.\\sdjeroeg)", "722")) |> 
  map(~str_replace_all(., "(?<=^423)\\,\\sp(?=ambang)", ". g")) |> 
  map(~str_replace_all(., "(?<=^424)\\,", ".")) |> 
  map(~str_replace_all(., "\\s495\\/", "")) |> 
  map(~str_replace_all(., "(?<=\\s)496(?=\\.)", "495/496")) |> 
  map(~str_replace_all(., "kanéet", "kanèt")) |> 
  map(~str_replace_all(., "(?<=laranga\\s)197", "497")) |> 
  map(~str_replace_all(., "(?<=497\\.\\s)gambatae", "gambatāe")) |> 
  map(~str_replace_all(., "\\s199\\.", " 499.")) |> 
  map(~str_replace_all(., "(?<=\\<8)\\&(?=\\>)", "")) |> 
  map(~str_replace_all(., " eu gaiabaten", " 501/502. gambateu")) |> 
  map(~str_replace(., "bissd'", "bisō'")) |> 
  map(\(x) x[nzchar(x)]) |> 
  unlist() |> 
  str_replace("(?<=508\\.\\s)gala", "galá") |> 
  str_replace("(?<=509\\.\\s)pats\\!", "patō'") |> 
  str_replace("^Wes lengk.p ", "441/442. lengkěp ") |> 
  str_replace("210\\.\\ssendog", "510. sendog") |> 
  str_replace("pit\\.\\ssendo\\!", "511. sendo'") |> 
  str_replace("(?<=^446\\.\\s)tekéntiae", "tekěntiae") |> 
  str_replace("(?<=\\s)913(?=\\.)", "513") |> 
  str_replace("\\|\\s212\\+\\sF", "515. f") |> 
  str_replace("^454\\. bafa'", "454. bāfa'") |> 
  str_replace("(?<=\\s)217\\.", "517.") |> 
  str_replace("(^456.+?(?=518)|^458.+?(?=522)|^472\\/\\s(?=535))", "") |> 
  str_replace("^457", "456/457") |> 
  str_replace("^459", "458/459") |> 
  str_replace("\\s525\\-", "") |> 
  str_replace("527", "525-527") |> 
  str_replace("\\,\\sauwho", ", auwhō") |> 
  str_replace("929", "529") |> 
  str_replace("dinoetong", "dinoetōng") |> 
  str_replace("^473", "472/473") |> 
  str_replace("(?<=536\\.\\s)sambate$", "samba'e") |> 
  str_replace("(?<=537\\.\\s)dlde'$", "ālōe'") |> 
  str_replace("\\s539\\/", "") |> 
  str_replace("(?<=aiteu\\s)540", "539/540") |> 
  str_replace("^\\:\\s+", "") |> 
  str_replace("(?<=482\\.\\s)imbo'", "imbō'") |> 
  str_replace("\\s5\\s43\\-$", "") |> 
  str_replace("^feéawa ", "") |> 
  str_replace("toetoeng fandoe\\;", "toetoeng fandoe; feéawa") |> 
  str_replace("o\\%5\\.\\,", "543-545.") |> 
  str_replace("bomwafache", "bomwafachè") |> 
  str_replace("5950", "550") |> 
  str_replace("\\<1ll\\>", "<11>") |> 
  str_replace("\\s\\:\\s(?=553)", " ") |> 
  str_replace("nieitoeén", "nieitoeěn") |> 
  str_split("\\s(?=[0-9])") |> 
  unlist() |> 
  (\(x) tibble(lx = x))() |> 
  separate_wider_regex(lx, patterns = c(ID = "^[^\\s]+?", "\\.\\s", lx = ".+")) |> 
  mutate(nt = str_extract(lx, "\\<[0-9]+?\\>"),
         lx = str_replace_all(lx, "\\s\\<[^>]+?\\>", "")) |> 
  left_join(holle_gloss |> rename(ID = Index), by = join_by(ID)) |> 
  mutate(nt = replace_na(nt, ""))
##### processing page 6 =====
txt2_df <- txt2 |> 
  str_replace(".*SIGULEI AND SALANG.*\\n+", "") |> 
  str_split("\\n") |> 
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace("(?<=561\\.\\s)bend'\\-bend'\\!", "benò'-benò'") |> 
  str_replace("(?<=634\\.\\s)tadedenih", "iadedenih") |> 
  str_replace("^569\\/\\s", "") |> 
  str_replace("(?<=636\\.\\s)niodh.$", "niòhé") |> 
  str_replace("^210\\.\\sbadjt", "569/570. badji") |> 
  str_replace("^oT\\!", "571/572.") |> 
  str_replace("bondi\\!", "bondā'") |> 
  str_replace("(?<=642\\.\\s).+", "sikèh") |> 
  str_replace("^579\\-.+", "643. badjà'") |> 
  str_replace("^581(?=\\.)", "579-581") |> 
  str_replace("\\!(?=\\s645\\.)", "") |> 
  str_replace("(?<=582\\.\\s)eundm", "eunǎm") |> 
  str_replace("\\s650\\.+", " 650. rèmbè'") |> 
  str_replace("\\s(654|662|671)\\/", "") |> 
  str_replace("\\s655", " 654/655") |> 
  str_replace("^600\\.\\s.+", "600. měchěman toefō'(mah)") |> 
  str_replace("^58\\.\\sloe.ng", "657/658. loeëng") |> 
  str_replace("\\s663.\\s.+", " 662/663. nitoněu") |> 
  str_replace("^605\\/\\s", "") |> 
  str_replace("^606", "605/606") |> 
  str_replace("(?<=^607\\.\\s)roedoed", "roedoeó") |> 
  str_replace("\\s672\\.", " 671/672.") |> 
  str_replace("a(?=\\s679\\.)", "å") |> 
  str_replace("chal.d.$", "chalèdè") |> 
  str_replace("(?<=690\\.\\s).+", "apěli") |> 
  str_replace("^\\(Ol", "701") |> 
  str_split("\\s(?=[0-9])") |> 
  unlist() |> 
  (\(x) tibble(lx = x))() |> 
  separate_wider_regex(lx, patterns = c(ID = "^[^\\s\\.]+?", "\\.\\s", lx = ".+")) |> 
  mutate(nt = str_extract(lx, "\\<[0-9]+?\\>"),
         lx = str_replace_all(lx, "\\s\\<[^>]+?\\>", "")) |> 
  left_join(holle_gloss |> rename(ID = Index), by = join_by(ID)) |> 
  mutate(nt = replace_na(nt, ""))
##### processing page 7 =====
txt3_df <- txt3 |> 
  str_replace(".*SIGULEI AND SALANG.*\\n+", "") |> 
  str_split("\\n") |> 
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace("a(?=\\s764\\.)", "å") |> 
  str_replace("o(?=\\'\\s765\\.)", "ō") |> 
  str_replace("(?<=\\s765\\.\\stali\\s).", "è") |> 
  str_replace("(?<=709\\.\\s)(ana')t(ha)", "\\1\\2") |> 
  str_replace("(?<=ramboetant)os'", "ō'") |> 
  str_replace("(?<=780\\.\\s).+", "tèněp") |> 
  str_replace("^718\\.", "716-718.") |> 
  str_replace("^716\\-\\s", "") |> 
  str_replace("(?<=784\\.\\smoeisung).", "ě") |> 
  str_replace("^fee(?=\\.\\sdjeroeg)", "722") |> 
  str_replace("(?<=788\\.\\soemoemb).", "ō") |> 
  str_replace("^fe3\\.\\skop", "723. kopi") |> 
  str_replace("(?<=^728\\.\\saf).", "ō") |> 
  str_replace("^fe(?=\\.\\s)", "729") |> 
  str_replace("\\((?=97\\.\\s)", "7") |> 
  str_replace("^\\((?=31\\.\\s)", "7") |> 
  str_replace("(\\s800_|\\s803\\/$|^739\\/\\s|^742\\-\\s)", "") |> 
  str_replace("(?<=\\s)802(?=\\.)", "800-802") |> 
  str_replace("(?<=\\s)804(?=\\.)", "803/804") |> 
  str_replace("^740", "739/740") |> 
  str_replace("(?<=\\s807\\.\\skoed).", "å") |> 
  str_replace("(?<=\\s811\\.\\snaho).", "ē") |> 
  str_replace("^744\\.\\s.+mbana\\'(?=\\s)", "742-744. lèmbana'") |> 
  str_replace("(?<=\\s813\\.\\s).+", "maō'") |> 
  str_replace("^\\:\\s(?=746\\.)", "") |> 
  str_replace("(?<=^748\\.\\sbeb).", "è") |> 
  str_replace("^\\(9", "75") |> 
  str_replace("^foe", "752") |> 
  str_replace("(?<=^753\\.\\sdafoe)d", "ō") |> 
  str_replace("832", "833") |> 
  str_replace("(?<=^754\\.\\soeloe)\\!", "'") |> 
  str_replace("(?<=\\s83)h.+$", "4. keàkè") |> 
  str_replace("^155", "755") |> 
  str_replace("849\\.\\smanoe2", "842. manoe²") |> 
  str_replace("b.n.(?=\\s843)", "bènè") |> 
  str_replace("(?<=\\s843\\.\\s)m\\&onjo", "mǎnjò") |> 
  str_replace("(?<=\\s844\\.\\smanoe)\\?", "²") |> 
  str_replace("^\\|\\s(?=761)", "") |> 
  str_replace("S46\\.\\sb.ngge$", "846. běnggè") |> 
  str_split("\\s(?=[0-9])") |> 
  unlist() |> 
  (\(x) tibble(lx = x))() |> 
  separate_wider_regex(lx, patterns = c(ID = "^[^\\s\\.]+?", "\\.\\s", lx = ".+")) |> 
  mutate(nt = str_extract(lx, "\\<[0-9]+?\\>"),
         lx = str_replace_all(lx, "\\s?\\<[^>]+?\\>", ""))|> 
  left_join(holle_gloss |> rename(ID = Index), by = join_by(ID)) |> 
  mutate(nt = replace_na(nt, ""))

sigulesalang_missing_df <- bind_rows(txt1_df, txt2_df, txt3_df) |> 
  mutate(nt = str_replace_all(nt, "[><]", "")) |> 
  mutate(cats = "the Salang-Sigule list") |> 
  select(-holle_match, -Dutch) |> 
  rename(de = English,
         dv = Indonesian)



sigulesalangfiles <- files |> 
  str_subset("SIGULEI AND SALANG") |> 
  str_subset("\\.txt$")

sigulesalang <- map(sigulesalangfiles, read_lines) |> 
  unlist()

sigulesalang_additional_data <- files |> 
  str_subset("SIGULEI AND SALANG") |> 
  str_subset("C6_.*_NOTES.xlsx") |> 
  read_xlsx(sheet = "Additional Data") |> 
  rename(lx = `WORD/EXPRESSION`,
         de = ENGLISH,
         dv = INDONESIAN,
         ps = PART_OF_SPEECH,
         nt_comment = NOTES) |> 
  mutate(ID = str_c("add_", row_number(), sep = ""))

sigulesalang_notes <- files |> 
  str_subset("SIGULEI AND SALANG") |> 
  str_subset("C6_.*_NOTES.xlsx") |> 
  read_xlsx(sheet = "Notes") |> 
  rename(nt = NOTE_ID) |> 
  mutate(nt = as.character(nt))
sigulesalang_notes <- sigulesalang_notes |> 
  mutate(across(where(is.logical), ~as.character(.))) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

### check the marker present ====
sigulesalang |> 
  str_extract("^\\\\[^ ]+?(?=\\s)") |> 
  unlist() |> 
  unique() |> 
  (\(x) x[!is.na(x)])()
# [1] "\\lx_skh" "\\ID_en"  "\\ps"     "\\de"     "\\dv"     "\\dt"     "\\nt"     "\\sn"

sigulesalang[which(!nzchar(sigulesalang))] <- "<separator>"


sigulesalang_a <- sigulesalang |> 
  str_c(collapse = "__") |> 
  str_split("\\<separator\\>") |> 
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace_all("(^__|__$)", "")

sigulesalang_a <- if_else(str_detect(sigulesalang_a, "\\\\sn\\s[1-9]"),
                          str_replace_all(sigulesalang_a, "__(\\\\(ps|de|dv))", "***\\1"),
                          sigulesalang_a)
sigulesalang_a <- if_else(str_detect(sigulesalang_a, "\\\\sn\\s[1-9]"),
                          str_replace_all(sigulesalang_a, "__(\\\\sn [2-9])", "; \\1"),
                          sigulesalang_a)
## handle anomaly in this data as the first six IDs are duplicated across the three students
### they could be remnant of one student's work provide example mode for the other twos who then forgot to delete these models
sigulesalang_a |> length()
sigulesalang_a |> unique() |> length()
sigulesalang_a <- unique(sigulesalang_a)
### yet still duplicated IDs
sigulesalang_still_duplicated_id <- sigulesalang_a |> 
  str_extract("ID_en \\d+_") |> 
  table() |> 
  sort() |> 
  rev() |> 
  (\(x) x[x > 1])()

sigulesalang_a_tb <- tibble(mc = sigulesalang_a) |> 
  separate_wider_delim(mc, delim = "__", names_sep = "", too_few = "debug") |> 
  select(-mcmc) |> 
  mutate(across(where(is.character), ~str_replace(., "^\\\\", "")))
## check the distribution of column pieces
sigulesalang_a_tb |> 
  count(mcmcpieces)
# A tibble: 5 × 2
# mcmcpieces     n
# <int> <int>
#   1          2     1
# 2          4     2
# 3          5     2
# 4          6   588
# 5          7    32

### in which column "ID" appear? =====
sigulesalang_a_tb |> select(where(~any(grepl("^ID", x = .))))
# A tibble: 625 × 2
# mc1                  mc2
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(ID = if_else(str_detect(mc1, "^ID"), mc1, NA),
         ID = if_else(str_detect(mc2, "^ID") & is.na(ID), mc2, ID))

### in which column "lx" appear? =====
sigulesalang_a_tb |> select(where(~any(grepl("^lx", x = .))))
# mc1
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(lx = if_else(str_detect(mc1, "^lx"), mc1, NA))

### in which column "note" appear? =====
sigulesalang_a_tb |> select(where(~any(grepl("^nt", x = .))))
# A tibble: 625 × 2
# mc2       mc3    
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(nt = if_else(str_detect(mc2, "^nt"), mc2, NA),
         nt = if_else(str_detect(mc3, "^nt") & is.na(nt), mc3, nt))

### in which column "ps" appear? =====
sigulesalang_a_tb |> select(where(~any(grepl("^ps", x = .))))
# A tibble: 625 × 2
# mc3     mc4 
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(ps = if_else(str_detect(mc3, "^ps"), mc3, NA),
         # ps = if_else(str_detect(mc4, "^ps") & is.na(ps), mc4, ps),
         ps = if_else(str_detect(mc4, "\\\\ps") & is.na(ps), str_extract(mc4, "(?<=\\\\)ps[^*]+?(?=[*]{3}\\\\de)"), ps))

### in which column "de" appear? =====
sigulesalang_a_tb |> select(where(~any(grepl("^de", x = .))))
# # A tibble: 956 × 3
# mc3     mc4                  mc5   
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(de = if_else(str_detect(mc3, "^de"), mc3, NA),
         de = if_else(str_detect(mc4, "^de") & is.na(de), mc4, de),
         de = if_else(str_detect(mc5, "^de") & is.na(de), mc5, de))

### in which column "dv" appear? =====
sigulesalang_a_tb |> select(where(~any(grepl("^dv", x = .))))
# A tibble: 827 × 3
# mc4                    mc5              mc6      
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(dv = if_else(str_detect(mc4, "^dv"), mc4, NA),
         dv = if_else(str_detect(mc5, "^dv") & is.na(dv), mc5, dv),
         dv = if_else(str_detect(mc6, "^dv") & is.na(dv), mc6, dv))

### in which column "dt" appear? =====
sigulesalang_a_tb |> select(where(~any(grepl("^dt", x = .))))
# A tibble: 827 × 5
# mc1                 mc3     mc5                mc6            mc7  
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(dt = if_else(str_detect(mc1, "^dt"), mc1, NA),
         dt = if_else(str_detect(mc3, "^dt") & is.na(dt), mc3, dt),
         dt = if_else(str_detect(mc5, "^dt") & is.na(dt), mc5, dt),
         dt = if_else(str_detect(mc6, "^dt") & is.na(dt), mc6, dt),
         dt = if_else(str_detect(mc7, "^dt") & is.na(dt), mc7, dt))

### in which column "hm" appear? =====
sigulesalang_a_tb |> select(where(~any(grepl("^hm", x = .))))
# A tibble: 827 × 5
# mc2 
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(hm = if_else(str_detect(mc2, "^hm"), mc2, NA))


#### cleaning up =====
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  select(!matches("^mc")) |> 
  mutate(across(where(is.character), ~str_replace_all(., "^[^ ]+?\\s", ""))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  ### remove the <...> in note ID
  mutate(nt = str_replace_all(nt, "(\\<|\\>)", "")) # |> 
  ### remove 122/123 as it does not exist in the original list
  # filter(ID != "122/123") |> 
  ### transferring note ID that appears in "lx" into "nt"
  # mutate(nt = if_else(str_detect(lx, "\\<[0-9]+\\>"),
  #                     str_extract(lx, "(?<=\\<)[0-9]+(?=\\>)"),
  #                     nt)) |>
  # mutate(lx = if_else(str_detect(lx, "\\<[0-9]+\\>"),
  #                     str_replace_all(lx, "\\s\\<[0-9]+\\>", ""),
  #                     lx))

##### add note types/categories ====
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(cats = if_else(str_detect(ID, "^15[0-9]{2}"), "no. 249", "the Sigule-Salang list"))

##### combine missing list with the present list ====
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  bind_rows(sigulesalang_missing_df) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

###### check the missing note =====
setdiff(unique(sigulesalang_notes$nt), unique(sigulesalang_a_tb$nt[sigulesalang_a_tb$nt != ""]))
# [1] "38" <- missing because of typo; this belongs to ID 1395 but student typed in 39 for the note ID instead of 38

setdiff(unique(sigulesalang_a_tb$nt[sigulesalang_a_tb$nt != ""]), unique(sigulesalang_notes$nt))
# character(0)

# add the missing notes
sigulesalang_a_tb <- sigulesalang_a_tb |> 
  mutate(nt = replace(nt, ID == "1395", "38"))

### COMBINE main table with the notes =====
sigulesalang_main <- sigulesalang_a_tb |> 
  left_join(sigulesalang_notes, by = join_by(nt)) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))

### CHECK ID in the list with the main holle list =====
setdiff(sigulesalang_main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# [1] ""

## fix the error
sigulesalang_main <- sigulesalang_main |> 
  filter(ID != "")
setdiff(sigulesalang_main$ID, c(holle_tb$Index, holle_1904_tb$Index, holle_1931_tb$Index))
# character(0)

### COMBINE THE RE-ORDERING ID in the list with the main holle list =====
sigulesalang_main <- sigulesalang_main |> 
  left_join(holle_index, by = join_by(ID)) |> 
  arrange(ORDERS) |> 
  select(-ORDERS) |> 
  rename(nt_form = `WORD/EXPRESSION`,
         nt_eng = ENGLISH,
         nt_idn = INDONESIAN)

#### Combine the Sigulei and Salang additional data into the main data ======
sigulesalang_main <- sigulesalang_main |> 
  bind_rows(sigulesalang_additional_data |> 
              mutate(cats = "the Sigule-Salang list")) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

### TO DO: split multiple forms in a lexical entry =====

### ADD empty lx with nt_form =====
sigulesalang_main <- sigulesalang_main |> 
  mutate(lx_all = if_else(lx == "" & nt_form != "",
                          nt_form,
                          lx))
sigulesalang_main |> 
  write_rds("output/sigulesalang_tb.rds")





# COMBINE ALL REGIONAL LISTS to check their glosses and IDs with the main Holle list ======
holle_region <- bind_rows(mutate(salangsigule_main, lang_name = "salangsigule"), 
                          mutate(semalur_main, lang_name = "semalur"), 
                          mutate(sigulesalang_main, lang_name = "sigulesalang"),
                          mutate(nias1905main, lang_name = "nias1905"), 
                          mutate(nias1911main, lang_name = "nias1911"), 
                          mutate(mentawai_main, lang_name = "mentawai_nd")) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(holle_match = if_else(str_detect(ID, "add_"),
                               "",
                               str_c(str_trim(ID, "both"),
                                     str_trim(de, "both"),
                                     str_trim(dv, "both"),
                                     sep = "_")),
         holle_match = str_to_lower(holle_match))

## check which gloss from the regional list does not match with the main list
in_regional_list_but_not_in_main_list <- setdiff(holle_region$holle_match[holle_region$holle_match != ""], holle_gloss$holle_match)

## turn the unmatched gloss into a tibble
df_in_regional_list_but_not_in_main_list <- in_regional_list_but_not_in_main_list |> 
  (\(x) tibble(unmatched = x))() |> 
  separate_wider_delim(unmatched, 
                       delim = "_", names_sep = "_",
                       cols_remove = FALSE) |> 
  rename(Index = unmatched_1) |> 
  
  ## re-join the main holle list gloss table for later use in correcting the regional list
  left_join(holle_gloss) |> 
  rename(holle_match_orig = holle_match,
         holle_match = unmatched_unmatched,
         cats_orig = cats)

## correct the unmatched English and Indonesian in the regional list with the ones from the main list
holle_region_1 <- holle_region |> 
  left_join(df_in_regional_list_but_not_in_main_list) |>
  mutate(de = if_else(!is.na(Index) & English != "", English, de), 
         dv = if_else(!is.na(Index) & Indonesian != "", Indonesian, dv))

## save
holle_region_1 |> write_rds("output/_holle_region.rds")

