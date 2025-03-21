library(tidyverse)
library(readxl)

# list the file
files <- dir(pattern = ".+\\.txt|xlsx", recursive = TRUE)
files
write_lines(files, "files_list.txt")

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
  mutate(across(where(is.character), ~replace_na(., "")))
##### removing note in lx to nt ====
nias1905a_tb <- nias1905a_tb |> 
  mutate(nt = if_else(str_detect(lx, "\\<[0-9]+\\>"), str_extract(lx, "\\<[0-9]+\\>"), nt),
         lx = if_else(str_detect(lx, "\\<[0-9]+\\>"), str_replace(lx, "\\s+\\<[0-9]+\\>", ""), lx)) |> 
  ### remove the <...> in note ID
  mutate(nt = str_replace_all(nt, "(\\<|\\>)", ""))

##### add note types/categories ====
nias1905a_tb <- nias1905a_tb |> 
  mutate(cats = if_else(str_detect(ID, "^15[0-9]{2}"), "no. 102", "the Nias list"))
##### split the comma (of multiple items in a cell) into separate rows ====
nias1905a_tb <- nias1905a_tb |> 
  separate_longer_delim(cols = lx, delim = ", ")

##### edit punctuation that is originally "-" but turned into "/" =====
nias1905a_tb <- nias1905a_tb |> 
  mutate(ID = replace(ID, ID == "281/286", "281-286"),
         ID = replace(ID, ID == "291/294", "291-294"))

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
  add_row(tibble_row(ID = "1026-1028", nt = "66", cats = "the Nias list")) |> 
  add_row(tibble_row(ID = "310/314", nt = "18", cats = "the Nias list")) |> 
  add_row(tibble_row(ID = "1025", nt = "65", cats = "the Nias list")) |> 
  add_row(tibble_row(ID = "828", nt = "50", cats = "the Nias list")) |> 
  add_row(tibble_row(ID = "893", lx = "hambaé, kalimango", nt = "58", ps = "Noun", de = "crab", dv = "kepiting", cats = "the Nias list"))

setdiff(unique(nias1905note_tb$nt), unique(nias1905a_tb$nt[nias1905a_tb$nt != ""]))
# note ID 31, 41, and 54 are not in the word list but appear in the NOTES!

### COMBINE main table with the notes =====
nias1905main <- nias1905a_tb |> 
  left_join(nias1905note_tb) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))
# Warning message:
#   In left_join(nias1905a_tb, nias1905note_tb) :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 18 of `x` matches multiple rows in `y`.
# ℹ Row 36 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence
# this warning.
## The warning above IS EXPECTED!







# Nias 1911 ====
nias1911files <- files |> 
  str_subset("Nias_1911") |> 
  str_subset("[nN]otes?", negate = TRUE)
nias1911 <- map(nias1911files, read_lines) |> 
  unlist()
nias1911_notes <- files |> 
  str_subset("\\/NIAS_1911_NOTE") |> 
  read_xlsx() |> 
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
  mutate(cats = if_else(str_detect(ID, "^15[0-9]{2}"), "no. 249", "the Nias list"))

###### check the missing note =====
setdiff(unique(nias1911_notes$nt), unique(nias1911a_tb$nt[nias1911a_tb$nt != ""]))
# [1] "111"

setdiff(unique(nias1911a_tb$nt[nias1911a_tb$nt != ""]), unique(nias1911_notes$nt))
# character(0) - all notes ID in the notes file appear in the notes ID in the word list

nias1911a_tb <- nias1911a_tb |> # fix the note (111)
  mutate(nt = replace(nt, nt == "(111)", "111"))
setdiff(unique(nias1911_notes$nt), unique(nias1911a_tb$nt[nias1911a_tb$nt != ""]))

### COMBINE main table with the notes =====
nias1911main <- nias1911a_tb |> 
  left_join(nias1911_notes) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))






# Salang & Sigule ====
salangsigulefiles <- files |> 
  str_subset("SALANG") |> 
  str_subset("\\.txt$")

salangsigule <- map(salangsigulefiles, read_lines) |> 
  unlist()

salangsigule_notes <- files |> 
  str_subset("SALANG") |> 
  str_subset("\\/C03_NOTES\\.xlsx") |> 
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
salangsigule_a_tb <- tibble(mc = salangsigule_a) |> 
  separate_wider_delim(mc, delim = "__", names_sep = "", too_few = "debug") |> 
  select(-mcmc) |> 
  mutate(across(where(is.character), ~str_replace(., "^\\\\", "")))
## check the distribution of column pieces
salangsigule_a_tb |> 
  count(mcmcpieces)
# # A tibble: 7 × 2
# mcmcpieces     n
# <int> <int>
#   1          1     1
# 2          2     1
# 3          3     1
# 4          4     2
# 5          5    15
# 6          6  1393
# 7          7    39

### in which column "ID" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^ID", x = .))))
# A tibble: 826 × 3
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
# A tibble: 956 × 2
# mc2       mc3    
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(nt = if_else(str_detect(mc2, "^nt"), mc2, NA),
         nt = if_else(str_detect(mc3, "^nt") & is.na(nt), mc3, nt))

### in which column "ps" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^ps", x = .))))
# A tibble: 827 × 3
# mc2       mc3     mc4 
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(ps = if_else(str_detect(mc2, "^ps"), mc2, NA),
         ps = if_else(str_detect(mc3, "^ps") & is.na(ps), mc3, ps),
         ps = if_else(str_detect(mc4, "\\\\ps") & is.na(ps), str_extract(mc4, "(?<=\\\\)ps[^*]+?(?=[*]{3}\\\\de)"), ps))

### in which column "de" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^de", x = .))))
# # A tibble: 956 × 3
# mc3     mc4                  mc5   
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(de = if_else(str_detect(mc3, "^de"), mc3, NA),
         de = if_else(str_detect(mc4, "^de") & is.na(de), mc4, de),
         de = if_else(str_detect(mc5, "^de") & is.na(de), mc5, de))

### in which column "dv" appear? =====
salangsigule_a_tb |> select(where(~any(grepl("^dv", x = .))))
# A tibble: 827 × 3
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
  filter(ID != "122/123")

##### add note types/categories ====
salangsigule_a_tb <- salangsigule_a_tb |> 
  mutate(cats = if_else(str_detect(ID, "^15[0-9]{2}"), "no. 249", "the Salang-Sigule list"))

### COMBINE main table with the notes =====
salangsigule_main <- salangsigule_a_tb |> 
  left_join(salangsigule_notes) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))






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
                           nt_cats))
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
  left_join(mentawai_notes) |> # the warning of many-to-many relationship is OK
  mutate(across(where(is.character), ~replace_na(., "")))






# Seumalur =====
semalurfiles_all <- files |> 
  str_subset("SEUMALUR")

semalurfiles <- files |> 
  str_subset("SEUMALUR") |> 
  str_subset("\\.txt$")

semalur_xlsx <- files |> 
  str_subset("SEUMALUR") |> 
  str_subset("Seumalur\\.xlsx$")

semalur_notes_file <- files |> 
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