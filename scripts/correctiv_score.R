# Correctiv
# "" manuell zuordnen (15 textual ratings)
# 0 -> frei erfunden (neues label)
# 1 (alt) -> frei erfunden (damals noch "völlig falsch")
# 1 (neu) -> falsch
# 2 (alt) -> falsch
# 2 (neu) -> gr. falsch
# 3 (alt) -> gr. falsch
# 3 (neu) -> teilw. falsch
# 4 (alt) -> teilw. falsch
# 4 (neu) -> unbelegt, fehlender Kontext
# 5 (alt) -> gr. richtig
# 6 (alt) -> richtig
# 7 (neu) -> gr. richtig (ein paar gr. falsch -> manuell?)
# 8 (neu) -> richtig
# NA -> NA (54 textual ratings)

obj$meta$ratingValue_korrigiert = rep(NA_integer_, length(obj$meta$ratingValue))
# korrigiert:
# 1: frei erfunden
# 2: falsch
# 3: gr. falsch
# 4: teilw. falsch
# 5: unbelegt, fehlender Kontext
# 6: gr. richtig
# 7: richtig

obj$meta$ratingValue_korrigiert[obj$meta$bestRating == 6 &
                                  obj$meta$resource == "Correctiv" &
                                  !is.na(obj$meta$bestRating)] =
  match(obj$meta$ratingValue[obj$meta$bestRating == 6 &
                               obj$meta$resource == "Correctiv" &
                               !is.na(obj$meta$bestRating)],
        c("1", "2", "3", "4", "x", "5", "6"))
obj$meta$ratingValue_korrigiert[obj$meta$bestRating == 8 &
                                  obj$meta$resource == "Correctiv" &
                                  !is.na(obj$meta$bestRating)] =
  match(obj$meta$ratingValue[obj$meta$bestRating == 8 &
                               obj$meta$resource == "Correctiv" &
                               !is.na(obj$meta$bestRating)],
        c("0", "1", "2", "3", "4", "7", "8"))

# manuell
# ->
obj$meta$ratingValue_korrigiert[obj$meta$resource == "Correctiv" &
                                  is.na(obj$meta$ratingValue_korrigiert) &
                                  !is.na(obj$meta$textualRating)] =
  c(5,1,3,1,4,4,2,3,5,5,5,3,2,1,5,5,5)
