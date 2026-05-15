# ==============================================================================
# Internal helpers for fingerprint keying — not exported.
#
# Based on OpenRefine's FingerprintKeyer.java:
# https://github.com/OpenRefine/OpenRefine/blob/master/modules/core/src/main/
#   java/com/google/refine/clustering/binning/FingerprintKeyer.java
# ==============================================================================

# ------------------------------------------------------------------------------
# Non-diacritic substitution table.
# Characters whose Unicode NFD decomposition does NOT yield a plain base letter
# plus a combining diacritic mark need explicit replacement first.
# ------------------------------------------------------------------------------
.nondiacritics <- c(
  "\u00DF" = "ss",  # ß  -> ss
  "\u00E6" = "ae",  # æ  -> ae
  "\u00C6" = "ae",  # Æ  -> ae
  "\u0153" = "oe",  # œ  -> oe
  "\u0152" = "oe",  # Œ  -> oe
  "\u00D8" = "o",   # Ø  -> o
  "\u00F8" = "o",   # ø  -> o
  "\u0141" = "l",   # Ł  -> l
  "\u0142" = "l",   # ł  -> l
  "\u00D0" = "d",   # Ð  -> d
  "\u00F0" = "d",   # ð  -> d
  "\u0110" = "d",   # Đ  -> d
  "\u0111" = "d",   # đ  -> d
  "\u0126" = "h",   # Ħ  -> h
  "\u0127" = "h",   # ħ  -> h
  "\u0131" = "i",   # ı  -> i  (dotless i)
  "\u0138" = "k",   # ĸ  -> k  (kra)
  "\u013F" = "l",   # Ŀ  -> l
  "\u0140" = "l",   # ŀ  -> l
  "\u014A" = "n",   # Ŋ  -> n
  "\u014B" = "n",   # ŋ  -> n
  "\u0149" = "n",   # ŉ  -> n
  "\u017F" = "s",   # ſ  -> s  (long s)
  "\u0166" = "t",   # Ŧ  -> t
  "\u0167" = "t",   # ŧ  -> t
  "\u0174" = "w",   # Ŵ  -> w
  "\u0175" = "w",   # ŵ  -> w
  "\u0176" = "y",   # Ŷ  -> y
  "\u0177" = "y",   # ŷ  -> y
  "\u0178" = "y",   # Ÿ  -> y
  "\u01F7" = "w",   # Ƿ  -> w  (wynn)
  "\u01BF" = "w"    # ƿ  -> w
)

# ------------------------------------------------------------------------------
# Legal entity suffix list — EU-27 member states, United States, and China.
#
# Each entry is the *normalised* form (lowercase, no punctuation) matched
# against the already-normalised string, so punctuation variants such as
# "s.a.", "s.r.l.", "b.v." are caught automatically.
# Duplicates arising from shared tokens across jurisdictions are removed below.
# ------------------------------------------------------------------------------
.legal_entities <- c(

  # ---- United States ----------------------------------------------------------
  "inc", "incorporated",
  "corp", "corporation",
  "co",                        # Company / Corporation  (also covers "co.")
  "company",
  "llc",                       # Limited Liability Company
  "llp",                       # Limited Liability Partnership
  "lp",                        # Limited Partnership
  "ltd", "limited",
  "plc",                       # Public Limited Company
  "pllc",                      # Professional LLC
  "pc",                        # Professional Corporation
  "na",                        # National Association (banks)

  # ---- Pan-EU / general -------------------------------------------------------
  "sa",                        # Société Anonyme / Sociedad Anónima / etc.
  "sas",                       # Société par Actions Simplifiée (FR) /
                               # Società in Accomandita Semplice (IT)
  "sarl",                      # Société à Responsabilité Limitée (FR/LU/BE)
  "srl",                       # Società a Responsabilità Limitata (IT/RO/ES)
  "sro",                       # Společnost s Ručením Omezeným (CZ/SK)
  "sca",                       # Société en Commandite par Actions (FR/LU/PT/RO)
  "scs",                       # Société en Commandite Simple (FR/BE/LU)
  "snc",                       # Société en Nom Collectif (FR/BE/LU/IT)
  "se",                        # Societas Europaea

  # ---- Austria (AT) -----------------------------------------------------------
  "gmbh",                      # Gesellschaft mit beschränkter Haftung
  "ag",                        # Aktiengesellschaft
  "kg",                        # Kommanditgesellschaft
  "og",                        # Offene Gesellschaft
  "eg",                        # Eingetragene Genossenschaft

  # ---- Belgium (BE) -----------------------------------------------------------
  "nv",                        # Naamloze Vennootschap
  "bv",                        # Besloten Vennootschap (also NL)
  "bvba",                      # Besloten Vennootschap met Beperkte Aansprakelijkheid
  "cvba",                      # Coöperatieve Vennootschap met Beperkte Aansprakelijkheid

  # ---- Bulgaria (BG) ----------------------------------------------------------
  "ead",                       # Еднолично Акционерно Дружество
  "ad",                        # Акционерно Дружество
  "ood",                       # Дружество с Ограничена Отговорност
  "eood",                      # Еднолично ООД

  # ---- Croatia (HR) -----------------------------------------------------------
  "doo",                       # Društvo s Ograničenom Odgovornošću  (also SI)
  "dd",                        # Dioničko Društvo  (also SI)
  "jdoo",                      # Jednostavno Društvo s Ograničenom Odgovornošću

  # ---- Czech Republic (CZ) ----------------------------------------------------
  "as",                        # Akciová Společnost (also DK, EE, LV, LT, SE)

  # ---- Denmark (DK) -----------------------------------------------------------
  "aps",                       # Anpartsselskab
  "is",                        # Interessentskab

  # ---- Estonia (EE) -----------------------------------------------------------
  "ou",                        # Osaühing

  # ---- Finland (FI) -----------------------------------------------------------
  "oy",                        # Osakeyhtiö
  "oyj",                       # Julkinen Osakeyhtiö
  "ky",                        # Kommandiittiyhtiö
  "ay",                        # Avoin Yhtiö

  # ---- France (FR) ------------------------------------------------------------
  "sasu",                      # Société par Actions Simplifiée Unipersonnelle
  "eurl",                      # Entreprise Unipersonnelle à Responsabilité Limitée
  "sci",                       # Société Civile Immobilière
  "gie",                       # Groupement d'Intérêt Économique

  # ---- Germany (DE) -----------------------------------------------------------
  "ohg",                       # Offene Handelsgesellschaft
  "gbr",                       # Gesellschaft bürgerlichen Rechts
  "kgaa",                      # Kommanditgesellschaft auf Aktien
  "ug",                        # Unternehmergesellschaft (haftungsbeschränkt)
  "ev",                        # Eingetragener Verein

  # ---- Greece (GR) ------------------------------------------------------------
  "ae",                        # Ανώνυμη Εταιρεία
  "epe",                       # Εταιρεία Περιορισμένης Ευθύνης
  "oe",                        # Ομόρρυθμη Εταιρεία
  "ee",                        # Ετερόρρυθμη Εταιρεία
  "ike",                       # Ιδιωτική Κεφαλαιουχική Εταιρεία

  # ---- Hungary (HU) -----------------------------------------------------------
  "kft",                       # Korlátolt Felelősségű Társaság
  "zrt",                       # Zártkörűen Működő Részvénytársaság
  "nyrt",                      # Nyilvánosan Működő Részvénytársaság
  "bt",                        # Betéti Társaság
  "kkt",                       # Közkereseti Társaság

  # ---- Italy (IT) -------------------------------------------------------------
  "spa",                       # Società per Azioni
  "sapa",                      # Società in Accomandita per Azioni
  "ss",                        # Società Semplice
  "scarl",                     # Società Cooperativa a Responsabilità Limitata

  # ---- Latvia (LV) ------------------------------------------------------------
  "sia",                       # Sabiedrība ar Ierobežotu Atbildību

  # ---- Lithuania (LT) ---------------------------------------------------------
  "uab",                       # Uždaroji Akcinė Bendrovė
  "ab",                        # Akcinė Bendrovė
  "mb",                        # Mažoji Bendrija

  # ---- Luxembourg (LU) --------------------------------------------------------
  "senc",                      # Société en Nom Collectif

  # ---- Netherlands (NL) -------------------------------------------------------
  "vof",                       # Vennootschap onder Firma
  "cv",                        # Commanditaire Vennootschap
  "coop",                      # Coöperatie

  # ---- Poland (PL) ------------------------------------------------------------
  "sp zoo",                    # Spółka z Ograniczoną Odpowiedzialnością
  "sk",                        # Spółka Komandytowa
  "sj",                        # Spółka Jawna
  "psp",                       # Prosta Spółka Akcyjna

  # ---- Portugal (PT) ----------------------------------------------------------
  "lda",                       # Sociedade por Quotas (Limitada)

  # ---- Romania (RO) -----------------------------------------------------------
  "ra",                        # Regie Autonomă

  # ---- Slovakia (SK) ----------------------------------------------------------
  "ks",                        # Komanditná Spoločnosť
  "vos",                       # Verejná Obchodná Spoločnosť

  # ---- Slovenia (SI) ----------------------------------------------------------
  "sp",                        # Samostojni Podjetnik

  # ---- Spain (ES) -------------------------------------------------------------
  "sl",                        # Sociedad Limitada
  "slne",                      # Sociedad Limitada Nueva Empresa
  "cb",                        # Comunidad de Bienes

  # ---- Sweden (SE) ------------------------------------------------------------
  "hb",                        # Handelsbolag
  "kb",                        # Kommanditbolag

  # ---- China (CN) -------------------------------------------------------------
  "youxian gongsi",            # 有限公司  (Ltd / LLC — pinyin)
  "gufen youxian gongsi",      # 股份有限公司  (Joint-Stock Co.)
  "jituan",                    # 集团  (Group)
  "gongsi",                    # 公司  (Company — generic)

  # ---- Common trailing descriptors --------------------------------------------
  "group",
  "holding", "holdings",
  "international",
  "enterprises",
  "industries",
  "services",
  "solutions",
  "technologies",
  "ventures",
  "partners",
  "associates",
  "brothers",
  "sons",
  "and sons",
  "and co",
  "and company"
)

.legal_entities <- unique(.legal_entities)

# Pre-compile the legal-entity regex once at load time.
# Tokens sorted longest-first so multi-word forms (e.g. "and company") are
# tried before single-word sub-tokens ("co").
# Word-boundary matching via look-behind / look-ahead prevents partial hits
# (e.g. "sa" inside "samsung", "inc" inside "inca").
.fp_legal_regex <- local({
  tokens  <- .legal_entities[order(-nchar(.legal_entities))]
  escaped <- gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", tokens)
  pattern <- paste0(
    "(?:^|(?<=\\s))(?:",
    paste(escaped, collapse = "|"),
    ")(?=\\s|$)"
  )
  list(pattern = pattern)
})

# ------------------------------------------------------------------------------
# Term harmonisation table.
#
# Maps every known variant of a company-name descriptor to its canonical short
# form. Applied only when company = TRUE, AFTER legal-suffix stripping, so
# trailing descriptors already removed (e.g. "technologies") do not interfere
# with mid-name occurrences (e.g. "technology" in "Philips Technology Center").
#
# Matching is whole-word only (look-behind / look-ahead), so "management" is
# contracted but "mismanagement" is left untouched.
#
# To extend the list from your package simply append entries to .term_variants
# before .fp_term_regexes is compiled, or add a new list element.
# ------------------------------------------------------------------------------
.term_variants <- list(

  # Technology
  tech = c(
    "technology", "technologies", "technological",
    "techn", "tec"
  ),

  # Management
  mgt = c(
    "management", "managements",
    "mgmt", "mgnt", "mngmt"
  ),

  # Industry / Industries
  ind = c(
    "industry", "industries", "industrial", "industrials",
    "inds", "indus"
  ),

  # Company  (residual tokens that survived legal-suffix stripping mid-name)
  co = c(
    "company", "companies",
    "compan"
  ),

  # International
  intl = c(
    "international", "internationals",
    "intern", "intnl", "internat"
  ),

  # Laboratory / Laboratories
  lab = c(
    "laboratory", "laboratories",
    "labs", "labo", "laborat"
  ),

  # Manufacturing
  mfg = c(
    "manufacturing", "manufacturer", "manufacturers",
    "manufact", "manuf", "mfr", "mfrs"
  ),

  # Development
  dev = c(
    "development", "developments", "developer", "developers",
    "devel", "dvlp", "dvpt"
  )
)

# Pre-compile one PCRE regex per canonical term.
# Variants sorted longest-first so longer forms are matched before any shorter
# sub-token they contain (e.g. "technological" before "tech").
.fp_term_regexes <- lapply(names(.term_variants), function(canonical) {
  variants <- .term_variants[[canonical]]
  variants <- variants[order(-nchar(variants))]
  escaped  <- gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", variants)
  pattern  <- paste0(
    "(?:^|(?<=\\s))(?:",
    paste(escaped, collapse = "|"),
    ")(?=\\s|$)"
  )
  list(canonical = canonical, pattern = pattern)
})

# ------------------------------------------------------------------------------
# .fp_apply_nondiacritics()
# ------------------------------------------------------------------------------
.fp_apply_nondiacritics <- function(s) {
  for (from in names(.nondiacritics)) {
    s <- gsub(from, .nondiacritics[[from]], s, fixed = TRUE)
  }
  s
}

# ------------------------------------------------------------------------------
# .fp_remove_diacritics()
# Uses stringi when available; falls back to iconv TRANSLIT otherwise.
# ------------------------------------------------------------------------------
.fp_remove_diacritics <- function(s) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    s <- stringi::stri_trans_nfd(s)
    s <- stringi::stri_replace_all_regex(s, "\\p{M}", "")
  } else {
    result <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    if (!is.na(result)) s <- result
  }
  s
}

# ------------------------------------------------------------------------------
# .fp_normalize()  — core normalisation pipeline (always applied)
#   1. Trim surrounding whitespace
#   2. Lowercase
#   3. Explicit non-diacritic substitutions  (ß -> ss, æ -> ae, …)
#   4. NFD decomposition + combining-mark removal
#   5. Strip punctuation and C0/C1 control characters
#      (whitespace preserved so we can split on it afterwards)
# ------------------------------------------------------------------------------
.fp_normalize <- function(s) {
  s <- trimws(s)
  s <- tolower(s)
  s <- .fp_apply_nondiacritics(s)
  s <- .fp_remove_diacritics(s)
  s <- gsub("[[:punct:]]", "", s)
  # R strings cannot contain a literal NUL byte; use perl = TRUE so PCRE
  # interprets the \\x00 escape inside the character class.
  s <- gsub("[\\x00-\\x08\\x0e-\\x1f\\x7f]", "", s, perl = TRUE)
  s
}

# ------------------------------------------------------------------------------
# .fp_strip_legal()
#
# Remove legal entity tokens from an already-normalised string.
# Applied iteratively until stable so stacked suffixes are fully removed
# (e.g. "philips bv holding" -> "philips bv" -> "philips").
# Returns the trimmed result unchanged if nothing matched or if stripping
# would leave an empty string.
# ------------------------------------------------------------------------------
.fp_strip_legal <- function(s) {
  pattern <- .fp_legal_regex$pattern
  repeat {
    stripped <- trimws(gsub(pattern, "", s, perl = TRUE))
    if (identical(stripped, s) || !nzchar(stripped)) break
    s <- stripped
  }
  s
}

# ------------------------------------------------------------------------------
# .fp_harmonise_terms()
#
# Contract long-form descriptors to their canonical abbreviations in an
# already-normalised string (e.g. "technology" -> "tech",
# "management" -> "mgt").  Run AFTER .fp_strip_legal() so trailing descriptors
# already removed do not interfere with mid-name occurrences.
# ------------------------------------------------------------------------------
.fp_harmonise_terms <- function(s) {
  for (rx in .fp_term_regexes) {
    s <- gsub(rx$pattern, rx$canonical, s, perl = TRUE)
  }
  trimws(s)
}

# ------------------------------------------------------------------------------
# .fp_company_pipeline()
#
# Convenience wrapper that runs both company-specific steps in the correct
# order on an already base-normalised string:
#   1. Strip legal entity suffixes
#   2. Harmonise term variants  (technology -> tech, management -> mgt, …)
# ------------------------------------------------------------------------------
.fp_company_pipeline <- function(s) {
  s <- .fp_strip_legal(s)
  s <- .fp_harmonise_terms(s)
  s
}

# ------------------------------------------------------------------------------
# .fp_canonical_lookup()
#
# Build a named character vector:  fingerprint key -> canonical raw value.
# Canonical = most frequent raw value; ties broken alphabetically.
# ------------------------------------------------------------------------------
.fp_canonical_lookup <- function(x, keys) {
  non_na  <- !is.na(keys)
  x_nn    <- x[non_na]
  keys_nn <- keys[non_na]

  unique_keys <- unique(keys_nn)

  canonical <- vapply(unique_keys, function(k) {
    members    <- x_nn[keys_nn == k]
    tbl        <- sort(table(members), decreasing = TRUE)
    max_freq   <- tbl[[1L]]
    candidates <- sort(names(tbl[tbl == max_freq]))
    candidates[[1L]]
  }, character(1L))

  stats::setNames(canonical, unique_keys)
}
