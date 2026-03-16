#' Classify Organizations into Research/Education, Government, or Private Sectors
#'
#' This function classifies organizations based on keyword pattern matching across
#' multiple European languages. It uses a hierarchical approach: first checking for
#' research/education indicators, then government patterns, and defaulting to private
#' sector for unmatched organizations.
#'
#' @param data A data frame or tibble containing organization names
#' @param org_column The name of the column containing organization names (unquoted)
#'
#' @return A data frame identical to the input with two additional columns:
#'   \item{org_type}{Character. Classification result: "Research_Education", "Government", or "Private"}
#'   \item{confidence}{Character. Confidence level: "High" (matched specific patterns),
#'         "Medium" (matched private sector patterns), or "Low" (defaulted to private)}
#'
#' @details
#' The function uses comprehensive keyword patterns covering:
#' \itemize{
#'   \item \strong{Research & Education}: Universities, research institutes, laboratories,
#'         academies, and famous research organizations (e.g., CNRS, Max Planck, Fraunhofer)
#'         across multiple European languages
#'   \item \strong{Government}: Ministries, municipalities, agencies, councils, parliaments,
#'         and public administrations in various European languages
#'   \item \strong{Private}: Legal entity forms (Ltd, GmbH, SA, BV, etc.) and company
#'         indicators across all EU countries
#' }
#'
#' The classification follows a hierarchical priority:
#' \enumerate{
#'   \item Research/Education patterns (highest priority)
#'   \item Government patterns
#'   \item Private sector patterns or default
#' }
#'
#' Organizations that don't match research/education or government patterns are
#' classified as "Private" by default.
#'
#' @examples
#' # Basic usage with a simple data frame
#' orgs <- data.frame(
#'   name = c("Université de Paris", "Siemens AG", "Ministry of Health"),
#'   country = c("FR", "DE", "UK")
#' )
#' classified <- classify_organization(orgs, name)
#'
#' # With CORDIS data
#' \dontrun{
#' cordis_classified <- classify_organization(cordis, org_name)
#' }
#'
#' # With Scopus data
#' \dontrun{
#' scopus_classified <- classify_organization(scopus, affiliation)
#' }
#'
#' # Filter by confidence level
#' \dontrun{
#' high_confidence <- classified %>%
#'   filter(confidence == "High")
#'
#' needs_review <- classified %>%
#'   filter(confidence == "Low")
#' }
#'
#' @seealso
#' Pattern lists used internally:
#' \code{\link{research_education_patterns}},
#' \code{\link{government_patterns}},
#' \code{\link{private_company_patterns}}
#'
#' @export
#' @importFrom dplyr mutate pull select bind_cols case_when
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#' @importFrom rlang enquo
classify_organization <- function(data, org_column) {

  # Extract organization names from the specified column
  org_names <- data %>% pull({{ org_column }})
  # Set the patterns: -------
  research_education_patterns <- c(

    # UNIVERSITY (all European languages)
    "universit",           # Covers university, université, università, universidad, universität, etc.
    "univers\\b",          # Universidad, Uniwersytet
    "univ\\b",             # Common abbreviation
    "\\buni\\b",           # Short form
    "universiteit",
    "research",
    "wageningen ur",
    "radboudumc",
    "instituut",
    "institute",
    "hogeschool",
    "tu delft",
    "tu eindhoven",
    "afdeling",
    "department",
    "nikhef",
    "centrum",
    "erasmus",
    "esa-estec",
    "hogescholen",
    "fontys",
    "ggz",
    "\\bdiffer\\b",
    "umc utrecht",
    "ziekenhuis",
    "\\bastron\\b",
    "unversity",
    "univerzita",
    "fakultetaza",
    "bundesinstitut",
    "fakultet",
    "institutet",
    "consortium",
    "hospital",
    "ecole",
    "institutt",
    "centro",
    "centre",
    "zentrum",
    "center",
    "library",
    "stichting",
    "accademia",
    "uniwersytet",
    "bilimsel",
    "deltares",
    "wetsus",
    "kitlv",
    "wageningen ur",
    "vu medisch centrum",
    "wodc–crs",
    "Wageningen Environmental Research",
    "centrum wiskunde and informatica (cwi)",
    "comprehensive cancer center",
    "\\bolvg\\b",
    "wageningen marine research",
    "haaglanden/haga",
    "Universitair",
    "wageningen food safety research",
    "foundation",
    "ewi, delft, netherlands",
    "unesco-ihe, delft, netherlands",
    "clinic",
    "imec",
    "cwi",
    "haaglanden",
    "fontys",
    "hogescholen",
    "radboudumc",
    "ziekenhuizen",
    "viecuri",
    "sterrewacht",
    "rijnstate",
    "Maastricht UMC",
    "medisch spectrum twente",
    "spaarne gasthuis",
    "maxima medisch centrum",
    "altrecht",
    "european association of urology",
    "stichting epilepsie instellingen",
    "Maastricht UMC-Holding B.V.",
    "maastricht umc+",
    "maastricht u.",
    "isric - world soil information",
    "catharina/maxima (check loc)",
    "GGZ",
    "adelante",

    # COLLEGE & SCHOOL
    "college",
    "escola",              # Portuguese, Catalan
    "école",               # French
    "escuela",             # Spanish
    "scuola",              # Italian
    "schule",              # German
    "school",              # English, Dutch
    "skola",               # Swedish
    "koulu",               # Finnish
    "szkoła",              # Polish

    # POLYTECHNIC & TECHNICAL
    "polytechnic",
    "polytech",
    "politecn",            # Politecnico (Italian/Spanish)
    "technische\\s+universit", # German
    "technical\\s+universit",
    "institute?\\s+of\\s+technology",
    "teknisk",             # Swedish/Norwegian/Danish
    "politechnika",
    "politechnique",

    # INSTITUTE & RESEARCH CENTER
    "\\binstitut\\b",      # Institut, Instituto, Istituto
    "instituto",           # Spanish/Portuguese
    "istituto",            # Italian
    "instytut",            # Polish
    "\\binst\\b",          # Common abbreviation

    "research\\s+cent",    # Research centre/center
    "research\\s+institut",
    "centre\\s+de\\s+recherche",     # French
    "centro\\s+de\\s+investigaci",   # Spanish
    "centro\\s+di\\s+ricerca",       # Italian
    "forskningscentr",     # Swedish
    "forschungszentrum",   # German
    "onderzoek",           # Dutch research

    # LABORATORY
    "laborator",           # Covers laboratory, laboratoire, laboratorio, etc.
    "\\blab\\b",
    "\\blabs\\b",

    # ACADEMY & ACADEMIES
    "academ",              # Academy, académie, academia, akademie, etc.
    "akadēmij",            # Latvian
    "akademie",
    "akademija",

    # FACULTY & DEPARTMENT
    "facult",              # Faculty, faculté, facultad, facoltà, etc.
    "department",
    "departamento",
    "dipartimento",
    "département",
    "faculdade",

    # HIGHER EDUCATION SPECIFIC TERMS
    "hochschule",          # German university of applied sciences
    "fachhochschule",      # German
    "haagse\\s+hogeschool", # Dutch
    "hogeschool",          # Dutch
    "hogskol",             # Swedish/Norwegian
    "korkeakoulu",         # Finnish
    "yliopisto",           # Finnish university

    # RESEARCH-SPECIFIC ORGANIZATIONS
    "observatory",
    "observatoire",
    "observatorio",
    "osservatorio",

    # FAMOUS EUROPEAN RESEARCH ORGS
    "\\bcnrs\\b",          # French National Centre for Scientific Research
    "\\binria\\b",         # French Institute for Research in Computer Science
    "\\binserm\\b",        # French National Institute of Health
    "\\bcea\\b",           # French Alternative Energies and Atomic Energy Commission
    "\\bcsic\\b",          # Spanish National Research Council
    "\\bcnr\\b",           # Italian National Research Council
    "\\bfnr\\b",           # Luxembourg National Research Fund
    "\\bfwo\\b",           # Research Foundation Flanders
    "\\bfrs\\b",           # Belgian Fund for Scientific Research
    "fraunhofer",          # German research organization
    "max\\s+planck",       # German research organization
    "helmholtz",           # German research organization
    "leibniz",             # German research organization
    "\\btno\\b",           # Netherlands Organisation for Applied Scientific Research
    "\\bvtt\\b",           # Finnish Technical Research Centre

    # MEDICAL SCHOOLS & HOSPITALS (academic)
    "medical\\s+school",
    "school\\s+of\\s+medicine",
    "faculty\\s+of\\s+medicine",
    "teaching\\s+hospital",
    "university\\s+hospital",
    "academic\\s+medical",
    "akademisch\\s+ziekenhuis",  # Dutch academic hospital
    "chu\\b",              # Centre Hospitalier Universitaire (French)
    "chru\\b",             # French
    "universitair\\s+medisch",   # Dutch
    "universitätsklinik",  # German
    "univerzitet",
    "akademie",
    "boards of regents",
    "universidad",
    "agentia",

    # DOCTORAL & GRADUATE SCHOOLS
    "doctoral\\s+school",
    "graduate\\s+school",
    "ph\\.?d\\.?\\s+program"
  )
  government_patterns <- c(

    # MINISTRY
    "ministr",             # Ministry, ministère, ministerio, ministero, ministerium, etc.
    "minister\\b",
    "ministère",
    "ministerio",
    "ministero",
    "ministerium",
    "ministerul",
    "ministere",
    "ministerie",
    "ministerstvo",
    "miniszterium",
    "bundesministerium",
    "ministerio",
    "ministerijos",
    "ministeerium",
    "ministerija",
    "ministarstvo",
    "Kultturiministeriö",
    "ministerstwo",
    "ministeru",
    "ministry",
    "dienst",
    "metropole",
    "chamber",
    "chambre",
    "kamer",
    "epimelitirio",
    "region",
    "agentschap",
    "fundacao",



    # GOVERNMENT
    "government",
    "gouvernement",
    "gobierno",
    "governo",
    "regierung",
    "reger",               # Swedish regering
    "hallitus",            # Finnish government
    "rząd",                # Polish government

    # MUNICIPALITY & LOCAL GOVERNMENT
    "municipality",
    "municipal",
    "commune",             # French/Belgian
    "ayuntamiento",        # Spanish city council
    "câmara\\s+municipal", # Portuguese
    "gemeente",            # Dutch municipality
    "kommun",              # Swedish
    "kunta",               # Finnish municipality
    "gmina",               # Polish municipality
    "stad\\b",             # Dutch/Swedish city
    "city\\s+council",
    "city\\s+of\\b",
    "ville\\s+de\\b",      # French

    # REGIONAL GOVERNMENT
    "regional\\s+government",
    "region",              # When governmental context
    "comunidad\\s+autónoma",  # Spanish autonomous community
    "generalitat",         # Catalan/Valencian government
    "junta\\s+de",         # Spanish regional government
    "land\\b",             # German state
    "bundesland",          # German federal state
    "kanton",              # Swiss canton
    "voivodeship",         # Polish region
    "county\\s+council",
    "département",         # French administrative division

    # NATIONAL/FEDERAL BODIES
    "national",
    "federal",
    "état\\b",             # French state
    "estado\\b",           # Spanish/Portuguese state
    "stato\\b",            # Italian state
    "staat\\b",            # German state
    "rijks",               # Dutch state/national
    "national\\s+institute",

    # PUBLIC AGENCIES
    "agency",
    "agence",              # French
    "agencia",             # Spanish/Portuguese
    "agentur",             # German
    "agenzia",             # Italian
    "bureau\\b",
    "office\\s+of",
    "administration",

    # PUBLIC HEALTH & SERVICES
    "public\\s+health",
    "santé\\s+publique",
    "salud\\s+pública",
    "health\\s+authority",
    "health\\s+service",
    "\\bnhs\\b",           # UK National Health Service
    "service\\s+public",

    # COUNCILS & COMMITTEES
    "council",
    "conseil",             # French
    "consejo",             # Spanish
    "consiglio",           # Italian
    "raad\\b",             # Dutch council
    "råd\\b",              # Swedish/Norwegian/Danish council
    "commission",
    "committee",
    "comité",
    "comitato",

    # DIRECTORATES
    "directorate",
    "direction\\s+générale",
    "dirección\\s+general",
    "direção\\s+geral",

    # PARLIAMENT & LEGISLATIVE
    "parliament",
    "parlement",
    "parlamento",
    "assemblé",            # French assembly
    "congreso",            # Spanish congress
    "senado\\b",           # Senate
    "bundestag",           # German parliament
    "folketing",           # Danish parliament

    # SPECIFIC EU/INTERNATIONAL GOVERNMENT
    "european\\s+commission",
    "european\\s+parliament",
    "eurostat",
    "\\beasa\\b",          # European Aviation Safety Agency
    "\\bema\\b",           # European Medicines Agency

    # COURTS & JUSTICE
    "court\\b",
    "tribunal",
    "cour\\b",             # French court
    "rechtbank",           # Dutch court
    "justice",

    # POLICE & SECURITY
    "police",
    "gendarmerie",
    "guardia\\s+civil",
    "carabinieri",

    # EMBASSIES & DIPLOMATIC
    "embassy",
    "ambassad",            # Embassy in various languages
    "consulate",
    "diplomatic",

    # OBSERVATORIES & STATISTICAL OFFICES (governmental)
    "statistical\\s+office",
    "statistics\\s+",
    "\\bins\\b",           # Often statistical institutes
    "census\\s+bureau"
  )
  private_company_patterns <- c(
    # Match legal forms with flexible spacing/dots
    "\\bb\\.?\\s?v\\.?\\b",        # BV, B.V., B.V, b.v.
    "\\bn\\.?\\s?v\\.?\\b",        # NV
    "\\bc\\.?\\s?v\\.?\\b",        # CV
    "\\bv\\.?\\s?o\\.?\\s?f\\.?\\b", # VOF
    "\\bpc\\b",

    # Belgium
    "\\bbvba\\b", "\\bsprl\\b", "\\bcvba\\b", "\\bscrl\\b",

    # Germany
    "\\bgmbh\\b", "\\bag\\b", "\\bkg\\b", "\\bohg\\b", "\\bgbr\\b", "\\bug\\b",

    # France
    "\\bs\\.?\\s?a\\.?\\b", "\\bsarl\\b", "\\bsas\\b", "\\bsasu\\b",
    "\\beurl\\b", "\\bsci\\b", "\\bsca\\b", "\\bsnc\\b", "\\beirl\\b",

    # Spain
    "\\bs\\.?\\s?l\\.?\\b", "\\bs\\.?\\s?a\\.?\\b", "\\bsll\\b", "\\bsal\\b",

    # Italy
    "\\bsrl\\b", "\\bspa\\b", "\\bss\\b",

    # UK/Ireland
    "\\bltd\\.?\\b", "\\bplc\\b", "\\bllp\\b", "limited", "\\bteorant\\b",

    # Portugal
    "\\blda\\b", "unipessoal",

    # Sweden
    "\\ba\\.?\\s?b\\.?\\b", "\\bhb\\b", "\\bkb\\b",

    # Denmark/Norway
    "\\ba\\.?\\s?s\\.?\\b", "\\baps\\b", "\\basa\\b", "\\bans\\b", "\\bba\\b",

    # Finland
    "\\boy\\b", "\\boyj\\b",

    # Poland
    "\\bz\\.?\\s?o\\.?\\s?o\\.?\\b", "spółka",

    # Czech/Slovakia
    "\\bsro\\b", "\\bs\\.?\\s?r\\.?\\s?o\\.?\\b",

    # Austria/Switzerland
    "\\bgmbh\\b", "\\bag\\b",

    # Greece
    "\\bae\\b", "\\bepe\\b", "\\boe\\b", "\\bee\\b",

    # Romania
    "\\bsrl\\b",

    # Hungary
    "\\bkft\\b", "\\brt\\b", "\\bbt\\b", "\\bzrt\\b",

    # Bulgaria
    "\\bood\\b", "\\bad\\b", "\\bkd\\b",

    # Croatia/Slovenia
    "\\bd\\.?\\s?o\\.?\\s?o\\.?\\b", "\\bdd\\b",

    # Estonia/Latvia/Lithuania
    "\\bou\\b", "\\bsia\\b", "\\buab\\b",

    # Luxembourg
    "\\bsoparfi\\b",

    # International
    "\\binc\\.?\\b", "\\bcorp\\.?\\b", "\\bllc\\b", "\\bl\\.?\\s?p\\.?\\b",
    "\\bco\\.?\\b", "corporation", "incorporated",

    # Company indicators
    "company", "compan", "corporate", "enterprises", "group\\b", "holding",

    # Industry-specific
    "pharmaceutical", "pharma\\b", "biotech", "therapeutics",
    "medical\\s+device", "software", "solutions", "technologies",
    "consulting", "consultancy", "engineering", "systems", "industries", "fundacao", "\\bLTD\\b"
  )
  # End patterns ------------
  results <- tibble(
    org_name = org_names,
    org_lower = tolower(org_names)
  ) %>%
    mutate(
      # First check Research & Education (highest priority)
      is_research_edu = str_detect(org_lower,
                                   paste(research_education_patterns, collapse = "|")),

      # Then check Government
      is_government = str_detect(org_lower,
                                 paste(government_patterns, collapse = "|")),

      # Finally check Private indicators
      is_private = str_detect(org_lower,
                              paste(private_company_patterns, collapse = "|")),

      # Classification logic
      org_type = case_when(
        is_research_edu ~ "Research_Education",
        is_government ~ "Government",
        TRUE ~ "Private"  # Default: if we can't classify, assume private
      ),

      # Confidence flag
      confidence = case_when(
        is_research_edu ~ "High",
        is_government ~ "High",
        is_private ~ "Medium",
        TRUE ~ "Low"
      )
    ) %>%
    select(org_name, org_type, confidence)

  # Bind results back to original data
  data_with_classification <- data %>%
    bind_cols(
      results %>% select(org_type, confidence)
    )

  return(data_with_classification)
}



