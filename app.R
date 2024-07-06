# Add this line at the beginning of your app.R script
cat("Current working directory is:", getwd(), "\n")

# List of packages to install
packages <- c("shiny", "webchem", "stringr")

# Install missing packages
install.packages(setdiff(packages, rownames(installed.packages())))

# Load all packages
lapply(packages, library, character.only = TRUE)

library(shiny)
library(webchem)
library(stringr)

# Function to get SMILES string from a single molecule name using different services
get_smiles_single <- function(molecule_name) {
  cat("Trying CIR for", molecule_name, "...\n")
  compound_info <- tryCatch({
    cir_query(molecule_name, 'smiles')
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(compound_info) && "smiles" %in% names(compound_info) && !is.na(compound_info$smiles[1]) && compound_info$smiles[1] != "") {
    cat("Found SMILES string using CIR for", molecule_name, ":", compound_info$smiles[1], "\n")
    return(compound_info$smiles[1])
  }
  
  cat("No SMILES string found using CIR. Trying ChemSpider for", molecule_name, "...\n")
  compound_info <- tryCatch({
    cs_compinfo(molecule_name, "smiles")
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(compound_info) && "smiles" %in% names(compound_info) && !is.na(compound_info$smiles[1]) && compound_info$smiles[1] != "") {
    cat("Found SMILES string using ChemSpider for", molecule_name, ":", compound_info$smiles[1], "\n")
    return(compound_info$smiles[1])
  }
  
  cat("No SMILES string found using ChemSpider. Trying PubChem PUG REST for", molecule_name, "...\n")
  cids <- tryCatch({
    get_cid(molecule_name)
  }, error = function(e) {
    NULL
  })
  if (!is.null(cids) && length(cids) > 0) {
    compound_info <- tryCatch({
      pc_prop(cids[1], properties = "canonical_smiles")
    }, error = function(e) {
      NULL
    })
  } else {
    compound_info <- NULL
  }
  
  if (!is.null(compound_info) && "canonical_smiles" %in% names(compound_info) && !is.na(compound_info$canonical_smiles[1]) && compound_info$canonical_smiles[1] != "") {
    cat("Found SMILES string using PubChem PUG REST for", molecule_name, ":", compound_info$canonical_smiles[1], "\n")
    return(compound_info$canonical_smiles[1])
  }
  
  cat("No SMILES string found using any service for", molecule_name, ".\n")
  return(NULL)
}

# Function to get synonyms for a molecule name from multiple databases
get_all_synonyms <- function(molecule_name) {
  synonyms <- c()
  
  cat("Fetching synonyms from PubChem for", molecule_name, "...\n")
  pubchem_synonyms <- tryCatch({
    pc_synonyms(molecule_name)
  }, error = function(e) {
    cat("Error fetching synonyms from PubChem for", molecule_name, ":", e$message, "\n")
    NULL
  })
  if (!is.null(pubchem_synonyms) && length(pubchem_synonyms$synonyms) > 0) {
    synonyms <- unique(c(synonyms, pubchem_synonyms$synonyms))
  }
  
  cat("Fetching synonyms from ChemSpider for", molecule_name, "...\n")
  chemspider_synonyms <- tryCatch({
    cs_compinfo(molecule_name, "synonyms")
  }, error = function(e) {
    cat("Error fetching synonyms from ChemSpider for", molecule_name, ":", e$message, "\n")
    NULL
  })
  if (!is.null(chemspider_synonyms) && "synonyms" %in% names(chemspider_synonyms) && length(chemspider_synonyms$synonyms) > 0) {
    synonyms <- unique(c(synonyms, chemspider_synonyms$synonyms))
  }
  
  cat("Fetching synonyms from ChEMBL for", molecule_name, "...\n")
  chembl_synonyms <- tryCatch({
    get_chembl_synonyms(molecule_name)
  }, error = function(e) {
    cat("Error fetching synonyms from ChEMBL for", molecule_name, ":", e$message, "\n")
    NULL
  })
  if (!is.null(chembl_synonyms) && "synonyms" %in% names(chembl_synonyms) && length(chembl_synonyms$synonyms) > 0) {
    synonyms <- unique(c(synonyms, chembl_synonyms$synonyms))
  }
  
  return(synonyms)
}

# Function to generate a basic SMILES string if none is found
generate_basic_smiles <- function(molecule_name) {
  basic_smiles <- list(
    "water" = "O",
    "methane" = "C",
    "ethanol" = "CCO",
    "benzene" = "c1ccccc1",
    "glucose" = "C(C1C(C(C(C(O1)O)O)O)O)O"
  )
  
  # A simple heuristic approach to generate basic SMILES
  if (tolower(molecule_name) %in% names(basic_smiles)) {
    smiles <- basic_smiles[[tolower(molecule_name)]]
    cat("Generated basic SMILES string for", molecule_name, ":", smiles, "\n")
    return(smiles)
  }
  
  # Attempt to construct SMILES from basic components
  if (grepl("water", tolower(molecule_name))) {
    return("O")
  } else if (grepl("methane", tolower(molecule_name))) {
    return("C")
  } else if (grepl("ethanol", tolower(molecule_name))) {
    return("CCO")
  } else if (grepl("benzene", tolower(molecule_name))) {
    return("c1ccccc1")
  } else if (grepl("glucose", tolower(molecule_name))) {
    return("C(C1C(C(C(C(O1)O)O)O)O)O")
  }
  
  # Add more heuristic rules as needed
  if (grepl("chloroform", tolower(molecule_name))) {
    return("C(Cl)(Cl)Cl")
  } else if (grepl("acetic acid", tolower(molecule_name))) {
    return("CC(=O)O")
  } else if (grepl("acetone", tolower(molecule_name))) {
    return("CC(=O)C")
  }
  
  cat("No predefined or heuristic SMILES string available for", molecule_name, "\n")
  return(NULL)
}

# Function to get SMILES string and synonyms for a list of molecule names
get_smiles_and_synonyms <- function(molecule_names) {
  all_results <- list()
  
  for (name in molecule_names) {
    synonyms <- NULL
    # Try to find SMILES for the main name
    smiles <- get_smiles_single(name)
    if (is.null(smiles)) {
      # Try to find synonyms and search for SMILES using them
      cat("Searching for synonyms of", name, "...\n")
      synonyms <- get_all_synonyms(name)
      
      if (!is.null(synonyms)) {
        for (synonym in synonyms) {
          cat("Trying synonym", synonym, "...\n")
          smiles <- get_smiles_single(synonym)
          if (!is.null(smiles)) {
            break
          }
        }
      }
    }
    
    if (is.null(smiles)) {
      # Generate a basic SMILES if none is found
      smiles <- generate_basic_smiles(name)
    }
    
    all_results[[name]] <- list(canonical_smiles = smiles, synonyms = synonyms)
  }
  
  return(all_results)
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Molecule to SMILES Converter"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("molecule_names", "Enter molecule names (one per line):", 
                placeholder = "E.g., water\nmethane\nethanol"),
      actionButton("convert", "Convert to SMILES")
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("results")
    )
  )
)

# Define server logic for application
server <- function(input, output) {
  
  observeEvent(input$convert, {
    molecule_names <- unlist(strsplit(input$molecule_names, "\n"))
    molecule_names <- str_trim(molecule_names)
    
    results <- get_smiles_and_synonyms(molecule_names)
    
    output_text <- c()
    for (name in names(results)) {
      entry <- results[[name]]
      canonical_smiles <- entry$canonical_smiles
      synonyms <- entry$synonyms
      if (!is.null(canonical_smiles)) {
        output_text <- c(output_text, paste(name, ":", canonical_smiles))
      } else {
        output_text <- c(output_text, paste(name, ": SMILES string not found"))
      }
      if (!is.null(synonyms) && length(synonyms) > 0) {
        output_text <- c(output_text, paste("Synonyms for", name, ":", paste(synonyms, collapse = ", ")))
      }
    }
    
    output$results <- renderText({
      paste(output_text, collapse = "\n")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
