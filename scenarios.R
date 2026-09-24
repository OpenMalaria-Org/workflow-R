is_xml <- function(x) inherits(x, c("xml_node", "xml_nodeset"))

variable <- function(x) {
  x <- substitute(x)
  if (!is.symbol(x)) stop("variable() must contain a column name")
  structure(as.character(x), class = "scenario_variable")
}

is_variable <- function(x) inherits(x, "scenario_variable")

create_template <- function(scaffold, ...) {
  replacements <- list(...)

  if (length(replacements) &&
      (is.null(names(replacements)) || any(!nzchar(names(replacements))))) {
    stop('Name each replacement with its XPath, e.g. "demography/@popSize" = 200')
  }

  valid <- function(x) is_xml(x) || is_variable(x) || length(x) == 1 && !is.list(x)

  if (!all(vapply(replacements, valid, logical(1)))) {
    stop("Use scalar or XML fixed values, or variable(name) for scenario variables")
  }

  list(scaffold = scaffold, replacements = replacements)
}


template_variables <- function(template) {
  variables <- Filter(is_variable, template$replacements)
  unique(vapply(variables, as.character, character(1)))
}


write_scenarios <- function(template, scenarios, experiment_folder, om,
                            validate_xml = TRUE, overwrite = FALSE) {

  if (!"index" %in% names(scenarios)) {
    stop("The scenarios table must contain an index column")
  }

  if (anyNA(scenarios$index) || anyDuplicated(scenarios$index)) {
    stop("The scenario indices must be unique and non-missing")
  }

  variables <- template_variables(template)
  missing <- setdiff(variables, names(scenarios))

  if (length(missing)) {
    stop("The following variables were mapped but are missing from the scenarios table: ", paste(missing, collapse = ", "))
  }

  if (validate_xml) {
    schema <- xml2::read_xml(
      file.path(om$path, paste0("scenario_", om$version, ".xsd"))
    )
  }

  xml_folder <- file.path(experiment_folder, "xml")

  if (file.exists(xml_folder) && !overwrite) {
    stop(
      "XML folder already exists: ", xml_folder,
      ". Use overwrite = TRUE to replace it."
    )
  }

  if (overwrite && unlink(xml_folder, recursive = TRUE)) {
    stop("Could not remove: ", xml_folder)
  }

  if (!dir.create(xml_folder, recursive = TRUE)) {
    stop("Could not create: ", xml_folder)
  }

  xml <- paste(readLines(template$scaffold, warn = FALSE), collapse = "\n")
  
  xml <- sub(
    "http://openmalaria.org/schema/scenario_[0-9]+",
    paste0("http://openmalaria.org/schema/scenario_", om$version),
    xml
  )
  
  for (row in seq_len(nrow(scenarios))) {
    index <- scenarios$index[row]
    
    doc <- xml2::read_xml(xml)
    root <- xml2::xml_root(doc)

    xml2::xml_set_attr(root, "schemaVersion", om$version)
    xml2::xml_set_attr(root, "xsi:schemaLocation", paste0(
      "http://openmalaria.org/schema/scenario_", om$version,
      " scenario_", om$version, ".xsd"
    ))

    for (replacement in seq_along(template$replacements)) {
      path <- names(template$replacements)[replacement]
      value <- template$replacements[[replacement]]

      # variable(name) means:
      # use the value from scenarios$name for this row
      if (is_variable(value)) {
        value <- scenarios[[as.character(value)]][row]
      }

      nodes <- xml2::xml_find_all(root, path, xml2::xml_ns(doc))

      if (!length(nodes)) {
        stop("XPath did not match: ", path)
      }

      if (is_xml(value)) {
        if (xml2::xml_type(nodes[[1]]) == "attribute") {
          stop("Cannot add XML to an attribute: ", path)
        }

        children <- if (inherits(value, "xml_nodeset")) value else list(value)

        for (child in children) {
          xml2::xml_add_child(nodes, child)
        }

      } else if (xml2::xml_type(nodes[[1]]) == "attribute") {

        xml2::xml_set_attr(
          xml2::xml_parent(nodes),
          xml2::xml_name(nodes[[1]]),
          value
        )

      } else {
        xml2::xml_set_text(nodes, value)
      }
    }

    if (validate_xml) {
      valid <- xml2::xml_validate(doc, schema)

      if (!valid) {
        stop(
          "Invalid scenario ", index, ":\n",
          paste(attr(valid, "errors"), collapse = "\n")
        )
      }
    }

    xml2::write_xml(
      doc,
      file.path(xml_folder, paste0(index, ".xml"))
    )
  }

  message(nrow(scenarios), " scenarios created")
}
