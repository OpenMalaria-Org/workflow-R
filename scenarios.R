is_xml <- function(x) inherits(x, c("xml_node", "xml_nodeset"))

s <- function(scaffold, ...) {
  replacements <- list(...)

  if (length(replacements) &&
      (is.null(names(replacements)) || any(!nzchar(names(replacements))))) {
    stop('Name each replacement with its XPath, e.g. "demography/@popSize" = 200')
  }

  valid <- function(x) is_xml(x) || length(x) == 1 &&
    (!is.list(x) ||
       length(names(x)) == 1 &&
       nzchar(names(x)) &&
       length(x[[1]]) > 0)

  if (!all(vapply(replacements, valid, logical(1)))) {
    stop("Use scalar or XML fixed values, or list(name = values) for varying values")
  }

  list(scaffold = scaffold, replacements = replacements)
}


mapping_variables <- function(mapping) {
  variables <- unname(Filter(
    function(x) is.list(x) && !is_xml(x),
    mapping$replacements
  ))

  if (!length(variables)) return(list())

  variables <- do.call(c, variables)
  variables[!duplicated(names(variables), fromLast = TRUE)]
}


create_scenarios <- function(mapping) {
  variables <- mapping_variables(mapping)

  if (!length(variables)) {
    return(data.table::data.table(index = 1L))
  }

  scenarios <- data.table::as.data.table(expand.grid(
    variables,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ))

  scenarios[, index := .I]
  data.table::setcolorder(scenarios, "index")

  scenarios
}


write_scenarios <- function(mapping, scenarios, experiment_folder, om,
                            validate_xml = TRUE, overwrite = FALSE) {

  if (!"index" %in% names(scenarios)) {
    stop("scenarios must contain an index column")
  }

  if (anyNA(scenarios$index) || anyDuplicated(scenarios$index)) {
    stop("scenario indices must be unique and non-missing")
  }

  variables <- mapping_variables(mapping)
  missing <- setdiff(names(variables), names(scenarios))

  if (length(missing)) {
    stop("Missing scenario columns: ", paste(missing, collapse = ", "))
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

  xml <- paste(readLines(mapping$scaffold, warn = FALSE), collapse = "\n")
  
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

    for (replacement in seq_along(mapping$replacements)) {
      path <- names(mapping$replacements)[replacement]
      value <- mapping$replacements[[replacement]]

      # list(name = values) means:
      # use the value from scenarios$name for this row
      if (is.list(value) && !is_xml(value)) {
        value <- scenarios[[names(value)]][row]
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
