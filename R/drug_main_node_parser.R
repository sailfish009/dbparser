drug_elements_names <-
  c(
    "drugbank-id",
    "name",
    "description",
    "cas-number",
    "unii",
    "average-mass",
    "monoisotopic-mass",
    "state",
    "synthesis-reference",
    "indication",
    "pharmacodynamics",
    "mechanism-of-action",
    "metabolism",
    "absorption",
    "half-life",
    "protein-binding",
    "route-of-elimination",
    "volume-of-distribution",
    "clearance",
    "drug",
    "international-brands",
    "fda-label",
    "msds" ,
    "toxicity"
  )
# Extract drug df
drug_row <- function(drug) {
  drug_attributes <- drug_element_value(drug)
  drug_elements <- map_dfc(xml_find_all(drug, "./*[not(*)]"),
                           ~drug_element_value(.x))
  if ("other_Keys" %in% names(drug_elements)) {
    drug_elements <- drug_elements %>%
      unite("Keys", starts_with("other_Keys"), sep = "_")
  }
  return(bind_cols(drug_elements, drug_attributes))
}

drug_element_value <- function(d) {
  d_name <- xml_name(d)
  if (!d_name %in% drug_elements_names) {
    return()
  }
  if (d_name == "drug") {
    return(bind_rows(xml_attrs(d)))
  }
  d_atrribute <- xml_attr(d, "primary")
  if (d_name == "drugbank-id" & is.na(d_atrribute)) {
    d_name <- "Other_Keys"
  }
  d_value <- xml_text(d)
  return(tibble(!!d_name := d_value))
}
