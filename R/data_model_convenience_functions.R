data_model_columnn <- function(column_name, domain){
  data.table::data.table(
    column_name=column_name,
    domain=domain,
    domain_type = classify_external_data_domains(domain)
  )
}


extract_ <- function(i, what) {
  lapply(i, function(x)
    x[[what]])
}
