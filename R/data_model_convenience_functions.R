data_model_columnn <- function(column_name, domain, full_name){
  data.table::data.table(
    column_name=column_name,
    domain=domain,
    domain_type = classify_external_data_domains_2(domain),
    full_name = full_name
  )
}


extract_ <- function(i, what) {
  lapply(i, function(x)
    x[[what]])
}
