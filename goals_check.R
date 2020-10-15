# PII Reviews
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
installed_packages = as.data.frame(installed.packages())
required_packages = c("tidyverse", "lubridate", "httr", "rjson", "odbc", "rvest", "xml2",
                      "XML", "jsonlite", "data.table", "curl")
for(p in required_packages) {
  if(!p %in% installed_packages$Package) {
    install.packages(p)
  }
  library(p, character.only = T)
}
rm(list = ls(pattern = "_packages"))

# Credentials
ss_api_key = readRegistry("Environment", hive = "HCU")$smartsheet_api_key
ss_api_pwd = readRegistry("Environment", hive = "HCU")$smartsheet_pwd
qb_api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
qb_api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
qb_url = readRegistry("Environment", hive = "HCU")$quickbase_api_url
api_uid = readRegistry("Environment", hive = "HCU")$email_address

# Define general GET call and begin error handling
get_call = GET(
  str_c(
    qb_url,
    "?a", "=", "API_GrantedDBs", # API_GrantedDBs function,
    "&", "usertoken", "=", qb_api_key # use API user key to authenticate (could also use ticket)
  )
) %>% 
  content() 

# Get databases to which the user has access
dbs = get_call %>% 
  xmlToDataFrame( # turn into a data frame
    doc = ., # parsed XML content from above
    homogeneous = F, # F because not all fields are uniform, filled in
    nodes = getNodeSet(xmlParse(.), "//dbinfo"), # specify the particular nodes in the XML doc to add to the data frame
    stringsAsFactors = F
  ) %>% 
  as_tibble() 

# Query database for all requests in Pending PII review status
drs = GET(
  str_c(
    str_replace(qb_url, "main", dbs$dbid[str_detect(dbs$dbname, "Status")]), 
    "?a", "=", "API_DoQuery", 
    "&query={'2'.IR.'last+1000+d'}", 
    "&clist=a", 
    "&", "usertoken", "=", qb_api_key 
  )
) %>% 
  content() %>% 
  xmlToDataFrame(
    doc = ., 
    homogeneous = F, 
    nodes = getNodeSet(xmlParse(.), "//record"), 
    stringsAsFactors = F
  ) %>% 
  as_tibble() %>% 
  arrange(desc(data_request_id), desc(as.numeric(date_modified))) %>% 
  filter(!str_detect(str_to_lower(purpose_of_data_request), "test")) %>% 
  mutate(
    date_created = as.POSIXct(as.numeric(date_created) / 1000, origin = "1970-01-01"),
    date_modified = as.POSIXct(as.numeric(date_created), origin = "1970-01-01"),
  ) %>% 
  arrange(data_request_id, date_modified) %>%
  group_by(data_request_id) %>% 
  summarize(
    date_created = min(date_created),
    date_modified = max(date_modified), 
    first_status = first(status),
    last_status = last(status),
    data_request_manager = last(data_request_manager)
  )
  
# Average length of time to complete data requests
mutate(
  drs, 
  fy = case_when(
    between(date_created, ymd_hms('2018-10-01 00:00:01'), ymd_hms('2019-09-30 11:59:00')) ~ "FY2019",
    between(date_created, ymd_hms('2019-10-01 00:00:01'), ymd_hms('2020-09-30 11:59:00')) ~ "FY2020"
    
  )
) %>% 
  filter(last_status %in% c("Closed", "Complete")) %>%
  group_by(fy) %>% 
  summarize(
    avg_time = mean(difftime(date_modified, date_created, units = 'days'), na.rm = T)
  ) %>% 
  ungroup()
  


# %>% 
#   summarize(
#     
#     requestor = first(requesting_organization_final),
#     topic_area = first(topic_area),
#     type_of_data = first(level_of_data),
#     dsa_in_place = first(request___moa_status),
#     date_submitted_for_approval = min(as.POSIXct(as.numeric(date_date_submitted_for_pending_pii_approval) / 1000, origin = "1970-01-01")),
#     analyst = first(data_request_analyst),
#     qa = first(data_request_qa),
#     data_location = first(file_location),
#     data_sharing_mechanism = first(data_sharing_mechanism),
#     status = first(status)
#   ) %>% 
#   arrange(date_requested)

# %>% 
#   filter(str_detect(status, "Pending PII")) %>% 
#   mutate(
#     dsa_in_place = case_when(
#       dsa_in_place == "MoA not required" ~ "Not needed",
#       str_detect(dsa_in_place, "on file") ~ "Y",
#       str_detect(dsa_in_place, "not required") ~ "Not needed",
#     ),
#     type_of_data = case_when(
#       str_detect(type_of_data, "-sup") & str_detect(type_of_data, "-unsup") ~ "Aggregate suppressed and unsuppressed",
#       str_detect(type_of_data, "-sup") ~ "Aggregate suppressed",
#       str_detect(type_of_data, "-unsup") ~ "Aggregate unsuppressed",
#       str_detect(type_of_data, "Indiv") & str_detect(type_of_data, "Agg") ~ "Individual-level and aggregate",
#       str_detect(type_of_data, "Indiv") ~ "Individual-level",
#       str_detect(type_of_data, "record") ~ "Student education record"
#     ),
#     date_requested = as.character(date_requested + hours(6)) %>% 
#       str_replace_all(" ", "T") %>% 
#       str_c("Z"),
#     date_submitted_for_approval = as.character(now() + hours(6)) %>% 
#       str_replace_all(" ", "T") %>% 
#       str_c("Z")
#   )
