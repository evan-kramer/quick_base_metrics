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

# Query database for all requests
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
  ) %>% 
  ungroup()


  
# Average length of time to complete data requests
avg_length = mutate(
  drs, 
  fy = case_when(
    between(date_created, ymd_hms('2018-10-01 00:00:01'), ymd_hms('2019-09-30 11:59:00')) ~ "FY2019",
    between(date_created, ymd_hms('2019-10-01 00:00:01'), ymd_hms('2020-09-30 11:59:00')) ~ "FY2020"
  )
) %>% 
  filter(last_status %in% c("Closed", "Complete")) %>%
  group_by(fy) %>% 
  summarize(
    n = n(), 
    avg_time = mean(difftime(date_modified, date_created, units = 'days'), na.rm = T)
  ) %>% 
  ungroup()
print(avg_length)

# Percent change
print((as.numeric(avg_length$avg_time[avg_length$fy == "FY2020"])[1] - 
         as.numeric(avg_length$avg_time[avg_length$fy == "FY2019"])[1]) / 
        as.numeric(avg_length$avg_time[avg_length$fy == "FY2019"])[1])
