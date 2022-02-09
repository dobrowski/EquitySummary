

library(here)
library(tidyverse)
library(MCOE)

options(scipen=999)


con <- MCOE::mcoe_sql_con()



staff_demo <- tbl(con, "STAFF_DEMO")  %>%
    filter( CountyName == "Monterey"|CountyName =="San Benito"|CountyName =="Santa Cruz",
            AcademicYear == "1819"
    ) %>%
    collect()


staff_demo2 <- staff_demo %>%
    mutate(EthnicGroup = as.character(EthnicGroup)) %>%
    left_join_codebook("STAFF_DEMO", "EthnicGroup") %>%
    group_by(DistrictName, definition) %>%
    count() %>%
    group_by(DistrictName) %>%
    mutate(perc = n/sum(n))  

staff.color <- staff_demo2 %>%
    filter( str_detect(definition, "Latino") ) 
    distinct()
    
    
    
    write_csv(staff.color, here("grant","Perc Teacher Latino By District.csv"))

    
    
    
    
    enrollment <- tbl(con, "ENROLLMENT")  %>%
        #   head() %>%
        filter( County == "Monterey"|County =="San Benito"|County =="Santa Cruz") %>%
                
        collect()
    
    
    enrollment.sum <- enrollment %>%
        filter(YEAR == max(YEAR)) %>%
        group_by(DISTRICT, ETHNIC) %>%
        summarise(TOTAL = sum(ENR_TOTAL)) %>%
        mutate(ETHNIC = as.character(ETHNIC),
               Percent = TOTAL/sum(TOTAL)) %>%
        left_join_codebook("ENROLLMENT", "ETHNIC" )%>%
        filter( str_detect(definition, "Latino") ) 
    
    write_csv(enrollment.sum, here("grant","Perc Student Latino By District.csv"))
    
    
    
    
    
    
    staff_cred <- tbl(con, "STAFF_CRED")  %>%
         filter(  AcademicYear == "1819"
 ) %>%
        collect() %>%
        select(AcademicYear:AuthorizationType)

    
    joint <- staff_demo %>%
        left_join(staff_cred) %>%
        mutate(full.cred = if_else(CredentialType == 10,TRUE,FALSE))
    
    joint2 <- joint %>%
        filter(FTE_Teaching > 0,
               FTE_administrative < 90,
               FTE_PupilServices < 90) %>%
        group_by(RecID,DistrictName) %>%
        transmute(all.full = mean(full.cred)) %>%
        unique() %>%
        mutate(all.full2 = if_else(all.full <1, FALSE, TRUE),
               all.full2 = if_else(is.na(all.full2),FALSE,all.full2)  ) %>%
        ungroup() %>%
        group_by(DistrictName) %>%
        transmute(count = n(),
               perc = mean(all.full2)) %>%
        unique()
        
    
    write_csv(joint2, here("grant","Perc Teacher with Full Creditial By District.csv"))
    
    
        
    
    upc <- tbl(con, "UPC")  %>%
         filter( county_name == "Monterey"|county_name =="San Benito"|county_name =="Santa Cruz",
                 academic_year == "2020-2021"
         ) %>%
 #       head(5) %>%
        collect()
    
    
    upc2 <- upc %>%
        group_by(district_name) %>%
        transmute(school_name,
                  total_enrollment,
                  calpads_unduplicated_pupil_count_upc,
                  perc = sum(calpads_unduplicated_pupil_count_upc)/sum(total_enrollment))

    
    upc3 <- upc2 %>%
        select(district_name, perc) %>%
        unique()

    
    write_csv(upc3, here("grant","Perc Undup By District.csv"))
    
