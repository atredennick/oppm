##  quick script to make spreadsheet with site and species information

if(file.exists("../../data/site_species_list.csv")==FALSE)
{
  sites <- list.files("../../data/")[which(list.files("../../data/") != "Idaho_modern")]
  
  site_species_df <- list()
  for(do_site in sites){
    site_files <- list.files(paste0("../../data/",do_site,"/speciesData/"))
    species <- site_files[which(nchar(site_files)==4)]
    tmpdf <- data.frame(site=do_site,species=species)
    site_species_df <- rbind(site_species_df,tmpdf)
  }
  
  write.csv(site_species_df, file = "../../data/site_species_list.csv")
}
