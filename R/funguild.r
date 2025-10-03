read_raw_funguild_data <- function(fname){
  # === Reading in the FUNGuild data ===
  # The json file containing the FUNGuild database was obtained via
  # http://stbates.org/funguild_db.php. Saved as a file it had to be cleaned since
  # it contained html headers and footers.
  #
  # Immediately after reading we filter out everything with a taxonomicLevel of !=
  # 13, since this seems to be the magic number from where a genus is defined.
  #
  # FUNGuild has the confidence rankings 'Probable', 'Possible' and 'Highly
  # Probable'. We exclude for 'Possible' to strengthen the data.
  #
  # TODO: Find a solid source that taxonomicLevel == 13 is genus level. Either
  # direct contact with Mycobank or FUNGuild authors.
  funguild_data <- fromJSON(fname) |>
    filter(taxonomicLevel == 13,
           confidenceRanking != "Possible")
}

simplify_funguild_raw <- function(funguild_data){
  # The first mutate is to handle multiple guilds. As far as I understand, the
  # primary guild is in pipes, e.g., "|Wood Saprotroph|" and all other guilds are
  # in dashes. So In case we have "Lichen Parasite-|Lichenized|" we only want to
  # retain "|Lichenized|".
  # https://forum.qiime2.org/t/funguild-best-practice-for-otus-with-multiple-guilds-no-primary-and-handling-plant-pathogen-priority/33595?
  # TODO: Find confirmation for that! Either in documentation or contact authors
  #
  # The second mutate is a clean-up step to equalize e.g. "|Wood Saprotroph|" and
  # "Wood Saprotroph", i.e., to remove all pipe signs.
  #
  # The third mutate cleans up trailing whitespaces from the trophicMode.
  funguild_data |> 
    mutate(guild = if_else(str_detect(guild, "\\|"),          # Detect multiple guilds
                           str_extract(guild, "\\|[^|]+\\|"), # keep primary
                           str_extract(guild, "^[^-]+"))) |>  # else first before '-' (or whole string)
    mutate(guild = str_remove_all(guild, "\\|")) |>
    mutate(trophicMode = str_trim(trophicMode, side = "left"))
}