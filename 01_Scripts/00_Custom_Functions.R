
# Check for error ---------------------------------------------------------

f_Error_Check <- function(expr){
  any(class(tryCatch(expr, error = function(e) e)) == "error")
}


# Read sql file -----------------------------------------------------------

# Read SQL query into string
f_Get_SQL <- function(filepath){
  readr::read_file(filepath)
}
#cat(f_Get_SQL(here::here("06_Test", "get_sql_query.sql")))

# Replace variables/names in query
f_Replace_SQL <- function(query, pattern, replacement){
  gsub(pattern, replacement, query)
}
#cat(f_Replace_SQL(f_Get_SQL(here::here("06_Test", "get_sql_query.sql")), "my_count", "your_count"))



# SKD Sector classification ---------------------------------------------------

f_SKD_To_Short <- function(skd){
  skd <- as.numeric(str_sub(skd, 1, 2))
  x <- ifelse(skd <= 3, "A", 
              ifelse(skd <= 9, "B",
                     ifelse(skd <= 33, "C",
                            ifelse(skd <= 35, "D",
                                   ifelse(skd <= 39, "E",
                                          ifelse(skd <= 43, "F",
                                                 ifelse(skd <= 47, "G",
                                                        ifelse(skd <= 53, "H",
                                                               ifelse(skd <= 56, "I",
                                                                      ifelse(skd <= 63, "J",
                                                                             ifelse(skd <= 66, "K",
                                                                                    ifelse(skd <= 68, "L",
                                                                                           ifelse(skd <= 75, "M",
                                                                                                  ifelse(skd <= 82, "N",
                                                                                                         ifelse(skd <= 84, "O",
                                                                                                                ifelse(skd <= 85, "P",
                                                                                                                       ifelse(skd <= 88, "Q",
                                                                                                                              ifelse(skd <= 93, "R",
                                                                                                                                     ifelse(skd <= 96, "S",
                                                                                                                                            ifelse(skd <= 98, "T",
                                                                                                                                                   "U"))))))))))
                                                                      ))))))))))
}



f_SKD_To_Long <- function(skd){
  skd <- as.numeric(str_sub(skd, 1, 2))
  x <- ifelse(skd <= 3, "KMETIJSTVO IN LOV, GOZDARSTVO, RIBIŠTVO", 
              ifelse(skd <= 9, "RUDARSTVO",
                     ifelse(skd <= 33, "PREDELOVALNE DEJAVNOST",
                            ifelse(skd <= 35, "OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO",
                                   ifelse(skd <= 39, "OSKRBA Z VODO; RAVNANJE Z ODPLAKAMI IN ODPADKI; SANIRANJE OKOLJA",
                                          ifelse(skd <= 43, "GRADBENIŠTVO",
                                                 ifelse(skd <= 47, "TRGOVINA; VZDRŽEVANJE IN POPRAVILA MOTORNIH VOZIL",
                                                        ifelse(skd <= 53, "PROMET IN SKLADIŠČENJE",
                                                               ifelse(skd <= 56, "GOSTINSTVO",
                                                                      ifelse(skd <= 63, "INFORMACIJSKE IN KOMUNIKACIJSKE DEJAVNOSTI",
                                                                             ifelse(skd <= 66, "FINANČNE IN ZAVAROVALNIŠKE DEJAVNOSTI",
                                                                                    ifelse(skd <= 68, "POSLOVANJE Z NEPREMIČNINAMI",
                                                                                           ifelse(skd <= 75, "STROKOVNE, ZNANSTVENE IN TEHNIČNE DEJAVNOSTI",
                                                                                                  ifelse(skd <= 82, "DRUGE RAZNOVRSTNE POSLOVNE DEJAVNOSTI",
                                                                                                         ifelse(skd <= 84, "DEJAVNOST JAVNE UPRAVE IN OBRAMBE; DEJAVNOST OBVEZNE SOCIALNE VARNOSTI",
                                                                                                                ifelse(skd <= 85, "IZOBRAŽEVANJE",
                                                                                                                       ifelse(skd <= 88, "ZDRAVSTVO IN SOCIALNO VARSTVO",
                                                                                                                              ifelse(skd <= 93, "KULTURNE, RAZVEDRILNE IN REKREACIJSKE DEJAVNOST",
                                                                                                                                     ifelse(skd <= 96, "DRUGE DEJAVNOSTI",
                                                                                                                                            ifelse(skd <= 98, "DEJAVNOST GOSPODINJSTEV Z ZAPOSLENIM HIŠNIM OSEBJEM; PROIZVODNJA ZA LASTNO RABO",
                                                                                                                                                   "DEJAVNOST EKSTERITORIALNIH ORGANIZACIJ IN TELES"))))))))))
                                                                      ))))))))))
}
