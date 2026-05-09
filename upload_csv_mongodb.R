# ==========================================
# LIBRERÍAS
# ==========================================
library(mongolite)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# ==========================================
# CSV
# ==========================================
data <- read_csv("C://Users//claud//OneDrive//Escritorio//Hospital_Admissions_ULTIMATE.csv")

# ==========================================
# BACKEND
# ==========================================
data_clean <- data %>%
  mutate(

    # ID
    Patient_ID = toupper(Patient_ID),
    Patient_ID = str_extract(Patient_ID, "\\d+"),
    Patient_ID = paste0("P-", str_pad(Patient_ID, 4, pad="0")),

    # Date
    Date_of_Birth = ifelse(
      Date_of_Birth %in% c("Unknown","") | str_detect(Date_of_Birth,"2099"),
      NA,
      Date_of_Birth
    ),

    Date_of_Birth = suppressWarnings(case_when(
      str_detect(Date_of_Birth, "^\\d{2}-\\d{2}-\\d{4}$") ~ as.character(dmy(Date_of_Birth)),
      str_detect(Date_of_Birth, "^\\d{2}/\\d{2}/\\d{4}$") ~ as.character(dmy(Date_of_Birth)),
      str_detect(Date_of_Birth, "^\\d{4}-\\d{2}-\\d{2}$") ~ as.character(ymd(Date_of_Birth)),
      str_detect(Date_of_Birth, "^\\d{4}/\\d{2}/\\d{2}$") ~ as.character(ymd(Date_of_Birth)),
      TRUE ~ NA_character_
    )),

    # Age

    # Sex
    Sex = case_when(
      str_to_lower(Sex) %in% c("m","male") ~ "Male",
      str_to_lower(Sex) %in% c("f","female") ~ "Female",
      str_to_lower(Sex) == "x" ~ NA_character_,
      TRUE ~ NA_character_
    ),

    # Weight 
    Weight = as.numeric(gsub("[^0-9.]", "", Weight)),

    # Height
    Height = suppressWarnings(as.numeric(gsub("[^0-9.]", "", Height))),
    Height = round(Height, 2),

    # Blood
    Blood_Type = case_when(
      str_detect(Blood_Type, "^AB\\+") ~ "AB+",
      str_detect(Blood_Type, "O\\s*Positive|O\\+") ~ "O+",
      str_detect(Blood_Type, "A\\s*Pos|A\\+") ~ "A+",
      str_detect(Blood_Type, "B\\s*Pos|B\\+") ~ "B+",
      str_detect(Blood_Type, "A-") ~ "A-",
      str_detect(Blood_Type, "B-") ~ "B-",
      str_detect(Blood_Type, "O-") ~ "O-",
      str_detect(Blood_Type, "AB-") ~ "AB-",
      TRUE ~ NA_character_
    ),

    # Dosage
    Dosage_mg = suppressWarnings(as.numeric(gsub("[^0-9.]", "", Dosage_mg))),

    # Smoker
    Smoker = case_when(
      str_to_lower(Smoker) %in% c("yes","y","smoker") ~ "Yes",
      str_to_lower(Smoker) %in% c("no","n","non-smoker") ~ "No",
      TRUE ~ NA_character_
    ),
      
    # Doctor
    Doctor_Name = case_when(
      str_detect(str_to_lower(Doctor_Name), "starnge") ~ "Dr. Strange",
      str_detect(str_to_lower(Doctor_Name), "huose") ~ "Dr. House",
      str_detect(str_to_lower(Doctor_Name), "^dr\\.\\s*w$|^w$") ~ "Dr. Who",
      str_detect(str_to_lower(Doctor_Name), "house") ~ "Dr. House",
      str_detect(str_to_lower(Doctor_Name), "strange") ~ "Dr. Strange",
      str_detect(str_to_lower(Doctor_Name), "who") ~ "Dr. Who",
      TRUE ~ Doctor_Name
    )
  ) %>%

  distinct(Patient_ID, .keep_all = TRUE) %>%

  # ==========================================
  # RENOMBRAR COLUMNAS (ORDEN ORIGINAL)
  # ==========================================
  rename(
    "Patient ID" = Patient_ID,
    "Birth Date" = Date_of_Birth,
    "Age" = Age,
    "Sex" = Sex,
    "Weight (kg)" = Weight,
    "Height (m)" = Height,
    "Blood Type" = Blood_Type,
    "Diagnosis Code" = Diagnosis_Code,
    "Dosage (mg)" = Dosage_mg,
    "Smoker" = Smoker,
    "Doctor" = Doctor_Name
  )

# ==========================================
# CONEXIÓN
# ==========================================
con <- mongo(
  collection = "patient-collection",
  db = "hospital_db",
  url = "YOUR_MONGODB_URL"
)

# ==========================================
# LIMPIAR COLECCIÓN
# ==========================================
con$remove('{}')

# ==========================================
# INSERTAR
# ==========================================
# con$insert(data_clean)
for(i in 1:nrow(data_clean)){
  tryCatch({
    con$insert(data_clean[i,])
  }, error = function(e){
    print(paste("❌ Error en fila:", i))
  })
}

# ==========================================
# CHECK
# ==========================================
print(con$count())
head(con$find())