library(dplyr)      # Untuk manipulasi data (filter, select, mutate)
library(tidyr)      # Untuk merapikan data dan menangani NA
library(lubridate)  # Untuk memanipulasi format tanggal dan waktu
library(janitor)    # Untuk membersihkan nama kolom
library(readxl)
library(readr)



dataMentah <- read_excel("../Coffee Shop Sales Dataset.xlsx")
# any(is.na(dataMentah)) #Mengecek Secara Keseluruhan apk ada shell kosong
View(dataMentah)

# ----------------MEMBANGUN SISTEM--------------------

#KAMUS
#clean_nk = Membersihkan nama kolom (menghilangkan spasi, mengubah ke huruf kecil)
#clean_p = Penyetaraan format penulisan
#clean_sk = clean shell kosong

# 1. Membersihkan nama kolom (menghilangkan spasi, mengubah ke huruf kecil)
clean_nk <- dataMentah %>% clean_names()

# 2. Transformasi Tipe Data (Penyetaraan format penulisan)
clean_p <- clean_nk %>% 
  mutate(
    # Mengubah format tanggal menjadi standar Date (asumsi format asal DD-MM-YYYY)
    transaction_date = excel_numeric_to_date(as.numeric(transaction_date)),
    # Menyeragamkan teks produk menjadi huruf kecil semua agar tidak bias
    product_detail = tolower(product_detail), 
    product_category = tolower(product_category), 
    product_detail = tolower(product_detail),
  )

# 3. Membuang Outlier Ekstrem pada Kuantitas (Menggunakan metode IQR)
Q1 <- quantile(clean_p$transaction_qty, 0.25, na.rm = TRUE)
Q3 <- quantile(clean_p$transaction_qty, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
Upper_Bound <- Q3 + 1.5 * IQR_value

df_final <- clean_p %>%
  filter(transaction_qty <= Upper_Bound)

View(df_final)
any(is.na(df_final))
sum(is.na(df_final))
colSums(is.na(df_final))

clean_sk <- df_final %>% drop_na()

any(is.na(clean_sk))

write.csv(clean_sk, "hasil_dataCleaning.csv", row.names = FALSE)