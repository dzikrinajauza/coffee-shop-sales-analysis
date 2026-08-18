library(dplyr)
library(readr)
library(ggplot2)

df_cleaned <- read.csv("hasil_dataCleaning.csv")
View(df_cleaned)


# a) 5 Produk Paling Banyak Terjual (Kuantitas)

# Strategi: 
# Kelompokkan data berdasarkan product_detail, 
top_5_products <- df_cleaned %>% 
  group_by(product_detail) %>%
  # jumlahkan transaction_qty, 
  summarise(total_quantity = sum(transaction_qty, na.rm = TRUE)) %>%
  # urutkan dari yang terbesar, 
  arrange(desc(total_quantity)) %>%
  # lalu ambil 5 teratas.
  head(5)
print(top_5_products)

# b) Hari Paling Sibuk (Jumlah Transaksi)

# Strategi: 
# Kelompokkan data berdasarkan day_name (atau day_of_week), 
busiest_day <- df_cleaned %>% 
  group_by(day_name) %>%
  # hitung jumlah transaksi unik (transaction_id), lalu urutkan. 
  summarise(transaction_count = n_distinct(transaction_id)) %>%
  # Menghitung ID transaksi unik lebih akurat daripada menghitung baris data, karena satu transaksi bisa berisi beberapa produk.
  arrange(desc(transaction_count))
print(busiest_day)

# c) Jam Penjualan Tertinggi (Total Bill)

# Strategi: 
# Kelompokkan data berdasarkan hour, 
peak_hour <- df_cleaned %>%
  group_by(hour) %>%
  # jumlahkan total_bill, 
  summarise(total_sales = sum(total_bill, na.rm = TRUE)) %>%
  # lalu urutkan dari yang paling besar untuk melihat jam puncaknya.
  arrange(desc(total_sales))
head(peak_hour, 1)

# d) 5 Kategori Produk Teratas Tiap Bulan (Total Bill)

top_categories_per_month <- df_cleaned %>%
  # 1. Kelompokkan berdasarkan bulan dan kategori, lalu jumlahkan pendapatannya
  group_by(month_name, product_category) %>%
  summarise(total_sales = sum(total_bill, na.rm = TRUE), .groups = 'drop') %>%
  # 2. Kelompokkan lagi hanya berdasarkan bulan untuk melakukan pemotongan
  group_by(month_name) %>%
  # 3. Urutkan berdasarkan penjualan tertinggi di dalam masing-masing bulan
  arrange(month_name, desc(total_sales)) %>%
  # 4. Ambil 5 teratas untuk tiap grup (bulan)
  slice_head(n = 5)

print(top_categories_per_month)


#--------------------------BUAT VISUALISASI----------------------------------------

# Plot a) 5 Produk Teratas
ggplot(top_5_products, aes(x = reorder(product_detail, total_quantity), y = total_quantity)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # Memutar diagram agar nama produk horizontal
  labs(title = "5 Produk Paling Banyak Terjual",
       x = "Nama Produk",
       y = "Total Kuantitas Terjual") +
  theme_minimal()

# Mengubah day_name menjadi urutan hari yang benar
busiest_day$day_name <- factor(busiest_day$day_name, 
                               levels = c("Monday", "Tuesday", "Wednesday", 
                                          "Thursday", "Friday", "Saturday", "Sunday"))

# Plot b) Hari Paling Sibuk
ggplot(busiest_day, aes(x = day_name, y = transaction_count)) +
  geom_col(fill = "coral") +
  geom_text(aes(label = transaction_count), vjust = -0.5) + # Menambahkan angka di atas batang
  labs(title = "Jumlah Transaksi Berdasarkan Hari",
       x = "Hari",
       y = "Jumlah Transaksi") +
  theme_minimal()

# Plot c) Jam Puncak
ggplot(peak_hour, aes(x = hour, y = total_sales)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "red", size = 2) +
  # Memastikan sumbu X menampilkan setiap angka jam dengan pas
  scale_x_continuous(breaks = min(peak_hour$hour):max(peak_hour$hour)) +
  labs(title = "Tren Total Penjualan Berdasarkan Jam",
       x = "Jam Operasional",
       y = "Total Penjualan (Bill)") +
  theme_minimal()

# Plot d) Kategori Teratas per Bulan
ggplot(top_categories_per_month, aes(x = month_name, y = total_sales, fill = product_category)) +
  # position = "dodge" memisahkan batang agar bersebelahan (tidak ditumpuk)
  geom_col(position = "dodge") + 
  labs(title = "Kategori Produk Teratas Berdasarkan Bulan",
       x = "Bulan",
       y = "Total Penjualan (Bill)",
       fill = "Kategori Produk") +
  theme_minimal()


