# Konfigurasi utama Sleman Tourism Insight
APP_TITLE <- "Sleman Tourism Insight"
APP_SUBTITLE <- "Analisis statistik kunjungan wisatawan Kabupaten Sleman"
SOURCE_DESCRIPTION <- paste(
  "Sumber: Dinas Pariwisata Kabupaten Sleman; dokumen statistik 2023,",
  "rekap CSV 2024, dan STATISTIK-2026.pdf (data 2025)."
)
MONTHS_ID <- c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des")
MONTH_LABELS <- c(
  Jan = "Januari", Feb = "Februari", Mar = "Maret", Apr = "April",
  Mei = "Mei", Jun = "Juni", Jul = "Juli", Agu = "Agustus",
  Sep = "September", Okt = "Oktober", Nov = "November", Des = "Desember"
)
# Kategori III mengikuti label pada dokumen kebutuhan. Jika judul kategori pada
# Excel berbeda (mis. Wisata Buatan), data_loader mempertahankan label sumber.
CATEGORY_DEFAULTS <- c(
  "I" = "Wisata Alam",
  "II" = "Wisata Budaya",
  "III" = "Wisata Kota dan Desa"
)
# Pengecualian diteruskan dari aturan awal repository. Ubah hanya setelah
# memastikan cakupannya terhadap dokumen sumber.
EXCLUDED_SUBCATEGORY_PATTERN <- "event|mice|olahraga|mall"

# Alias destinasi hanya diisi setelah identitas objek dikonfirmasi.
DESTINATION_ALIASES <- c()
