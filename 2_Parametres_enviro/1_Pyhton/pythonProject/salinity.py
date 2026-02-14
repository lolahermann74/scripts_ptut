import copernicusmarine
from datetime import datetime

# ----------------------------
# Dossier de sortie (Windows)
# ----------------------------
OUTPUT_DIRECTORY = r"G:\PTUT_AVIZONS\PYTHON\output_python\salinite"

# Coordonnées et profondeur
MIN_LON = -12.353146876467676
MAX_LON = -0.8468741731035068
MIN_LAT = 43.056314684468795
MAX_LAT = 52.91883414449522
MIN_DEPTH = 0.4940253794193268
MAX_DEPTH = 0.4940253794193268

# Variables
VARIABLES = ["so"]

# Période complète
START_YEAR = 2000
END_YEAR = 2015

# Dataset
DATASET_ID = "cmems_mod_ibi_phy-sal_my_0.027deg_P1D-m"
FILE_FORMAT = "netcdf"

# ----------------------------
# Boucle année par année
# ----------------------------
for year in range(START_YEAR, END_YEAR + 1):
    start_date = f"{year}-01-01"
    end_date = f"{year}-12-31"

    output_filename = f"salinity_ibi_{year}.nc"

    print(f"📥 Téléchargement de l'année {year} ...")

    copernicusmarine.subset(
        dataset_id=DATASET_ID,
        variables=VARIABLES,
        minimum_longitude=MIN_LON,
        maximum_longitude=MAX_LON,
        minimum_latitude=MIN_LAT,
        maximum_latitude=MAX_LAT,
        start_datetime=start_date,
        end_datetime=end_date,
        minimum_depth=MIN_DEPTH,
        maximum_depth=MAX_DEPTH,
        output_directory=OUTPUT_DIRECTORY,
        output_filename=output_filename,
        file_format=FILE_FORMAT
    )

    print(f"✅ Année {year} téléchargée et sauvegardée dans {output_filename}")

print("🎉 Téléchargement complet de toutes les années terminé !")
