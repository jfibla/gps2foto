# 📸 gps2foto — Assign GPS Coordinates to JPG Photos with Shiny

**gps2foto** is an interactive [Shiny](https://shiny.rstudio.com/) application that allows you to add GPS coordinates to JPG photos by clicking on a map.  
The updated photos can be downloaded to your computer with the coordinates embedded in their EXIF metadata.

---

## 🚀 Features

- **Multi-language interface**: Spanish, Catalan, and English (live switching).
- **Photo upload**: Import one or multiple JPG files.
- **Photo viewer**: Browse, preview, and navigate through the uploaded images.
- **Read EXIF metadata**: Check if GPS coordinates are already present.
- **Interactive map**: Click anywhere to assign new coordinates.
- **Save coordinates**:  
  - Apply to the selected photo.  
  - Apply to all selected photos in the table.  
- **Automatic renaming**: New files are saved with the suffix `_gps` to avoid overwriting.
- **Download options**:  
  - Download the last modified copy.  
  - Download all modified copies as a ZIP archive.
- **Modern UI**: Styled with [bslib](https://rstudio.github.io/bslib/) (light/dark theme toggle).

---

## 🖥️ How It Works

1. **Upload photos** using the file input.  
2. **Preview photos** in the side panel.  
3. **Click on the map** to select the GPS coordinates.  
4. **Assign GPS data** to one or more photos.  
5. **Download the new photos** with the `_gps` suffix.

---

## 📦 Requirements

- R ≥ 4.2  
- The following R packages:  
  - `shiny`  
  - `bslib`  
  - `leaflet`  
  - `leaflet.extras`  
  - `DT`  
  - `dplyr`  
  - `mime`  
  - `exiftoolr`

Additionally, the app requires [ExifTool](https://exiftool.org/) installed on your system, since `exiftoolr` is just an R interface to it.

---

## ▶️ Running the App

Clone or download this repository, then launch the app from R:

```r
shiny::runApp("gps2foto")

📂 Output

Modified photos are written to a temporary folder during the session.

You can download them directly to your computer via the provided buttons:

Download last copy: retrieves the last modified photo.

Download all (ZIP): packages all modified photos into a ZIP archive.

## 🗺️ Example Workflow

Upload photo1.jpg and photo2.jpg.

Read their metadata → shows that neither has GPS coordinates.

Click on the map near Barcelona.

Assign GPS to all selected photos.

Download the _gps versions to your computer.

## ⚠️ Notes

The original photos remain untouched.

Only the copies with _gps suffix contain the new coordinates.

Browser downloads rely on Shiny’s downloadHandler — they work in hosted apps (e.g., shinyapps.io) or local runs.

📜 License

MIT License © 2025 Joan Fibla
