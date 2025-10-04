import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import pandas as pd
from PIL import Image
import matplotlib.patches as patches

Image.MAX_IMAGE_PIXELS = None
img = Image.open('$HOME/HadalTrench_ARGs/Fig4a/basemaps/gebco_downscaled.png')

df = pd.read_csv('$HOME/HadalTrench_ARGs/Fig4a/Fig4a.sourceData.tsv', sep='\t')
def standardize_longitude(lon):
    return lon if lon >= 0 else 360 + lon
df['Longitude_std'] = df['Longitude_num'].apply(standardize_longitude)

fig = plt.figure(figsize=(12,8)) 
ax = plt.axes(projection=ccrs.PlateCarree(central_longitude=180)) 
ax.set_extent([89.5, 190, -40.5, 20], crs=ccrs.PlateCarree()) 
ax.imshow(img, origin='upper', extent=[80, 200, -50, 30], transform=ccrs.PlateCarree(),interpolation='none')

ax.scatter(df['Longitude_num'], df['Latitude_num'], 
    c='red', s=30, alpha=1, 
    transform=ccrs.PlateCarree()) 

# ---------- Latitude and longitude tags ----------
yticks = range(-40, 21, 10)
ax.set_yticks(yticks, crs=ccrs.PlateCarree())
ax.set_yticklabels([f"{abs(y)}°S" if y < 0 else ("EQ" if y == 0 else f"{y}°N") for y in yticks],fontdict={'family': 'sans-serif', 'weight': 'bold', 'style': 'italic', 'size': 10})

xticks = range(90, 191, 20)
ax.set_xticks(xticks, crs=ccrs.PlateCarree())
ax.set_xticklabels([f"{x}°E" if x <= 180 else f"{360 - x}°W" for x in xticks],fontdict={'family': 'sans-serif', 'weight': 'bold', 'style': 'italic', 'size': 10})

ax.tick_params(axis='both', which='both', length=0)

# ---------- Directional arrow ----------
arrow_color = '#333333'
ax.text(94, 16, 'N', ha='center', va='center',
        transform=ccrs.PlateCarree(), color=arrow_color,fontdict={'family': 'sans-serif', 'weight': 'bold', 'style': 'italic', 'size': 10})
ax.arrow(92, 14, 0, 2, transform=ccrs.PlateCarree(),
         width=0.3,head_width=1, head_length=1.2, color=arrow_color)

# ---------- Black and white scale ----------

bbox = ax.get_position()
x0, y0, width, height = bbox.x0, bbox.y0, bbox.width, bbox.height

for i, lat in enumerate(range(-40, 21, 10)):
    color = 'black' if i % 2 == 0 else 'white'
    rect = patches.Rectangle((89.5, lat), 0.5, 10, linewidth=0,
                             edgecolor='none', facecolor=color,
                             transform=ccrs.PlateCarree(), zorder=100)
    ax.add_patch(rect)

for i, lon in enumerate(range(90, 191, 10)):
    color = 'black' if i % 2 == 0 else 'white'
    rect = patches.Rectangle((lon, -40.5), 10, 0.5, linewidth=0,
                             edgecolor='none', facecolor=color,
                             transform=ccrs.PlateCarree(), zorder=100)
    ax.add_patch(rect)

# ---------- Regional Annotation ----------
trench_labels = {
    'Mariana Trench': (150, 14),
    'Massau Trench': (163, -4),
    'Kermadec Trench': (177, -27),
    'Yap Trench': (143, 7),
    'Diamantina Trench': (103, -30),
    'New Britain Trench': (160, 2),
}

for name, (lon, lat) in trench_labels.items():
    ax.text(lon, lat, name, fontsize=12, transform=ccrs.PlateCarree(),
            fontweight='bold',fontname='sans-serif', style='italic',
            ha='center', va='center', color='white', zorder=200)

plt.savefig('$HOME/HadalTrench_ARGs/Fig4a/Fig4a.pdf',dpi=600, bbox_inches='tight', pad_inches=0.1)