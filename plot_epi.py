import os
# os.system("source activate seis")
import pygmt
import pandas as pd
import numpy as np
import obspy
from obspy import read
from glob import glob
import matplotlib.pyplot as plt
from obspy.clients.iris import Client
from obspy.core import Stream

## Input Catalog combined of IGP and international agencies for tomography
cata = pd.read_csv("00_evt",
                   skiprows=0,
                   usecols=range(0,10),
                   delim_whitespace=True,
                   #names=None
                   names=["date","time","lat","long","dep","mag","erh","erz","rms","id"]
                   )
## Stations
stas = pd.read_csv("00_sta",
                      delim_whitespace=True,
                      names=["name","lat","long","elv","idx"]
)

fig = pygmt.Figure()
# Figure setting
minlon, maxlon = 75, 90
minlat, maxlat = 25, 32

# Config the figure 
pygmt.config(FONT_LABEL="13p,Times-Bold,black")
pygmt.config(FONT_TITLE="13p,Times-Bold,black")
pygmt.config(FONT_ANNOT_PRIMARY="10p,Times-Bold,black")
pygmt.config(FONT_ANNOT_SECONDARY="10p,Times-Roman,black")
#
grid = pygmt.datasets.load_earth_relief(resolution="30s", region=[100, 110, 18, 25])
#
fig.coast(
    region=[minlon, maxlon, minlat, maxlat],
    projection='M5i',
#    projection='X10i/5i',
    shorelines=True,
    frame=["a2g1.0","NSWE"],
    water="cornflowerblue",
#     land="lightgray",
    land="gainsboro",
    dcw="NP+glightbrown+p0.2p",
#     Td=["jCM","+w5c","+pthicker,#306998","+t361/30/15"],
#         "jCM",  # Plot compass on Center Middle
#         "+w5c",  # Width of 5 centimeter
#         "+pthicker,#306998",  # Python 'blue' ring
#         "+t361/30/15",  # Label compass ticks every 361, 30 and 15 degrees
#         "+o0.2c"
#     Td=["jTL+w2c+o0.2c+pthicker+t361/30/15"], # Compass
    borders=["1/1.5p,black","2/1.5p,black","3/1.5p,black"],
)
# Stations
fig.plot(
    x=stas.long,
    y=stas.lat,
    style="t0.08i",
    color="darkblue",
    pen="0.5p,darkblue"
)
# Background seismicity
pygmt.makecpt(cmap="seis", series=(0,800,1),reverse=True)
# Catalog
fig.plot(
    y=cata.lat,
    x=cata.long,
    size=0.1*1.1**cata.mag,
    color=cata.dep,
    cmap=True,
    style="cc",
    pen="0.5p,black"
)
#
for m in [2,3,4,5,6]:
    mag = 0.1*1.1**m
    fig.plot(
        x=[0],
        y=[0],
        #color="mediumpurple",
        style='c{}'.format(mag), 
        pen='0.5p,black',label="ML={}".format(m)
    )
fig.legend(position='JTR+jTR+o0.5c/0.5c',box='+gwhite+p0.5p',transparency=10)
fig.colorbar(
    frame=['xa100f500+l"Focal depth"', "ya50f10+l(km)"],
#    position="JML+o2c/3.95c+w-5c/0.3c+n+mc",
    position="JBL+jBL+o1.0c/0.5c+w-3.0c/0.12c",
    box='+p1p',
    transparency=10
)
fig.savefig("00_Seismicity.png",crop=True, dpi=1000, transparent=True)
# fig.show(method='external')
# fig.show()