import seaborn as sns
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
import time

df = pd.read_csv('results.csv')
# df = pd.read_csv('slices.csv')

pkmn_type_colors = ['#78C850',  # Grass
                    '#F08030',  # Fire
                    '#6890F0',  # Water
                    '#A8B820',  # Bug
                    '#A8A878',  # Normal
                    '#A040A0',  # Poison
                    '#F8D030',  # Electric
                    '#E0C068',  # Ground
                    '#EE99AC',  # Fairy
                    '#C03028',  # Fighting
                    '#F85888',  # Psychic
                    '#B8A038',  # Rock
                    '#705898',  # Ghost
                    '#98D8D8',  # Ice
                    '#7038F8',  # Dragon
                   ]

g = sns.barplot(x='scheduler', y='cluster-performance', data=df, palette=pkmn_type_colors)

# sns.set()
# df.set_index(['scheduler']).plot(kind='bar', stacked=True)

for index, row in df.iterrows():
    g.text(row.name, row["cluster-performance"], round(row["cluster-performance"], 2), color='black', ha="center")



plt.ylabel('cluster performance - requests/s')
#plt.ylabel('CPU/RAM assigned')
#plt.xticks(fontsize=10, rotation='horizontal')
plt.show()
