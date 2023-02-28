# FrenchNFI

Repository to download and format tree and plot level data of the French National Forest Infentory (NFI) to FUNDIV template. Based on functions initially coded by Natheo Beauchamp (INRAE). 

Package ```targets``` is needed to run the script. Once ```targets``` is loaded, just run ```tar_make()``` from R and the script will download the packages and data needed. 

The script writes two files in a directory "output": FrenchNFI_plot.csv and FrenchNFI_tree.csv, that respectively contains plot level and tree level data.
