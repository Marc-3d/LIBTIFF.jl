# LIBTIFF.jl

This repository was adapted to open three-dimensional datasets stored in TIFF files. 

The code relies on metaprogramming to bring into Julia the relevant functions from the libtiff API written in C. 

After these function are defined in Julia, they can be used without writting `ccall`'s everywhere, which would otherwise make the code quite lengthy. 
