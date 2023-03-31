# Only grayscale scanline tiffs have been tested so far, so do not trust read_rga_scanline or read_tile

""" GENERAL READING INTERFACE """ 

function tiffread( filename::String; typ=nothing, sample=1 )
	f    = tiffopen(filename)
	data = tiffread(f, typ=typ, sample=sample )
	tiffclose(f)
    return data
end

function tiffread!( data, filename::String; typ=nothing, sample=1 )
	f = tiffopen(filename)
	data = tiffread!( data, f, typ=typ, sample=sample )
	tiffclose(f)
    return data
end


""" READING SCANLINE TIFFs """

function tiffread( f::TIFFFile{Scanline,N}; typ=nothing, sample=(1,1,1) ) where {N}
	return ( tiff_samplesperpixel(f) > 1 ) ? read_rgba_scanline(f) : read_scanline(f, typ=typ, sample=sample );  
end

function tiffread!( data, f::TIFFFile{Scanline,N}; typ=nothing, sample=(1,1,1) ) where {N}
	return ( tiff_samplesperpixel(f) > 1 ) ? read_rgba_scanline!(data, f) : read_scanline!(data, f, typ=typ, sample=sample );  
end

function read_scanline( f::TIFFFile{Scanline,N}; typ=nothing, sample=(1,1,1) ) where {N}

    typ = ( typ == nothing ) ? eltype(f) : typ; 

	maxZ = ( N == 2 ) ? 1 : f.dims[3]; 

	# vertical, horizontal and depth indices
	vids = 1:sample[1]:f.dims[1]
	hids = 1:sample[2]:f.dims[2]
	dids = 1:sample[3]:maxZ

	# dims after considering sampling
	sdims = ( length(vids), length(hids), length(dids) ); 
	data  = zeros( typ, sdims ); 

    return read_scanline!( data, f, typ=typ, sample=sample ); 
end

function read_scanline!( data, f::TIFFFile{Scanline,N}; typ=nothing, sample=(1,1,1) ) where {N}

    typ  = ( typ == nothing ) ? eltype(f) : typ; 
	maxZ = ( N == 2 ) ? 1 : f.dims[3]; 

	# vertical, horizontal and depth indices
	vids = collect(1:sample[1]:f.dims[1])
	hids = 1:sample[2]:f.dims[2]
	dids = 1:sample[3]:maxZ

    buff = zeros( eltype(f), 1, f.dims[2] );

	zi = 1; 
	for z = dids

		#print( " " ); 
		# by printing, we don't get stuck at warnings. TODO: find a better way to fix this

		TIFFSetDirectory( f.fptr, Cushort(z-1) )
 
		ri = 1; 
		for r = 1:vids[end]

			TIFFReadScanline( f.fptr, buff, Cint(r-1), Cint(0) ); 

			if r == vids[ri]

		    	data[ri, :, zi] = convert.( typ, buff[hids] )

				ri += 1;
			end
		end

	zi += 1;
	end

	#println(); 

    return data
end



function read_rgba_scanline(f::TIFFFile{Scanline,2}) 
    buffer = Array{UInt32}(f.dims[2], f.dims[1]) # transposed because libtiff saves row major
	TIFFReadRGBAImageOriented( f.fptre, f.dims[2], f.dims[1], 1, 0 )
    buffer = buffer'
    n0f8_data = unsafe_wrap(Array, Ptr{UInt8}(pointer(buffer)), (4, f.dims[1], f.dims[2]))
    return n0f8_data
end



""" READING TILED TIFFs """
# Images are divided into squares, tiles, of size n*16. Each tile has coordinates (x,y) where x and y are between 0:imgWidth÷tileWidth and 0:imgHeight÷tileHeight. Generally, one can restrict image loading to a rectangle by providing (minCol, maxCol, minRow, maxRow) and setting x and y between minCol÷tileWidth:maxCol÷tileWidth and minRow÷tileWidth:maxRow÷tileWidth

function tiffread(f::TIFFFile{Tile,2}; typ=nothing, sample=1 )
	return read_tile(f, 1:f.dims[1], 1:f.dims[2])
end

function read_tile(f::TIFFFile{Tile,2}, i1::Int_Range, i2::Int_Range ) 
    last(i1) <= f.dims[1] && last(i2) <= f.dims[2] || throw("BoundError in read_tile") 
    return read_tile( first(i2)-1, last(i2), first(i1)-1, last(i1), f )
end

# should work for 3 channels (RGB) and 1 channel. Can't test it. I need test images that work. 
function read_tile(minW, maxW, minH, maxH, f::TIFFFile{Tile}) 
	
	tileH,tileW = tifftilesize(f); # offY,offX=startY*tileW-1,startX*tileH-1; 
	println( tileH, " ", tileW )
	tileHcoords = minH÷tileH:maxH÷tileW; 
	tileWcoords = minW÷tileW:maxW÷tileW; 
	ntilesH     = length( tileHcoords );
	ntilesW     = length( tileWcoords ); 
	nchannels   = tiff_samplesperpixel(f); 
	data = zeros( eltype(f), nchannels, tileW*ntilesW, tileH*ntilesH )
	buff = zeros( eltype(f), nchannels, tileW, tileH )

    for row in tileHcoords, col in tileWcoords
		println( row, " ", col, " ", tileH, "  ", tileW ) 
		TIFFReadTile(f.fptr, buff, Cint(col), Cint(row), Cint(0), Cint(0)); 
        data[:, col+1:col+tileW, row+1:row+tileH] = permutedims(buff, [1, 3, 2]);
    end
    return data[:, minW+1:maxW, minH+1:maxH ]
end