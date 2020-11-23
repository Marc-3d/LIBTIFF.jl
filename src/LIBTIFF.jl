module LIBTIFF

import Base: eltype, size, getindex, summary
export TIFFFile, tiffopen, tiffclose, tiffsize, tiffread
# using FixedPointNumbers, ColorTypes

""" NOTES FOR ME IN THE FUTURE
 > I am missing error handling, aka err != 1 && ( nsamples[1] = 1 )  in samples_per_pixel 
 > if bits is not divisible by 2, julia will automatically give an error in getType
 > not so suitable if i1::Int and i2::Int, no tile fits between first(i2)-1 and last(i2)
 > the images in /test_data/ are not well labeled and some are not even openable by fiji
 > a tiff file can contain multiple images of different sizes if the tif file contains multi-resolution views of the same image. I didn't consider this case. 
""" 

abstract type Tile     end
abstract type Scanline end

struct TIFFFile{T <: Union{Tile,Scanline},N}
    fptr::Ptr{Cvoid}
	dims::NTuple{N,Int}
end 


# Handy variables ------------------------------------

TIFFField  = (:TIFFGetField,"libtiff");
Int_Range  = Union{UnitRange{Int},Int}; 

TYPES      = ( UInt8 , Int8,      NaN, 
               UInt16, Int16, Float16, 
               UInt32, Int32, Float32,
               UInt64, Int64, Float64 
			 );

exceptions = Dict( 
			   Cstring      => String,
               Ptr{Nothing} => Union{Array,Ptr{Cvoid}}
             );


""" METAPROGRAMMING TO BRING LIBTIFF FUNCTIONS TO JULIA """

# TIFFGetField acts in-place, overwriting an input array with the Field's value(s). The input must be an array because some TIFFFields return 2 or 3 values, not only 1. Because it is in-place, a variables needs to be created before each call to TIFFGetField, which results in long code. Metaprogramming is used to automate the definition of out-of-place functions for each relevant TIFFTag. 
tag_data = ( ( "tiff_length"         , UInt32 ,   1   ,  Cuint   , 257 ), 
			 ( "tiff_width"          , UInt32 ,   1   ,  Cuint   , 256 ), 
			 ( "tiff_tile_length"    , UInt32 ,   1   ,  Cuint   , 323 ), 
			 ( "tiff_tile_width"     , UInt32 ,   1   ,  Cuint   , 322 ), 
			 ( "tiff_sampleformat"   , UInt16 ,   1   ,  Cushort , 339 ), 
			 ( "tiff_nbits"          , UInt16 ,   1   ,  Cushort , 258 ), 
			 ( "tiff_samplesperpixel", UInt16 ,   1   ,  Cushort , 277 ) )

for ( p, r, a, w, n ) in tag_data 
    f1 = """
    function $p( h::Ptr{Cvoid} )
	    in = ones( $r, $a );
	    er = ccall($TIFFField, Cint, (Ptr{Cvoid},$w,Ptr{Cvoid}), h, $n, in);
		( er == 0 ) && throw("Error retrieving $(p[6:end]) from tiff file")
	    return ( $a == 1 ) ? round(Int64,in[1]) : round.(Int64,in)
    end """
	f2 = "$p(f::TIFFFile) = $p( f.fptr )"
	eval( Meta.parse( f1 ) ); 
	eval( Meta.parse( f2 ) );
end

# metaprogramming is used to create julia functions for relevant libtiff functions, so I don't need to write "ccall( ... )" and to memorize the signature of each function. 
tifffuns = (
("TIFFOpen"                 , Ptr{Cvoid}, (Cstring,Cstring)                               ),
("TIFFClose"                , Cint      , (Ptr{Cvoid},)                                   ), 
("TIFFReadDirectory"        , Cint      , (Ptr{Cvoid},)                                   ), 
("TIFFSetDirectory"         , Cint      , (Ptr{Cvoid},Cushort)                            ),
("TIFFIsTiled"              , Cint      , (Ptr{Cvoid},)                                   ), 
("TIFFReadScanline"         , Cint      , (Ptr{Cvoid},Ptr{Cvoid},Cint,Cint)               ),
("TIFFReadRGBAImageOriented", Cint      , (Ptr{Cvoid},Clong,Clong,Ptr{Cvoid},Cshort,Cint) ), 
("TIFFReadTile"             , Csize_t   , (Ptr{Cvoid},Ptr{Cvoid},Cint,Cint,Cint,Cint)     ) ) 

CtoJL( x::DataType ) = ( x in keys(exceptions) ) ? exceptions[x] : x ; 

for ( n, o, i ) in tifffuns
    args, vars = "", ""; 
    [ args = args*" a"*string(x)*"::"*string(CtoJL(i[x]))*"," for x in 1:length(i) ]; 
    [ vars = vars*" a"*string(x)*"," for x in 1:length(i) ]; 
    f = """
    $n( $(args[1:end-1]...) ) = ccall( (:$n,"libtiff"), $o, $i, $(vars[1:end-1]...) ); 
    """
    # println( f ) # uncomment to see what the automated function code looks like
    eval( Meta.parse( f ) )
end


""" OPENING and CLOSING TIFF FILES """

function tiffopen(filename::String, mode::String="r")
	isfile(filename) || throw("tif file doesn't exist")
	h    = TIFFOpen(filename, mode)
	d, N = tiffdims(h); 
    return tiffistiled(h) ? TIFFFile{Tile,N}(h,d) : TIFFFile{Scanline,N}(h,d);
end

function tiffopen(fun::Function, filename::String, mode::String)
 
    file = tiffopen(filename, mode)
    try fun(file) finally; tiffclose(file) end
end

function tiffclose(f::TIFFFile)
	TIFFClose( f.fptr )	
end 


""" UTILITY FUNCTIONS """

# 3D datasets are stored as stacks, where multiple images (directives) are combined in the same tiff file. This function counts the number of directives in a tiff file. All images in a stack should have the same size. After determing the number of images, the directory "pointer" is set back to 0, the first image.
function tiffdims(fp::Ptr{Cvoid}) 
	ndir = 1
	w, h = tiff_width(fp), tiff_length(fp);
	while Bool( TIFFReadDirectory( fp ) ) 
		ndir += 1
		wdir, hdir = tiff_width(fp), tiff_length(fp);
		wdir == w && hdir == h || throw("Stack of images of different sizes."); 
	end
	TIFFSetDirectory( fp, Cushort(0) ) 
	return ( ndir > 1 ) ? (( h, w, ndir ), 3) : (( h, w ), 2)
end

tiffistiled(fp::Ptr{Cvoid}) = Bool( TIFFIsTiled(fp) )

      getType(format, bits) = TYPES[ Int(log(2,bits)-3)*3 + format ];

  tifftilesize(f::TIFFFile) = Int.([tiff_tile_length(f),tiff_tile_width(f)])   

       summary(f::TIFFFile) = "$(size(f)[1])x$(size(f)[2])$(typeof(f))" 

function eltype(f::TIFFFile) # work around, one of my stacks was stored in UInt, which is sampleformat code 1, but got an error. Defaulting to Cushort(1); 
	 sf = Cushort(1); 
	 try
		 sf = tiff_sampleformat(f); 
	 catch
	 end
	 return getType( sf, tiff_nbits(f)) 
end



""" READING SCANLINE TIFFs """

function tiffread(f::TIFFFile{Scanline,N}; typ=nothing, sample=1 ) where {N}
	return ( tiff_samplesperpixel(f) > 1 ) ? read_rgba_scanline(f) : read_scanline(f, typ=typ, sample=sample );  
end

function read_scanline(f::TIFFFile{Scanline,N}; typ=nothing, sample=1 ) where {N}
	if typ == nothing
		typ = eltype(f)
	end 

	# vertical, horizontal and depth indices
	vids = collect(1:sample:f.dims[1])
	hids = 1:sample:f.dims[2]
	dids = 1:sample:f.dims[3]

	# dims after considering sampling
	sdims = ( length(vids), length(hids), length(dids) ); 

	data = zeros(   typ    ,   sdims   ); 
    buff = zeros( eltype(f), 1, f.dims[2] );

	maxZ = ( N == 2 ) ? 1 : f.dims[3] - 1; 

	zi = 1; 
	for z = 1:sample:maxZ

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


""" GENERAL READING INTERFACE """ 

function tiffread(filename::String; typ=nothing, sample=1 )
	f = tiffopen(filename)
	data = tiffread(f, typ=typ, sample=sample )
	tiffclose(f)
    return data
end


function getindex(file::TIFFFile, indices::Vararg{Union{UnitRange{Int}, Int}, 2})
    return tiffread(file, indices...)
end

end # module
