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

TYPES      = ( UInt8 , Int8,      NaN, 
               UInt16, Int16, Float16, 
               UInt32, Int32, Float32,
               UInt64, Int64, Float64 
			);

exceptions = Dict(  Cstring      => String,
                    Ptr{Nothing} => Union{Array,Ptr{Cvoid}}
            );

CtoJL( x::DataType ) = ( x in keys(exceptions) ) ? exceptions[x] : x ; 

TIFFGetField  = (:TIFFGetField,"libtiff");
TIFFSetField  = (:TIFFSetField,"libtiff");

Int_Range  = Union{UnitRange{Int},Int}; 


""" METAPROGRAMMING TO BRING LIBTIFF FUNCTIONS TO JULIA """

# TIFFGetField acts in-place, overwriting an input array with the Field's value(s). The input must be an array because some TIFFFields return 2 or 3 values, not only 1. Because it is in-place, a variables needs to be created before each call to TIFFGetField, which results in long code. Metaprogramming is used to automate the definition of out-of-place functions for each relevant TIFFTag. 
get_tag = ( 
	( "tiff_length"          , UInt32 ,  1  ,  Cuint , 257 ), 
	( "tiff_width"           , UInt32 ,  1  ,  Cuint , 256 ), 
	( "tiff_tile_length"     , UInt32 ,  1  ,  Cuint , 323 ), 
	( "tiff_tile_width"      , UInt32 ,  1  ,  Cuint , 322 ), 
	( "tiff_sampleformat"    , UInt32 ,  1  ,  Cuint , 339 ),  # documentation says the three fields below (339,258,277) 
	( "tiff_nbits"           , UInt32 ,  1  ,  Cuint , 258 ),  # should be UInt16, but I get errors unless I store them 
	( "tiff_samplesperpixel" , UInt32 ,  1  ,  Cuint , 277 ),  # as UInt32
	( "tiff_planarconfig"    , UInt32 ,  1  ,  Cuint , 284 ),
)

for ( p, r, a, w, n ) in get_tag 
    f1 = """
    function $p( h::Ptr{Cvoid} )
	    in = ones( $r, $a );
	    er = ccall($TIFFGetField, Cint, (Ptr{Cvoid},$w,Ptr{Cvoid}), h, $n, in);
		( er == 0 ) && throw("Error retrieving $(p[6:end]) from tiff file")
	    return ( $a == 1 ) ? round(Int64,in[1]) : round.(Int64,in)
    end """
	f2 = "$p(f::TIFFFile) = $p( f.fptr )"
	eval( Meta.parse( f1 ) ); 
	eval( Meta.parse( f2 ) );
end

set_tag = ( 
	( "tiff_set_length"          , (Ptr{Cvoid},Cuint,Cuint)       , 257 ), 
	( "tiff_set_width"           , (Ptr{Cvoid},Cuint,Cuint)       , 256 ), 
	( "tiff_set_sampleformat"    , (Ptr{Cvoid},Cuint,Cuint)       , 339 ),  
	( "tiff_set_nbits"           , (Ptr{Cvoid},Cuint,Cuint)       , 258 ),  
	( "tiff_set_samplesperpixel" , (Ptr{Cvoid},Cuint,Cuint)       , 277 ),  
	( "tiff_set_planarconfig"    , (Ptr{Cvoid},Cuint,Cuint)       , 284 ),
	( "tiff_set_pagenumber"      , (Ptr{Cvoid},Cuint,Cuint,Cuint) , 297 ),
	( "tiff_set_subfiletype"     , (Ptr{Cvoid},Cuint,Cuint)       , 254 )
)

for ( s, i, n ) in set_tag 
	args, vars = "h::Ptr{Cvoid}, ", "h, $n, ";  
    [ args = args*" a"*string(x)*"::"*string(CtoJL(i[x]))*"," for x in 3:length(i) ]; 
	[ vars = vars*" a"*string(x)*"," for x in 3:length(i) ]; 
    f1 = """
    function $s( $(args[1:end-1]...) )
	    er = ccall($TIFFSetField, Cint, $i,  $(vars[1:end-1]...) );
		( er == 0 ) && throw("Error setting $(s[10:end]) in tiff context")
	    return er
    end """
	#println( f1 )
	eval( Meta.parse( f1 ) ); 
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
	("TIFFReadTile"             , Csize_t   , (Ptr{Cvoid},Ptr{Cvoid},Cint,Cint,Cint,Cint)     ), 
	("TIFFScanlineSize"         , Clonglong , (Ptr{Cvoid},)                                   ), 
	("TIFFWriteScanline"        , Cint      , (Ptr{Cvoid},Ptr{Cvoid},Cuint,Cushort)           ),
	("TIFFWriteDirectory"       , Cint      , (Ptr{Cvoid},)                                   ),
) 

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

function ignoreWarnings( a, b, c )::Ptr{Cvoid}
	println("shit happened")
end
ignoreWarnings_c = @cfunction( ignoreWarnings, Ptr{Cvoid}, ( Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8} ) ); 

function ignoreWarningsExt( a, b, c )::Ptr{Cvoid}
	println("shit happenedExt")
end
ignoreWarningsExt_c = @cfunction( ignoreWarningsExt, Ptr{Cvoid}, ( Ptr{Cvoid}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8} ) ); 

out = ccall( (:TIFFSetWarningHandler,"libtiff"), Ptr{Cvoid}, ( Ptr{Cvoid}, ), ignoreWarnings_c )
out = ccall( (:TIFFSetWarningHandlerExt,"libtiff"), Ptr{Cvoid}, ( Ptr{Cvoid}, ), ignoreWarningsExt_c )

dump( out )

include("read.jl"); 
include("write.jl");

end # module
