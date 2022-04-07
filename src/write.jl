# http://raymondlo84.blogspot.com/2015/09/how-to-write-multipage-tiff-file.html
function writeScanline( data::Array{T,N}, fn ) where {T,N}

	tf = LIBTIFF.TIFFOpen( fn, "w" )

	h, w, d = size( data );  

	for z in 1:d
		
		LIBTIFF.tiff_set_width( tf, UInt32( w ) )
		LIBTIFF.tiff_set_length( tf, UInt32( h ) )
		LIBTIFF.tiff_set_subfiletype( tf, UInt32( 1 ) )
		LIBTIFF.tiff_set_nbits( tf, UInt32( T.size * 8 ) )
		LIBTIFF.tiff_set_samplesperpixel( tf, UInt32( 1 ) )
		LIBTIFF.tiff_set_pagenumber( tf, UInt32( z-1 ), UInt32( d ) )
		
		for r in 1:h
			col  = data[ r, :, z ]; 
			err  = LIBTIFF.TIFFWriteScanline( tf, col, UInt32( r-1 ), UInt16( 1 ) )
		end

		LIBTIFF.TIFFWriteDirectory( tf )
	end

	LIBTIFF.TIFFClose( tf )
end