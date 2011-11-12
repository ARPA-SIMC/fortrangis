TYPE gdal_gcp BIND(C)
  TYPE(c_ptr) :: pszid, pszinfo
  REAL(kind=c_double) :: dfGCPPixel, dfGCPLine, dfGCPX, dfGCPY, dfGCPZ
END TYPE gdal_gcp

TYPE gdalrpcinfo BIND(C)
  REAL(kind=c_double) :: dfline_off, dfsamp_off, dflat_off, dflong_off, dfheight_off
  REAL(kind=c_double) :: dfline_scale, dfsamp_scale, dflat_scale, dflong_scale, dfheight_scale
  REAL(kind=c_double) :: adfline_num_coeff(20), adfline_den_coeff(20), &
   adfsamp_num_coeff(20), adfsamp_den_coeff(20)
  REAL(kind=c_double) :: dfmin_long, dfmin_lat, dfmax_long, dfmax_lat
END TYPE gdalrpcinfo

TYPE gdalcolorentry BIND(C)
 INTEGER(kind=c_short) :: c1, c2, c3, c4
END TYPE gdalcolorentry

