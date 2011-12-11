#!/usr/bin/python
import sys
import re
import textwrap

# usage:
# cpp -Ixx src.c | cproto | ../mkintcproto.py > interface.f90

declre = re.compile(r"^(const )?(.*[ *] ?)([a-zA-Z_][a-zA-Z_0-9]*)\((.*)\) *; *$")
argre = re.compile(r"^(const )?([a-zA-Z_][a-zA-Z_0-9]* ?)(\*\*?)? ?([a-zA-Z_][a-zA-Z_0-9]*)")

enumtypes = {
    'GDALDataType':'int',
    'GDALAsyncStatusType':'int',
    'GDALAccess':'int',
    'GDALRWFlag':'int',
    'GDALColorInterp':'int',
    'GDALPaletteInterp':'int',
    'GDALRATFieldType':'int',
    'GDALRATFieldUsage':'int',
    'CPLErr':'int'
}

dertypes_ptr = {
    'GDALDatasetH':'TYPE(gdaldataseth)',
    'GDALRasterBandH':'TYPE(gdalrasterbandh)',
    'GDALDriverH':'TYPE(gdaldriverh)',
    'GDALColorTableH':'TYPE(gdalcolortableh)',
    'GDALRasterAttributeTableH':'TYPE(gdalrasterattributetableh)',
}
dertypes = {
    'GDAL_GCP':'TYPE(gdal_gcp)',
    'GDALRPCInfo':'TYPE(gdalrpcinfo)',
    'GDALColorEntry':'TYPE(gdalcolorentry)',
}

def makearg(carg, valuetag=',VALUE', result=False):
    argmatch = argre.search(carg)
    if argmatch is None: return ('unknown1:'+carg,'err')
    gr = argmatch.groups()
    cdef = gr[1].strip()
    name = gr[3].lower()
    comm = ''
    intent = ''
    arr = ''
    if enumtypes.has_key(cdef):
        comm = ' ! '+cdef
        cdef = enumtypes[cdef]
    if (cdef == 'void' or result) and gr[2] == '*':
        refval = valuetag
        type = 'TYPE(c_ptr)'
        comm = ' ! '+cdef+'*'
    else:
        if gr[2] == '*':
            refval = ''
            arr = '(*)'
        elif gr[2] == '**':
            cdef = 'charpp'
            comm = ' ! TYPE(charpp)'
            refval = ''
        else:
            refval = valuetag
        if cdef == 'char':
            type = 'CHARACTER(kind=c_char)'
        elif cdef == 'int':
            type = 'INTEGER(kind=c_int)'
        elif cdef in ('float', 'double'):
            type = 'REAL(kind=c_%s)' % (cdef,)
        else:
            if dertypes.has_key(cdef):
                type = dertypes[cdef]
            elif dertypes_ptr.has_key(cdef):
                type = dertypes_ptr[cdef]
                refval = valuetag
            else:
                type = 'TYPE(c_ptr)'
                refval = valuetag
            arr = ''
#            else:
#                return 'unknown2:'+gr[1]+':'+carg
    if gr[0] is not None and refval == '' and not result:
        intent = ',INTENT(in)'
    return (type+refval+intent+' :: '+name+arr+comm, name)

ic = open(sys.argv[1])
dot = sys.argv[1].rfind('.')
if dot == -1: dot=len(sys.argv[1])
otype = open(sys.argv[1][:dot]+'_type.f90', "w")
ointerf = open(sys.argv[1][:dot]+'_interf.f90', "w")

for ctype,ftype in dertypes_ptr.iteritems():
    otype.write("""TYPE,BIND(C) :: %s
  TYPE(c_ptr) :: ptr
END TYPE %s

""" % (ctype, ctype))

for line in ic.readlines():
    declmatch = declre.search(line)
    if declmatch is not None:
        gr = declmatch.groups()
        type = gr[1].strip()
        name = gr[2]
        if gr[3].strip() == '':
            args = ()
        else:
            args = tuple(arg.strip() for arg in gr[3].split(','))
        ointerf.write("INTERFACE\n")
        if type == 'void': 
            proc = 'SUBROUTINE'
            proctype = ''
        else:
            if gr[0] == 'const ':
                type = gr[0]+type
            proc = 'FUNCTION'
            proctype, dummy = makearg(type+' '+name, valuetag='', result=True)
        arglist = ''
        argdecllist = []
        for arg in args:
            fdecl, fname = makearg(arg)
            argdecllist.append(fdecl)
            if arglist == '': arglist = fname
            else: arglist = arglist+', '+fname

        ointerf.write(" &\n".join(textwrap.wrap( \
            ("%s %s(%s) BIND(C,name='%s')") % \
            (proc, name.lower(), arglist, name),\
                width=127, initial_indent="  ", subsequent_indent="   ")))
        ointerf.write("\n  IMPORT\n")

        for line in argdecllist: ointerf.write("  "+line+"\n")

        if proctype != '':
                ointerf.write("  "+proctype+"\n")
        ointerf.write("""  END %s %s
END INTERFACE

""" % (proc,name.lower()))
