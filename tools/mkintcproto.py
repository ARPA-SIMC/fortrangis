#!/usr/bin/python
import sys
import re

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

print "MODULE X"
print "USE,INTRINSIC :: ISO_C_BINDING"
print "IMPLICIT NONE"
print

for line in sys.stdin.readlines():
    declmatch = declre.search(line)
    if declmatch is not None:
        gr = declmatch.groups()
        type = gr[1].strip()
        name = gr[2]
        if gr[3].strip() == '':
            args = ()
        else:
            args = tuple(arg.strip() for arg in gr[3].split(','))
        print "INTERFACE"
        if type == 'void': 
            proc = 'SUBROUTINE'
            proctype = ''
        else:
            if gr[0] == 'const ':
#                print 'quiqui',gr[0]
                type = gr[0]+type
#                print 'quiqui',':'+type+' '+name+':'
            proc = 'FUNCTION'
            proctype, dummy = makearg(type+' '+name, valuetag='', result=True)
        arglist = ''
        argdecllist = []
        for arg in args:
            fdecl, fname = makearg(arg)
            argdecllist.append(fdecl)
            if arglist == '': arglist = fname
            else: arglist = arglist+', '+fname

        print ("  %s %s(%s) BIND(C,name='%s')") % \
            (proc, name.lower(), arglist, name)
        print "  IMPORT"

        for line in argdecllist: print "  "+line

        if proctype != '':
            print "  "+proctype
        print "  END %s %s" % (proc,name.lower())
        print "END INTERFACE"
        print

print "END MODULE X"
