#include <string.h>
#include "shapefil.h"

void SHPAPI_CALL SHPSetObjectFortran(void*, SHPObject*, 
 int*, int*, int*, int*, int*, int*,
 double*, double*, double*, double*,
 double*, double*, double*, double*,
 double*, double*, double*, double*);


int SHPAPI_CALL SHPReadObjectInt(SHPHandle hshp, int iShape, void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPReadObject(hshp, iShape);

  if (psObject) {
    SHPSetObjectInt(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}


int SHPAPI_CALL SHPCreateSimpleObjectInt(int nSHPType, int nVertices,
   double *padfX, double *padfY, double *padfZ, void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPCreateSimpleObject(nSHPType, nVertices, padfX, padfY, padfZ);
  if (psObject) {
    SHPSetObjectInt(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}
						 

int SHPAPI_CALL SHPCreateObjectInt(int nSHPType, int iShape,
   int nParts, int *panPartStart, int *panPartType,
   int nVertices, double *padfX, double *padfY, double *padfZ, double *padfM,
   void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPCreateObject(nSHPType, iShape, nParts, panPartStart,
			     panPartType, nVertices, padfX, padfY, padfZ, padfM);
  if (psObject) {
    SHPSetObjectInt(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}
						 

void SHPAPI_CALL SHPComputeExtentsInt(SHPObject *psObject, void *ftnobject) {

  SHPComputeExtents(psObject);
  SHPSetObjectInt(ftnobject, psObject);

}


void SHPSetObjectInt(void *ftnobject, SHPObject *psObject) {
  SHPSetObjectFortran(ftnobject, psObject,
		      &psObject->nSHPType, &psObject->nShapeId, &psObject->nParts,
		      psObject->panPartStart, psObject->panPartType,
		      &psObject->nVertices,
		      psObject->padfX, psObject->padfY,
		      psObject->padfZ, psObject->padfM,
		      &psObject->dfXMin, &psObject->dfYMin,
		      &psObject->dfZMin, &psObject->dfMMin,
		      &psObject->dfXMax, &psObject->dfYMax,
		      &psObject->dfZMax, &psObject->dfMMax);
}


/* void SHPAPI_CALL shpdestroyobject_int(SHPObject **psObject) { */

/*   SHPDestroyObject(*psObject); */

/* } */


int SHPAPI_CALL SHPRewindObjectInt(SHPHandle *hSHP, SHPObject *psObject,
				   void *ftnobject) {
  int res;
  res = SHPRewindObject(*hSHP, psObject);
  SHPSetObjectFortran(ftnobject, psObject,
		      &psObject->nSHPType, &psObject->nShapeId, &psObject->nParts,
		      psObject->panPartStart, psObject->panPartType,
		      &psObject->nVertices,
		      psObject->padfX, psObject->padfY,
		      psObject->padfZ, psObject->padfM,
		      &psObject->dfXMin, &psObject->dfYMin,
		      &psObject->dfZMin, &psObject->dfMMin,
		      &psObject->dfXMax, &psObject->dfYMax,
		      &psObject->dfZMax, &psObject->dfMMax);
  return res;

}


void SHPAPI_CALL DBFReadStringAttributeInt(DBFHandle hDBF, int iShape, int iField,
				      char *attr, int lattr) {
  const char *lstr;
  int i;

  lstr = DBFReadStringAttribute(hDBF, iShape, iField);
  for(i = 0; i < lattr && lstr[i] != '\0'; i++) attr[i] = lstr[i];
  for(;i < lattr; i++) attr[i] = ' ';

}
