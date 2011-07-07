#include <string.h>
#include "shapefil.h"

void SHPAPI SHPSetObjectFortran(void*, SHPObject*, 
 int*, int*, int*, int*, int*, int*,
 double*, double*, double*, double*,
 double*, double*, double*, double*,
 double*, double*, double*, double*);


int SHPAPI SHPReadObjectInt(SHPHandle hshp, int iShape, void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPReadObject(hshp, iShape);

  if (psObject) {
    SHPSetObjectInt(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}


int SHPAPI SHPCreateSimpleObjectInt(int nSHPType, int nVertices,
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
						 

int SHPAPI SHPCreateObjectInt(int nSHPType, int iShape,
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
						 

void SHPAPI SHPComputeExtentsInt(SHPObject *psObject, void *ftnobject) {

  SHPComputeExtents(psObject);
  shpset_object_int(ftnobject, psObject);

}


void SHPSetObjectInt(SHPObject *psObject, void *ftnobject) {
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


/* void SHPAPI shpdestroyobject_int(SHPObject **psObject) { */

/*   SHPDestroyObject(*psObject); */

/* } */


/* int SHPAPI shprewindobject_int(SHPHandle *hSHP, SHPObject *psObject) { */

/*    return SHPRewindObject(*hSHP, psObject); */
/*   return 0; */

/* } */


void SHPAPI DBFReadStringAttributeInt(DBFHandle hDBF, int iShape, int iField,
				      char *attr, int lattr) {
  const char *lstr;
  int i;

  lstr = DBFReadStringAttribute(hDBF, iShape, iField);
  for(i = 0; i < lattr && lstr[i] != '\0'; i++) attr[i] = lstr[i];
  for(;i < lattr; i++) attr[i] = ' ';

}
