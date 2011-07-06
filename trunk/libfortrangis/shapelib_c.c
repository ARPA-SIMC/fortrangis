#include <string.h>
#include "shapefil.h"

void SHPAPI shpset_object(void*, SHPObject**, 
 int*, int*, int*, int*, int*, int*,
 double*, double*, double*, double*,
 double*, double*, double*, double*,
 double*, double*, double*, double*);


int SHPAPI shpreadobject_int(SHPHandle hshp, int iShape, void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPReadObject(hshp, iShape);

  if (psObject) {
    shpset_object_int(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}


int SHPAPI shpcreatesimpleobject_int(int nSHPType, int nVertices,
   double *padfX, double *padfY, double *padfZ,
   void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPCreateSimpleObject(nSHPType, nVertices, padfX, padfY, padfZ);
  if (psObject) {
    shpset_object_int(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}
						 

int SHPAPI shpcreateobject_int(int nSHPType, int iShape,
   int nParts, int *panPartStart, int *panPartType,
   int nVertices, double *padfX, double *padfY, double *padfZ, double *padfM,
   void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPCreateObject(nSHPType, iShape, nParts, panPartStart,
			     panPartType, nVertices,
			     padfX, padfY, padfZ, padfM);
  if (psObject) {
    shpset_object_int(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}
						 

void SHPAPI shpcomputeextents_int(SHPObject *psObject, void *ftnobject) {

  SHPComputeExtents(psObject);
  shpset_object_int(ftnobject, psObject);

}


void shpset_object_int(SHPObject *psObject, void *ftnobject) {
  shpset_object(ftnobject, psObject,
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


void SHPAPI DBFReadStringAttribute_int(DBFHandle hDBF, int iShape, int iField, char *attr, int lattr) {
  const char *lstr;
  int i;

  lstr = DBFReadStringAttribute(hDBF, iShape, iField);
  for(i = 0; i < lattr && lstr[i] != '\0'; i++) attr[i] = lstr[i];
  for(;i < lattr; i++) attr[i] = ' ';

}
