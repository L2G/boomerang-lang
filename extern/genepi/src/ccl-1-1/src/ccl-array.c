#include "ccl-memory.h"
#include "ccl-array.h"

void
ccl_array_resize(int *poldsize, int elsize, void **data, int newsize, int trim)
{
  char *newdata;
  int oldsize = *poldsize;

  if( newsize < oldsize )
    {
      if( trim ) {
	oldsize = newsize;
      } else {
	return;
      }
    }

  newdata = ccl_calloc(elsize,newsize);
  if( oldsize ) 
    ccl_memcpy(newdata,*data,oldsize*elsize);
  ccl_free(*data);
  *data = newdata;  
  *poldsize = newsize;
}
