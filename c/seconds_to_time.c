#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( SECONDS_TO_TIME )
{
    int iPCount = hb_pcount();

    if(iPCount > 0) {

        HB_BOOL bError = HB_FALSE;

        if(!HB_ISNUM(1)) {
            bError = HB_TRUE;
        }

        if(hb_parns(1) < 0) {
            bError = HB_TRUE;
        }
    
        if(!bError) {
            int iSeconds = hb_itemGetNI(hb_param(1, HB_IT_INTEGER));
            int iHour = iSeconds / 3600;
            iSeconds %= 3600;
            int iMin = iSeconds / 60;
            iSeconds %= 60;

            PHB_ITEM pArray = hb_itemArrayNew(3);
            hb_arraySetNI(pArray, 1, iHour);
            hb_arraySetNI(pArray, 2, iMin);
            hb_arraySetNI(pArray, 3, iSeconds);

            hb_itemReturnRelease(pArray);
        }
    }
}
