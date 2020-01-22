#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( SWAP )
{
    PHB_ITEM pFirstItem = hb_param(1, HB_IT_ANY);
    PHB_ITEM pSecondItem = hb_param(2, HB_IT_ANY);

    hb_itemSwap(pFirstItem, pSecondItem);
}
