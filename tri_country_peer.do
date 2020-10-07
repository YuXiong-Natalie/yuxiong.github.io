
cd "/Users/yuxiong/Dropbox/Econometrics"
import delimited tri_country_peer.csv, clear 
ren ïid iid

destring *, replace

reshape long peerrstw peercdw depressionw learnw antisow yx_anxietyw,i(iid) j(period)

xtset iid period
save tri_country_peer.dta, replace

use tri_country_peer.dta, clear
//whole sample
xtreg yx_anxietyw peerrstw peercdw ,fe cluster (iid)
xtreg depressionw peerrstw peercdw ,fe cluster (iid)
xtreg learnw peerrstw peercdw ,fe cluster (iid)
xtreg antisow peerrstw peercdw ,fe cluster (iid)
eststo

//lagged x
use tri_country_peer.dta, clear
xtreg yx_anxietyw L.peerrstw L.peercdw ,fe cluster (iid)
xtreg depressionw L.peerrstw L.peercdw  ,fe cluster (iid)
xtreg learnw L.peerrstw L.peercdw ,fe cluster (iid)
xtreg antisow L.peerrstw L.peercdw ,fe cluster (iid)
eststo

//united states
use tri_country_peer.dta, clear
xtreg yx_anxietyw peerrstw peercdw if country==1,fe cluster (iid)
xtreg depressionw peerrstw peercdw if country==1,fe cluster (iid)
xtreg learnw peerrstw peercdw if country==1,fe cluster (iid)
xtreg antisow peerrstw peercdw if country==1,fe cluster (iid)
eststo

//lagged x
xtreg yx_anxietyw L.peerrstw L.peercdw  if country==1,fe cluster (iid)
xtreg depressionw L.peerrstw L.peercdw  if country==1,fe cluster (iid)
xtreg learnw L.peerrstw L.peercdw  if country==1,fe cluster (iid)
xtreg antisow L.peerrstw L.peercdw if country==1,fe cluster (iid)
eststo

//china
use tri_country_peer.dta, clear
xtreg yx_anxietyw peerrstw peercdw if country==2,fe cluster (iid)
xtreg depressionw peerrstw peercdw if country==2,fe cluster (iid)
xtreg learnw peerrstw peercdw if country==2,fe cluster (iid)
xtreg antisow peerrstw peercdw if country==2,fe cluster (iid)
eststo

//Lagged x
xtreg yx_anxietyw L.peerrstw L.peercdw  if country==2,fe cluster (iid)
xtreg depressionw L.peerrstw L.peercdw  if country==2,fe cluster (iid)
xtreg learnw L.peerrstw L.peercdw  if country==2,fe cluster (iid)
xtreg antisow L.peerrstw L.peercdw if country==2,fe cluster (iid)
eststo

//interaction
xtreg yx_anxietyw peerrstw peercdw i.country##(c.peerrstw c.peercdw),fe cluster (iid)
xtreg depressionw peerrstw peercdw i.country##(c.peerrstw c.peercdw),fe cluster (iid)
xtreg learnw peerrstw peercdw i.country##(c.peerrstw c.peercdw),fe cluster (iid)
xtreg antisow peerrstw peercdw i.country##(c.peerrstw c.peercdw),fe cluster (iid)
eststo
