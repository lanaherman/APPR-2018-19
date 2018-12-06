# Analiza podatkov s programom R, 2018/19

# Lana Herman

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2018/19

* [![Shiny](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/lanaherman/APPR-2018-19/master?urlpath=shiny/APPR-2018-19/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/lanaherman/APPR-2018-19/master?urlpath=rstudio) RStudio

## Ekološko kmetovanje v Sloveniji

Analizirala bom sektor kmetijstvo, kjer se bom osredotočila na ekološko usmerjene kmetije. Analizirala bom njihove pridelke, primerjala bom strukturo pridelkov ekoloških in neekoloških kmetij.

### Tabele

* Tabela 1: Število kmetijskih gospodarstev - KMETIJSKA PODJETJA, DRUŽINSKE KMETIJE, LETO
* Tabela 2: Rastlinski pridelki - VRSTE PRIDELKOV, ŠTEVILO KMETIJSKIH GOSPODARSTEV, LETO
* Tabela 3: Število živine - VRSTA ŽIVINE, ŠTEVILO KMETIJSKIH GOSPODARSTEV, LETO
* Tabela 4: Število ekoloških kmetijskih gospodarstev - EKOLOŠKA KMETIJSKA GOSPODARSTVA, KMETIJSKA GOSPODARSTVA V PREUSMERITVI, LETO
* Tabela 5: Pridelava ekoloških rastlinskih pridelkov - SKUPINE KMETIJSKIH KULTUR , LETO
* Tabela 6: Število živali v ekološki reji - VRSTE ŽIVALI , LETO
* Tabela 7: Kmetijska gospodarstva - splošni pregled po statističnih regijah - LETO, STATISTIČNA REGIJA

### Viri

* Tabela 1: (https://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1556001S&ti=&path=../Database/Okolje/15_kmetijstvo_ribistvo/03_kmetijska_gospod/00_15560_splosno/&lang=2)
* Tabela 2: (https://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1516501S&ti=&path=../Database/Okolje/15_kmetijstvo_ribistvo/03_kmetijska_gospod/01_15165_zemljisca/&lang=2)
* Tabela 3: (https://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1516602S&ti=&path=../Database/Okolje/15_kmetijstvo_ribistvo/03_kmetijska_gospod/02_15166_zivinoreja/&lang=2)
* Tabela 4: (https://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1561901S&ti=&path=../Database/Okolje/15_kmetijstvo_ribistvo/11_15619_ekolosko_kmet/&lang=2)
* Tabela 5: (https://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1561905S&ti=&path=../Database/Okolje/15_kmetijstvo_ribistvo/11_15619_ekolosko_kmet/&lang=2)
* Tabela 6: (https://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1561903S&ti=&path=../Database/Okolje/15_kmetijstvo_ribistvo/11_15619_ekolosko_kmet/&lang=2)
* Tabela 7:  (https://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1573801S&ti=&path=../Database/Okolje/15_kmetijstvo_ribistvo/03_kmetijska_gospod/08_15738_kmet_gosp_stat_reg/&lang=2)

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-201819)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem.zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
