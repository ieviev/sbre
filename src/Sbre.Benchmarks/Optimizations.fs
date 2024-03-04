module Sbre.Benchmarks.Optimizations

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open BenchmarkDotNet.Attributes
open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Optimizations
open System
open Sbre.Pat
open Sbre.Types
open System.Text.Json.Nodes
open System.Buffers
// let fullInput = __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText
let fullInput = __SOURCE_DIRECTORY__ + "/data/sherlock.txt" |> System.IO.File.ReadAllText

let frequenciesJsonText = __SOURCE_DIRECTORY__ + "/data/charFreqWithControl.json"  |> System.IO.File.ReadAllText

let shortInput20k = fullInput[..19999] // 20k chars limit
let shortInput10k = fullInput[..1000] // 10k chars limit
let testInput =
                // "wa as dfeas dann dasdw q wasd"
                fullInput
                // |> String.replicate 10
                |> String.replicate 100
// let testInput = "yabcabca"


module Patterns =

    [<Literal>] // rust-src-tools-3b0d4813.txt
    let DATE = @"((19\d\d01[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d01[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d02[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d02[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d03[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d03[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d04[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d04[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d05[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d05[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d06[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d06[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d07[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d07[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d08[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d08[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d09[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d09[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d10[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d10[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d11[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d11[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d12[0-3]\d[0-5]\d[0-5]\d[0-5]\d|20\d\d12[0-3]\d[0-5]\d[0-5]\d[0-5]\d|19\d\d01[0123]\d|20\d\d01[0123]\d|19\d\d02[0123]\d|20\d\d02[0123]\d|19\d\d03[0123]\d|20\d\d03[0123]\d|19\d\d04[0123]\d|20\d\d04[0123]\d|19\d\d05[0123]\d|20\d\d05[0123]\d|19\d\d06[0123]\d|20\d\d06[0123]\d|19\d\d07[0123]\d|20\d\d07[0123]\d|19\d\d08[0123]\d|20\d\d08[0123]\d|19\d\d09[0123]\d|20\d\d09[0123]\d|19\d\d10[0123]\d|20\d\d10[0123]\d|19\d\d11[0123]\d|20\d\d11[0123]\d|19\d\d12[0123]\d|20\d\d12[0123]\d|19\d\d01|20\d\d01|19\d\d02|20\d\d02|19\d\d03|20\d\d03|19\d\d04|20\d\d04|19\d\d05|20\d\d05|19\d\d06|20\d\d06|19\d\d07|20\d\d07|19\d\d08|20\d\d08|19\d\d09|20\d\d09|19\d\d10|20\d\d10|19\d\d11|20\d\d11|19\d\d12|20\d\d12|(-?(:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])T(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(?:[.,]+([0-9]+))?((?:Z|[+-](?:2[0-3]|[01][0-9]):[0-5][0-9]))?)|((((\d{1,2}):(\d{1,2})(:(\d{1,2}))?([.,](\d{1,6}))?\s*(a.m.|am|p.m.|pm)?\s*(ACDT|ACST|ACT|ACWDT|ACWST|ADDT|ADMT|ADT|AEDT|AEST|AFT|AHDT|AHST|AKDT|AKST|AKTST|AKTT|ALMST|ALMT|AMST|AMT|ANAST|ANAT|ANT|APT|AQTST|AQTT|ARST|ART|ASHST|ASHT|AST|AWDT|AWST|AWT|AZOMT|AZOST|AZOT|AZST|AZT|BAKST|BAKT|BDST|BDT|BEAT|BEAUT|BIOT|BMT|BNT|BORT|BOST|BOT|BRST|BRT|BST|BTT|BURT|CANT|CAPT|CAST|CAT|CAWT|CCT|CDDT|CDT|CEDT|CEMT|CEST|CET|CGST|CGT|CHADT|CHAST|CHDT|CHOST|CHOT|CIST|CKHST|CKT|CLST|CLT|CMT|COST|COT|CPT|CST|CUT|CVST|CVT|CWT|CXT|ChST|DACT|DAVT|DDUT|DFT|DMT|DUSST|DUST|EASST|EAST|EAT|ECT|EDDT|EDT|EEDT|EEST|EET|EGST|EGT|EHDT|EMT|EPT|EST|ET|EWT|FET|FFMT|FJST|FJT|FKST|FKT|FMT|FNST|FNT|FORT|FRUST|FRUT|GALT|GAMT|GBGT|GEST|GET|GFT|GHST|GILT|GIT|GMT|GST|GYT|HAA|HAC|HADT|HAE|HAP|HAR|HAST|HAT|HAY|HDT|HKST|HKT|HLV|HMT|HNA|HNC|HNE|HNP|HNR|HNT|HNY|HOVST|HOVT|HST|ICT|IDDT|IDT|IHST|IMT|IOT|IRDT|IRKST|IRKT|IRST|ISST|IST|JAVT|JCST|JDT|JMT|JST|JWST|KART|KDT|KGST|KGT|KIZST|KIZT|KMT|KOST|KRAST|KRAT|KST|KUYST|KUYT|KWAT|LHDT|LHST|LINT|LKT|LMT|LMT|LMT|LMT|LRT|LST|MADMT|MADST|MADT|MAGST|MAGT|MALST|MALT|MART|MAWT|MDDT|MDST|MDT|MEST|MET|MHT|MIST|MIT|MMT|MOST|MOT|MPT|MSD|MSK|MSM|MST|MUST|MUT|MVT|MWT|MYT|NCST|NCT|NDDT|NDT|NEGT|NEST|NET|NFT|NMT|NOVST|NOVT|NPT|NRT|NST|NT|NUT|NWT|NZDT|NZMT|NZST|OMSST|OMST|ORAST|ORAT|PDDT|PDT|PEST|PET|PETST|PETT|PGT|PHOT|PHST|PHT|PKST|PKT|PLMT|PMDT|PMMT|PMST|PMT|PNT|PONT|PPMT|PPT|PST|PT|PWT|PYST|PYT|QMT|QYZST|QYZT|RET|RMT|ROTT|SAKST|SAKT|SAMT|SAST|SBT|SCT|SDMT|SDT|SET|SGT|SHEST|SHET|SJMT|SLT|SMT|SRET|SRT|SST|STAT|SVEST|SVET|SWAT|SYOT|TAHT|TASST|TAST|TBIST|TBIT|TBMT|TFT|THA|TJT|TKT|TLT|TMT|TOST|TOT|TRST|TRT|TSAT|TVT|ULAST|ULAT|URAST|URAT|UTC|UYHST|UYST|UYT|UZST|UZT|VET|VLAST|VLAT|VOLST|VOLT|VOST|VUST|VUT|WARST|WART|WAST|WAT|WDT|WEDT|WEMT|WEST|WET|WFT|WGST|WGT|WIB|WIT|WITA|WMT|WSDT|WSST|WST|WT|XJT|YAKST|YAKT|YAPT|YDDT|YDT|YEKST|YEKST|YEKT|YEKT|YERST|YERT|YPT|YST|YWT|zzz|pacific|eastern|mountain|central)?)|((\d{1,2})\s*(a.m.|am|p.m.|pm)\s*(ACDT|ACST|ACT|ACWDT|ACWST|ADDT|ADMT|ADT|AEDT|AEST|AFT|AHDT|AHST|AKDT|AKST|AKTST|AKTT|ALMST|ALMT|AMST|AMT|ANAST|ANAT|ANT|APT|AQTST|AQTT|ARST|ART|ASHST|ASHT|AST|AWDT|AWST|AWT|AZOMT|AZOST|AZOT|AZST|AZT|BAKST|BAKT|BDST|BDT|BEAT|BEAUT|BIOT|BMT|BNT|BORT|BOST|BOT|BRST|BRT|BST|BTT|BURT|CANT|CAPT|CAST|CAT|CAWT|CCT|CDDT|CDT|CEDT|CEMT|CEST|CET|CGST|CGT|CHADT|CHAST|CHDT|CHOST|CHOT|CIST|CKHST|CKT|CLST|CLT|CMT|COST|COT|CPT|CST|CUT|CVST|CVT|CWT|CXT|ChST|DACT|DAVT|DDUT|DFT|DMT|DUSST|DUST|EASST|EAST|EAT|ECT|EDDT|EDT|EEDT|EEST|EET|EGST|EGT|EHDT|EMT|EPT|EST|ET|EWT|FET|FFMT|FJST|FJT|FKST|FKT|FMT|FNST|FNT|FORT|FRUST|FRUT|GALT|GAMT|GBGT|GEST|GET|GFT|GHST|GILT|GIT|GMT|GST|GYT|HAA|HAC|HADT|HAE|HAP|HAR|HAST|HAT|HAY|HDT|HKST|HKT|HLV|HMT|HNA|HNC|HNE|HNP|HNR|HNT|HNY|HOVST|HOVT|HST|ICT|IDDT|IDT|IHST|IMT|IOT|IRDT|IRKST|IRKT|IRST|ISST|IST|JAVT|JCST|JDT|JMT|JST|JWST|KART|KDT|KGST|KGT|KIZST|KIZT|KMT|KOST|KRAST|KRAT|KST|KUYST|KUYT|KWAT|LHDT|LHST|LINT|LKT|LMT|LMT|LMT|LMT|LRT|LST|MADMT|MADST|MADT|MAGST|MAGT|MALST|MALT|MART|MAWT|MDDT|MDST|MDT|MEST|MET|MHT|MIST|MIT|MMT|MOST|MOT|MPT|MSD|MSK|MSM|MST|MUST|MUT|MVT|MWT|MYT|NCST|NCT|NDDT|NDT|NEGT|NEST|NET|NFT|NMT|NOVST|NOVT|NPT|NRT|NST|NT|NUT|NWT|NZDT|NZMT|NZST|OMSST|OMST|ORAST|ORAT|PDDT|PDT|PEST|PET|PETST|PETT|PGT|PHOT|PHST|PHT|PKST|PKT|PLMT|PMDT|PMMT|PMST|PMT|PNT|PONT|PPMT|PPT|PST|PT|PWT|PYST|PYT|QMT|QYZST|QYZT|RET|RMT|ROTT|SAKST|SAKT|SAMT|SAST|SBT|SCT|SDMT|SDT|SET|SGT|SHEST|SHET|SJMT|SLT|SMT|SRET|SRT|SST|STAT|SVEST|SVET|SWAT|SYOT|TAHT|TASST|TAST|TBIST|TBIT|TBMT|TFT|THA|TJT|TKT|TLT|TMT|TOST|TOT|TRST|TRT|TSAT|TVT|ULAST|ULAT|URAST|URAT|UTC|UYHST|UYST|UYT|UZST|UZT|VET|VLAST|VLAT|VOLST|VOLT|VOST|VUST|VUT|WARST|WART|WAST|WAT|WDT|WEDT|WEMT|WEST|WET|WFT|WGST|WGT|WIB|WIT|WITA|WMT|WSDT|WSST|WST|WT|XJT|YAKST|YAKT|YAPT|YDDT|YDT|YEKST|YEKST|YEKT|YEKT|YERST|YERT|YPT|YST|YWT|zzz|pacific|eastern|mountain|central)*))|(19\d\d|20\d\d)|(first|second|third|fourth|fifth|sixth|seventh|eighth|nineth|tenth)|(\d+)(st|th|rd|nd)?|(monday|tuesday|wednesday|thursday|friday|saturday|sunday|mandag|tirsdag|onsdag|torsdag|fredag|lordag|sondag|mon|tue|tues|wed|thu|thur|thurs|fri|sat|sun|man|tir|tirs|ons|tor|tors|fre|lor|son)|(january|february|march|april|may|june|july|august|september|october|november|december|enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre|januar|februar|marts|april|maj|juni|juli|august|september|oktober|november|december|jan[.\s]|ene[.\s]|feb[.\s]|mar[.\s]|apr[.\s]|abr[.\s]|may[.\s]|maj[.\s]|jun[.\s]|jul[.\s]|aug[.\s]|ago[.\s]|sep[^A-Za-z]|sept[.\s]|oct[.\s]|okt[.\s]|nov[.\s]|dec[.\s]|dic[.\s])|([/:\-,.\s_+@]+)|(next|last)|(due|by|on|during|standard|daylight|savings|time|date|dated|of|to|through|between|until|at|day)){1,1})"
    [<Literal>] // rust-src-tools-3b0d4813.txt
    let URL = @"((?:(?:(?:https?|ftp)://(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?))|(?:(?:https?|ftp)://)?(?:[a-z0-9%.]+:[a-z0-9%]+@)?(?:(?:[a-z0-9_~]\-?){0,62}[a-z0-9]\.)*(?:(?:(?:[a-z0-9]\-?){0,62}[a-z0-9])|(?:xn--[a-z0-9\-]+))\.(?:XN--VERMGENSBERATUNG-PWB|XN--VERMGENSBERATER-CTB|XN--CLCHC0EA0B2G2A9GCD|XN--W4R85EL8FHU5DNRA|NORTHWESTERNMUTUAL|TRAVELERSINSURANCE|XN--3OQ18VL8PN36A|XN--5SU34J936BGSG|XN--BCK1B9A5DRE4C|XN--MGBAH1A3HJKRD|XN--MGBAI9AZGQP6J|XN--MGBERP4A5D4AR|XN--XKC2DL3A5EE0H|XN--FZYS8D69UVGM|XN--MGBA7C0BBN0A|XN--MGBCPQ6GPA1A|XN--XKC2AL3HYE2A|AMERICANEXPRESS|KERRYPROPERTIES|SANDVIKCOROMANT|XN--I1B6B1A6A2E|XN--KCRX77D1X4A|XN--LGBBAT1AD8J|XN--MGBA3A4F16A|XN--MGBAAKC7DVF|XN--MGBC0A9AZCG|XN--NQV7FS00EMA|AFAMILYCOMPANY|AMERICANFAMILY|BANANAREPUBLIC|CANCERRESEARCH|COOKINGCHANNEL|KERRYLOGISTICS|WEATHERCHANNEL|XN--54B7FTA0CC|XN--6QQ986B3XL|XN--80AQECDR1A|XN--B4W605FERD|XN--FIQ228C5HS|XN--H2BREG3EVE|XN--JLQ480N2RG|XN--JLQ61U9W7B|XN--MGBA3A3EJT|XN--MGBAAM7A8H|XN--MGBAYH7GPA|XN--MGBBH1A71E|XN--MGBCA7DZDO|XN--MGBI4ECEXP|XN--MGBX4CD0AB|XN--RVC1E0AM3E|INTERNATIONAL|LIFEINSURANCE|TRAVELCHANNEL|WOLTERSKLUWER|XN--CCKWCXETD|XN--ECKVDTC9D|XN--FPCRJ9C3D|XN--FZC2C9E2C|XN--H2BRJ9C8C|XN--TIQ49XQYJ|XN--YFRO4I67O|XN--YGBI2AMMX|CONSTRUCTION|LPLFINANCIAL|SCHOLARSHIPS|VERSICHERUNG|XN--3E0B707E|XN--45BR5CYL|XN--4DBRK0CE|XN--80ADXHKS|XN--80ASEHDB|XN--8Y0A063A|XN--GCKR3F0F|XN--MGB9AWBF|XN--MGBAB2BD|XN--MGBGU82A|XN--MGBPL2FH|XN--MGBT3DHD|XN--MK1BU44C|XN--NGBC5AZD|XN--NGBE9E0A|XN--OGBPF8FL|XN--QCKA1PMC|ACCOUNTANTS|BARCLAYCARD|BLACKFRIDAY|BLOCKBUSTER|BRIDGESTONE|CALVINKLEIN|CONTRACTORS|CREDITUNION|ENGINEERING|ENTERPRISES|FOODNETWORK|INVESTMENTS|KERRYHOTELS|LAMBORGHINI|MOTORCYCLES|OLAYANGROUP|PHOTOGRAPHY|PLAYSTATION|PRODUCTIONS|PROGRESSIVE|REDUMBRELLA|WILLIAMHILL|XN--11B4C3D|XN--1CK2E1B|XN--1QQW23A|XN--2SCRJ9C|XN--3BST00M|XN--3DS443G|XN--3HCRJ9C|XN--42C2D9A|XN--45BRJ9C|XN--55QW42G|XN--6FRZ82G|XN--80AO21A|XN--9KRT00A|XN--CCK2B3B|XN--CZR694B|XN--D1ACJ3B|XN--EFVY88H|XN--FCT429K|XN--FJQ720A|XN--FLW351E|XN--G2XX48C|XN--GECRJ9C|XN--GK3AT1E|XN--H2BRJ9C|XN--HXT814E|XN--IMR513N|XN--J6W193G|XN--JVR189M|XN--KPRW13D|XN--KPRY57D|XN--MGBBH1A|XN--MGBTX2B|XN--MIX891F|XN--NYQY26A|XN--OTU796D|XN--PGBS0DH|XN--Q9JYB4C|XN--RHQV96G|XN--ROVU88B|XN--S9BRJ9C|XN--SES554G|XN--T60B56A|XN--VUQ861B|XN--W4RS40L|XN--XHQ521B|XN--ZFR164B|ACCOUNTANT|APARTMENTS|ASSOCIATES|BASKETBALL|BNPPARIBAS|BOEHRINGER|CAPITALONE|CONSULTING|CREDITCARD|CUISINELLA|EUROVISION|EXTRASPACE|FOUNDATION|HEALTHCARE|IMMOBILIEN|INDUSTRIES|MANAGEMENT|MITSUBISHI|NEXTDIRECT|PROPERTIES|PROTECTION|PRUDENTIAL|REALESTATE|REPUBLICAN|RESTAURANT|SCHAEFFLER|SWIFTCOVER|TATAMOTORS|TECHNOLOGY|UNIVERSITY|VLAANDEREN|VOLKSWAGEN|XN--30RR7Y|XN--3PXU8K|XN--45Q11C|XN--4GBRIM|XN--55QX5D|XN--5TZM5G|XN--80ASWG|XN--90A3AC|XN--9DBQ2A|XN--9ET52U|XN--C2BR7G|XN--CG4BKI|XN--CZRS0T|XN--CZRU2D|XN--FIQ64B|XN--FIQS8S|XN--FIQZ9S|XN--IO0A7I|XN--KPUT3I|XN--MXTQ1M|XN--O3CW4H|XN--PSSY2U|XN--Q7CE6A|XN--UNUP4Y|XN--WGBH1C|XN--WGBL6A|XN--Y9A3AQ|ACCENTURE|ALFAROMEO|ALLFINANZ|AMSTERDAM|ANALYTICS|AQUARELLE|BARCELONA|BLOOMBERG|CHRISTMAS|COMMUNITY|DIRECTORY|EDUCATION|EQUIPMENT|FAIRWINDS|FINANCIAL|FIRESTONE|FRESENIUS|FRONTDOOR|FURNITURE|GOLDPOINT|HISAMITSU|HOMEDEPOT|HOMEGOODS|HOMESENSE|INSTITUTE|INSURANCE|KUOKGROUP|LANCASTER|LANDROVER|LIFESTYLE|MARKETING|MARSHALLS|MELBOURNE|MICROSOFT|PANASONIC|PASSAGENS|PRAMERICA|RICHARDLI|SCJOHNSON|SHANGRILA|SOLUTIONS|STATEBANK|STATEFARM|STOCKHOLM|TRAVELERS|VACATIONS|XN--90AIS|XN--C1AVG|XN--D1ALF|XN--E1A4C|XN--FHBEI|XN--J1AEF|XN--J1AMH|XN--L1ACC|XN--NGBRX|XN--NQV7F|XN--P1ACF|XN--QXA6A|XN--TCKWE|XN--VHQUV|YODOBASHI|ABUDHABI|AIRFORCE|ALLSTATE|ATTORNEY|BARCLAYS|BAREFOOT|BARGAINS|BASEBALL|BOUTIQUE|BRADESCO|BROADWAY|BRUSSELS|BUDAPEST|BUILDERS|BUSINESS|CAPETOWN|CATERING|CATHOLIC|CIPRIANI|CITYEATS|CLEANING|CLINIQUE|CLOTHING|COMMBANK|COMPUTER|DELIVERY|DELOITTE|DEMOCRAT|DIAMONDS|DISCOUNT|DISCOVER|DOWNLOAD|ENGINEER|ERICSSON|ETISALAT|EXCHANGE|FEEDBACK|FIDELITY|FIRMDALE|FOOTBALL|FRONTIER|GOODYEAR|GRAINGER|GRAPHICS|GUARDIAN|HDFCBANK|HELSINKI|HOLDINGS|HOSPITAL|INFINITI|IPIRANGA|ISTANBUL|JPMORGAN|LIGHTING|LUNDBECK|MARRIOTT|MASERATI|MCKINSEY|MEMORIAL|MERCKMSD|MORTGAGE|OBSERVER|PARTNERS|PHARMACY|PICTURES|PLUMBING|PROPERTY|REDSTONE|RELIANCE|SAARLAND|SAMSCLUB|SECURITY|SERVICES|SHOPPING|SHOWTIME|SOFTBANK|SOFTWARE|STCGROUP|SUPPLIES|TRAINING|VANGUARD|VENTURES|VERISIGN|WOODSIDE|XN--90AE|XN--NODE|XN--P1AI|XN--QXAM|YOKOHAMA|ABOGADO|ACADEMY|AGAKHAN|ALIBABA|ANDROID|ATHLETA|AUCTION|AUDIBLE|AUSPOST|AVIANCA|BANAMEX|BAUHAUS|BENTLEY|BESTBUY|BOOKING|BROTHER|BUGATTI|CAPITAL|CARAVAN|CAREERS|CHANNEL|CHARITY|CHINTAI|CITADEL|CLUBMED|COLLEGE|COLOGNE|COMCAST|COMPANY|COMPARE|CONTACT|COOKING|CORSICA|COUNTRY|COUPONS|COURSES|CRICKET|CRUISES|DENTIST|DIGITAL|DOMAINS|EXPOSED|EXPRESS|FARMERS|FASHION|FERRARI|FERRERO|FINANCE|FISHING|FITNESS|FLIGHTS|FLORIST|FLOWERS|FORSALE|FROGANS|FUJITSU|GALLERY|GENTING|GODADDY|GROCERY|GUITARS|HAMBURG|HANGOUT|HITACHI|HOLIDAY|HOSTING|HOTELES|HOTMAIL|HYUNDAI|ISMAILI|JEWELRY|JUNIPER|KITCHEN|KOMATSU|LACAIXA|LANXESS|LASALLE|LATROBE|LECLERC|LIMITED|LINCOLN|MARKETS|MONSTER|NETBANK|NETFLIX|NETWORK|NEUSTAR|OKINAWA|OLDNAVY|ORGANIC|ORIGINS|PHILIPS|PIONEER|POLITIE|REALTOR|RECIPES|RENTALS|REVIEWS|REXROTH|SAMSUNG|SANDVIK|SCHMIDT|SCHWARZ|SCIENCE|SHIKSHA|SINGLES|STAPLES|STORAGE|SUPPORT|SURGERY|SYSTEMS|TEMASEK|THEATER|THEATRE|TICKETS|TIFFANY|TOSHIBA|TRADING|WALMART|WANGGOU|WATCHES|WEATHER|WEBSITE|WEDDING|WHOSWHO|WINDOWS|WINNERS|XFINITY|YAMAXUN|YOUTUBE|ZUERICH|ABARTH|ABBOTT|ABBVIE|AFRICA|AGENCY|AIRBUS|AIRTEL|ALIPAY|ALSACE|ALSTOM|AMAZON|ANQUAN|ARAMCO|AUTHOR|BAYERN|BEAUTY|BERLIN|BHARTI|BOSTIK|BOSTON|BROKER|CAMERA|CAREER|CASINO|CENTER|CHANEL|CHROME|CHURCH|CIRCLE|CLAIMS|CLINIC|COFFEE|COMSEC|CONDOS|COUPON|CREDIT|CRUISE|DATING|DATSUN|DEALER|DEGREE|DENTAL|DESIGN|DIRECT|DOCTOR|DUNLOP|DUPONT|DURBAN|EMERCK|ENERGY|ESTATE|EVENTS|EXPERT|FAMILY|FLICKR|FUTBOL|GALLUP|GARDEN|GEORGE|GIVING|GLOBAL|GOOGLE|GRATIS|HEALTH|HERMES|HIPHOP|HOCKEY|HOTELS|HUGHES|IMAMAT|INSURE|INTUIT|JAGUAR|JOBURG|JUEGOS|KAUFEN|KINDER|KINDLE|KOSHER|LANCIA|LATINO|LAWYER|LEFRAK|LIVING|LOCKER|LONDON|LUXURY|MADRID|MAISON|MAKEUP|MARKET|MATTEL|MOBILE|MONASH|MORMON|MOSCOW|MUSEUM|MUTUAL|NAGOYA|NATURA|NISSAN|NISSAY|NORTON|NOWRUZ|OFFICE|OLAYAN|ONLINE|ORACLE|ORANGE|OTSUKA|PFIZER|PHOTOS|PHYSIO|PICTET|QUEBEC|RACING|REALTY|REISEN|REPAIR|REPORT|REVIEW|ROCHER|ROGERS|RYUKYU|SAFETY|SAKURA|SANOFI|SCHOOL|SCHULE|SEARCH|SECURE|SELECT|SHOUJI|SOCCER|SOCIAL|STREAM|STUDIO|SUPPLY|SUZUKI|SWATCH|SYDNEY|TAIPEI|TAOBAO|TARGET|TATTOO|TENNIS|TIENDA|TJMAXX|TKMAXX|TOYOTA|TRAVEL|UNICOM|VIAJES|VIKING|VILLAS|VIRGIN|VISION|VOTING|VOYAGE|VUELOS|WALTER|WEBCAM|XIHUAN|YACHTS|YANDEX|ZAPPOS|ACTOR|ADULT|AETNA|AMFAM|AMICA|APPLE|ARCHI|AUDIO|AUTOS|AZURE|BAIDU|BEATS|BIBLE|BINGO|BLACK|BOATS|BOSCH|BUILD|CANON|CARDS|CHASE|CHEAP|CISCO|CITIC|CLICK|CLOUD|COACH|CODES|CROWN|CYMRU|DABUR|DANCE|DEALS|DELTA|DRIVE|DUBAI|EARTH|EDEKA|EMAIL|EPSON|FAITH|FEDEX|FINAL|FOREX|FORUM|GALLO|GAMES|GIFTS|GIVES|GLADE|GLASS|GLOBO|GMAIL|GREEN|GRIPE|GROUP|GUCCI|GUIDE|HOMES|HONDA|HORSE|HOUSE|HYATT|IKANO|IRISH|JETZT|KOELN|KYOTO|LAMER|LEASE|LEGAL|LEXUS|LILLY|LINDE|LIPSY|LIXIL|LOANS|LOCUS|LOTTE|LOTTO|MACYS|MANGO|MEDIA|MIAMI|MONEY|MOVIE|NEXUS|NIKON|NINJA|NOKIA|NOWTV|OMEGA|OSAKA|PARIS|PARTS|PARTY|PHONE|PHOTO|PIZZA|PLACE|POKER|PRAXI|PRESS|PRIME|PROMO|QUEST|RADIO|REHAB|REISE|RICOH|ROCKS|RODEO|RUGBY|SALON|SENER|SEVEN|SHARP|SHELL|SHOES|SKYPE|SLING|SMART|SMILE|SOLAR|SPACE|SPORT|STADA|STORE|STUDY|STYLE|SUCKS|SWISS|TATAR|TIRES|TIROL|TMALL|TODAY|TOKYO|TOOLS|TORAY|TOTAL|TOURS|TRADE|TRUST|TUNES|TUSHU|UBANK|VEGAS|VIDEO|VODKA|VOLVO|WALES|WATCH|WEBER|WEIBO|WORKS|WORLD|XEROX|YAHOO|AARP|ABLE|ADAC|AERO|AKDN|ALLY|AMEX|ARAB|ARMY|ARPA|ARTE|ASDA|ASIA|AUDI|AUTO|BABY|BAND|BANK|BBVA|BEER|BEST|BIKE|BING|BLOG|BLUE|BOFA|BOND|BOOK|BUZZ|CAFE|CALL|CAMP|CARE|CARS|CASA|CASE|CASH|CBRE|CERN|CHAT|CITI|CITY|CLUB|COOL|COOP|CYOU|DATA|DATE|DCLK|DEAL|DELL|DESI|DIET|DISH|DOCS|DUCK|DVAG|ERNI|FAGE|FAIL|FANS|FARM|FAST|FIAT|FIDO|FILM|FIRE|FISH|FLIR|FOOD|FORD|FREE|FUND|GAME|GBIZ|GENT|GGEE|GIFT|GMBH|GOLD|GOLF|GOOG|GUGE|GURU|HAIR|HAUS|HDFC|HELP|HERE|HGTV|HOST|HSBC|ICBC|IEEE|IMDB|IMMO|INFO|ITAU|JAVA|JEEP|JOBS|JPRS|KDDI|KIWI|KPMG|KRED|LAND|LEGO|LGBT|LIDL|LIFE|LIKE|LIMO|LINK|LIVE|LOAN|LOFT|LOVE|LTDA|LUXE|MAIF|MEET|MEME|MENU|MINI|MINT|MOBI|MODA|MOTO|NAME|NAVY|NEWS|NEXT|NICO|NIKE|OLLO|OPEN|PAGE|PARS|PCCW|PICS|PING|PINK|PLAY|PLUS|POHL|PORN|POST|PROD|PROF|QPON|RAID|READ|REIT|RENT|REST|RICH|RMIT|ROOM|RSVP|RUHR|SAFE|SALE|SARL|SAVE|SAXO|SCOT|SEAT|SEEK|SEXY|SHAW|SHIA|SHOP|SHOW|SILK|SINA|SITE|SKIN|SNCF|SOHU|SONG|SONY|SPOT|STAR|SURF|TALK|TAXI|TEAM|TECH|TEVA|TIAA|TIPS|TOWN|TOYS|TUBE|VANA|VISA|VIVA|VIVO|VOTE|VOTO|WANG|WEIR|WIEN|WIKI|WINE|WORK|XBOX|YOGA|ZARA|ZERO|ZONE|AAA|ABB|ABC|ACO|ADS|AEG|AFL|AIG|ANZ|AOL|APP|ART|AWS|AXA|BAR|BBC|BBT|BCG|BCN|BET|BID|BIO|BIZ|BMS|BMW|BOM|BOO|BOT|BOX|BUY|BZH|CAB|CAL|CAM|CAR|CAT|CBA|CBN|CBS|CEO|CFA|CFD|COM|CPA|CRS|CSC|DAD|DAY|DDS|DEV|DHL|DIY|DNP|DOG|DOT|DTV|DVR|EAT|ECO|EDU|ESQ|EUS|FAN|FIT|FLY|FOO|FOX|FRL|FTR|FUN|FYI|GAL|GAP|GAY|GDN|GEA|GLE|GMO|GMX|GOO|GOP|GOT|GOV|HBO|HIV|HKT|HOT|HOW|IBM|ICE|ICU|IFM|INC|ING|INK|INT|IST|ITV|JCB|JIO|JLL|JMP|JNJ|JOT|JOY|KFH|KIA|KIM|KPN|KRD|LAT|LAW|LDS|LLC|LLP|LOL|LPL|LTD|MAN|MAP|MBA|MED|MEN|MIL|MIT|MLB|MLS|MMA|MOE|MOI|MOM|MOV|MSD|MTN|MTR|NAB|NBA|NEC|NET|NEW|NFL|NGO|NHK|NOW|NRA|NRW|NTT|NYC|OBI|OFF|ONE|ONG|ONL|OOO|ORG|OTT|OVH|PAY|PET|PHD|PID|PIN|PNC|PRO|PRU|PUB|PWC|QVC|RED|REN|RIL|RIO|RIP|RUN|RWE|SAP|SAS|SBI|SBS|SCA|SCB|SES|SEW|SEX|SFR|SKI|SKY|SOY|SPA|SRL|STC|TAB|TAX|TCI|TDK|TEL|THD|TJX|TOP|TRV|TUI|TVS|UBS|UNO|UOL|UPS|VET|VIG|VIN|VIP|WED|WIN|WME|WOW|WTC|WTF|XIN|XXX|XYZ|YOU|YUN|ZIP|AC|AD|AE|AF|AG|AI|AL|AM|AO|AQ|AR|AS|AT|AU|AW|AX|AZ|BA|BB|BD|BE|BF|BG|BH|BI|BJ|BM|BN|BO|BR|BS|BT|BV|BW|BY|BZ|CA|CC|CD|CF|CG|CH|CI|CK|CL|CM|CN|CO|CR|CU|CV|CW|CX|CY|CZ|DE|DJ|DK|DM|DO|DZ|EC|EE|EG|ER|ES|ET|EU|FI|FJ|FK|FM|FO|FR|GA|GB|GD|GE|GF|GG|GH|GI|GL|GM|GN|GP|GQ|GR|GS|GT|GU|GW|GY|HK|HM|HN|HR|HT|HU|ID|IE|IL|IM|IN|IO|IQ|IR|IS|IT|JE|JM|JO|JP|KE|KG|KH|KI|KM|KN|KP|KR|KW|KY|KZ|LA|LB|LC|LI|LK|LR|LS|LT|LU|LV|LY|MA|MC|MD|ME|MG|MH|MK|ML|MM|MN|MO|MP|MQ|MR|MS|MT|MU|MV|MW|MX|MY|MZ|NA|NC|NE|NF|NG|NI|NL|NO|NP|NR|NU|NZ|OM|PA|PE|PF|PG|PH|PK|PL|PM|PN|PR|PS|PT|PW|PY|QA|RE|RO|RS|RU|RW|SA|SB|SC|SD|SE|SG|SH|SI|SJ|SK|SL|SM|SN|SO|SR|SS|ST|SU|SV|SX|SY|SZ|TC|TD|TF|TG|TH|TJ|TK|TL|TM|TN|TO|TR|TT|TV|TW|TZ|UA|UG|UK|US|UY|UZ|VA|VC|VE|VG|VI|VN|VU|WF|WS|YE|YT|ZA|ZM|ZW))(?::\d{2,5})?(?:/[a-z0-9/\-_%$@&()!?'=~*+:;,.]+)*/?(?:[?#]\S*)*/?)"
    [<Literal>] // en-sampled.txt
    let SHERLOCK = @"Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"
    [<Literal>] // en-sampled.txt
    let SHERLOCK_CASEIGNORE = @"(?i)Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"

    [<Literal>] // twain
    let WORD_END = @"\w+nn\W" // \b\w+nn\b

    [<Literal>] // twain
    let HUCK_SAW = @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"

    [<Literal>] // twain
    let HAVE_THERE = ".*have.*&.*there.*"







let collectNullablePositionsNoSkip ( matcher: RegexMatcher<TSet>, loc: byref<Location> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            loc.Position <- loc.Position

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount


let collectNullablePositionsOriginal ( matcher: RegexMatcher<TSet>, loc: byref<Location> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            matcher.TrySkipInitialRev(&loc, &currentStateId) |> ignore

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount

let loadJsonCharFrequencies (jsonText: string) =
    let json = JsonValue.Parse jsonText
    (json.Item "characters").AsArray() |> Seq.map (fun charFreq ->
        ((charFreq.Item "character").GetValue<char>(), (charFreq.Item "frequency").GetValue<float>())
        ) |> dict

let characterFreq = loadJsonCharFrequencies frequenciesJsonText

let commonalityScore (charSet: char array) =
    charSet |> Array.map (fun c ->
        if Char.IsAsciiLetterLower c then 10
        else 0)
    |> Array.sum

let commonalityScore3 (charSet: char array) =
    charSet |> Array.map (fun c ->
        if characterFreq.ContainsKey(c) then characterFreq.Item c
        else 0)
    |> Array.sum

let prefixSearchWeightedReversed (loc: byref<Location>) (cache: RegexCache<TSet>)
    (weightedSets: inref<(int * MintermSearchValues) list>) =
    let textSpan = loc.Input
    let rarestCharSet = snd weightedSets[0]
    let rarestCharSetIndex = fst weightedSets[0]
    let mutable searching = true

    let mutable prevMatch = loc.Position
    while searching do
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.SearchValues) with
        // | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length <= textSpan.Length) ->
        | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length <= loc.Position) ->
            let absMatchStart = curMatch - rarestCharSetIndex
            let mutable fullMatch = true
            let mutable i = 1
            while i < weightedSets.Length && absMatchStart + (fst weightedSets[i]) < textSpan.Length && fullMatch do
                let set = snd weightedSets[i]
                if textSpan.Slice(absMatchStart + (fst weightedSets[i]), 1).IndexOfAny(set.SearchValues) = -1 then
                    fullMatch <- false
                else
                    i <- i + 1
            prevMatch <- absMatchStart + rarestCharSetIndex
            if fullMatch && i = weightedSets.Length then
                searching <- false
                loc.Position <- absMatchStart + weightedSets.Length
        | -1 ->
            searching <- false
            loc.Position <- 0
        | outOfBounds -> prevMatch <- outOfBounds
    ()

let prefixSearchWeightedReversed2
    (loc: byref<Location>)
    (weightedSets: inref<struct(int * MintermSearchValues) array>) =
    // (a * b) is a reference tuple, struct(a * b) is a struct tuple
    let textSpan = loc.Input
    let struct(rarestCharSetIndex, rarestCharSet) = weightedSets[0]
    let mutable searching = true
    let mutable prevMatch = loc.Position
    while searching do
        // todo: .SearchValues
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.SearchValues) with
        | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length <= loc.Position) ->
            let absMatchStart = curMatch - rarestCharSetIndex
            let mutable fullMatch = true
            let mutable i = 1
            let struct(weightedSetIndex,weightedSet) = weightedSets[i]
            while fullMatch && i < weightedSets.Length && absMatchStart + (weightedSetIndex) < textSpan.Length do
                let set = weightedSet
                if not (set.Contains(textSpan[absMatchStart + weightedSetIndex])) then
                    fullMatch <- false
                else
                    i <- i + 1
            // prevMatch <-
            //     if rarestCharSetIndex = 0 then absMatchStart - 1 else
            //     absMatchStart + rarestCharSetIndex
            prevMatch <- absMatchStart + rarestCharSetIndex
            if fullMatch && i = weightedSets.Length then
                searching <- false
                loc.Position <- absMatchStart + weightedSets.Length
        | -1 ->
            searching <- false
            loc.Position <- 0
        | outOfBounds -> prevMatch <- outOfBounds
    ()

let prefixSearchWeightedReversed3
    (loc: byref<Location>)
    (weightedSets: inref<struct(int * MintermSearchValues) array>) =
    let textSpan = loc.Input
    let currentPosition = loc.Position
    let charSetsCount = weightedSets.Length
    let struct(rarestCharSetIndex, rarestCharSet) = weightedSets[0]
    let mutable searching = true
    let mutable prevMatch = currentPosition
    while searching do
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.SearchValues) with
        // | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length < textSpan.Length) ->
        | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + charSetsCount <= currentPosition) ->
            let absMatchStart = curMatch - rarestCharSetIndex 
            let mutable fullMatch = true
            let mutable i = 1
            while fullMatch && i < charSetsCount do
                let struct (weightedSetIndex,weightedSet) = weightedSets[i]
                if not (weightedSet.Contains(textSpan[absMatchStart + weightedSetIndex])) then
                    fullMatch <- false
                else
                    i <- i + 1
            // ?
            // prevMatch <-
            //     if rarestCharSetIndex = 0 then absMatchStart - 1 else
            //     absMatchStart + rarestCharSetIndex
            prevMatch <- absMatchStart + rarestCharSetIndex
            if fullMatch && i = charSetsCount then
                searching <- false
                loc.Position <- absMatchStart + charSetsCount
        | -1 ->
            searching <- false
            loc.Position <- 0
        | outOfBounds -> prevMatch <- outOfBounds
    ()

let prefixSearchWeightedReversed4
    (cache: RegexCache<_>)
    (loc: byref<Location>)
    (weightedSets: inref<struct(int * MintermSearchValues) array>) =
    let textSpan = loc.Input
    let currentPosition = loc.Position
    let charSetsCount = weightedSets.Length
    let struct(rarestCharSetIndex, rarestCharSet) = weightedSets[0]
    let mutable searching = true
    let mutable prevMatch = currentPosition
    while searching do
        let nextMatch =
            match rarestCharSet.Mode with
            | MintermSearchMode.InvertedSearchValues -> textSpan.Slice(0, prevMatch).LastIndexOfAnyExcept(rarestCharSet.SearchValues)
            | MintermSearchMode.SearchValues -> textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.SearchValues)
            | MintermSearchMode.TSet ->
                // let slice = textSpan.Slice(0, prevMatch)
                // // [| 0; 1; 3; ;4 5|]
                // //       ________
                // let mutable e = slice.GetEnumerator()
                // let sequenceOfChars =
                //     seq {
                //         while e.MoveNext() do
                //             yield e.Current
                //     }
                // let index =
                //     sequenceOfChars
                //     |> Seq.findIndex (fun v ->
                //         let mt = cache.Classify(v)
                //         Solver.elemOfSet mt rarestCharSet.Minterm
                    // )
                failwith "todo: search TSet from text"
            | _ -> failwith "invalid enum"


        match nextMatch with
        // | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length < textSpan.Length) ->
        | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + charSetsCount <= currentPosition) ->
            let absMatchStart = curMatch - rarestCharSetIndex
            let mutable fullMatch = true
            let mutable i = 1
            while fullMatch && i < charSetsCount do
                let struct (weightedSetIndex,weightedSet) = weightedSets[i]
                if not (weightedSet.Contains(textSpan[absMatchStart + weightedSetIndex])) then
                    fullMatch <- false
                else
                    i <- i + 1
            // ?
            // prevMatch <-
            //     if rarestCharSetIndex = 0 then absMatchStart - 1 else
            //     absMatchStart + rarestCharSetIndex
            prevMatch <- absMatchStart + rarestCharSetIndex
            if fullMatch && i = charSetsCount then
                searching <- false
                loc.Position <- absMatchStart + charSetsCount
        | -1 ->
            searching <- false
            loc.Position <- 0
        | outOfBounds -> prevMatch <- outOfBounds
    ()


let collectNullablePositionsWeightedSkip ( matcher: RegexMatcher<TSet>, loc: byref<Location>, weightedSets: inref<(int * MintermSearchValues) list> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed &loc matcher.Cache &weightedSets

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount

// ---------- slightly modified

let collectNullablePositionsWeightedSkip2 ( matcher: RegexMatcher<TSet>, loc: byref<Location>, weightedSets: inref<struct (int * MintermSearchValues) array> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed2 &loc &weightedSets

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount

let collectNullablePositionsWeightedSkip3 ( matcher: RegexMatcher<TSet>, loc: byref<Location>, weightedSets: inref<struct (int * MintermSearchValues) array> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed3 &loc &weightedSets

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount

let collectNullablePositionsWeightedSkip4 ( matcher: RegexMatcher<TSet>, loc: byref<Location>, weightedSets: inref<struct (int * MintermSearchValues) array> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed4 matcher.Cache &loc &weightedSets

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount






[<MemoryDiagnoser(true)>]
// [<ShortRunJob>]
type PrefixCharsetSearch () =

    // let regex = Sbre.Regex("Huck[a-zA-Z]+|Saw[a-zA-Z]+")
    // let regex = Sbre.Regex("[a-zA-Z]+ckle|[a-zA-Z]+awy")
    // let regex = Sbre.Regex(".*have.*&.*there.*")
    
    let regex = Sbre.Regex(@"\w+nn[ \n.,!?']")
    // let regex = Sbre.Regex(@"\w+(nn[ \n.,!?']|xxx)")
    // let regex = Sbre.Regex(@"\w+(nn\W|xxx)")
    // let regex = Sbre.Regex(@"\w+(nn\W|xx\w)")
    
    // let regex = Sbre.Regex(@"\w+nn\W")

    // let regex = Sbre.Regex("Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty")
    // Sets:          [IJlo];[or];[ ceh.0];[LMkn];[ eo];[ HWrs];[Aaiot];[adlrt];[almrs];[deot];[enrsy]
    // Weighted sets 1: [or];[ eo];[IJlo];[LMkn];[ HWrs];[ ceh];[deot];[Aaiot];[adlrt];[almrs];[enrsy]
    // Weighted sets 0: [IJlo];[or];[LMkn];[ eo];[ HWrs];[ ceh];[Aaiot];[deot];[adlrt];[almrs];[enrsy]

    // let regex = Sbre.Regex("abca|xxx")

    // let rs = "[a-zA-Z]+ckl|[a-zA-Z]+awy"
    // [<Params("[a-zA-Z]+ckle|[a-zA-Z]+awy", "Huck[a-zA-Z]+|Saw[a-zA-Z]+", ".*have.*&.*there.*")>]
    // member val rs: string = "" with get, set


    let cache = regex.TSetMatcher.Cache
    let matcher = regex.TSetMatcher
    let optimizations = matcher.InitialOptimizations

    let prefixSets =
        match optimizations with
        | InitialOptimizations.SetsPotentialStart(prefixMem) ->
            Array.toList (prefixMem.ToArray()) |> List.rev
        | InitialOptimizations.SearchValuesPotentialStart(_,prefixMem) ->
            Array.toList (prefixMem.ToArray()) |> List.rev
        | InitialOptimizations.SetsPrefix(prefix, transitionNodeId) ->
            Array.toList (prefix.ToArray()) |> List.rev
        | _ -> failwith "incorrect optimizations"

    
    let weightedSets = []
    let weightedSets3 = []
    // let weightedSets = prefixSets |> List.mapi (fun i set ->
    //         (i, set, commonalityScore (cache.MintermChars(set).Value.Span.ToArray())))
    //                    |> List.sortBy (fun (_, _, score) -> score )
    //                    |> List.map (fun (i, set, _) -> (i, set))
    // let weightedSets3 = prefixSets |> List.mapi (fun i set ->
    //         (i, set, commonalityScore3 (cache.MintermChars(set).Value.Span.ToArray())))
    //                    |> List.sortBy (fun (_, _, score) -> score )
    //                    |> List.map (fun (i, set, _) -> (i, set))
    let weightedSets4 = prefixSets |> List.mapi (fun i set ->
            let mintermSV = cache.MintermSearchValues(set)
            match mintermSV.Mode with
            | MintermSearchMode.TSet -> (i, mintermSV, 100000.0)
            | MintermSearchMode.SearchValues -> (i, mintermSV, commonalityScore3 (mintermSV.CharactersInMinterm.Value.Span.ToArray()))
            | MintermSearchMode.InvertedSearchValues -> (i, mintermSV, 10000.0)
            | _ -> failwith "impossible!")
                       |> List.sortBy (fun (_, _, score) -> score )
                       |> List.map (fun (i, set, _) -> struct(i, set))
                       |> List.toArray


    let weightedCharsetsArray1 =
        weightedSets
        |> Seq.map (fun (int,tset) ->
            let chars = matcher.Cache.MintermSearchValues(tset)
            (int, chars)
        )
        |> Seq.toList
        // |> Seq.toArray


    let weightedCharsetsArray2 =
        weightedSets
        |> Seq.map (fun (int,tset) ->
            // TSet may contain up to 65k characters
            // so if is too large it's better to use "Solver.elemOfSet (matcher.Cache.Classify(char)) tset"
            // when such an example comes
            let chars = matcher.Cache.MintermSearchValues(tset)
            // ^ this allocation should really be moved out of the match algorithm
            // --
            // let isInverted = matcher.Cache.IsInverted(tset)
            // ^ this is not used for simplicity because it's not needed
            // but signals MintermChars is inverted
            struct(int, chars)
        )
        |> Seq.toArray


    let weightedCharsetsArray3 =
        weightedSets3
        |> Seq.map (fun (int,tset) ->
            let chars = matcher.Cache.MintermSearchValues(tset)
            struct(int, chars)
        )
        |> Seq.toArray


    [<Benchmark>]
    member this.NoSkip() =
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsNoSkip (matcher, &loc)

    [<Benchmark>]
    member this.Original() =
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsOriginal (matcher, &loc)


    // [<Benchmark>]
    member this.Weighted() =
        // let a = Optimizations.printPrefixSets cache (prefixSets)
        // let b = Optimizations.printPrefixSets cache (weightedSets |> List.map snd )
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip (matcher, &loc, &weightedCharsetsArray1)

    // [<Benchmark>]
    member this.Weighted2() =
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip2 (matcher, &loc, &weightedCharsetsArray2)
        // |> (function 8562 -> () | n -> failwith $"invalid result {n}") // sanity check for .*have.*&.*there.*

    // [<Benchmark>]
    member this.Weighted3() =
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip3 (matcher, &loc, &weightedCharsetsArray3)

    [<Benchmark>]
    member this.Weighted4() =
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip4 (matcher, &loc, &weightedSets4)





    // [<Benchmark>]
    // member this.WeightedCharsetSearch() =
    //     let regex = Regex(this.rs)
    //     let cache = regex.TSetMatcher.Cache
    //     let prefix = regex.InitialReversePrefix

    //     let prefixSets =
    //         match prefix with
    //         | InitialOptimizations.PotentialStartPrefix(prefixMem) ->
    //             Array.toList (prefixMem.ToArray()) |> List.rev
    //         | _ -> failwith "debug"

    //     let commonalityScore (charSet: char array) =
    //         charSet |> Array.map (fun c ->
    //             if Char.IsAsciiLetterLower c then 10
    //             else 0)
    //         |> Array.sum

    //     let weightedSets = prefixSets |> List.mapi (fun i set ->
    //         (i, set, commonalityScore (cache.MintermChars(set).ToArray())))
    //                        |> List.sortBy (fun (_, _, score) -> score )
    //                        |> List.map (fun (i, set, _) -> (i, set))

    //     let rarestCharSet = cache.MintermChars(snd weightedSets[0]).ToArray().AsMemory()
    //     let charSetIndex = fst weightedSets[0]
    //     let mutable searching = true
    //     let mutable matchPos = 0
    //     let textSpan = fullInput.AsSpan()
    //     // let potMatches = ResizeArray(100)

    //     while searching do
    //         match textSpan.Slice(matchPos).IndexOfAny(rarestCharSet.Span) with
    //         | -1 -> searching <- false
    //         | spanMatchStart when (spanMatchStart + matchPos - charSetIndex >= 0) ->
    //             let absMatchStart = spanMatchStart + matchPos - charSetIndex
    //             let mutable fullMatch = true
    //             let mutable i = 1
    //             while i < weightedSets.Length && absMatchStart + (fst weightedSets[i]) < textSpan.Length && fullMatch do
    //                 let set = cache.MintermChars(snd weightedSets[i])
    //                 if textSpan.Slice(absMatchStart + (fst weightedSets[i]), 1).IndexOfAny(set) = -1 then
    //                     fullMatch <- false
    //                 else
    //                     i <- i + 1
    //             matchPos <- absMatchStart + 1 + charSetIndex
    //             // if fullMatch then potMatches.Add({MatchPosition.Index = absMatchStart; Length = weightedSets.Length })
    //         | _ -> ()
    //     // potMatches


    // [<Benchmark>]
    // member this.NonWeightedCharsetSearch() =
    //     let regex = Regex(this.rs)
    //     let cache = regex.TSetMatcher.Cache
    //     let prefix = regex.InitialReversePrefix

    //     let prefixSets =
    //         match prefix with
    //         | InitialOptimizations.PotentialStartPrefix(prefixMem) ->
    //             Array.toList (prefixMem.ToArray()) |> List.rev
    //         | _ -> failwith "debug"

    //     let firstCharSet = cache.MintermChars(prefixSets[0]).ToArray().AsMemory()
    //     let mutable searching = true

    //     let mutable startPos = 0
    //     let textSpan = fullInput.AsSpan()
    //     // let potMatches = ResizeArray(100)
    //     while searching do
    //         match textSpan.Slice(startPos).IndexOfAny(firstCharSet.Span) with
    //         | -1 -> searching <- false
    //         | spanMatchStart ->
    //             let absMatchStart = spanMatchStart + startPos
    //             let mutable fullMatch = true
    //             let mutable i = 1
    //             while i < prefixSets.Length && absMatchStart + i < textSpan.Length && fullMatch do
    //                 let set = cache.MintermChars(prefixSets[i])
    //                 if textSpan.Slice(absMatchStart + i, 1).IndexOfAny(set) = -1 then
    //                     fullMatch <- false
    //                 else
    //                     i <- i + 1
    //             startPos <- absMatchStart + 1
    //             // if fullMatch then potMatches.Add({MatchPosition.Index = absMatchStart; Length = prefixSets.Length })
    //     // potMatches


    // [<Benchmark>]
    // member this.SbreCount() =
    //     let regex = Regex(this.rs)
    //     regex.Count fullInput

[<MemoryDiagnoser>]
[<ShortRunJob>]
// [<AbstractClass>]
type StringPrefix(pattern:string) =
    let regex = Sbre.Regex(pattern)
    let matcher = regex.TSetMatcher
    // find optimized prefix for regex
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )

    let charToTSet (chr:char) = matcher.Cache.CharToMinterm(chr)
    let isElemOfSet (tset1:TSet) (tset2:TSet) = Solver.elemOfSet tset1 tset2

    let svals = [|'n'|].AsMemory()

    // [<Benchmark>]
    // member x.SpanIndexOf() =
    //     // let tc = fullInput.AsSpan().Count("Twain")
    //     let tc = fullInput.AsSpan().Count(")")
    //     ()
        // if tc <> 811 then failwith $"invalid count {tc}"
        // if tc <> 2673 then failwith $"invalid count {tc}"

    [<Benchmark>]
    member x.SpanIndexOf1() =
        let span = fullInput.AsSpan()
        let mutable currpos = fullInput.Length
        let mutable looping = true
        let mutable tc = 0
        let tlen = "Twain".Length
        while looping do
            // vectorize only to first char
            let slice = span.Slice(0,currpos)
            let newPos = slice.IndexOfAny(svals.Span)
            if newPos = -1 || newPos < tlen then looping <- false else
            currpos <- newPos
            let mstart = currpos - tlen + 1
            let validStart = slice.Slice(mstart).StartsWith("Twain")
            if validStart then
                tc <- tc + 1
                currpos <- mstart
            else currpos <- currpos - 1
        if tc <> 811 then failwith $"invalid count: {tc}"


    // member x.VecLastIndex(vecSpans:ReadOnlySpan<Vector256<uint16>>) =
    //     let enumerator = vecSpans.Slice(0, )
    //     // for (var i = 0; i < vInts.Length; i++)
    //     // {
    //     //     var result = Vector256.Equals(vInts[i], compareValue);
    //     //     if (result == Vector256<int>.Zero) continue;
    //     //
    //     //     for (var k = 0; k < vectorLength; k++)
    //     //         if (result.GetElement(k) != 0)
    //     //             return i * vectorLength + k;
    //     // }

    [<Benchmark>]
    member x.SpanIndexOf2() =
        let origspan = fullInput.AsSpan()
        let mutable tc = 0
        let alignAmount = origspan.Length % 16
        let alignSpan = origspan.Slice(alignAmount)
        let inputVectors = MemoryMarshal.Cast<char, Vector256<uint16>>(alignSpan)
        let searchVector = Vector256.Create<uint16>(uint16 'n')
        let onevec = Vector256<uint16>.AllBitsSet
        let idx = inputVectors.Length - 1
        let tlen = "Twain".Length
        let outArray = Array.zeroCreate<uint16> 16
        let outSpan = outArray.AsSpan()

        for i = idx downto 0 do
            let result = Vector256.Equals(inputVectors[i], searchVector)
            if not (Vector256.EqualsAny(result, onevec)) then () else
            Vector256.CopyTo(result,outSpan)
            for j = 0 to 15 do
                if outSpan[j] <> 0us then
                    if j > 0 && inputVectors[i][j-1] <> uint16 'i' then () else
                    let absoluteIndex = (i * 16) + j
                    let mstart = absoluteIndex - tlen + 1
                    let validStart = alignSpan.Slice(mstart).StartsWith("Twain")
                    if validStart then
                        tc <- tc + 1
        if tc <> 811 then failwith $"invalid count: {tc}"




[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
[<AbstractClass>]
type SetsPrefix(pattern:string) =
    let regex = Sbre.Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    // find optimized prefix for regex
    let optimizations =
        Sbre.Optimizations.findInitialOptimizations
            getder
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache
            matcher.ReversePattern
            matcher.ReverseTrueStarredPattern
    let prefixSets =
        match optimizations with
        | InitialOptimizations.SetsPotentialStart(prefix) ->
            let reverseSpan = prefix.Span
            reverseSpan.Reverse()
            reverseSpan.ToArray()
        | InitialOptimizations.SetsPrefix(prefix, transitionid) ->
            let reverseSpan = prefix.Span
            reverseSpan.Reverse()
            reverseSpan.ToArray()
        | _ -> failwith "could not get prefix"

    let charToTSet (chr:char) = matcher.Cache.Classify(chr)
    let isElemOfSet (tset1:TSet) (tset2:TSet) = Solver.elemOfSet tset1 tset2

    [<Benchmark>]
    member x.FirstSetIndexOfTSet() =
        let inputSpan = fullInput.AsSpan()
        let mutable currpos = 0
        let mutable searching = true
        while searching do
            let currSet = charToTSet inputSpan[currpos]
            if isElemOfSet currSet prefixSets[0] then
                searching <- false
            else
                currpos <- currpos + 1

    [<Benchmark>]
    member x.FirstSetIndexOfChars() =
        let firstSetChars = matcher.Cache.MintermChars(prefixSets[0]).Value.Span
        let inputSpan = fullInput.AsSpan()
        let mutable searching = true
        while searching do
            match inputSpan.IndexOfAny(firstSetChars) with
            | -1 -> failwith "failed search"
            | n ->
                searching <- false





[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Prefix1() =
    // inherit StringPrefix("Twain")
    inherit StringPrefix("there")

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Prefix2() =
    // [HF][ui][cn][kn]
    inherit SetsPrefix("Huck|Finn")