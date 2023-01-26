module runsvs_mod

implicit none

integer, parameter :: runsvs_busnvar = 223
integer, parameter :: runsvs_ntiles = 5
integer NG
integer, parameter :: INDX_SOIL    =  1
integer, parameter :: INDX_GLACIER =  2
integer, parameter :: INDX_WATER   =  3
integer, parameter :: INDX_ICE     =  4
integer, parameter :: INDX_AGREGE  =  5
!integer, parameter :: INDX_URB     =  6
!integer, parameter :: INDX_MAX     =  6
integer, dimension(runsvs_busnvar), save :: runsvs_buspos
integer, dimension(runsvs_busnvar), save :: runsvs_buslev
integer, save :: runsvs_busdim
character(len = 20), dimension(runsvs_busnvar), save :: runsvs_busvarname
character(len = 4), dimension(runsvs_busnvar), save :: runsvs_busoutname
integer, save ::  accevap  
integer, save ::  acroot  
integer, save ::  alen  
integer, save ::  alfaq  
integer, save ::  alfat  
integer, save ::  algr  
integer, save ::  alvh  
integer, save ::  alvis  
integer, save ::  alvl  
integer, save ::  avg_gwsol  
integer, save ::  bcoef  
integer, save ::  bm  
integer, save ::  bt  
integer, save ::  cgsat  
integer, save ::  clay  
integer, save ::  clayen  
integer, save ::  co2i1  
integer, save ::  cveg  
integer, save ::  cvh  
integer, save ::  cvl  
integer, save ::  d50  
integer, save ::  d95  
integer, save ::  deciduous  
integer, save ::  dlat  
integer, save ::  dlon  
integer, save ::  drain  
integer, save ::  drainaf  
integer, save ::  draindens  
integer, save ::  drnden  
integer, save ::  dsst  
integer, save ::  dtdiag  
integer, save ::  eflux  
integer, save ::  eg  
integer, save ::  emis  
integer, save ::  emisgr  
integer, save ::  emistg  
integer, save ::  emistgen  
integer, save ::  emisvh  
integer, save ::  emisvl  
integer, save ::  er  
integer, save ::  etr  
integer, save ::  evergreen  
integer, save ::  fbcof  
integer, save ::  fc  
integer, save ::  fcor  
integer, save ::  fdsi    
integer, save ::  fl  
integer, save ::  fluslop  
integer, save ::  flusolis 
integer, save ::  fq  
integer, save ::  frootd  
integer, save ::  frv  
integer, save ::  ftemp  
integer, save ::  fv  
integer, save ::  fvap  
integer, save ::  fvapliq  
integer, save ::  fvapliqaf  
integer, save ::  gamvh  
integer, save ::  gamvl  
integer, save ::  glacen  
integer, save ::  glacier  
integer, save ::  glsea0  
integer, save ::  glseaen  
integer, save ::  grkef  
integer, save ::  grksat  
integer, save ::  hfluxsa  
integer, save ::  hfluxsv  
integer, save ::  hst  
integer, save ::  humoins 
integer, save ::  husurf  
integer, save ::  hv  
integer, save ::  icedp  
integer, save ::  icedpen  
integer, save ::  iceline  
integer, save ::  icelinen  
integer, save ::  ilmo  
integer, save ::  impervu  
integer, save ::  insmavg  
integer, save ::  isoil  
integer, save ::  isoilen  
integer, save ::  khc  
integer, save ::  ksat  
integer, save ::  ksatc  
integer, save ::  laictem  
integer, save ::  laideci  
integer, save ::  laiva  
integer, save ::  laivf26  
integer, save ::  laivh  
integer, save ::  laivhen  
integer, save ::  laivl  
integer, save ::  laivlen  
integer, save ::  latflaf  
integer, save ::  latflw  
integer, save ::  leg  
integer, save ::  ler  
integer, save ::  les  
integer, save ::  lesv  
integer, save ::  letr  
integer, save ::  lev  
integer, save ::  melts  
integer, save ::  meltsr  
integer, save ::  mgen  
integer, save ::  pmoins  
integer, save ::  psi  
integer, save ::  psisat  
integer, save ::  psngrvl  
integer, save ::  psnvh  
integer, save ::  psnvha  
integer, save ::  qdiag  
integer, save ::  qsurf  
integer, save ::  rainrate 
integer, save ::  rcctem  
integer, save ::  resaef  
integer, save ::  resagr  
integer, save ::  resasa  
integer, save ::  resasv  
integer, save ::  resavg  
integer, save ::  rglvh  
integer, save ::  rglvl  
integer, save ::  rnet_s  
integer, save ::  rnetsa  
integer, save ::  rnetsv  
integer, save ::  rootdp  
integer, save ::  rsnowsa  
integer, save ::  rsnowsv  
integer, save ::  rst  
integer, save ::  runofftot  
integer, save ::  runofftotaf  
integer, save ::  rveg  
integer, save ::  sand  
integer, save ::  sanden  
integer, save ::  sfcwgt  
integer, save ::  skin_depth  
integer, save ::  skin_inc  
integer, save ::  skyview  
integer, save ::  slop  
integer, save ::  slopen  
integer, save ::  snoal  
integer, save ::  snoalen  
integer, save ::  snoden  
integer, save ::  snodenen  
integer, save ::  snodp  
integer, save ::  snodpen  
integer, save ::  snodpl  
integer, save ::  snodplen  
integer, save ::  snoma  
integer, save ::  snomaen  
integer, save ::  snoro  
integer, save ::  snowrate 
integer, save ::  snval  
integer, save ::  snvalen  
integer, save ::  snvden  
integer, save ::  snvdenen  
integer, save ::  snvdp  
integer, save ::  snvdpen  
integer, save ::  snvma  
integer, save ::  snvmaen  
integer, save ::  snvro  
integer, save ::  stomrvh  
integer, save ::  stomrvl  
integer, save ::  svs_wta  
integer, save ::  tdiag  
integer, save ::  tglacen  
integer, save ::  tglacier  
integer, save ::  tground  
integer, save ::  tgrounden  
integer, save ::  thetaa  
integer, save ::  tmice  
integer, save ::  tmicen  
integer, save ::  tmoins  
integer, save ::  tnolim  
integer, save ::  tsa  
integer, save ::  tsnavg  
integer, save ::  tsnow  
integer, save ::  tsnowen  
integer, save ::  tsnowveg  
integer, save ::  tsnowvegen  
integer, save ::  tsrad 
integer, save ::  tsurf 
integer, save ::  tsvavg  
integer, save ::  tvege  
integer, save ::  tvegeen  
integer, save ::  twater  
integer, save ::  twateren  
integer, save ::  udiag  
integer, save ::  umoins  
integer, save ::  urban  
integer, save ::  vdiag  
integer, save ::  vegf  
integer, save ::  vegfen  
integer, save ::  vegfrac  
integer, save ::  vegh  
integer, save ::  veghen  
integer, save ::  vegl  
integer, save ::  veglen  
integer, save ::  vegtrans  
integer, save ::  vgctem  
integer, save ::  vmoins  
integer, save ::  watflow  
integer, save ::  wfc  
integer, save ::  wfcdp  
integer, save ::  wfcint  
integer, save ::  wflux  
integer, save ::  wfluxaf  
integer, save ::  wsat  
integer, save ::  wsnow  
integer, save ::  wsnowen  
integer, save ::  wsnv  
integer, save ::  wsnven  
integer, save ::  wsoil  
integer, save ::  wsoilen  
integer, save ::  wsoilm  
integer, save ::  wveg  
integer, save ::  wvegen  
integer, save ::  wwilt  
integer, save ::  z0    
integer, save ::  z0en  
integer, save ::  z0ha  
integer, save ::  z0mvh  
integer, save ::  z0mvl  
integer, save ::  z0t  
integer, save ::  ztsl  
integer, save ::  zusl  

contains

subroutine svs_bus_init(NGRIDCELLS)

  implicit none

  integer NGRIDCELLS

  NG = NGRIDCELLS
  runsvs_buspos = (/(1-1)*NG+1, (2-1)*NG+1, (13-1)*NG+1, (14-1)*NG+1, (15-1)*NG+1, (16-1)*NG+1,& 
 (17-1)*NG+1, (18-1)*NG+1, (23-1)*NG+1, (24-1)*NG+1, (25-1)*NG+1, (36-1)*NG+1, (37-1)*NG+1,& 
 (42-1)*NG+1, (43-1)*NG+1, (54-1)*NG+1, (65-1)*NG+1, (74-1)*NG+1, (75-1)*NG+1, (76-1)*NG+1,& 
 (77-1)*NG+1, (78-1)*NG+1, (79-1)*NG+1, (80-1)*NG+1, (81-1)*NG+1, (82-1)*NG+1, (93-1)*NG+1,& 
 (94-1)*NG+1, (95-1)*NG+1, (96-1)*NG+1, (97-1)*NG+1, (98-1)*NG+1, (99-1)*NG+1, (100-1)*NG+1,& 
 (101-1)*NG+1, (102-1)*NG+1, (103-1)*NG+1, (104-1)*NG+1, (105-1)*NG+1, (106-1)*NG+1, (107-1)*NG+1,& 
 (108-1)*NG+1, (109-1)*NG+1, (120-1)*NG+1, (125-1)*NG+1, (126-1)*NG+1, (127-1)*NG+1, (128-1)*NG+1,& 
 (129-1)*NG+1, (130-1)*NG+1, (131-1)*NG+1, (142-1)*NG+1, (147-1)*NG+1, (152-1)*NG+1, (157-1)*NG+1,& 
 (162-1)*NG+1, (163-1)*NG+1, (164-1)*NG+1, (165-1)*NG+1, (166-1)*NG+1, (167-1)*NG+1, (168-1)*NG+1,& 
 (169-1)*NG+1, (170-1)*NG+1, (171-1)*NG+1, (182-1)*NG+1, (183-1)*NG+1, (184-1)*NG+1, (189-1)*NG+1,& 
 (190-1)*NG+1, (191-1)*NG+1, (192-1)*NG+1, (193-1)*NG+1, (194-1)*NG+1, (195-1)*NG+1, (196-1)*NG+1,& 
 (201-1)*NG+1, (202-1)*NG+1, (203-1)*NG+1, (214-1)*NG+1, (225-1)*NG+1, (236-1)*NG+1, (247-1)*NG+1,& 
 (258-1)*NG+1, (267-1)*NG+1, (268-1)*NG+1, (269-1)*NG+1, (295-1)*NG+1, (296-1)*NG+1, (297-1)*NG+1,& 
 (298-1)*NG+1, (299-1)*NG+1, (300-1)*NG+1, (311-1)*NG+1, (312-1)*NG+1, (313-1)*NG+1, (314-1)*NG+1,& 
 (315-1)*NG+1, (316-1)*NG+1, (317-1)*NG+1, (318-1)*NG+1, (319-1)*NG+1, (320-1)*NG+1, (321-1)*NG+1,& 
 (332-1)*NG+1, (343-1)*NG+1, (344-1)*NG+1, (345-1)*NG+1, (346-1)*NG+1, (347-1)*NG+1, (352-1)*NG+1,& 
 (353-1)*NG+1, (354-1)*NG+1, (355-1)*NG+1, (356-1)*NG+1, (357-1)*NG+1, (358-1)*NG+1, (359-1)*NG+1,& 
 (360-1)*NG+1, (361-1)*NG+1, (362-1)*NG+1, (363-1)*NG+1, (364-1)*NG+1, (365-1)*NG+1, (366-1)*NG+1,& 
 (367-1)*NG+1, (368-1)*NG+1, (373-1)*NG+1, (378-1)*NG+1, (379-1)*NG+1, (390-1)*NG+1, (401-1)*NG+1,& 
 (406-1)*NG+1, (407-1)*NG+1, (408-1)*NG+1, (409-1)*NG+1, (410-1)*NG+1, (411-1)*NG+1, (412-1)*NG+1,& 
 (413-1)*NG+1, (414-1)*NG+1, (415-1)*NG+1, (420-1)*NG+1, (425-1)*NG+1, (426-1)*NG+1, (427-1)*NG+1,& 
 (428-1)*NG+1, (429-1)*NG+1, (430-1)*NG+1, (431-1)*NG+1, (432-1)*NG+1, (433-1)*NG+1, (434-1)*NG+1,& 
 (435-1)*NG+1, (436-1)*NG+1, (437-1)*NG+1, (438-1)*NG+1, (439-1)*NG+1, (440-1)*NG+1, (441-1)*NG+1,& 
 (442-1)*NG+1, (447-1)*NG+1, (448-1)*NG+1, (450-1)*NG+1, (452-1)*NG+1, (454-1)*NG+1, (456-1)*NG+1,& 
 (457-1)*NG+1, (457-1)*NG+1, (457-1)*NG+1, (458-1)*NG+1, (459-1)*NG+1, (460-1)*NG+1, (461-1)*NG+1,& 
 (463-1)*NG+1, (465-1)*NG+1, (467-1)*NG+1, (469-1)*NG+1, (470-1)*NG+1, (471-1)*NG+1, (472-1)*NG+1,& 
 (474-1)*NG+1, (476-1)*NG+1, (477-1)*NG+1, (478-1)*NG+1, (479-1)*NG+1, (480-1)*NG+1, (481-1)*NG+1,& 
 (482-1)*NG+1, (508-1)*NG+1, (534-1)*NG+1, (539-1)*NG+1, (540-1)*NG+1, (541-1)*NG+1, (542-1)*NG+1,& 
 (543-1)*NG+1, (544-1)*NG+1, (553-1)*NG+1, (554-1)*NG+1, (566-1)*NG+1, (577-1)*NG+1, (578-1)*NG+1,& 
 (589-1)*NG+1, (590-1)*NG+1, (591-1)*NG+1, (602-1)*NG+1, (603-1)*NG+1, (604-1)*NG+1, (605-1)*NG+1,& 
 (606-1)*NG+1, (617-1)*NG+1, (628-1)*NG+1, (629-1)*NG+1, (630-1)*NG+1, (631-1)*NG+1, (642-1)*NG+1,& 
 (647-1)*NG+1, (648-1)*NG+1, (649-1)*NG+1, (650-1)*NG+1, (651-1)*NG+1, (656-1)*NG+1, (657-1)*NG+1/)
  runsvs_busdim = 658*NG
  runsvs_buslev = (/1, 11, 1, 1, 1, 1, 1, 5, 1,& 
 1, 11, 1, 5, 1, 11, 11, 9, 1, 1,& 
 1, 1, 1, 1, 1, 1, 11, 1, 1, 1,& 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,& 
 1, 1, 1, 11, 5, 1, 1, 1, 1, 1,& 
 1, 11, 5, 5, 5, 5, 1, 1, 1, 1,& 
 1, 1, 1, 1, 1, 11, 1, 1, 5, 1,& 
 1, 1, 1, 1, 1, 1, 5, 1, 1, 11,& 
 11, 11, 11, 11, 9, 1, 1, 26, 1, 1,& 
 1, 1, 1, 11, 1, 1, 1, 1, 1, 1,& 
 1, 1, 1, 1, 11, 11, 1, 1, 1, 1,& 
 5, 1, 1, 1, 1, 1, 1, 1, 1, 1,& 
 1, 1, 1, 1, 1, 1, 1, 5, 5, 1,& 
 11, 11, 5, 1, 1, 1, 1, 1, 1, 1,& 
 1, 1, 5, 5, 1, 1, 1, 1, 1, 1,& 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,& 
 1, 5, 1, 2, 2, 2, 2, 1, 0, 0,& 
 1, 1, 1, 1, 2, 2, 2, 2, 1, 1,& 
 1, 2, 2, 1, 1, 1, 1, 1, 1, 26,& 
 26, 5, 1, 1, 1, 1, 1, 9, 1, 12,& 
 11, 1, 11, 1, 1, 11, 1, 1, 1, 1,& 
 11, 11, 1, 1, 1, 11, 5, 1, 1, 1,& 
 1, 5, 1, 1/)
  runsvs_busvarname = [ character(len=20) :: &
"accevap", "acroot", "alen", "alfaq", "alfat",& 
 "algr", "alvh", "alvis", "alvl", "avg_gwsol", "bcoef",& 
 "bm", "bt", "cgsat", "clay", "clayen", "co2i1",& 
 "cveg", "cvh", "cvl", "d50", "d95", "deciduous",& 
 "dlat", "dlon", "drain", "drainaf", "draindens", "drnden",& 
 "dsst", "dtdiag", "eflux", "eg", "emis", "emisgr",& 
 "emistg", "emistgen", "emisvh", "emisvl", "er", "etr",& 
 "evergreen", "fbcof", "fc", "fcor", "fdsi", "fl",& 
 "fluslop", "flusolis", "fq", "frootd", "frv", "ftemp",& 
 "fv", "fvap", "fvapliq", "fvapliqaf", "gamvh", "gamvl",& 
 "glacen", "glacier", "glsea0", "glseaen", "grkef", "grksat",& 
 "hfluxsa", "hfluxsv", "hst", "humoins", "husurf", "hv",& 
 "icedp", "icedpen", "iceline", "icelinen", "ilmo", "impervu",& 
 "insmavg", "isoil", "isoilen", "khc", "ksat", "ksatc",& 
 "laictem", "laideci", "laiva", "laivf26", "laivh", "laivhen",& 
 "laivl", "laivlen", "latflaf", "latflw", "leg", "ler",& 
 "les", "lesv", "letr", "lev", "melts", "meltsr",& 
 "mgen", "pmoins", "psi", "psisat", "psngrvl", "psnvh",& 
 "psnvha", "qdiag", "qsurf", "rainrate", "rcctem", "resaef",& 
 "resagr", "resasa", "resasv", "resavg", "rglvh", "rglvl",& 
 "rnet_s", "rnetsa", "rnetsv", "rootdp", "rsnowsa", "rsnowsv",& 
 "rst", "runofftot", "runofftotaf", "rveg", "sand", "sanden",& 
 "sfcwgt", "skin_depth", "skin_inc", "skyview", "slop", "slopen",& 
 "snoal", "snoalen", "snoden", "snodenen", "snodp", "snodpen",& 
 "snodpl", "snodplen", "snoma", "snomaen", "snoro", "snowrate",& 
 "snval", "snvalen", "snvden", "snvdenen", "snvdp", "snvdpen",& 
 "snvma", "snvmaen", "snvro", "stomrvh", "stomrvl", "svs_wta",& 
 "tdiag", "tglacen", "tglacier", "tground", "tgrounden", "thetaa",& 
 "tmice", "tmicen", "tmoins", "tnolim", "tsa", "tsnavg",& 
 "tsnow", "tsnowen", "tsnowveg", "tsnowvegen", "tsrad", "tsurf",& 
 "tsvavg", "tvege", "tvegeen", "twater", "twateren", "udiag",& 
 "umoins", "urban", "vdiag", "vegf", "vegfen", "vegfrac",& 
 "vegh", "veghen", "vegl", "veglen", "vegtrans", "vgctem",& 
 "vmoins", "watflow", "wfc", "wfcdp", "wfcint", "wflux",& 
 "wfluxaf", "wsat", "wsnow", "wsnowen", "wsnv", "wsnven",& 
 "wsoil", "wsoilen", "wsoilm", "wveg", "wvegen", "wwilt",& 
 "z0", "z0en", "z0ha", "z0mvh", "z0mvl", "z0t",& 
 "ztsl", "zusl"]
  runsvs_busoutname = [ character(len=4) :: & 
"ACWF", "ACRT", "1A  ", "AQ  ", "AT  ", "ALGR", "ALVH",& 
 "AL  ", "ALVL", "AGWS", "1G  ", "BM  ", "BT  ", "6I  ", "J2  ",& 
 "2H  ", "CO3 ", "CV  ", "CVH ", "CVL ", "d50 ", "d95 ", "DECI",& 
 "LA  ", "LO  ", "DR  ", "O1  ", "DRND", "DDEN", "DSST", "DLIM",& 
 "EFLX", "EG  ", "EMI1", "EMGR", "EMTG", "ETG1", "EMVH", "EMVL",& 
 "ER  ", "ETR ", "EVER", "3G  ", "FC  ", "WC  ", "FI  ", "FL  ",& 
 "FBSL", "FB  ", "FQ  ", "FRTD", "B5  ", "FT  ", "FV  ", "HF  ",& 
 "HFLQ", "AHFL", "GGVH", "GGVL", "1F  ", "2F  ", "GY  ", "4A  ",& 
 "GKE", "GKS  ", "HFSA", "HFSV", "H1  ", "H8  ", "FH  ", "HV  ",& 
 "I8  ", "2I  ", "ICEL", "ICLE", "IO  ", "IMPU", "IMAV", "ISOL",& 
 "2Q  ", "KHC ", "KSAT  ", "KSTC  ", "LC    ", "LAID", "LAIA", "LAVF",& 
 "LAIH", "SVS1", "LAIL", "SVS2", "ALAT", "LATF", "L2  ", "LR  ",& 
 "LS  ", "LSV ", "LT  ", "LV  ", "MLTS", "MLTR", "6A  ", "P8  ",& 
 "PSI ", "D5  ", "PSGL", "PSVH", "PSVA", "DQ  ", "J9  ", "U1  ",& 
 "RCC ", "RSEF", "RSGR", "RSSA", "RSSV", "RSVG", "RGVH", "RGVL",& 
 "NR  ", "RNSA", "RNSV", "D2  ", "RSA ", "RSV ", "R1  ", "TRUN",& 
 "TRAF", "RVG ", "J1  ", "2G  ", "WT  ", "SDEP", "SINC", "SVF ",& 
 "SLOP", "SLOE", "SNAL", "5H  ", "SNDN", "MI12", "SD  ", "7A  ",& 
 "SNDP", "MI05", "SNM ", "MI02", "SNDR", "U3  ", "SVAL", "MI03",& 
 "SVDN", "MI13", "SVDP", "MI04", "SVM ", "MI09", "SVDR", "RSVH",& 
 "RSVL", "SVSW", "TJ  ", "2W  ", "I9  ", "TGR ", "MI06", "N1  ",& 
 "I7  ", "2S  ", "T8  ", "TNOL", "TSA ", "ATSN", "TSN ", "MI07",& 
 "TSNV", "MI08", "TG  ", "J8  ", "ATSV", "TVG ", "MI10", "TM  ",& 
 "8A  ", "UD  ", "U8  ", "URBF", "VD  ", "2V  ", "2U  ", "K1  ",& 
 "VEGH", "SVS3", "VEGL", "SVS4", "VGTR", "VGCT", "V8  ", "WFL ",& 
 "WFC ", "WFCD", "WFCI", "M8  ", "N7  ", "WSAT", "WSN ", "2R  ",& 
 "WSV ", "MI11", "WSOL", "MI01", "WSLM", "WVEG", "2L  ", "WWLT",& 
 "Z0  ", "2B  ", "Z0HA", "Z0VH", "Z0VL", "ZT  ", "ZH  ", "ZU  "]
   accevap  = (1-1)*NG+1
   acroot  = (2-1)*NG+1
   alen  = (13-1)*NG+1
   alfaq  = (14-1)*NG+1
   alfat  = (15-1)*NG+1
   algr  = (16-1)*NG+1
   alvh  = (17-1)*NG+1
   alvis  = (18-1)*NG+1
   alvl  = (23-1)*NG+1
   avg_gwsol  = (24-1)*NG+1
   bcoef  = (25-1)*NG+1
   bm  = (36-1)*NG+1
   bt  = (37-1)*NG+1
   cgsat  = (42-1)*NG+1
   clay  = (43-1)*NG+1
   clayen  = (54-1)*NG+1
   co2i1  = (65-1)*NG+1
   cveg  = (74-1)*NG+1
   cvh  = (75-1)*NG+1
   cvl  = (76-1)*NG+1
   d50  = (77-1)*NG+1
   d95  = (78-1)*NG+1
   deciduous  = (79-1)*NG+1
   dlat  = (80-1)*NG+1
   dlon  = (81-1)*NG+1
   drain  = (82-1)*NG+1
   drainaf  = (93-1)*NG+1
   draindens  = (94-1)*NG+1
   drnden  = (95-1)*NG+1
   dsst  = (96-1)*NG+1
   dtdiag  = (97-1)*NG+1
   eflux  = (98-1)*NG+1
   eg  = (99-1)*NG+1
   emis  = (100-1)*NG+1
   emisgr  = (101-1)*NG+1
   emistg  = (102-1)*NG+1
   emistgen  = (103-1)*NG+1
   emisvh  = (104-1)*NG+1
   emisvl  = (105-1)*NG+1
   er  = (106-1)*NG+1
   etr  = (107-1)*NG+1
   evergreen  = (108-1)*NG+1
   fbcof  = (109-1)*NG+1
   fc  = (120-1)*NG+1
   fcor  = (125-1)*NG+1
   fdsi    = (126-1)*NG+1
   fl  = (127-1)*NG+1
   fluslop  = (128-1)*NG+1
   flusolis = (129-1)*NG+1
   fq  = (130-1)*NG+1
   frootd  = (131-1)*NG+1
   frv  = (142-1)*NG+1
   ftemp  = (147-1)*NG+1
   fv  = (152-1)*NG+1
   fvap  = (157-1)*NG+1
   fvapliq  = (162-1)*NG+1
   fvapliqaf  = (163-1)*NG+1
   gamvh  = (164-1)*NG+1
   gamvl  = (165-1)*NG+1
   glacen  = (166-1)*NG+1
   glacier  = (167-1)*NG+1
   glsea0  = (168-1)*NG+1
   glseaen  = (169-1)*NG+1
   grkef  = (170-1)*NG+1
   grksat  = (171-1)*NG+1
   hfluxsa  = (182-1)*NG+1
   hfluxsv  = (183-1)*NG+1
   hst  = (184-1)*NG+1
   humoins = (189-1)*NG+1
   husurf  = (190-1)*NG+1
   hv  = (191-1)*NG+1
   icedp  = (192-1)*NG+1
   icedpen  = (193-1)*NG+1
   iceline  = (194-1)*NG+1
   icelinen  = (195-1)*NG+1
   ilmo  = (196-1)*NG+1
   impervu  = (201-1)*NG+1
   insmavg  = (202-1)*NG+1
   isoil  = (203-1)*NG+1
   isoilen  = (214-1)*NG+1
   khc  = (225-1)*NG+1
   ksat  = (236-1)*NG+1
   ksatc  = (247-1)*NG+1
   laictem  = (258-1)*NG+1
   laideci  = (267-1)*NG+1
   laiva  = (268-1)*NG+1
   laivf26  = (269-1)*NG+1
   laivh  = (295-1)*NG+1
   laivhen  = (296-1)*NG+1
   laivl  = (297-1)*NG+1
   laivlen  = (298-1)*NG+1
   latflaf  = (299-1)*NG+1
   latflw  = (300-1)*NG+1
   leg  = (311-1)*NG+1
   ler  = (312-1)*NG+1
   les  = (313-1)*NG+1
   lesv  = (314-1)*NG+1
   letr  = (315-1)*NG+1
   lev  = (316-1)*NG+1
   melts  = (317-1)*NG+1
   meltsr  = (318-1)*NG+1
   mgen  = (319-1)*NG+1
   pmoins  = (320-1)*NG+1
   psi  = (321-1)*NG+1
   psisat  = (332-1)*NG+1
   psngrvl  = (343-1)*NG+1
   psnvh  = (344-1)*NG+1
   psnvha  = (345-1)*NG+1
   qdiag  = (346-1)*NG+1
   qsurf  = (347-1)*NG+1
   rainrate = (352-1)*NG+1
   rcctem  = (353-1)*NG+1
   resaef  = (354-1)*NG+1
   resagr  = (355-1)*NG+1
   resasa  = (356-1)*NG+1
   resasv  = (357-1)*NG+1
   resavg  = (358-1)*NG+1
   rglvh  = (359-1)*NG+1
   rglvl  = (360-1)*NG+1
   rnet_s  = (361-1)*NG+1
   rnetsa  = (362-1)*NG+1
   rnetsv  = (363-1)*NG+1
   rootdp  = (364-1)*NG+1
   rsnowsa  = (365-1)*NG+1
   rsnowsv  = (366-1)*NG+1
   rst  = (367-1)*NG+1
   runofftot  = (368-1)*NG+1
   runofftotaf  = (373-1)*NG+1
   rveg  = (378-1)*NG+1
   sand  = (379-1)*NG+1
   sanden  = (390-1)*NG+1
   sfcwgt  = (401-1)*NG+1
   skin_depth  = (406-1)*NG+1
   skin_inc  = (407-1)*NG+1
   skyview  = (408-1)*NG+1
   slop  = (409-1)*NG+1
   slopen  = (410-1)*NG+1
   snoal  = (411-1)*NG+1
   snoalen  = (412-1)*NG+1
   snoden  = (413-1)*NG+1
   snodenen  = (414-1)*NG+1
   snodp  = (415-1)*NG+1
   snodpen  = (420-1)*NG+1
   snodpl  = (425-1)*NG+1
   snodplen  = (426-1)*NG+1
   snoma  = (427-1)*NG+1
   snomaen  = (428-1)*NG+1
   snoro  = (429-1)*NG+1
   snowrate = (430-1)*NG+1
   snval  = (431-1)*NG+1
   snvalen  = (432-1)*NG+1
   snvden  = (433-1)*NG+1
   snvdenen  = (434-1)*NG+1
   snvdp  = (435-1)*NG+1
   snvdpen  = (436-1)*NG+1
   snvma  = (437-1)*NG+1
   snvmaen  = (438-1)*NG+1
   snvro  = (439-1)*NG+1
   stomrvh  = (440-1)*NG+1
   stomrvl  = (441-1)*NG+1
   svs_wta  = (442-1)*NG+1
   tdiag  = (447-1)*NG+1
   tglacen  = (448-1)*NG+1
   tglacier  = (450-1)*NG+1
   tground  = (452-1)*NG+1
   tgrounden  = (454-1)*NG+1
   thetaa  = (456-1)*NG+1
   tmice  = (457-1)*NG+1
   tmicen  = (457-1)*NG+1
   tmoins  = (457-1)*NG+1
   tnolim  = (458-1)*NG+1
   tsa  = (459-1)*NG+1
   tsnavg  = (460-1)*NG+1
   tsnow  = (461-1)*NG+1
   tsnowen  = (463-1)*NG+1
   tsnowveg  = (465-1)*NG+1
   tsnowvegen  = (467-1)*NG+1
   tsrad = (469-1)*NG+1
   tsurf = (470-1)*NG+1
   tsvavg  = (471-1)*NG+1
   tvege  = (472-1)*NG+1
   tvegeen  = (474-1)*NG+1
   twater  = (476-1)*NG+1
   twateren  = (477-1)*NG+1
   udiag  = (478-1)*NG+1
   umoins  = (479-1)*NG+1
   urban  = (480-1)*NG+1
   vdiag  = (481-1)*NG+1
   vegf  = (482-1)*NG+1
   vegfen  = (508-1)*NG+1
   vegfrac  = (534-1)*NG+1
   vegh  = (539-1)*NG+1
   veghen  = (540-1)*NG+1
   vegl  = (541-1)*NG+1
   veglen  = (542-1)*NG+1
   vegtrans  = (543-1)*NG+1
   vgctem  = (544-1)*NG+1
   vmoins  = (553-1)*NG+1
   watflow  = (554-1)*NG+1
   wfc  = (566-1)*NG+1
   wfcdp  = (577-1)*NG+1
   wfcint  = (578-1)*NG+1
   wflux  = (589-1)*NG+1
   wfluxaf  = (590-1)*NG+1
   wsat  = (591-1)*NG+1
   wsnow  = (602-1)*NG+1
   wsnowen  = (603-1)*NG+1
   wsnv  = (604-1)*NG+1
   wsnven  = (605-1)*NG+1
   wsoil  = (606-1)*NG+1
   wsoilen  = (617-1)*NG+1
   wsoilm  = (628-1)*NG+1
   wveg  = (629-1)*NG+1
   wvegen  = (630-1)*NG+1
   wwilt  = (631-1)*NG+1
   z0    = (642-1)*NG+1
   z0en  = (647-1)*NG+1
   z0ha  = (648-1)*NG+1
   z0mvh  = (649-1)*NG+1
   z0mvl  = (650-1)*NG+1
   z0t  = (651-1)*NG+1
   ztsl  = (656-1)*NG+1
   zusl  = (657-1)*NG+1
end subroutine
end module runsvs_mod
