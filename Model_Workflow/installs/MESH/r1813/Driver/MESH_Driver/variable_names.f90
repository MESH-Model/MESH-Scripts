module variable_names

    implicit none

    !> Date/time columns.
    character(len = *), parameter :: VN_YEAR = 'YEAR'
    character(len = *), parameter :: VN_MONTH = 'MONTH'
    character(len = *), parameter :: VN_DAY = 'DAY'
    character(len = *), parameter :: VN_JDAY = 'JDAY'
    character(len = *), parameter :: VN_HOUR = 'HOUR'
    character(len = *), parameter :: VN_MINS = 'MINS'

    !> Suffixes.
    character(len = *), parameter :: VN_ACC = 'ACC'
    character(len = *), parameter :: VN_AVG = 'AVG'
    character(len = *), parameter :: VN_MAX = 'MAX'
    character(len = *), parameter :: VN_MIN = 'MIN'
    character(len = *), parameter :: VN_RNG = 'RNG'
    character(len = *), parameter :: VN_MEAS = 'MEAS'
    character(len = *), parameter :: VN_SIM = 'SIM'

    !> Labels.
    character(len = *), parameter :: VN_GRD = 'GRD'
    character(len = *), parameter :: VN_GRU = 'GRU'
    character(len = *), parameter :: VN_NML = 'NML'
    character(len = *), parameter :: VN_IG = 'IG'

    !> Basin attributes (general).
    character(len = *), parameter :: VN_NEXT = 'NEXT'
    character(len = *), parameter :: VN_GRIDAREA = 'GRIDAREA'
    character(len = *), parameter :: VN_ELEV = 'ELEV'
    character(len = *), parameter :: VN_TOPOSLOPE = 'TOPOSLOPE'

    !> Drainage/routing attributes.
    character(len = *), parameter :: VN_IRVR = 'IRVR'
    character(len = *), parameter :: VN_CHNLSLOPE = 'CHNLSLOPE'
    character(len = *), parameter :: VN_CHNLLENGTH = 'CHNLLENGTH'
    character(len = *), parameter :: VN_ICHNL = 'ICHNL'
    character(len = *), parameter :: VN_IREACH = 'IREACH'
    character(len = *), parameter :: VN_DA = 'DA'
    character(len = *), parameter :: VN_BNKFLL = 'BNKFLL'

    !> Meteorology/climatology variables.
    character(len = *), parameter :: VN_FSIN = 'FSIN'
    character(len = *), parameter :: VN_FSVS = 'FSVS'
    character(len = *), parameter :: VN_FSIR = 'FSIR'
    character(len = *), parameter :: VN_FSDR = 'FSDR'
    character(len = *), parameter :: VN_FSDFF = 'FSDFF'
    character(len = *), parameter :: VN_FSOUT = 'FSOUT'
    character(len = *), parameter :: VN_FLIN = 'FLIN'
    character(len = *), parameter :: VN_FLOUT = 'FLOUT'
    character(len = *), parameter :: VN_TA = 'TA'
    character(len = *), parameter :: VN_QA = 'QA'
    character(len = *), parameter :: VN_PRES = 'PRES'
    character(len = *), parameter :: VN_UU = 'UU'
    character(len = *), parameter :: VN_VV = 'VV'
    character(len = *), parameter :: VN_UV = 'UV'
    character(len = *), parameter :: VN_WDIR = 'WDIR'
    character(len = *), parameter :: VN_PRERN = 'PRERN'
    character(len = *), parameter :: VN_PRESNO = 'PRESNO'
    character(len = *), parameter :: VN_PRE = 'PRE'
    character(len = *), parameter :: VN_PRECRN = 'PRECRN'
    character(len = *), parameter :: VN_PRECSNO = 'PRECSNO'
    character(len = *), parameter :: VN_PREC = 'PREC'

    !> Canopy variables.
    character(len = *), parameter :: VN_LQWSCAN = 'LQWSCAN'
    character(len = *), parameter :: VN_FZWSCAN = 'FZWSCAN'
    character(len = *), parameter :: VN_CMAS = 'CMAS'
    character(len = *), parameter :: VN_TCAN = 'TCAN'
    character(len = *), parameter :: VN_GRO = 'GRO'

    !> Snow variables.
    character(len = *), parameter :: VN_FSNO = 'FSNO'
    character(len = *), parameter :: VN_SNO = 'SNO'
    character(len = *), parameter :: VN_RHOSNO = 'RHOSNO'
    character(len = *), parameter :: VN_ZSNO = 'ZSNO'
    character(len = *), parameter :: VN_LQWSSNO = 'LQWSSNO'
    character(len = *), parameter :: VN_TSNO = 'TSNO'
    character(len = *), parameter :: VN_ALBSNO = 'ALBSNO'
    character(len = *), parameter :: VN_DRAINSNO = 'DRAINSNO'

    !> Surface variables.
    character(len = *), parameter :: VN_ALBT = 'ALBT'
    character(len = *), parameter :: VN_ALVS = 'ALVS'
    character(len = *), parameter :: VN_ALIR = 'ALIR'
    character(len = *), parameter :: VN_GTE = 'GTE'
    character(len = *), parameter :: VN_ZPND = 'ZPND'
    character(len = *), parameter :: VN_LQWSPND = 'LQWSPND'
    character(len = *), parameter :: VN_TPND = 'TPND'
    character(len = *), parameter :: VN_PNDCAF = 'PNDCAF'
    character(len = *), parameter :: VN_POTEVP = 'POTEVP'
    character(len = *), parameter :: VN_ET = 'ET'
    character(len = *), parameter :: VN_EVPB = 'EVPB'
    character(len = *), parameter :: VN_ARRD = 'ARRD'
    character(len = *), parameter :: VN_OVRFLW = 'OVRFLW'
    character(len = *), parameter :: VN_QEVP = 'QEVP'
    character(len = *), parameter :: VN_QSENS = 'QSENS'
    character(len = *), parameter :: VN_GZERO = 'GZERO'
    character(len = *), parameter :: VN_TSURF = 'TSURF'

    !> Ice/glacier variables.
    character(len = *), parameter :: VN_LQWSICE = 'LQWSICE'
    character(len = *), parameter :: VN_TICE = 'TICE'

    !> Subsurface/soil variables.
    character(len = *), parameter :: VN_THLQSOL = 'THLQSOL'
    character(len = *), parameter :: VN_THICSOL = 'THICSOL'
    character(len = *), parameter :: VN_LQWSSOL = 'LQWSSOL'
    character(len = *), parameter :: VN_FZWSSOL = 'FZWSSOL'
    character(len = *), parameter :: VN_ALWSSOL = 'ALWSSOL'
    character(len = *), parameter :: VN_TSOL = 'TSOL'
    character(len = *), parameter :: VN_GFLX = 'GFLX'
    character(len = *), parameter :: VN_LATFLW = 'LATFLW'
    character(len = *), parameter :: VN_ZSOLSAT = 'ZSOLSAT'
    character(len = *), parameter :: VN_DRAINSOL = 'DRAINSOL'

    !> Groundwater/lower zone storage variables.
    character(len = *), parameter :: VN_RCHG = 'RCHG'
    character(len = *), parameter :: VN_STGGW = 'STGGW'
    character(len = *), parameter :: VN_LKG = 'LKG'
!-    character(len = *), parameter :: VN_DZS = 'DZS'

    !> Diagnostic variables.
    character(len = *), parameter :: VN_STGE = 'STGE'
    character(len = *), parameter :: VN_DSTGE = 'DSTGE'
    character(len = *), parameter :: VN_STGW = 'STGW'
    character(len = *), parameter :: VN_DSTGW = 'DSTGW'

    !> Routing variables.
    character(len = *), parameter :: VN_RFF = 'RFF'
    character(len = *), parameter :: VN_ROF = 'ROF'
    character(len = *), parameter :: VN_QI = 'QI'
    character(len = *), parameter :: VN_QO = 'QO'
    character(len = *), parameter :: VN_STGCH = 'STGCH'
    character(len = *), parameter :: VN_ZLVL = 'ZLVL'

    !> Auxilary.
    character(len = *), parameter :: VN_DUMMY_LENGTH = 'VN_DUMMY_LENGTH'

end module
