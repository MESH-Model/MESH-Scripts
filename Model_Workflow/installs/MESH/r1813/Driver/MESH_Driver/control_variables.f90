!> Description:
!>  Module for storing control variable types.
module control_variables

    implicit none

    !> Description:
    !>  Global run options.
    type run_options

        !> RUNFLAGS: Flags to enable or disable processes (default all: .true.).
        !*  RUNCLIM: Read meteorological forcing input.
        !*  RUNCHNL: Run channel flow balance and output.
        !*  RUNBALWB: Run water balance and output.
        !*  RUNBALEB: Run energy balance and output.
        !*  RUNLSS: Run land surface scheme and output.
        !*  RUNTILE: Run tile processes (e.g., LSS).
        !*  RUNGRID: Run grid processes (e.g., multi-point run).
        logical :: RUNCLIM = .true.
        logical :: RUNCHNL = .true.
        logical :: RUNBALWB = .true.
        logical :: RUNBALEB = .true.
        logical :: RUNLSS = .true.
        logical :: RUNTILE = .true.
        logical :: RUNGRID = .true.

    end type !run_options

    !* ro: Instance of global run options.
    type(run_options), save :: ro

end module
