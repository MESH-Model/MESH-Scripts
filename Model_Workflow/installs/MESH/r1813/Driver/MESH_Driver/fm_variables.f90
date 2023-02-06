!>
!> Description:
!>  Contains variable types for structures or 'forms' in the model, such
!>  as landmark locations like streamflow gauge, irrigation demand,
!>  lake, and reservoir locations.
!>
!> Instances of these types are accessible by the
!> 'sa_mesh_variables' module: fms%
!>
!> Types:
!*  outlet_location: Location of an outlet location.
!>
module fm_variables

    implicit none

    !> Type: outlet_location
    !>  Attributes of an outlet location.
    !>
    !> Attributes:
    !*  iun: File unit (default: 100).
    !*  fname: File name (default: '' to avoid random characters).
    !*  lopen: Status of file opened, .true. if the file is open and attached 'iun'; .false. otherwise.
    !*  ffmt: File format (e.g., 'txt', 'tb0').
    type fm_config_file
        integer :: iun = 100
        character(len = 50) :: fname = ''
        logical :: lopen = .false.
        character(len = 8) :: ffmt = ''
    end type

    !> Type: outlet_location
    !>  Attributes of an outlet location.
    !>
    !> Attributes:
    !*  name: ID printed to output files.
    !*  y: Y-coordinate of outlet location.
    !*  x: X-coordinate of outlet location.
    !*  iy: Vertical index of the grid-cell containing the location.
    !*  jx: Horizontal index of the grid-cell containing the location.
    !*  n: Rank or index of the grid-cell containing the location.
    type outlet_location
        character(len = 8), dimension(:), allocatable :: name
        real, dimension(:), allocatable :: y, x
        integer, dimension(:), allocatable :: iy, jx, rnk
    end type

    !> Type: time_series
    !>  Attributes of a time-series (e.g., observed/measured flow at a location)
    !>
    !> Attributes:
    !*  type: Type of data (e.g., float).
    !*  units: Units of data.
    !*  readmode: Column reading mode (e.g., 'n', 'subset')
    !*  val: Time-series of data at the location.
    !*  dts: Time-step of the series. [s].
    !*  iyear: Year of the start date of data.
    !*  ijday: Day in the year of the start date of data.
    !*  imonth: Month of the start date of data.
    !*  iday: Day in the month of the start date of data.
    !*  ihour: Hour in the day of the start date of data.
    !*  imins: Minutes in the hour of the start date of data.
    type time_series
        type(fm_config_file) fls
        character(len = 8) type, units, readmode
        real, dimension(:), allocatable :: val
        integer dts, iyear, ijday, imonth, iday, ihour, imins
    end type

    !> Type: release_outlet
    !>  Attibutes of outlets with release coefficients.
    !>
    !> Attributes:
    !*  cfn: Type of release curve function.
    !*  b[1-5]: Flow release curve coefficients.
    !*  b6: Reach area required by RPN reservoir routing in RTE. [m2].
    !*  b7: Minimum level for zero flow required by RPN reservoir routing in RTE. [m].
    !*  area: Reach area to derive level; separate from b6 so as to not active RPN reservoir routing in RTE. [m2].
    type release_outlet
        integer, dimension(:), allocatable :: cfn
        real, dimension(:), allocatable :: &
            b1, b2, b3, b4, b5, &
            b6, b7, &
            area, zlvl0
    end type

    !> Type: streamflow_gauge_location
    !>  Attributes of streamflow gauge locations.
    !>
    !> Indices:
    !*  n: Number of locations dimensioned.
    !>
    !> Attributes:
    !*  DA: Drainage area.
    type streamflow_gauge_location
        integer :: n = 0
        type(outlet_location) meta
        real, dimension(:), allocatable :: da
        type(time_series) qomeas
    end type

    !> Type: reservoir_outlet_location
    !>  Attributes of lake and reservoir outlet locations.
    !>
    !> Indices:
    !*  n: Number of locations dimensioned.
    !>
    type reservoir_outlet_location
        integer :: n = 0
        type(outlet_location) meta
        type(release_outlet) rls
        type(time_series) rlsmeas
    end type

    !> Type: abstraction_point_location
    !>  Attributes of an abstraction point location for reservoir demand.
    !>
    !> Indices:
    !*  n: Number of locations dimensioned.
    !>
    !> Attributes:
    !*  s: Storage available. [m3].
    !*  smin: Minimum storage to keep in the channel. [m3].
    !*  fsmin: Fraction of storage to keep in the channel (e.g., if 'smin' is not used). [--].
    !*  zlvl0: Minimum level to keep in the channel (e.g., if storage is not used). [m].
    type abstraction_point_location
        integer :: n = 0
        type(outlet_location) meta
        real, dimension(:), allocatable :: s, smin, fsmin, zlvl0
        type(time_series) sabst
    end type

    !> Type: forms
    !>
    !> Description:
    !>  Contains structures or 'forms' in the model, such
    !>  as landmark locations like streamflow gauge, irrigation demand,
    !>  and lake and reservoir locations.
    type forms
        type(streamflow_gauge_location) stmg
        type(reservoir_outlet_location) rsvr
        type(abstraction_point_location) absp
    end type

    !* fms: Collection of structures in the basin.
    type(forms), save :: fms

    contains

    !> Subroutines to allocate base types.

    subroutine allocate_outlet_location(meta, n, ierr)
        type(outlet_location) meta
        integer n, ierr
        allocate( &
            meta%name(n), meta%y(n), meta%x(n), &
            meta%iy(n), meta%jx(n), &
            meta%rnk(n), stat = ierr)
        if (ierr == 0) then
            meta%name(n) = ''; meta%y(n) = 0.0; meta%x(n) = 0.0
            meta%iy(n) = 0; meta%jx(n) = 0
            meta%rnk(n) = 0
        end if
    end subroutine

    subroutine allocate_time_series(ts, n, ierr)
        type(time_series) ts
        integer n, ierr
        ts%type = ''; ts%units = ''; ts%readmode = ''
        allocate(ts%val(n), stat = ierr)
        if (ierr == 0) ts%val = 0.0
    end subroutine

    subroutine allocate_release_outlet(rls, n, ierr)
        type(release_outlet) rls
        integer n, ierr
        allocate( &
            rls%cfn(n), &
            rls%b1(n), rls%b2(n), rls%b3(n), rls%b4(n), rls%b5(n), &
            rls%b6(n), rls%b7(n), &
            rls%area(n), rls%zlvl0(n), stat = ierr)
        if (ierr == 0) then
            rls%cfn = 0
            rls%b1 = 0.0; rls%b2 = 0.0; rls%b3 = 0.0; rls%b4 = 0.0; rls%b5 = 0.0
            rls%b6 = 0.0; rls%b7 = 0.0
            rls%area = 0.0; rls%zlvl0 = 0.0
        end if
    end subroutine

    !> Subroutines to allocate locations types.

    subroutine allocate_streamflow_gauge_location(stmg, n, ierr)
        type(streamflow_gauge_location) stmg
        integer n, ierr
        stmg%n = n
        call allocate_outlet_location(stmg%meta, n, ierr)
        if (ierr /= 0) return
        allocate(stmg%DA(n), stat = ierr)
        if (ierr /= 0) return
        stmg%DA = 0.0
        call allocate_time_series(stmg%qomeas, n, ierr)
    end subroutine

    subroutine allocate_reservoir_outlet_location(rsvr, n, ierr)
        type(reservoir_outlet_location) rsvr
        integer n, ierr
        rsvr%n = n
        call allocate_outlet_location(rsvr%meta, n, ierr)
        if (ierr /= 0) return
        call allocate_release_outlet(rsvr%rls, n, ierr)
        if (ierr /= 0) return
        call allocate_time_series(rsvr%rlsmeas, n, ierr)
    end subroutine

    subroutine allocate_abstraction_point_location(absp, n, ierr)
        type(abstraction_point_location) absp
        integer n, ierr
        absp%n = n
        call allocate_outlet_location(absp%meta, n, ierr)
        if (ierr /= 0) return
        allocate(absp%s(n), absp%smin(n), absp%fsmin(n), absp%zlvl0(n), stat = ierr)
        if (ierr /= 0) return
        absp%s = 0.0; absp%smin = 0.0; absp%fsmin = 0.0; absp%zlvl0 = 0.0
        call allocate_time_series(absp%sabst, n, ierr)
    end subroutine

end module
