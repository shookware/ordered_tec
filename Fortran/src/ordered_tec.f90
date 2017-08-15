!-----------------------------------------------------------------------
!Module mod_ ordered_tec
!-----------------------------------------------------------------------
module ordered_tec
!use
    use StringiFor
    use Penf, only: R8P, R4P, I4P

    implicit none
    private ! all by default

    type, private :: string_map

        type(string) :: name
        type(string) :: value

    end type string_map

    type, private :: real_pair

        real(R8P) :: first
        real(R8P) :: second

    end type real_pair

    type, private ::  tec_file_base

        private
        type(string) :: filePath
        type(string) :: fileName
        type(string) :: title
        type(string), allocatable :: variables(:)
        integer(I4P) :: fileType
        type(string_map), allocatable :: auxiliary(:)

    end type tec_file_base

    type, private :: tec_zone_base

        private
        real(R8P), public :: solutionTime
        integer(I4P), public :: strandId
        integer(I4P), public :: shape(3)
        integer(I4P), public :: skip(3)
        integer(I4P), public :: begin(3)
        integer(I4P), public :: end(3)
        type(string) :: zoneName
        integer(I4P) :: dimValue
        integer(I4P) :: realMax(3)
        integer(I4P) :: realDim
        type(string_map), allocatable :: auxiliary(:)

    end type tec_zone_base

    type, private :: tec_data_base

        integer(I4P) :: typeValue

    end type tec_data_base

    type, extends(tec_data_base), public :: tec_data

        private
        class(*), allocatable :: data(:, :, :)

      contains

        procedure :: Finalize => Finalize_data

        procedure, private :: MinMax
        procedure, private :: WritePltData => WritePltData_data


    end type tec_data

    interface tec_data

        module procedure :: tec_data_define_r4p_1d
        module procedure :: tec_data_define_r4p_2d
        module procedure :: tec_data_define_r4p_3d
        module procedure :: tec_data_define_r8p_1d
        module procedure :: tec_data_define_r8p_2d
        module procedure :: tec_data_define_r8p_3d
        module procedure :: tec_data_define_i4p_1d
        module procedure :: tec_data_define_i4p_2d
        module procedure :: tec_data_define_i4p_3d
        module procedure :: tec_data_define_expand_2D_r4p
        module procedure :: tec_data_define_expand_1D3D_r4p
        module procedure :: tec_data_define_expand_2D3D_r4p
        module procedure :: tec_data_define_expand_2D_r8p
        module procedure :: tec_data_define_expand_1D3D_r8p
        module procedure :: tec_data_define_expand_2D3D_r8p
        module procedure :: tec_data_define_expand_2D_i4p
        module procedure :: tec_data_define_expand_1D3D_i4p
        module procedure :: tec_data_define_expand_2D3D_i4p

    end interface tec_data

    type, extends(tec_zone_base), public :: tec_zone

        private
        type(tec_data), allocatable :: datas(:)

      contains

        procedure :: GetRealMax
        procedure :: GetRealDim
        procedure :: Finalize => Finalize_zone
                !
        generic :: AddData => AddData_single, &
                          &   AddData_multi
        generic :: AddAuxiliaryData => AddAuxiliaryDataString_zone, &
                                     & AddAuxiliaryDataDouble_zone

        procedure, private :: AddAuxiliaryDataString_zone
        procedure, private :: AddAuxiliaryDataDouble_zone

        procedure, private :: GatherRealSize
        procedure, private :: WritePltPre => WritePltPre_zone
        procedure, private :: WritePltHead => WritePltHead_zone
        procedure, private :: WritePltData => WritePltData_zone
        procedure, private :: AddData_single
        procedure, private :: AddData_multi

    end type tec_zone

    interface  tec_zone

        module procedure :: tec_zone_define

    end interface tec_zone

    type, extends(tec_file_base), public :: tec_file

        type(tec_zone), allocatable, public :: zones(:)

      contains

        procedure :: WritePlt => WritePlt_file
        procedure :: Finalize => Finalize_file

        generic :: AddVar => AddVar_single, &
                        &    AddVar_multi
        generic :: AddZone => AddZone_single, &
                        &     AddZone_multi
        generic :: AddAuxiliaryData => AddAuxiliaryDataString_file, &
                                    &  AddAuxiliaryDataDouble_file
        !
        procedure, private :: AddAuxiliaryDataString_file
        procedure, private :: AddAuxiliaryDataDouble_file
        !
        procedure, private :: WritePltPre => WritePltPre_file
        procedure, private :: WritePltHead => WritePltHead_file
        procedure, private :: WritePltData => WritePltData_file
        procedure, private :: AddVar_single
        procedure, private :: AddVar_multi
        procedure, private :: AddZone_single
        procedure, private :: AddZone_multi

    end type tec_file

    interface tec_file

        module procedure :: tec_file_define

    end interface tec_file

    interface w2f
      module procedure :: w_int32
      module procedure :: w_r4p
      module procedure :: w_r8p
      module procedure :: w_string
    end interface

    contains

    !-----------------------------------------------------------------------
    ! Function tec_file_define
    !-------------------------------------------------------
    function  tec_file_define(name, path, title, fileType) result(this)

        implicit none

        character(len=*), intent(in) :: name
        character(len=*), optional, intent(in) :: path
        character(len=*), optional, intent(in) :: title
        Character(len=*), intent(in), optional :: filetype
        type(tec_file) :: this

        this%fileName = 'null'
        this%filePath = '.'
        this%title    = 'untitle'
        this%fileType = 0

        this%fileName = name
        if(present(path)) this%filePath = path
        if(present(title)) this%title    = title

        if(present(fileType)) then
          select case (fileType)
          case ('FULL', 'full', 'Full')
            this%fileType=0
          case ('GRID', 'grid', 'Grid')
            this%fileType=1
          case ('SOLUTION', 'solution', 'Solution')
            this%fileType=2
          case default
            write(*,*)'The fileType is error, a FULL type file is adopted'
            this%fileType=0
          end select
        endif

    end function tec_file_define

    !-----------------------------------------------------------------------
    ! Function tec_zone_define
    !-------------------------------------------------------
    function  tec_zone_define(name) result(this)

        implicit none

        character(len=*), intent(in), optional :: name
        type(tec_zone) :: this

        if(.not. present(name))then
            this%zoneName='untitled_zone'
        else
            this%zoneName=Name
        endif
        this%strandId = -1
        this%solutionTime = 0.0_R8P
        this%shape = [1, 1, 1]
        this%skip = [1, 1, 1]
        this%begin = [0, 0, 0]
        this%end =[0, 0, 0]

    end function tec_zone_define

    !-----------------------------------------------------------------------
    ! Function tec_zone_define
    !-------------------------------------------------------
    function  tec_data_define_r4p_1d(idata) result(this)

        implicit none

        real(R4P), intent(in) :: idata(:)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=1
        shape(3)=1

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_r4p_1d

    function  tec_data_define_r4p_2d(idata) result(this)

        implicit none

        real(R4P), intent(in) :: idata(:, :)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=size(idata, dim=2)
        shape(3)=1

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_r4p_2d

    function  tec_data_define_r4p_3d(idata) result(this)

        implicit none

        real(R4P), intent(in) :: idata(:, :, :)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=size(idata, dim=2)
        shape(3)=size(idata, dim=3)

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_r4p_3d

    function tec_data_define_expand_2d_r4p(idata, expand, dim) result(this)

        implicit none
        real(R4P), intent(in) :: idata(:)
        character(len=1), intent(in) :: expand
        integer, intent(in) :: dim
        type(tec_data) :: this

        print*, shape(spread(idata, 1, dim))
        print*, shape(spread(idata, 2, dim))
        pause

        select case (expand)
        case ("i")
          this=tec_data(spread(idata, 1, dim))
        case ("j")
          this=tec_data(spread(idata, 2, dim))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_2d_r4p

    function tec_data_define_expand_1d3d_r4p(idata, expand, dim) result(this)

        implicit none
        real(R4P), intent(in) :: idata(:)
        character(len=2), intent(in) :: expand
        integer, intent(in) :: dim(2)
        type(tec_data) :: this

        select case (expand)
        case ("ij")
          this=tec_data(spread(spread(idata, 1, dim(1)), 2, dim(2)))
        case ("ik")
          this=tec_data(spread(spread(idata, 1, dim(1)), 3, dim(2)))
        case ("jk")
          this=tec_data(spread(spread(idata, 2, dim(1)), 3, dim(2)))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_1d3d_r4p

    function tec_data_define_expand_2d3d_r4p(idata, expand, dim) result(this)

        implicit none
        real(R4P), intent(in) :: idata(:, :)
        character(len=1), intent(in) :: expand
        integer, intent(in) :: dim
        type(tec_data) :: this

        select case (expand)
        case ("i")
          this=tec_data(spread(idata, 1, dim))
        case ("j")
          this=tec_data(spread(idata, 2, dim))
        case ("k")
          this=tec_data(spread(idata, 3, dim))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_2d3d_r4p

    function  tec_data_define_r8p_1d(idata) result(this)

        implicit none

        real(R8P), intent(in) :: idata(:)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=1
        shape(3)=1

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_r8p_1d

    function  tec_data_define_r8p_2d(idata) result(this)

        implicit none

        real(R8P), intent(in) :: idata(:, :)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=size(idata, dim=2)
        shape(3)=1

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_r8p_2d

    function  tec_data_define_r8p_3d(idata) result(this)

        implicit none

        real(R8P), intent(in) :: idata(:, :, :)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=size(idata, dim=2)
        shape(3)=size(idata, dim=3)

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_r8p_3d

    function tec_data_define_expand_2d_r8p(idata, expand, dim) result(this)

        implicit none
        real(R8P), intent(in) :: idata(:)
        character(len=1), intent(in) :: expand
        integer, intent(in) :: dim
        type(tec_data) :: this

        print*, shape(spread(idata, 1, dim))
        print*, shape(spread(idata, 2, dim))
        pause

        select case (expand)
        case ("i")
          this=tec_data(spread(idata, 1, dim))
        case ("j")
          this=tec_data(spread(idata, 2, dim))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_2d_R8P

    function tec_data_define_expand_1d3d_R8P(idata, expand, dim) result(this)

        implicit none
        real(R8P), intent(in) :: idata(:)
        character(len=2), intent(in) :: expand
        integer, intent(in) :: dim(2)
        type(tec_data) :: this

        select case (expand)
        case ("ij")
          this=tec_data(spread(spread(idata, 1, dim(1)), 2, dim(2)))
        case ("ik")
          this=tec_data(spread(spread(idata, 1, dim(1)), 3, dim(2)))
        case ("jk")
          this=tec_data(spread(spread(idata, 2, dim(1)), 3, dim(2)))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_1d3d_R8P

    function tec_data_define_expand_2d3d_R8P(idata, expand, dim) result(this)

        implicit none
        real(R8P), intent(in) :: idata(:, :)
        character(len=1), intent(in) :: expand
        integer, intent(in) :: dim
        type(tec_data) :: this

        select case (expand)
        case ("i")
          this=tec_data(spread(idata, 1, dim))
        case ("j")
          this=tec_data(spread(idata, 2, dim))
        case ("k")
          this=tec_data(spread(idata, 3, dim))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_2d3d_R8P

    function  tec_data_define_i4p_1d(idata) result(this)

        implicit none

        integer(I4P), intent(in) :: idata(:)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=1
        shape(3)=1

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_i4p_1d

    function  tec_data_define_i4p_2d(idata) result(this)

        implicit none

        integer(I4P), intent(in) :: idata(:, :)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=size(idata, dim=2)
        shape(3)=1

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_i4p_2d

    function  tec_data_define_i4p_3d(idata) result(this)

        implicit none

        integer(I4P), intent(in) :: idata(:, :, :)
        type(tec_data) :: this
        integer(I4P) :: shape(3)
        integer(I4P) :: i

        shape(1)=size(idata, dim=1)
        shape(2)=size(idata, dim=2)
        shape(3)=size(idata, dim=3)

        allocate(this%data, source=reshape(idata, shape))
        this%typeValue = 1

    end function tec_data_define_i4p_3d

    function tec_data_define_expand_2d_i4p(idata, expand, dim) result(this)

        implicit none
        integer(I4P), intent(in) :: idata(:)
        character(len=1), intent(in) :: expand
        integer, intent(in) :: dim
        type(tec_data) :: this

        print*, shape(spread(idata, 1, dim))
        print*, shape(spread(idata, 2, dim))
        pause

        select case (expand)
        case ("i")
          this=tec_data(spread(idata, 1, dim))
        case ("j")
          this=tec_data(spread(idata, 2, dim))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_2d_i4p

    function tec_data_define_expand_1d3d_i4p(idata, expand, dim) result(this)

        implicit none
        integer(I4P), intent(in) :: idata(:)
        character(len=2), intent(in) :: expand
        integer, intent(in) :: dim(2)
        type(tec_data) :: this

        select case (expand)
        case ("ij")
          this=tec_data(spread(spread(idata, 1, dim(1)), 2, dim(2)))
        case ("ik")
          this=tec_data(spread(spread(idata, 1, dim(1)), 3, dim(2)))
        case ("jk")
          this=tec_data(spread(spread(idata, 2, dim(1)), 3, dim(2)))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_1d3d_i4p

    function tec_data_define_expand_2d3d_i4p(idata, expand, dim) result(this)

        implicit none
        integer(I4P), intent(in) :: idata(:, :)
        character(len=1), intent(in) :: expand
        integer, intent(in) :: dim
        type(tec_data) :: this

        select case (expand)
        case ("i")
          this=tec_data(spread(idata, 1, dim))
        case ("j")
          this=tec_data(spread(idata, 2, dim))
        case ("k")
          this=tec_data(spread(idata, 3, dim))
        case default
          stop "Expand flag is error"
        end select

    end function tec_data_define_expand_2d3d_i4p

    subroutine w_int32(a, iounit)

      implicit none
      integer(I4P), intent(in) :: a
      integer(I4P), intent(in) :: iounit

      write(iounit)a

    end subroutine w_int32

    subroutine w_r4p(a, iounit)

      implicit none
      real(R4P), intent(in) :: a
      integer(I4P), intent(in) :: iounit

      write(iounit)a

    end subroutine w_r4p

    subroutine w_r8p(a, iounit)

      implicit none
      real(R8P), intent(in) :: a
      integer(I4P), intent(in) :: iounit

      write(iounit)a

    end subroutine w_r8p

    subroutine w_string(a, iounit)

      implicit none
      type(string), intent(in) :: a
      integer(I4P), intent(in) :: iounit
      character(len=:), allocatable :: stringChar
      integer(I4P) :: n, i

      n = a%len_trim()
      allocate(stringChar, source=a//'')
      do i=1, n
        call w_int32(ichar(stringChar(i:i), kind=kind(1)), iounit)
      enddo
      call w_int32(0, iounit)

    end subroutine w_string

    subroutine AddAuxiliaryDataString_generic(Auxiliary, name, value)

      implicit none
      type(string_map), allocatable, intent(inout) :: Auxiliary(:)
      type(string), intent(in) :: name
      type(string), intent(in) :: value
      type(string_map), allocatable :: Temp(:)
      integer(I4P) :: n

      if(.not. allocated(Auxiliary)) then
        allocate(Auxiliary(1))
        Auxiliary(1) = string_map(name, value)
      else
        n = size(Auxiliary, dim=1)
        allocate(Temp(n+1))
        Temp(1:n) = Auxiliary(1:n)
        Temp(n+1) = string_map(name, value)
        call move_alloc (Temp, Auxiliary)
      endif

    end subroutine AddAuxiliaryDataString_generic

    subroutine AddAuxiliaryDataDouble_generic(Auxiliary, name, num)

      implicit none
      type(string_map), allocatable, intent(inout) :: Auxiliary(:)
      type(string), intent(in) :: name
      real(R8P), intent(in) :: num
      type(string_map), allocatable :: Temp(:)
      integer(I4P) :: n
      type(string) :: value

      value = num
      if(.not. allocated(Auxiliary)) then
        allocate(Auxiliary(1))
        Auxiliary(1) = string_map(name, value)
      else
        n = size(Auxiliary, dim=1)
        allocate(Temp(n+1))
        Temp(1:n) = Auxiliary(1:n)
        Temp(n+1) = string_map(name, value)
        call move_alloc (Temp, Auxiliary)
      endif

    end subroutine AddAuxiliaryDataDouble_generic

    subroutine AddAuxiliaryDataString_file(this, name, value)

      implicit none
      class(tec_file), intent(inout) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: value
      type(string) :: valStr, nameStr

      nameStr= name
      valStr = value

      call AddAuxiliaryDataString_generic(this%Auxiliary, nameStr, valStr)

    end subroutine AddAuxiliaryDataString_file

    subroutine AddAuxiliaryDataDouble_file(this, name, value)

      implicit none
      class(tec_file), intent(inout) :: this
      character(len=*), intent(in) :: name
      real(R8P), intent(in) :: value
      type(string) :: nameStr

      nameStr=name

      call AddAuxiliaryDataDouble_generic(this%Auxiliary, nameStr, value)

    end subroutine AddAuxiliaryDataDouble_file

    subroutine AddAuxiliaryDataString_zone(this, name, value)

      implicit none
      class(tec_zone), intent(inout) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: value
      type(string) :: valStr
      type(string) :: nameStr

      nameStr=name
      valStr = value
      call AddAuxiliaryDataString_generic(this%Auxiliary, nameStr, valStr)

    end subroutine AddAuxiliaryDataString_zone

    subroutine AddAuxiliaryDataDouble_zone(this, name, value)

      implicit none
      class(tec_zone), intent(inout) :: this
      character(len=*), intent(in) :: name
      real(R8P), intent(in) :: value
      type(string) :: nameStr

      call AddAuxiliaryDataDouble_generic(this%Auxiliary, nameStr, value)

    end subroutine AddAuxiliaryDataDouble_zone

    subroutine AddVar_single(this, name)

      implicit none
      class(tec_file), intent(inout) :: this
      character(len=*), intent(in) :: name
      type(string), allocatable :: Temp(:)
      integer(I4P) :: n, i

      if(.not. allocated(this%variables)) then
        allocate(this%variables(1))
        this%variables(1) = name
      else
        n = size(this%variables, dim=1)
        allocate(Temp(n+1))
        do i=1, n
          Temp(i) = this%variables(i)
        enddo
        Temp(n+1) = name
        n=n+1
        call move_alloc (temp, this%variables)
      endif

    end subroutine AddVar_single

    subroutine AddVar_multi(this, name)

      implicit none
      class(tec_file), intent(inout) :: this
      character(len=*), intent(in) :: name(:)
      type(string), allocatable :: Temp(:)
      integer(I4P) :: n, i
      integer(I4P) :: num

      num = size(name, dim=1)
      if(.not. allocated(this%variables)) then
        allocate(this%variables(num))
        this%variables(1:num) = name(1:num)
      else
        n = size(this%variables, dim=1)
        allocate(Temp(n+num))
        do i=1, n
          Temp(i) = this%variables(i)
        enddo
        Temp(n+1:n+num) = name(1:num)
        call move_alloc (temp, this%variables)
      endif

    end subroutine AddVar_multi

    subroutine AddZone_single(this, zone)

      implicit none
      class(tec_file), intent(inout) :: this
      type(tec_zone), intent(in) :: zone
      type(tec_zone), allocatable :: Temp(:)
      integer(I4P) :: n, i

      if(.not. allocated(this%zones)) then
        allocate(this%zones(1))
        this%zones(1) = zone
      else
        n = size(this%zones, dim=1)
        allocate(Temp(n+1))
        do i=1, n
          Temp(i) = this%zones(i)
        enddo
        Temp(n+1) = zone
        call move_alloc (temp, this%zones)
      endif

    end subroutine AddZone_single

    subroutine AddZone_multi(this, zone)

      implicit none
      class(tec_file), intent(inout) :: this
      type(tec_zone), intent(in) :: zone(:)
      type(tec_zone), allocatable :: Temp(:)
      integer(I4P) :: n, i
      integer(I4P):: num

      num=size(zone, dim=1)
      if(.not. allocated(this%zones)) then
        allocate(this%zones(num))
        this%zones(1:num) = zone(1:num)
      else
        n = size(this%zones, dim=1)
        allocate(Temp(n+num))
        do i=1, n
          Temp(i) = this%zones(i)
        enddo
        Temp(n+1:n+num) = zone(1:num)
        call move_alloc (temp, this%zones)
      endif

    end subroutine AddZone_multi

    subroutine AddData_single(this, data)

      implicit none
      class(tec_zone), intent(inout) :: this
      type(tec_data), intent(in) :: data
      type(tec_data), allocatable :: Temp(:)
      integer(I4P) :: n, i


      if(.not. allocated(this%datas)) then
        if( any(this%shape /= shape(data%data)))then
          write(*,*)'ZONE('//This%zoneName//'): The shape of the DATA &
          &     into the ZONE is not equal to the pre-assignment or is not &
          &     pre-assigned.The shape of the DATA is adopted.'
          write(*,*)'The DATA number is', 1
          this%shape=shape(data%data)
        endif
        allocate(this%datas(1))
        this%datas(1) = data
      else
        n = size(this%datas, dim=1)
        if( any(this%shape /= shape(data%data))) then
          write(*,*)'ZONE('//This%zoneName//'): The shape of the DATA into the &
          &     ZONE is not equal to the shape of other DATA. Please Check it.'
          write(*,*)'The DATA number is', n+1
          stop
        endif
        allocate(Temp(n+1))
        do i=1, n
          Temp(i) = this%datas(i)
        enddo
        Temp(n+1) = data
        n=n+1
        call move_alloc (temp, this%datas)
      endif

    end subroutine AddData_single

    subroutine AddData_multi(this, data)

      implicit none
      class(tec_zone), intent(inout) :: this
      type(tec_data), intent(in) :: data(:)
      type(tec_data), allocatable :: Temp(:)
      integer(I4P) :: n, i
      integer(I4P) :: num

      num = size(data, dim=1)
      if(.not. allocated(this%datas)) then
        if( any(this%shape /= shape(data(1)%data)))then
          write(*,*)'ZONE('//This%zoneName//'): The shape of the DATA into the &
          &     ZONE is not equal to the pre-assignment or is not pre-assigned.&
          &     The shape of the DATA is adopted.'
          write(*,*)'The DATA number is', 1
          this%shape=shape(data(1)%data)
        endif
        do i=2, num
          if( any(this%shape /= shape(data(i)%data))) then
            write(*,*)'ZONE('//This%zoneName//'): The shape of the DATA into the &
            &     ZONE is not equal to the shape of other DATA. Please Check it.'
            write(*,*)'The DATA number is', i
            stop
          endif
        enddo
        allocate(this%datas(num))
        this%datas(1:num) = data(1:num)
      else
        n = size(this%datas, dim=1)
        do i=1, num
          if( any(this%shape /= shape(data(i)%data))) then
            write(*,*)'ZONE('//This%zoneName//'): The shape of the DATA into the &
            &     ZONE is not equal to the shape of other DATA. Please Check it.'
            write(*,*)'The DATA number is', i+n
            stop
          endif
        enddo
        allocate(Temp(n+num))
        do i=1, n
          Temp(i) = this%datas(i)
        enddo
        Temp(n+1:n+num) = data(1:num)
        call move_alloc (temp, this%datas)
      endif

    end subroutine AddData_multi
    !-----------------------------------------------------------------------
    !Subroutine GatherRealSize
    !-----------------------------------------------------------------------
    subroutine GatherRealSize(this)

      implicit none
      class(tec_zone), intent(inout) :: this
      integer(I4P) :: i
      character, parameter :: INDEX(3)=['I', 'J', 'K']

      do i=1, 3
        if(this%shape(i)==0) then
          write(*,*)'Zone '//this%zoneName//':'//INDEX(i)//' Max cannot be zero'
          stop
        endif
        if(this%skip(i)==0) then
          write(*,*)'Zone '//this%zoneName//':'//INDEX(i)//' Max cannot be zero'
          stop
        endif
        if(this%begin(i)+this%end(i)>=this%shape(i)) then
          write(*,*)'Zone '//this%zoneName//': sum of '//INDEX(i)// &
        & ' Begin and '//INDEX(i)//' End is not smaller than '//INDEX(i)//' Max'
          stop
        endif
      enddo

      this%dimValue = 3
      do i=3, 1, -1
          if(this%shape(i)==1) this%dimValue=this%dimValue-1
      enddo

      this%realDim = 3

      associate (realMax=> this%realMax, &
               & shape => this%shape, &
               & begin    => this%begin, &
               & end      => this%end, &
               & skip     => this%skip )
        do i=3, 1, -1
          realMax(i)=(shape(i)-begin(i)-end(i))/skip(i)

          if(mod((shape(i)-begin(i)-end(i)), skip(i))) &
            & realMax(i)=realMax(i)+1
          if(realMax(i)==1) this%realDim=this%realDim-1
        enddo
      end associate

    end subroutine GatherRealSize

    !-----------------------------------------------------------------------
    !Function GetRealSize
    !-----------------------------------------------------------------------
    function  GetRealMax(this, name) result(ans)

      implicit none
      class(tec_zone), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer(I4P) :: ans(3)

      call this%GatherRealSize()
      select case (name)
      case ('realmax')
        ans = this%realmax
      case default
        stop 'get_real_size : size code wrong'
      end select

    end function GetRealMax

    function  GetRealDim(this, name) result(ans)

      implicit none
      class(tec_zone), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer(I4P) :: ans

      call this%GatherRealSize()
      select case (name)
      case ('realdim')
        ans = this%realDim
      case default
        stop 'get_real_size : size code wrong'
      end select

    end function GetRealDim

    function MinMax (this) result(ans)

      implicit none
      class(tec_data), intent(in) :: this
      type(real_pair) :: ans

      select type (data => this%data)
      type is (real(R4P))
        call get_minmax_r4p(data, ans)
      type is (real(R8P))
        call get_minmax_R8P(data, ans)
      type is (integer(I4P))
        call get_minmax_int32(data,ans)
      class default
        stop 'Please input a correct data type!'
      end select

    end function MinMax

    subroutine get_minmax_r4p(data, ans)

      implicit none
      real(R4P) :: data(:, :, :)
      type(real_pair) :: ans

      ans%first = dble(minval(data))
      ans%second = dble(maxval(data))

    end subroutine get_minmax_r4p

    subroutine get_minmax_R8P(data, ans)

      implicit none
      real(R8P) :: data(:, :, :)
      type(real_pair) :: ans

      ans%first = dble(minval(data))
      ans%second = dble(maxval(data))

    end subroutine get_minmax_R8P

    subroutine get_minmax_int32(data, ans)

      implicit none
      integer(I4P) :: data(:, :, :)
      type(real_pair) :: ans

      ans%first = dble(minval(data))
      ans%second = dble(maxval(data))

    end subroutine get_minmax_int32

    subroutine WritePlt_file(this)

      implicit none
      class(tec_file), intent(inout) :: this
      integer(I4P) :: iounit, ios
      real(R4P) :: rDNum

      call this%WritePltPre()
      call random_seed()
      call random_number(rDnum)
      iounit = int(rDNum*1000.0d0)
      open(newunit=iounit, file=this%filePath//'/'//this%fileName//'', &
            &   access='STREAM', iostat=ios)
      if (ios /= 0) then
        write(*,*)"Error opening file "//this%fileName//''
        stop
      endif
      ! Header section
      call this%WritePltHead(iounit)
      ! EOHMARKER, value=357.0
      call w2f(357.0_R4P, iounit)
      ! DATA section
      call this%WritePltData(iounit)
      ! close file
      close(iounit)

    end subroutine WritePlt_file

    !> Pre-Write Tecplot file.
    subroutine WritePltPre_file(this)

        implicit none
        class(tec_file), intent(inout) :: this
        integer(I4P) :: i

        if(.not. allocated(this%variables)) then
          write(*,*)"File "//this%fileName//":Variables is empty"
          stop
        endif
        if(.not. allocated(this%zones))then
          write(*,*)"File "//this%fileName//":Zones is empty"
          stop
        endif
        do i=1, size(this%zones, dim=1)
          call this%zones(i)%WritePltPre(this)
        enddo

    end subroutine WritePltPre_file

    !> Pre-Write Tecplot file.
    subroutine WritePltPre_zone(this, thisfile)

        implicit none
        class(tec_zone), intent(inout) :: this
        class(tec_file), intent(in) :: thisfile
        integer(I4P) :: i

        call this%GatherRealSize()
        if(.not. allocated(this%datas)) then
          write(*,*)"Zone "//this%zoneName//":Data is empty"
          stop
        endif
        if(size(this%datas, dim=1) /= size(thisfile%variables, dim=1))then
          write(*,*)"Zone "//this%zoneName//":the size of data is not equal to &
            &       the size of variables of the file"
          stop
        endif
        do i=1, size(this%datas, dim=1)
          if((.not. allocated(this%datas(i)%data))) then
            write(*, *)"Zone "//this%zoneName//": Variable "// &
          &          thisfile%variables(i)//" has a problem"
            stop
          endif
        enddo

    end subroutine WritePltPre_zone

    !TODO:CHECK_TEC_DATA for its shape.

    !> Wite Tecplot file header.
    subroutine WritePltHead_file(this, iounit)

        implicit none
        class(tec_file), intent(in) :: this
        integer(I4P), intent(in) :: iounit
        integer(I4P) :: varNum
        integer(I4P) :: i

        ! Head Section
        ! version number
        !8 Bytes, exact characters "#!TDV112". Version number follows the "V"
        !and consumes the next 3 characters (for example: "V75", "V101")
        write(iounit)'#!TDV112'
        ! integer(I4P) value of 1
        ! This is used to determine the byte order of the reader, relative to the writer
        call w2f(1, iounit)
        ! Title and variable names
        call w2f(this%fileType, iounit) !FileType 0 = FULL, 1 = GRID, 2 = SOLUTION
        call w2f(this%title, iounit) ! The title
        varNum=size(this%variables, dim=1)
        call w2f(varNum, iounit) ! Number of the Variables
        do i=1, varNum
          call w2f(this%variables(i), iounit) !
        enddo

        !!  Head of Zones
        do i=1, size(this%zones, dim=1)
          call this%zones(i)%WritePltHead(iounit)
        enddo

        ! DataSet Auxiliary Data
        do i=1, size(this%Auxiliary, dim=1)
          call w2f(799.0_R4P, iounit)
          call w2f(this%Auxiliary(i)%name, iounit)
          call w2f(0, iounit)
          call w2f(this%Auxiliary(i)%value, iounit)
        enddo

    end subroutine WritePltHead_file

    !> Wite Tecplot zone header.
    subroutine WritePltHead_zone(this, iounit, thisfile)

        implicit none
        class(tec_zone), intent(in) :: this
        integer(I4P), intent(in) :: iounit
        type(tec_file), intent(in), optional :: thisfile
        integer(I4P) :: i

        call w2f(299.0_R4P, iounit) !zone maker value=299.0
        call w2f(this%zoneName, iounit) ! zone name
        call w2f(-1, iounit) !ParentZone
        call w2f(this%strandID, iounit) ! StrandID
        call w2f(this%solutionTime, iounit) !SolutionTime
        call w2f(-1, iounit) ! not used
        call w2f(0, iounit) ! ZoneType 0=ORDERED
        call w2f(0, iounit) ! specify Var location. 0 = Do not secify, all data
                            ! is located at the nodes.

        ! the var location: 0=node, 1=cell center. not used when specify=0
        !do i=1, size(thisfile%variables, dim=1)
        !  call w2f(0, iounit) ! valueLocations
        !enddo

        !Are raw local 1-to-1 face neighbors supplied? (0=FALSE 1=TRUE) ORDERED
        !and FELINESEG zones must specify 0
        call w2f(0, iounit)
        !Number of miscellaneous user-defined face neighbor connections (value >= 0)
        call w2f(0, iounit)
        call w2f(this%realMax(1), iounit) !IMAX
        call w2f(this%realMax(2), iounit) !JMAX
        call w2f(this%realMax(3), iounit) !KMAX
        call w2f(0, iounit) ! no auxiliary name / value pair to follow TODO:这里需要测试顺序

    end subroutine WritePltHead_zone

    subroutine WritePltData_file(this, iounit)

      implicit none
      class(tec_file), intent(in) :: this
      integer(I4P), intent(in) :: iounit
      integer(I4P) :: i, n

      n=size(this%zones, dim=1)
      do i=1, n
        call this%zones(i)%WritePltData(iounit)
      enddo

    end subroutine WritePltData_file

    subroutine WritePltData_zone(this, iounit)

      implicit none
      class(tec_zone), intent(in) :: this
      integer(I4P), intent(in) :: iounit
      integer(I4P) :: i, n
      type(real_pair) :: mm
      integer(I4P) :: s(3), e(3)

      call w2f(299.0_R4P, iounit)
      n=size(this%datas, dim=1)
      do i=1, n
        call w2f(this%datas(i)%typevalue, iounit)
      enddo
      call w2f(0, iounit) ! has passive variables: 0 = no
      call w2f(0, iounit) ! has variables sharing: 0 = no
      call w2f(-1, iounit) ! Zero based zone number to share connectivity list with (-1 = no sharing)
      do i=1, n
        mm=this%datas(i)%minmax()
        call w2f(mm%first, iounit)
        call w2f(mm%second, iounit)
      enddo

      associate(begin=>this%begin, &
        &       end => this%end, &
        &       skip => this%skip, &
        &       datas=>this%datas)
        do i=1, n
          s=begin
          e=end
          where (s == 0) s = 1
          where (e == 0) e = this%shape
          call datas(i)%WritePltData(s, e, skip, iounit)
        enddo
      end associate

    end subroutine WritePltData_zone

    subroutine WritePltData_data(this, s, e, skip, iounit)

      implicit none
      class(tec_data), intent(in) :: this
      integer(I4P), intent(in) :: s(3), e(3), skip(3)
      integer(I4P), intent(in) :: iounit
      integer :: i, j, k

      select type (data => this%data)
      type is (real(R4P))
        write(iounit)(((data(i, j, k), i=s(1), e(1), skip(1)), &
                            &          j=s(2), e(2), skip(2)), &
                            &          k=s(3), e(3), skip(3))

      type is (real(R8P))
        write(iounit)(((data(i, j, k), i=s(1), e(1), skip(1)), &
                            &          j=s(2), e(2), skip(2)), &
                            &          k=s(3), e(3), skip(3))
      type is (integer(I4P))
        write(iounit)(((data(i, j, k), i=s(1), e(1), skip(1)), &
                            &          j=s(2), e(2), skip(2)), &
                            &          k=s(3), e(3), skip(3))
      class default
        stop "Data Type is not defined"
      end select

    end subroutine WritePltData_data

    subroutine Finalize_data(this)

      implicit none
      class(tec_data), intent(inout) :: this

      if(allocated(this%data))deallocate(this%data)
      this%typeValue=0

    end subroutine Finalize_data

    subroutine Finalize_zone(this)

      implicit none
      class(tec_zone), intent(inout) :: this
      integer :: i

      do i = 1, size(this%datas, dim=1)
        call this%datas(i)%Finalize()
      enddo
      this%strandId = -1
      this%solutionTime = 0.0_R8P
      this%shape = [0, 0, 0]
      this%skip = [0, 0, 0]
      this%begin = [0, 0, 0]
      this%end =[0, 0, 0]
      this%zoneName=''
      this%dimValue=0
      this%realMax=[0, 0, 0]
      this%realDim=0
      if(allocated(this%datas    )) deallocate(this%datas)
      if(allocated(this%auxiliary)) deallocate(this%auxiliary)

    end subroutine Finalize_zone

    subroutine Finalize_file(this)

      implicit none
      class(tec_file), intent(inout) :: this
      integer :: i

      do i=1, size(this%zones, dim=1)
        call this%zones(i)%Finalize()
      enddo
      this%filePath=''
      this%fileName=''
      this%title=''
      this%filetype=-1
      if(allocated(this%variables)) deallocate(this%variables)
      if(allocated(this%auxiliary)) deallocate(this%auxiliary)

    end subroutine Finalize_file

end module ordered_tec

! !-----------------------------------------------------------------------
! !Main program test
! !-----------------------------------------------------------------------
! program test
! use ordered_tec
! use penf
! implicit none
!
! ! contains
! type(tec_file) :: tecfile, tecflow
! real :: x(5)=[1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
! real :: y(6)=[2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0, 7.0d0]
! real :: z(7)=[3.0d0, 4.0d0, 5.0d0, 6.0d0, 7.0d0, 8.0d0, 9.0d0]
!
! tecfile=tec_file(name='test.plt',path='.',title='TestFile', filetype='GRID')
! call tecfile%addVar(['x','y'])
! call tecfile%addVar('z')
! call tecfile%AddAuxiliaryData('Re', '1.0e7')
! call tecfile%AddAuxiliaryData('Ma', 7.0d0)
! call tecfile%AddZone(tec_zone())
! tecfile%zones(1)%shape=[5,6,7]
! call tecfile%zones(1)%AddData(tec_data(x, expand='jk', dim=[6, 7]))
! call tecfile%zones(1)%AddData([tec_data(y, expand='ik', dim=[5, 7]), &
!                                tec_data(z, expand='ij', dim=[5, 6])])
! call tecfile%WritePlt()
! call tecfile%finalize()
!
! tecfile=tec_file(name='flow.plt',path='.',title='TestFile', filetype='SOLUTION')
! call tecfile%addVar('vel')
! call tecfile%AddZone(tec_zone())
! tecfile%zones(1)%shape=[5,6,7]
! call tecfile%zones(1)%AddData(tec_data(x, expand='jk', dim=[6, 7]))
! call tecfile%WritePlt()
! call tecfile%finalize()
!
! end program test
