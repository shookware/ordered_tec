!-----------------------------------------------------------------------
!Module plot3d_loade
!-----------------------------------------------------------------------
module plot3d_loader
!use
  use ordered_tec
  use stringifor
  implicit none
  private ! all by default

  type, private :: grid_t

    type(tec_data), allocatable :: x(:,:)
    type(string), allocatable :: coordName(:)

  end type grid_t

  type, private :: flow_t

    integer, allocatable :: ln(:)
    type(tec_data), allocatable :: q(:, :)
    type(string), allocatable :: varName(:)

  end type flow_t

  type, public :: plot3d_t

    integer :: blockNum
    integer, allocatable :: in(:, :)

    type(string) :: gridFile
    type(string) :: flowFile
    type(string) :: nmlFile
    type(string), allocatable :: zoneNames(:)

    type(string) :: accessType
    type(string) :: formType
    logical :: isBlock
    logical :: isWhole
    logical :: isFormatted
    logical :: isReal8
    logical :: isQ
    integer :: rank

    type(grid_t) :: grid
    type(flow_t) :: flow

  contains

    procedure, private :: GridLoader => Plot3dGridLoader
    procedure, private :: FlowLoader => Plot3dFlowLoader
    procedure, private :: BlockNumLoader
    procedure, private :: LoadGridArray_r8p
    procedure, private :: LoadGridArray_r4p

  end type plot3d_t

  interface ArrayLoader
    module procedure ArrayLoader_r8p
    module procedure ArrayLoader_r4p
  end interface ArrayLoader

  interface plot3d_t
    module procedure :: plot3d_con

  end interface

  contains

    !> Get command line to initialize the property of the plot3d object
    function plot3d_con() result(plot3d)

      use flap
      implicit none
      type(plot3d_t) :: plot3d
      type(command_line_interface) :: cli
      character(len=99) :: GridName
      character(len=99) :: flowName
      type(string) :: flowname_
      type(string) :: ext
      character(len=99) :: formType
      character(len=99) :: blockType
      character(len=99) :: solutionType
      character(len=99) :: realType
      character(len=99) :: varName
      type(string) :: varname_
      character(len=99) :: corName
      character(len=99) :: dimNum
      type(string) :: corname_

      integer :: error

      call cli%init(progname='plot3d_to_tec', version='1.0', help='null', &
                   & description='convert plot3d files to tecplot', &
                   &  authors='Liu Jianxin@tju', examples=[ &
                   &  'plot3d_to_tec -g grid.x', &
                   &  'plot3d_to_tec -g grid.x -f flow.bin', &
                   &  'plot3d_to_tec -h'], &
                   &   license='MIT')
      call cli%add(switch='--grid', switch_ab='-g', help='grid name', &
                 & required=.true., act='store')
      call cli%add(switch='--flow', switch_ab='-f', help='flow name', &
                 & required=.false., act='store', def='null')
      call cli%add(switch='--form', switch_ab='-F', help='formatted/unformatted/binary', &
                 & def='default')
      call cli%add(switch='--block', switch_ab='-b', help='blk/nblk', required=.false., def='default')
      call cli%add(switch='--flowtype', switch_ab='-q', help='Q/function solution file', required=.false., def='default')
      call cli%add(switch='--real', switch_ab='-r', help='8/4: real(8)/real(4)', required=.false., def='8')
      call cli%add(switch='--var', switch_ab='-V', help='var name', required=.false., def='null')
      call cli%add(switch='--cor', switch_ab='-c', help='coord name', required=.false., def='null')
      call cli%add(switch='--dim', switch_ab='-d', help='dimension number', required=.false., def='3')

      call cli%get(switch='-g',val=GridName, error=error) ;       if (error/=0) stop
      call cli%get(switch='-f',val=flowName, error=error) ; if (error/=0) stop
      call cli%get(switch='-F',val=formType, error=error) ; if (error/=0) stop
      call cli%get(switch='-b',val=blockType, error=error) ; if (error/=0) stop
      call cli%get(switch='-q',val=solutionType, error=error) ; if (error/=0) stop
      call cli%get(switch='-r',val=RealType, error=error) ; if (error/=0) stop
      call cli%get(switch='-V',val=varName, error=error) ; if (error/=0) stop
      call cli%get(switch='-c',val=corName, error=error) ; if (error/=0) stop
      call cli%get(switch='-d',val=dimNum, error=error) ; if (error/=0) stop
      plot3d%gridfile=trim(gridname)
      plot3d%flowfile=trim(flowname)
      flowname_=trim(flowname)
      ext=flowname_%extension()

      select case (trim(formType))
          case ('formatted')
            plot3d%formType='formatted'
            plot3d%accessType='sequential'
          case ('unformatted')
            plot3d%formType='unformatted'
            plot3d%accessType='sequential'
          case ('binary')
            plot3d%formType='unformatted'
            plot3d%accessType='stream'
          case ('default')
            select case (ext//'')
                case ('bin')
                    plot3d%formType='unformatted'
                    plot3d%accessType='stream'
                case ('dat')
                    plot3d%formType='unformatted'
                    plot3d%accessType='sequential'
                case default
                    plot3d%formType='unformatted'
                    plot3d%accessType='stream'
            end select
          case default
            write(*,*)'Error: The file format is error!', trim(formtype)
            write(*, *)'binary is adopted'
            plot3d%formType='unformatted'
            plot3d%accessType='sequential'            
      end select
      select case (trim(blockType))
          case ('blk')
            plot3d%isBlock=.True.
          case ('nblk')
            plot3d%isBlock=.False.
            plot3d%blocknum=1
          case ('default')
            plot3d%isBlock=.True.
            plot3d%blocknum=1
          case default
            write(*,*)'Error: The block type is error!', trim(blocktype)
            write(*,*)'default value (single block type is adopted)'
            plot3d%isBlock=.False.
            plot3d%blocknum=1
      end select
      select case (trim(solutionType))
          case ('default')
            select case (ext//'')
                case ('q', 'Q')
                    plot3d%isQ=.True.
                case ('bin', 'dat')
                    plot3d%isQ=.False.
                case default
                    plot3d%isQ=.False.
            end select
          case ('q', 'Q')
            plot3d%isQ=.True.
          case ('fun', 'FUN', 'Fun')
            plot3d%isQ=.False.
          case default
            write(*,*)'Error: The solution type is error!', trim(solutionType)
            write(*,*)'default value (function) is adopted!'
            plot3d%isQ=.False.
      end select
      select case (trim(realType))
          case ('8')
            plot3d%isReal8=.True.
          case ('4')
            plot3d%isReal8=.False.
          case default
            write(*,*)'Error: The data type is error!real', trim(realType)
            write(*,*)'real8 is adopted'
            plot3d%isReal8=.True.
      end select
      select case (trim(dimNum))
          case ('1', '1d', '1D')
            plot3d%rank=1
          case ('2', '2d', '2D')
            plot3d%rank=2
          case ('3', '3d', '3D')
            plot3d%rank=3
          case default
            write(*,*)'Error: The dimension is error!', trim(dimNum)
            write(*,*)'3D is adopted'
            plot3d%rank=3
      end select


      varname_=trim(varname)
      if(varname_=='null')then
        if(plot3d%isQ) then
            varname_='<greek>r</greek>, <greek>r</greek>U, <greek>r</greek>V, &
                      <greek>r</greek>W, E'
        else
            varname_='<greek>r</greek>, U, V, W, T'
        end if
      endif
      call varname_%split(tokens=plot3d%flow%varName, sep=', ')

      corname_=trim(corname)
      if(corname_=='null')then
        select case (plot3d%rank)
            case (3)
                corname_='x, y, z'
            case (2)
                corname_='x, y'
            case (1)
                corname_='x'
        end select
      endif
      call corname_%split(tokens=plot3d%grid%coordName, sep=', ')

      call plot3d%GridLoader()

      if(trim(flowName)/='null')then
        call plot3d%FlowLoader()
      end if

    end function plot3d_con

    subroutine BlockNumLoader(this, iounit)

      implicit none
      class(plot3d_t), intent(inout) :: this
      integer, intent(in) :: iounit

      this%blockNum=1
      if(this%isFormatted) then
        read(iounit, *)this%blocknum
      else
        read(iounit)this%blocknum
      endif

    end subroutine BlockNumLoader

    !> Grid
    subroutine Plot3dGridLoader(this)

      implicit none
      class(plot3d_t), intent(inout) :: this
      integer :: iounit, ios
      integer :: i, j

      iounit=99

      associate(gridFile   => this%gridFile, &
        &       accessType => this%accessType, &
        &       formType   => this%formType, &
        &       isBlock    => this%isBlock, &
        &       blockNum   => this%blockNum, &
        &       isFormatted => this%isFormatted, &
        &       isWhole    => this%isWhole, &
        &       isReal8    => this%isReal8, &
        &       rank       => this%rank)
        open(newunit=iounit, file=gridFile//'', &
        &    access=accessType//'', form=formType//'', iostat=ios, &
        &    status="old", action="read")
        if (ios /= 0) stop "Error opening file Plot3dGird"
        if(isBlock) call this%BlockNumLoader(iounit)
        allocate(this%in(blockNum, 3))
        this%in=1
        if(isFormatted) then
          read(iounit, *)((this%in(i, j), j=1, rank), i=1, blockNum)
        elseif(this%formType=='unformatted') then
          read(iounit)((this%in(i, j), j=1, rank), i=1, blockNum)
        endif
        if(isReal8) then
          call this%LoadGridArray_r8p(iounit)
        else
          call this%LoadGridArray_r4p(iounit)
        endif
        close(iounit)
      end associate

    end subroutine Plot3dGridLoader

    !> Flow
    subroutine Plot3dFlowLoader(this)

      implicit none
      class(plot3d_t), intent(inout) :: this
      integer :: iounit, ios
      integer :: i, j, itmp

      iounit=100

      associate(flowFile   => this%flowFile, &
        &       accessType => this%accessType, &
        &       formType   => this%formType, &
        &       isBlock    => this%isBlock, &
        &       blockNum   => this%blockNum, &
        &       isFormatted => this%isFormatted, &
        &       isWhole    => this%isWhole, &
        &       isReal8    => this%isReal8, &
        &       in         => this%in,      &
        &       rank       => this%rank,    &
        &       isQ        => this%isQ)
        open(newunit=iounit, file=flowFile//'', &
        &    access=accessType//'', form=formType//'', iostat=ios, &
        &    status="old", action="read")
        if (ios /= 0) stop "Error opening file Plot3dFlow"
        if(isBlock) call this%BlockNumLoader(iounit)
        allocate(this%flow%ln(blockNum)); this%flow%ln=5
        if(isQ)then
          if(isFormatted) then
            read(iounit, *)((itmp, j=1, rank), i=1, blockNum)
          elseif(this%formType=='unformatted') then
            read(iounit)((in(i, j), j=1, rank), i=1, blockNum)
          endif
        else
          if(isFormatted) then
            read(iounit, *)((itmp, j=1, rank), this%flow%ln(i), i=1, blockNum)
          elseif(this%formType=='unformatted') then
            read(iounit)((itmp, j=1, rank), this%flow%ln(i), i=1, blockNum)
          endif
        endif
        if(isReal8) then
          call this%LoadGridArray_r8p(iounit)
        else
          call this%LoadGridArray_r4p(iounit)
        endif
        close(iounit)
      end associate

    end subroutine Plot3dFlowLoader

    subroutine LoadGridArray_r4p (this, iounit)

      implicit none
      class(plot3d_t), intent(inout) :: this
      integer, intent(in) :: iounit
      real(R4P), allocatable :: X(:,:,:,:)
      integer :: i, j, k, l, lrank, k1, k2, iplane

      associate(in=> this%in, &
        &       isWhole=> this%isWhole, &
        &       blockNum=> this%blockNum, &
        &       rank => this%rank, &
        &       isFormatted=> this%isFormatted)
        allocate(this%grid%x(blockNum, rank))
        do l=1, blockNum
          allocate(X(in(l, 1), in(l, 2), in(l, 3), rank))
          if(isWhole)then
            k1=1; k2=in(l,3)
          else
            k1=in(l,3); k2=1
          endif
          call ArrayLoader(iounit, in(l, 1), in(l, 2), k1, k2, rank, &
          &               isFormatted, X)
          do lrank=1, rank
            this%grid%x(l, lrank)=tec_data(X(:,:,:, lrank))
          enddo
          deallocate(X)
        enddo

      end associate

    end subroutine LoadGridArray_r4p

    subroutine LoadGridArray_r8p (this, iounit)

      implicit none
      class(plot3d_t), intent(inout) :: this
      integer, intent(in) :: iounit
      real(R8P), allocatable :: X(:,:,:,:)
      integer :: i, j, k, l, lrank, k1, k2, iplane

      associate(in=> this%in, &
        &       isWhole=> this%isWhole, &
        &       blockNum=> this%blockNum, &
        &       rank => this%rank, &
        &       isFormatted=> this%isFormatted)
        allocate(this%grid%x(blockNum, rank))
        do l=1, blockNum
          allocate(X(in(l, 1), in(l, 2), in(l, 3), rank))
          if(isWhole)then
            k1=1; k2=in(l,3)
          else
            k1=in(l,3); k2=1
          endif
          call ArrayLoader(iounit, in(l, 1), in(l, 2), k1, k2, rank, &
          &               isFormatted, X)
          do lrank=1, rank
            this%grid%x(l, lrank)=tec_data(X(:,:,:, lrank))
          enddo
          deallocate(X)
        enddo
      end associate

    end subroutine LoadGridArray_r8p

    subroutine ArrayLoader_r4p(iounit, in, jn, kn1, kn2, ln, isFormatted, Var)

      implicit none
      integer :: iounit
      integer :: in, jn, kn1, kn2, ln
      logical :: isFormatted
      real(R4P) :: Var(:, :, :, :)
      integer :: iplane, i, j, k, l

      do iplane = 1, kn1
        if(isFormatted) then
          read(iounit, *)((((Var(i, j, k, l), i=1, in) &
                            &             , j=1, jn) &
                            &             , k=1, kn2) &
                            &             , l=1, ln)
        else
          read(iounit)((((Var(i, j, k, l), i=1, in) &
                            &          , j=1, jn) &
                            &          , k=1, kn2) &
                            &          , l=1, ln)
        endif
      enddo

    end subroutine ArrayLoader_r4p

    subroutine ArrayLoader_r8p(iounit, in, jn, kn1, kn2, ln, isFormatted, Var)

      implicit none
      integer :: iounit
      integer :: in, jn, kn1, kn2, ln
      logical :: isFormatted
      real(R8P) :: Var(:, :, :, :)
      integer :: iplane, i, j, k, l

      do iplane = 1, kn1
        if(isFormatted) then
          read(iounit, *)((((Var(i, j, k, l), i=1, in) &
                            &             , j=1, jn) &
                            &             , k=1, kn2) &
                            &             , l=1, ln)
        else
          read(iounit)((((Var(i, j, k, l), i=1, in) &
                            &          , j=1, jn) &
                            &          , k=1, kn2) &
                            &          , l=1, ln)
        endif
      enddo

    end subroutine ArrayLoader_r8p

end module plot3d_loader