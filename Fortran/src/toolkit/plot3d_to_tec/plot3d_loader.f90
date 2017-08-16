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
    logical :: isQ

  end type flow_t

  type, public :: plot3d_t

    integer :: blockNum
    integer, allocatable :: in(:, :)

    type(string) :: gridFile
    type(string) :: flowFile
    type(string) :: nmlFile

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

    procedure, private :: BlockNumLoader
    procedure, private :: LoadGridArray_r8p
    procedure, private :: LoadGridArray_r4p

  end type plot3d_t

  interface ArrayLoader
    module procedure ArrayLoader_r8p
    module procedure ArrayLoader_r4p
  end interface ArrayLoader

  contains

    !> Get command line to initialize the property of the plot3d object
    subroutine GetCli()

      implicit none


      

    end subroutine GetCli

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
        &       in         => this%in,      &
        &       rank       => this%rank)
        open(newunit=iounit, file=gridFile//'', &
        &    access=accessType//'', form=formType//'', iostat=ios, &
        &    status="old", action="read")
        if (ios /= 0) stop "Error opening file Plot3dGird"
        if(isBlock) call this%BlockNumLoader(iounit)
        allocate(this%in(blockNum, rank))
        in=1
        if(isFormatted) then
          read(iounit, *)((in(i, j), j=1, rank), i=1, blockNum)
        elseif(this%formType=='unformatted') then
          read(iounit)((in(i, j), j=1, rank), i=1, blockNum)
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
          allocate(X(in(1, l), in(2, l), in(3, l), rank))
          if(isWhole)then
            k1=1; k2=in(3,l)
          else
            k1=in(3,l); k2=1
          endif
          call ArrayLoader(iounit, in(1, l), in(2, l), k1, k2, rank, &
          &               isFormatted, X)
          do lrank=1, rank
            this%grid%x(lrank, l)=tec_data(X(:,:,:, lrank))
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
          allocate(X(in(1, l), in(2, l), in(3, l), this%flow%ln(l)))
          if(isWhole)then
            k1=1; k2=in(3,l)
          else
            k1=in(3,l); k2=1
          endif
          call ArrayLoader(iounit, in(1, l), in(2, l), k1, k2, rank, &
          &               isFormatted, X)
          do lrank=1, rank
            this%grid%x(lrank, l)=tec_data(X(:,:,:, lrank))
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
