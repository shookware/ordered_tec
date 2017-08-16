!-----------------------------------------------------------------------
!Main program ex1: Store a 2D grid
!-----------------------------------------------------------------------
program ex1
use ordered_tec
use penf
implicit none

type(tec_file) :: tecfile, tecflow
real(R4P) :: x(20, 40), y(20, 40)
integer :: i, j

do j=1, 40
  do i=1, 20
    x(i,j) = 1.0d0+5.0d0*real(i, kind=R4P)
    y(i,j) = 4.0d0+2.0d0*real(j, kind=R4P)
  end do
enddo

tecfile=tec_file(name='ex1.plt',path='.',title='ex1', filetype='GRID')
call tecfile%addVar(['x','y'])
call tecfile%AddZone(tec_zone('zone 1'))
tecfile%zones(1)%shape=[20,40,1]
call tecfile%zones(1)%AddData(tec_data(x))
call tecfile%zones(1)%AddData(tec_data(y))
call tecfile%WritePlt()
call tecfile%Finalize()

end program ex1
