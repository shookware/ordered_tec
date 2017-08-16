!-----------------------------------------------------------------------
!Main program ex1: Store a 3D full type data with exclude data
!-----------------------------------------------------------------------
program ex1
use ordered_tec
use penf
implicit none

type(tec_file) :: tecfile, tecflow
real(R8P) :: x(120, 240, 360), y(120, 240, 360), z(120, 240, 360), &
          &  w(120, 240, 360)
type(tec_data) :: tecdata
integer :: i, j, k

do k=1, 360
  do j=1, 240
    do i=1, 120
      x(i, j, k) = 1.0d0+5.0d0*real(i, kind=R8P)
      y(i, j, k) = 4.0d0+2.0d0*real(j, kind=R8P)
      z(i, j, k) = 2.0d0+3.0d0*real(k, kind=R8P)
    end do
  enddo
enddo
w=2.5*x**2+4.0*x*z+1.54*log(y)

tecfile=tec_file(name='ex2.plt',path='.',title='ex2', filetype='FULL')
call tecfile%addVar(['x','y','z'])
call tecfile%addVar('w')
call tecfile%AddZone(tec_zone('zone 1'))
tecfile%zones(1)%shape=[120,240,360]
tecdata=tec_data(x)
call tecfile%zones(1)%AddData([tec_data(x)])
call tecfile%zones(1)%AddData(tec_data(y))
call tecfile%zones(1)%AddData(tec_data(z))
call tecfile%zones(1)%AddData(tec_data(w))
tecfile%zones(1)%begin=[20,70,0]
tecfile%zones(1)%end=[0,220,161]
tecfile%zones(1)%skip=[1,14,5]

call tecfile%WritePlt()
call tecfile%Finalize()

end program ex1
