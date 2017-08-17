!------------------------------------------------------------------------------
! TJU/Department of Mechanics, Fluid Mechanics, Code ordered_tec_fortran
!------------------------------------------------------------------------------
!
!  File: plot3d_to_tec.f90
!> @file
!> @breif convert Plot3D file to Tecplot file.
!  DESCRIPTION:
!>
!! This toolkit can convert the Plot3D file to the tecplot file(binary).
!!
!------------------------------------------------------------------------------

!-----------------------------------------------------------------------
!Main program plot3d_to_tec
!-----------------------------------------------------------------------
program plot3d_to_tec
! use
use plot3d_loader
use ordered_tec

implicit none

  type(plot3d_t) :: plot3d
  type(tec_file) :: tecWriter
  integer :: l, i

  plot3d=plot3d_t()

  tecWriter=tec_file(name='grid.plt',path='.',title='Grid', filetype='GRID')
  call tecWriter%addVar([(plot3d%grid%coordName(i)//'', i=1, 3)])
  do l=1, plot3d%blockNum
    call tecWriter%AddZone(tec_zone('zone 1'))
    call tecWriter%zones(1)%AddData(plot3d%grid%x(l, :))
  enddo
  call tecWriter%WritePlt()


end program plot3d_to_tec
