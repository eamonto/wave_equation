
! ===========================================================================
! param.f90
! ===========================================================================
! Global parameters for the physical system.

!     Copyright (C) 2012  Edison Montoya, eamonto@gmail.com

!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.

!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.

!     You should have received a copy of the GNU General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.

! Up to date: 29 Feb 2012					


  module param

    use mylibrary

    implicit none

    !evolution
    real(double),parameter :: courant = 0.05D0 !Courant factor
    integer :: Ntime = 100                     !number of time steps
    integer :: boundary = 2                    !1=zero 2=eigenvalue

    character(100) :: output_dir = "test"      !output directory
    integer :: Nx = 1000                       !Number of points in the grid

    integer :: every_0D = 10                   !time output
    integer :: every_1D = 10                   !spatial output

    real(double),parameter :: dx_aux = 0.1D0   !dx homogeneous grid

    real(double) :: time =0.0D0                !initial time

    real(double) :: dt = courant*dx_aux        !time step

  end module param
