
! ===========================================================================
! boundaries.f90
! ===========================================================================
! Implementation of the boundaries conditions

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


  subroutine boundaries(phi2,pi2,psi2,x,dx,grid_points)

    use mylibrary
    use param

    implicit none

    type(dynamical_func) phi2,pi2,psi2
    type(extra_func)     x,dx   

    integer :: grid_points

    real(double) :: der_pi,der_psi

    if(boundary.eq.1) then

       pi2%s(0)  = 0.0D0
       psi2%s(0) = 0.0D0

       pi2%s(grid_points)  = 0.0D0
       psi2%s(grid_points) = 0.0D0

    else if(boundary.eq.2) then

       der_pi  = (- pi2%f(2)+4.0D0* pi2%f(1)-3.0D0* pi2%f(0))/(2.0D0*dx%f(0))
       der_psi = (-psi2%f(2)+4.0D0*psi2%f(1)-3.0D0*psi2%f(0))/(2.0D0*dx%f(0))

       pi2%s(0)  = (der_pi+der_psi)/2.0D0
       psi2%s(0) = (der_pi+der_psi)/2.0D0

       der_pi  = ( pi2%f(grid_points-2)-4.0D0* pi2%f(grid_points-1)+3.0D0 *pi2%f(grid_points))/(2.0D0*dx%f(grid_points))
       der_psi = (psi2%f(grid_points-2)-4.0D0*psi2%f(grid_points-1)+3.0D0*psi2%f(grid_points))/(2.0D0*dx%f(grid_points))

       pi2%s(grid_points) = -(der_pi-der_psi)/2.0D0
       psi2%s(grid_points) = (der_pi-der_psi)/2.0D0

    else

       print*,'Boundary condition not implemented!'
       stop
       
    endif

  end subroutine boundaries
