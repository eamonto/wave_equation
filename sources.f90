
! ===========================================================================
! sources.f90
! ===========================================================================
! Here are implemented the right hand side (derivatives) of the functions
! to be evolve in time.

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


  subroutine sources(phi2,pi2,psi2,x,dx,grid_points)
    
    use mylibrary

    implicit none

    type(dynamical_func) phi2,pi2,psi2
    type(extra_func) x,dx   

    integer :: grid_points

    integer :: i

    phi2%s = pi2%f

    do i=1,grid_points-1

       pi2%s(i) = (psi2%f(i+1)-psi2%f(i-1))/(2.0D0*dx%f(i))
       
       psi2%s(i) = (pi2%f(i+1)-pi2%f(i-1))/(2.0D0*dx%f(i))

    enddo

  end subroutine sources
