
! ===========================================================================
! main.f90
! ===========================================================================
! Principal program, this program solves the wave equation in 1+1 dimensions.

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


  program main

    use param
    use mylibrary

    implicit none

    integer :: l,k

    type(dynamical_func) phi2,pi2,psi2
    type(extra_func)     x,dx   

    !Allocation of function that are evolve
    call allocate_dyn(phi2,Nx)  !NO USER
    call allocate_dyn(pi2 ,Nx)  !NO USER
    call allocate_dyn(psi2,Nx)  !NO USER

    !Allocation of function that are not evolve
    call allocate_extra(x ,Nx)  !NO USER
    call allocate_extra(dx,Nx)  !NO USER

    !Initialization of the functions, including the grid
    call initial_data(phi2,pi2,psi2,x,dx,Nx)  !USER

    !Created the output file for the dynamical functions "func" in  
    !the "output_dir" with name "output_file" and id "file_number"
    call create_output(phi2,output_dir,'phi2.x',200)     !NO USER
    call create_output(pi2 ,output_dir,"pi2.x" ,201)     !NO USER
    call create_output(psi2,output_dir,"psi2.x",202)     !NO USER

    !Print the output of two scalar functions on the grid
    call output_obs_obs(x%f,phi2%f,phi2%name,phi2%id,Nx) !NO USER
    call output_obs_obs(x%f, pi2%f, pi2%name, pi2%id,Nx) !NO USER
    call output_obs_obs(x%f,psi2%f,psi2%name,psi2%id,Nx) !NO USER
 
    print *,''
    print *,'    Time     '
    print *,'-------------'
    write(*,"(F12.5)") time

    do l=1,Ntime

       !Store the initial values before the integration
       call store_levels_rk4(phi2)  !NO USER
       call store_levels_rk4(pi2)   !NO USER
       call store_levels_rk4(psi2)  !NO USER

       do k=1,4

          !Derivatives of the functions to be evolve
          call sources(phi2,pi2,psi2,x,dx,Nx)               !USER

          !Boundaries
          call boundaries(phi2,pi2,psi2,x,dx,Nx,boundary)   !USER

          !Implemetation of the Runge-Kutta 4 method 
          call evolution_rk4(k,phi2,dt)  !NO USER
          call evolution_rk4(k,pi2 ,dt)  !NO USER
          call evolution_rk4(k,psi2,dt)  !NO USER

       enddo

       time = time + dt

       if(mod(l,every_0D).eq.0) then
          write(*,"(F12.5)") time
       endif

       !Print the output of two scalar functions on the grid
       if(mod(l,every_1D).eq.0) then
          call output_obs_obs(x%f,phi2%f,phi2%name,phi2%id,Nx)  !NO USER
          call output_obs_obs(x%f, pi2%f, pi2%name, pi2%id,Nx)  !NO USER
          call output_obs_obs(x%f,psi2%f,psi2%name,psi2%id,Nx)  !NO USER
       endif

    enddo

    !Deallocation of function that are evolve
    call deallocate_dyn(phi2)  !NO USER
    call deallocate_dyn(pi2 )  !NO USER
    call deallocate_dyn(psi2)  !NO USER

    !Deallocation of function that are not evolve
    call deallocate_extra(x )  !NO USER
    call deallocate_extra(dx)  !NO USER

    print *,''
    print *,'   Finish'
    print *,''

  end program main
