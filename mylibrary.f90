
! ===========================================================================
! mylibrary.f90
! ===========================================================================
! Library to evolve an arbitry physical system on a 1 dimensional grid using 
! a Runge-Kutta 4 method. Also includes memory allocation and output routines.

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


module mylibrary

  !Presicion used
  INTEGER, PARAMETER :: double = SELECTED_REAL_KIND (13)


  !Functions on the grid to be integrated
  type :: dynamical_func
     real(double), allocatable, dimension (:) :: f !function
     real(double), allocatable, dimension (:) :: s !source (derivative)
     real(double), allocatable, dimension (:) :: i !initial (RK4) NO USER
     real(double), allocatable, dimension (:) :: a !advance (RK4) NO USER
     character(100) :: name   !name (output)
     integer :: id            !id   (output)
  end type  dynamical_func
  

  !Extra functions that are not evolve
  type :: extra_func
     real(double), allocatable, dimension (:) :: f !function
  end type  extra_func
  

CONTAINS


  !Implemetation of the Runge-Kutta 4 method 
  subroutine evolution_rk4(k,func,time_step)
    
    integer :: k
    type(dynamical_func) func
    real(double) :: time_step

    real(double) :: dtw,weight
    
    if (k.eq.1) then
       dtw = time_step/2.0D0
       weight = time_step/6.0D0

       func%f = func%i + dtw*func%s
       func%a = func%a + weight*func%s

    else if (k.eq.2) then
       dtw = time_step/2.0D0
       weight = time_step/3.0D0

       func%f = func%i + dtw*func%s
       func%a = func%a + weight*func%s

    else if (k.eq.3) then
       dtw = time_step
       weight = time_step/3.0D0

       func%f = func%i + dtw*func%s
       func%a = func%a + weight*func%s

    else if(k.eq.4) then
       weight = time_step/6.0D0

       func%f = func%a + weight*func%s
    end if

  end subroutine evolution_rk4


  !Store the initial values before the integration
  subroutine store_levels_rk4(func)
    
    type(dynamical_func) func
    
    func%i = func%f    
    func%a = func%f
    
  end subroutine store_levels_rk4


  !Allocation of function that are evolve
  subroutine allocate_dyn(func,grid_points)

    type(dynamical_func) func
    integer :: grid_points

    allocate(func%f(0:grid_points))
    allocate(func%s(0:grid_points))
    allocate(func%i(0:grid_points))
    allocate(func%a(0:grid_points))

    func%f = 0.0D0
    func%s = 0.0D0
    func%i = 0.0D0
    func%a = 0.0D0

  end subroutine allocate_dyn


  !Allocation of the function that are not evolve
  subroutine allocate_extra(func,grid_points)

    type(extra_func) func
    integer :: grid_points

    allocate(func%f(0:grid_points))

    func%f = 0.0D0

  end subroutine allocate_extra


  !Deallocation of function that are evolve
  subroutine deallocate_dyn(func)

    type(dynamical_func) func

    deallocate(func%f)
    deallocate(func%s)
    deallocate(func%i)
    deallocate(func%a)

  end subroutine deallocate_dyn


  !Allocation of function that are not evolve
  subroutine deallocate_extra(func)

    type(extra_func) func

    deallocate(func%f)

  end subroutine deallocate_extra


  !Created the output file for the dynamical functions "func" in  
  !the "output_dir" with name "output_file" and id "file_number"
  subroutine create_output(func,output_dir,output_file,file_number)

    type(dynamical_func) func
    character(len=*) :: output_dir
    character(len=*) :: output_file
    integer :: file_number

    call system('mkdir -p '//trim(output_dir))

    func%name = trim(output_dir)//'/'//trim(output_file)
    func%id   = file_number

    open (file_number,file=func%name,form='formatted',status='replace')
    close(file_number)

  end subroutine create_output


  !Print the output of two scalar functions on the grid
  subroutine output_obs_obs(axis1,axis2,output_file,file_number,Nx)

    real(double), DIMENSION(0:Nx) :: axis1,axis2
    character(len=*) :: output_file
    integer :: file_number
    integer :: i
    integer :: Nx

    real(double) :: eps = 1.0D-15

    open(file_number,file=trim(output_file),form='formatted',status='old',position='append')

    do i=0,Nx
       if(abs(axis2(i)).lt.eps) axis2(i)=0.0D0

       write(file_number,"(2ES24.16)") axis1(i),axis2(i)
    enddo

    write(file_number,*)
    write(file_number,*)
    close(file_number)

  end subroutine output_obs_obs

end module mylibrary
