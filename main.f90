
! cli module
module main_mod
    use, intrinsic :: iso_c_binding
    use estimate
    use form_fact_mod, only: get_q_values
    use csv_interface
    
    ! generated atom modules "use"
    include "atom_xyz/_build/mod_uses.txt"

    implicit none
    private
    
    public :: cli

contains

    !! command line interface for running tests
    subroutine cli(xyz_module_list_path, out_dir)
        
        ! file paths
        character(len=*), intent(in) :: xyz_module_list_path  ! FIXED: Added
        character(len=*), intent(in) :: out_dir
        character(len=:), allocatable :: path
        
        ! input data
        type(kdt) :: kdt_tree
        type(atom), dimension(:), allocatable :: atoms  
        real(c_double), allocatable :: q_vals(:)
        
        ! variables for file I/O
        integer :: xyz_unit, iostat_val, s, e, m, atms  ! FIXED: Added atms
        character(len=200) :: buff, name, mode
        character(len=*), parameter :: xyz_start_match = "xyz_"
        character(len=*), parameter :: xyz_end_match = "_mod.mod"
        
        ! user cli inputs
        real :: a_, e_, r_
        real(c_double) :: a, e, r
        logical :: c
        
        ! analysis results
        type(estimate) :: deby_rad, deby_kdt, prop_rad, prop_kdt
                    
        ! open xyz modules for analysis
        xyz_unit = 10
        open(unit=xyz_unit, file=xyz_module_list_path, status="old", iostat=iostat_val)
        if (iostat_val .ne. 0) then 
            print*, "Error opening xyz_modules.txt! Exiting..."
            stop 
        end if
        
        ! run analysis
        q_vals = get_q_values()
        do 
            read(xyz_unit, "(A)", iostat=iostat_val) buff
            if (iostat_val .ne. 0) exit  ! Exit on EOF or error

            ! match name of molecule
            s = len(xyz_start_match)
            e = len(xyz_end_match)
            m = len(trim(buff)) - s - e
            name = trim(buff)(s+1:len_trim(buff) - e)
            
            ! add switch cases here - defines which atom module to use
            include "atom_xyz/_build/mod_switches.txt"  ! FIXED: path
            atms = size(atoms)

            ! build KD-tree
            kdt_tree = kdt_creator(atoms)
            

            ! display number of atoms
            print*, "Number of atoms (n):", atms
            
            ! prompt user for advice
            do while (.true.)
                print*, "Input advice parameter a (must be ≥ ñ): "
                read(*,*) a_
                if (a_ .ge. atms) then
                    a = real(a_, kind=c_double)
                    exit
                else
                    print*, "ñ must be ≥ n, please retry"
                end if
            end do 
            
            ! prompt user for epsilon value
            do while (.true.)
                print*, "Input epsilon parameter ε (0 < ε < 1): "
                read(*,*) e_
                if (e_ .gt. 0 .and. e_ .lt. 1) then 
                    e = real(e_, kind=c_double)
                    exit
                else
                    print*, "must be 0 < ε < 1, please retry"
                end if
            end do
            
            ! prompt user for radius
            do while (.true.)
                print*, "Input search radius: "
                read(*,*) r_
                if (r_ > 0) then 
                    r = real(r_, kind=c_double)
                    exit
                else 
                    print*, "r must be > 0"
                end if
            end do

            ! prompt user for rounding mode
            do while (.true.)
                print*, "Rounding mode: \n\tDOWN:\tround down\n\tUP:\tround up: "
                read(*,*) mode
                if (mode .eq. "DOWN" ) then 
                    c = .false.
                    exit
                else if (mode .eq. "UP") then
                    c = .true.
                    exit
                else 
                    print*, "Invalid input!"
                end if
            end do
            
            print*, ""
            print*, "===================="
            print*, "Analyzing:\t", trim(name)
            
            
            print*, "Running debeyeEst_radial..."
            deby_rad = debeyeEst_radial(atoms, q_vals, name)  ! FIXED: atoms not kdt_tree
            path    = est_wrap(deby_rad, out_dir)
            print*, "analysis saved at: ", path
            print*, ""

            print*, "Running debeyeEst_kdt..."
            deby_kdt = debeyeEst_kdt(kdt_tree, r, q_vals, name)
            path    = est_wrap(deby_kdt, out_dir)
            print*, "analysis saved at: ", path
            print*, ""

            print*, "Running propEst_radial..."
            prop_rad = propEst_radial(kdt_tree, q_vals, a, e, c, name)
            path     = est_wrap(prop_rad, out_dir)
            print*, "analysis saved at: ", path
            print*, ""

            print*, "Running propEst_kdt..."
            prop_kdt = propEst_kdt(kdt_tree, r, q_vals, a, e, c, name)
            path     = est_wrap(prop_kdt, out_dir)
            print*, "analysis saved at: ", path
            print*, ""
            
        end do 
        
        print*, ""
        print*, "===================="
        print*, ""
        print*, "Finished analysis!"
        print*, "===================="
        close(xyz_unit)
    end subroutine cli

end module main_mod

program main 
    
    use, intrinsic :: iso_c_binding
    use main_mod
    use, intrinsic :: iso_fortran_env
    implicit none
    
    ! local variables
    integer :: arg_num
    character(len=256) :: xyz_module_list_path, out_dir 

    ! execute main
    arg_num = command_argument_count()
    if (arg_num .ne. 2) then 
        write(error_unit, '(A)') "Program expects 2 arguments!"
        stop 1
    else 
        call get_command_argument(1, xyz_module_list_path)
        call get_command_argument(2, out_dir)
        call cli(trim(xyz_module_list_path), trim(out_dir)) 
    end if

end program main