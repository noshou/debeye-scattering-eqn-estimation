! 1. give list of available atom modules
! 2. prompt user to choose one or multiple (numbereds)
! 3. prompt for advice parameters
! 4. initialize needed values
! 5. run tests 
! 6. give output

!! command line interface for running tests
subroutine cli(xyz_module_list_path)
        
    ! file path for xyz module
    character(len=15), intent(in) :: xyz_module_list_path
    ! variables for file I/O
    integer :: unit_num, iostat_val, i
    character(len=200) :: buff
        unit_num = 10; i = 1
    open(unit=unit_num, file=xyz_module_list_path, status="old", iostat=iostat_val)
    if (iostat_val .neq. 0) then 
        print*, "Error opening xyz_modules.txt! Exiting...\n"
        stop 
    end if
    print*, "Available modules:"
    
    ! print out module options
    do 
        read(unit_num, "(A)", iostat=iostat_val) buff
        if (iostat_val /= 0) exit  
        print*, "\ti: buff\n", i, buff
        i = i + 1
    end do 
    print*, "\t0: run all available modules"
    close(unit_num)
end subroutine cli