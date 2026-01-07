!> Fortran-OCaml bridge for exporting intensity estimates to CSV
module csv_interface
    use, intrinsic :: iso_c_binding; use estimate
    implicit none; private; public :: est_wrap
    
    !> OCaml runtime initialization flag
    logical, save :: is_init = .false.
    
    interface 
        !> C bridge function to export data to OCaml CSV writer
        !! @param est Intensity estimate structure
        !! @param fp C pointer to null-terminated file path
        subroutine fortran_to_ocaml(est, fp) bind(C, name="fortran_to_ocaml")
            import :: estimate, c_ptr
            type(estimate), intent(in) :: est
            type(c_ptr), value :: fp
        end subroutine fortran_to_ocaml
        
        !> @brief Initialize OCaml runtime (call once)
        subroutine init_ocaml() bind(C, name="init_ocaml")
        end subroutine 
    end interface    
    
contains

    !> Export intensity estimate to CSV file via OCaml
    !! @param est Intensity estimate to export
    !! @param out_dir Output directory path
    !! @return path of output
    function est_wrap(est, out_dir) result(csv_path_out)
        
        type(estimate), intent(in) :: est
        character(len=*), intent(in) :: out_dir
        character(len=:, kind=c_char), allocatable, target :: csv_path
        character(len=:), allocatable :: csv_path_out
        
        ! Initialize OCaml runtime on first call
        if (.not. is_init) then
            call init_ocaml()
            is_init = .true.
        end if
        
        ! Build null-terminated file path
        csv_path = trim(out_dir) // est%name // ".csv" // c_null_char
        
        ! Export to CSV
        call fortran_to_ocaml(est, c_loc(csv_path))

        ! Output file path
        csv_path_out = trim(out_dir) // est%name // ".csv"

    end function est_wrap
    
end module csv_interface