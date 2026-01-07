module estimate_mod
    use, intrinsic :: iso_c_binding
    use kdt_mod
    use atom_mod
    
    implicit none 
    private 
    public :: estimate, debeye_radial, debeye_kdt, prop_radial, prop_kdt
    
    ! intensity estimate type
    type, bind(C) :: estimate
        type(c_ptr)     :: q_vals    
        type(c_ptr)     :: i_vals     
        integer(c_int)  :: timing
        integer(c_int)  :: size  
        type(c_ptr)     :: name ! IS NOT NULL TERMINATED     
    end type estimate

    contains 
        include "func/new_intensity.f90"
        include "func/propEst/propEst.f90"
        include "func/sinc.f90"
        include "func/debeyeEst/debeyeEst_radial.f90"
        include "func/debeyeEst/debeyeEst_kdt.f90"
        include "func/propEst/propEst_radial.f90"
        include "func/propEst/propEst_kdt.f90"
end module estimate_mod