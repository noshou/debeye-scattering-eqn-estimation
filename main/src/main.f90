module main_mod
    use, intrinsic :: iso_c_binding; use estimate
    implicit none; private
    contains
        include "func/est.f90"
end module main_mod