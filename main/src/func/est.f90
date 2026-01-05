!! calls prop_radial
!! @param[in] k         K-d tree structure containing atomic positions and data
!! @param[in] q_vals    Array of Q values at which to calculate intensity
!! @param[in] a         Advice parameter for weight estimation (should be >= number of nodes)
!! @param[in] e         Epsilon parameter for accuracy control (must satisfy 0 < e < 1)
!! @param[in] c         Rounding mode flag: .true. for ceiling, .false. for floor
!! @param[in] name      Dataset identifier/name for the output structure
function call_prop_radial(k, q_vals, a, e, c, name) result(intensity_estimate)
    type(kdt), intent(in) :: k
    character(len=*), intent(in) :: name
    real(c_double), intent(in) :: a  !< Advice parameter (>= # nodes)
    real(c_double), intent(in) :: e  !< Epsilon (0 < e < 1)
    logical, intent(in) :: c         !< Ceiling flag
    type(estimate) :: intensity_estimate
    intensity_estimate = prop_radial(k,q_vals,a,e,c,name)
end function call_prop_radial

!! @param[in] k         K-d tree structure containing the data
!! @param[in] r         Search radius for nearest neighbor queries
!! @param[in] q_vals    Array of Q values to calculate I(Q) at (must be in valid range)
!! @param[in] a         Advice parameter for estimation (should be >= number of nodes)
!! @param[in] e         Epsilon parameter for accuracy control (must satisfy 0 < e < 1)
!! @param[in] c         Rounding mode flag: .true. for ceiling, .false. for floor
!! @param[in] name      Dataset identifier/name for logging or output
function call_prop_kdt(k, r, q_vals, a, e, c, name) result(intensity_estimate)
    type(kdt), intent(in) :: k
    real(c_double), intent(in) :: r
    character(len=*), intent(in) :: name
    real(c_double), intent(in) :: a  !< Advice parameter (>= # nodes)
    real(c_double), intent(in) :: e  !< Epsilon (0 < e < 1)
    logical, intent(in) :: c         !< Ceiling flag
    type(estimate) :: intensity_estimate
    intensity_estimate = prop_kdt(k,r,q_vals,a,e,c,name)
end function call_prop_kdt

!! @param atoms     List of atom objects in molecule
!! @param q_vals    Q values to calculate I(Q); NOTE: assumed to be in valid range!
!! @param name      Name of dataset
function call_debeye_radial(atoms, q_vals, name) result(intensity_estimate)
    character(len=*), intent(in) :: name
    type(atom), dimension(:), intent(in) :: atoms
    real(c_double), dimension(:), intent(in) :: q_vals
    type(estimate) :: intensity_estimate
    intensity_estimate = debeye_radial(atoms,q_vals,name)
end function call_debeye_radial

!! @param k         kdt tree
!! @param r         radius to search within
!! @param q_vals    Q values to calculate I(Q); NOTE: assumed to be in valid range!
!! @param name      Name of dataset
function call_debeye_kdt(k, r, q_vals, name) result(intensity_estimate)
    type(kdt), intent(in) :: k
    real(c_double), intent(in) :: r
    character(len=*), intent(in) :: name
    real(c_double), dimension(:), intent(in) :: q_vals
end function call_debeye_kdt

