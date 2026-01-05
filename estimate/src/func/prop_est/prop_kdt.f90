!! Performs radius-based searches in the k-d tree for each provided Q value
!! and computes intensity estimates. Returns both the computation time and
!! the resulting intensity data as a function of Q.
!!
!! @param[in] k         K-d tree structure containing the data
!! @param[in] r         Search radius for nearest neighbor queries
!! @param[in] q_vals    Array of Q values to calculate I(Q) at (must be in valid range)
!! @param[in] n_q       Number of Q values in the q_vals array
!! @param[in] a         Advice parameter for estimation (should be >= number of nodes)
!! @param[in] e         Epsilon parameter for accuracy control (must satisfy 0 < e < 1)
!! @param[in] c         Rounding mode flag: .true. for ceiling, .false. for floor
!! @param[in] name      Dataset identifier/name for logging or output
!!
!! @return              Returns a tuple containing:
!!                      - Execution time in nanoseconds (integer)
!!                      - Array of intensity_estimate type with Q vs I_real values
function prop_kdt(k, r, q_vals, n_q, a, e, c, name) result(intensity_estimate)
    type(kdt), intent(in) :: k
    real(c_double), intent(in) :: r
    integer, intent(in) :: n_q
    character(len=*), intent(in) :: name
    real(c_double), intent(in) :: a  !< Advice parameter (>= # nodes)
    real(c_double), intent(in) :: e  !< Epsilon (0 < e < 1)
    logical, intent(in) :: c         !< Ceiling flag
    
    ! Local variables
    type(atom), dimension(:), allocatable :: atoms             
    integer :: n_atoms
    real(c_double), dimension(n_q), intent(in) :: q_vals
    real(c_double), dimension(n_q) :: intensity
    real(c_double) :: norm                                 
    integer :: i, j, q_ij
    real(c_double) :: q_val 
    
    ! timing variables
    integer(c_int) :: timing
    integer :: start, finish, rate
    
    ! output data
    type(estimate) :: intensity_estimate
    
    ! variables for loop
    type(atom), dimension(:), allocatable :: atoms_found        
    type(atom) :: atom_i
    complex(c_double) :: atom_i_ff
    complex(c_double) :: w_est       ! proportional estimate of atomic form factors
    real(c_double) :: radial_contrib ! sinc(|Q-dst|)/(|Q-dst)
    real(c_double) :: atomic_contrib ! ff_i * w_est
    real(c_double) :: dst
    real(c_double) :: est            ! estimate of intensity at I(Q) 
    
    ! start timer, initialize
    call system_clock(start, rate)
    
    n_atoms = k%size()
    allocate(atoms(n_atoms))                                   
    atoms = k%atoms()
    norm = real(n_atoms ** 2, kind=c_double)
    
    do q_ij = 1, n_q
        q_val = q_vals(q_ij)
        w_est = prop_est(k, q_val, a, e, c)
        est = 0
        
        do i = 1, n_atoms
            atom_i = atoms(i)
            atom_i_ff = atom_i%form_factor(q_val)
            atomic_contrib = real(atom_i_ff * w_est, kind=c_double)
            
            ! do search, get list of atoms
            atoms_found = k%radial_search(atom_i, r)
            
            do j = 1, size(atoms_found)
                dst = q_val * abs(atom_i%dist_cart(atoms_found(j)))
                radial_contrib = sinc(dst)
                est = est + atomic_contrib * radial_contrib
            end do 
            
            ! since self is not picked up in radial search, 
            ! we add the case of atom_i_ff * conj(atom_i_ff)
            est = est + real(atom_i_ff * conjg(atom_i_ff), kind=c_double) 
        end do 
        
        intensity(q_ij) = est / norm
    end do
    
    ! stop timer
    call system_clock(finish)
    timing = int((finish - start) / rate, kind=c_int)
    
    ! output estimate
    intensity_estimate = new_intensity(timing, q_vals, intensity, name)
    
    ! cleanup
    deallocate(atoms)
    
end function prop_kdt