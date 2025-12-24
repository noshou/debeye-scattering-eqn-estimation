! fix: fix variable declarations, compare magnitudes of cmplx NOT .gt. etc

! Given a list of weights, their frequencies, and 
! an epsilon value 0 < e < 1:
! Find thresh such that P(w >= thresh) <= e/3
!
! Parameters:
!   w: array of weights
!   f: array of frequencies/probabilities for each weight
!   e: epsilon threshold
!   l: boolean flag (true = find smallest valid threshold, 
!                         false = find largest valid threshold)
!   n: total sample size
!   err: 1 if threshold not found, 0 if threshold found
function bern_est(w, f, e, l, n, err) result(t)
    
    ! function arguments/parameters
    complex(c_double), dimension(:), intent(in) :: w
    integer, dimension(:), intent(in) :: f
    real(c_double), intent(in) :: e
    logical, intent(in) :: l
    real(c_double) :: t
    integer, intent(out), optional :: err
    integer, intent(in) :: n
    ! local variables
    integer :: i, j
    real(c_double) :: thresh, p_curr, freq__, t_curr
    logical :: found_

    
    ! initialize variables
    thresh = ieee_value(thresh_result, ieee_quiet_nan)
    freq__ = real(0.0, kind=c_double)
    found_ = .false.

    ! loop to find optimal frequency
    do i = 1, size(w)
        p_curr = real(0.0, kind=c_double)
        t_curr = w(i)

        ! calculate cumulative frequency distribution
        do j = 1, size(w)
            if (w(j) >= t_curr) then 
                p_curr = p_curr + real( &
                    (real(f(j),kind=c_double)) &
                    / real(n,kind=c_double), &
                    kind=c_double)
            end if 
        end do 

        ! check if cdf <= e/3
        if (p_curr <= e/3) then 
            if (.not. found_) then
                thresh = t_curr
                found_ = .true. 
            else 
                if (l) then 
                    if (t_curr < thresh) thresh = t_curr 
                else 
                    if (t_curr > thresh) thresh = t_curr
                end if 
            end if 
        end if 
    end do 
    ! value not found; set threshold to NaN and set err to 1
    
    if (.not. found_) then
        if (present(err)) err = 1
    else 
        if (present(err)) err = 0
    end if
end function bern_est