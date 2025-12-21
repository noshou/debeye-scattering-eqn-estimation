! parent needs: type atom_res = type(atom) | null

! Median of medians algorithm
module kdt
	
    use iso_fortran_env, only: real64
    use atoms_mod
    implicit none

    ! public methods
    public :: median
    public :: hype
    
    !!!!!!!!!!!!!!!!!!
    !! CUSTOM TYPES !!
    !!!!!!!!!!!!!!!!!!
    
    ! group of atoms for median-of-median algorithm
    type, abstract :: hype; contains procedure :: str => str_method; end type hype
    type, extends(hype) :: X; end type X
    type, extends(hype) :: Y; end type Y
    type, extends(hype) :: Z; end type Z

    ! KDT tree
    type :: kdt  
        type(node),  allocatable :: root
    end type kdt 

    ! KDT node
    type :: node 
        type(kdt),   allocatable :: rch
        type(kdt),   allocatable :: lch
        type(atom),  allocatable :: atm 
        class(hype), allocatable :: axs
    end type node
    
    contains

        ! returns the hype of atom as a string
        function str_method (this) result(hyp)
            class(hype) :: this
            character(len=1)  :: plane
            select type (this)
                type is (X); plane = "x"
                type is (Y); plane = "y"
                type is (Z); plane = "z"
            end select 
        end function str_method
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !! MEDIAN OF MEDIAN ALGORITHM !!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! insertion sort for small arrays
        subroutine sort_bin(bin, axs)
            type(atom), intent(inout) :: bin(:)
            class(hype), intent(in) :: axs
            integer :: i, j
            type(atom) :: atom_i
            do i = 2, size(bin)
                atom_i = bin(i)
                j = i - 1
                do while (j >= 1 .and. bin(j)%cmp_axis(key, axs%str) > 0)
                    bin(j+1) = bin(j)
                    j = j - 1
                end do
                bin(j+1) = key
            end do
        end subroutine sort_bin

        ! takes the first n elements from an array and
        ! returns it as a new array
        function take(n, atoms) result(res)
            integer, intent(in) :: n
            type(atom), intent(in) :: atoms(:)
            type(atom) :: res(n)
            res = atoms(:n)
        end function take

        ! drops the first n elements from a list
        ! and returns the tail as a new list
        function drop(n, atoms) result(res)
            integer, intent(in) :: n
            type(atom), dimension(:), intent(in)  :: atoms
            type(atom), dimension(size(atoms)-n) :: res
            res = atoms(n+1:)
        end function drop

        ! groups list of atoms into n "bins" along 
        ! specified axis and finds the median of each bin. 
        ! Next, the median of all the bins is returned
        recursive function median(n, atoms, axs) result(med)
            integer, intent(in) :: n
            type(atom), intent(in) :: atoms(:)
            class(hype), intent(in) :: axs
            type(atom) :: med
            type(atom), allocatable :: bin(:), medians(:), rest(:)
            integer :: idx, num_bins, i, bin_size
            
            if (size(atoms) <= n) then
                bin = atoms
                call sort_bin(bin, axs)
                med = bin((size(bin)+1)/2)
                return
            end if
            
            num_bins = (size(atoms) + n - 1) / n
            allocate(medians(num_bins))
            allocate(rest(size(atoms)))
            rest = atoms
            
            do i = 1, num_bins
                bin_size = min(n, size(rest))
                bin = take(bin_size, rest)
                call sort_bin(bin, axs)
                medians(i) = bin((size(bin)+1)/2)
                if (size(rest) > n) rest = drop(n, rest)
            end do
            
            med = median(n, medians, axs)
        end function median

        !!!!!!!!!!!!!!!!!!!!!!!!
        !! KDT TREE ALGORITHM !!
        !!!!!!!!!!!!!!!!!!!!!!!!

        ! returns true of t is empty, false otherwise
        function is_empty(t) result(res)
            type(kdt), intent(in) :: t
            logical :: res
            res = .not. allocated(t%root)
        end function is_empty

        ! increments hyperplane axis: X -> Y -> Z -> X etc..
        function incr_axs(cur_axs) result(new_axs)
            class(hype), intent(in)   :: cur_axs
            class(hype), allocatable :: new_axs
            select type (cur_axs)
                type is (X); allocate(Y :: new_axs)
                type is (Y); allocate(Z :: new_axs)
                type is (Z); allocate(X :: new_axs)
            end select 
        end function incr_axs

        ! builds a static k-d tree (3 dimensions along x,y,z)
        recursive function kdt_creator(atoms, axs, bin_size_opt) result(t)
            
            ! input variables
            type(atom), dimension(:), intent(in) :: atoms
            class(hype), intent(in) :: axs

            ! optional bin size of splitting (defaults to 5)
            integer, intent(in), optional :: bin_size_opt
            integer :: bin_size
            if (present(bin_size_opt)) then
                bin_size = bin_size_opt
            else 
                bin_size = 5
            end if 
            
            ! local variables
            type(kdt) :: t
            type(atom) :: pivot
            type(atom), allocatable :: left_tree(:)
            type(atom), allocatable :: right_tree(:)
            integer :: i, left_incr, right_incr
            
            ! allocate arrays for right and left subtree;
            ! to be safe we just made it the size of atoms, 
            ! so when left_tree/right_tree are actually used,
            ! we must ensure only slice in range 1:incr is used 
            ! (since rest is uninitialized)
            allocate(left_tree(size(atoms)))
            allocate(right_tree(size(atoms)))
            left_incr = 0
            right_incr = 0

            ! get pivot, sort into left sub tree (< root)
            ! and right subtree (>= root)
            ! note: since we have already pre-allocated for right/left sub trees, 
            ! if either of the tree incrementors are 0 we deallocate the arrays :)
            !! if one axis is >= pivot; if other axis
            !! are equivalent, discard; else add to right sub_tree
            pivot = median(bin_size, atoms, axs)
            do i = 1, size(atoms)
                if (atoms(i)%cmp_axis(pivot, axs%str()) == 0) then 
                    if (.not. atoms(i)%cmp_axis(pivot, incr_axs(axs)%str()) == 0) then
                        if (.not. atoms(i)%cmp_axis(pivot, incr_axs(incr_axs(axs))%str) == 0) then
                            right_incr = right_incr + 1
                            right_tree(right_incr) = atoms(i)
                    end if 
                else if (atoms(i)%cmp_axis(pivot, axs%str()) > 0) then 
                    right_incr = right_incr + 1
                    right_tree(right_incr) = atoms(i)
                else 
                    left_incr = left_incr + 1
                    left_tree(left_incr) = atoms(i)
                end if 
            end do 

            ! build current node  
            allocate(t%root)
            allocate(t%root%atm, source=pivot)
            allocate(t%root%axs, source=axs)
            
            ! build left subtree
            if (left_incr > 0) then 
                allocate(t%root%lch)
                t%root%lch = kdt_creator(left_tree(1:left_incr), incr_axs(axs))
            end if 
            
            ! build right subtree
            if (right_incr > 0) then 
                allocate(t%root%rch)
                t%root%rch = kdt_creator(right_tree(1:right_incr), incr_axs(axs))
            end if 
        end function kdt_creator

end module kdt