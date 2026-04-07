! Simple implementation of the quicksort algorithm.
module mod_sort
    use, intrinsic :: iso_fortran_env, only: f64 => real64, i32 => int32

    implicit none

    private
    public  :: sort

    interface sort
        module procedure quicksort
    end interface sort
contains
    ! Returns the permutation that sorts the array,
    !leaving the original array unchanged.
    pure subroutine quicksort(array, permutation)
        real(f64), dimension(:), intent(in)  :: array
        integer,   dimension(:), intent(out) :: permutation

        integer :: i

        permutation(:) = [ (i,i=1,size(permutation)) ]
        call recursive_quicksort(array, 1, size(array), permutation)
    end subroutine quicksort

    pure recursive subroutine recursive_quicksort(array, first, last, permutation)
        ! See https://en.wikipedia.org/wiki/Quicksort for details
        real(f64), dimension(:), intent(in)    :: array
        integer,                 intent(in)    :: first, last
        integer,   dimension(:), intent(inout) :: permutation
        
        real(f64) :: pivot
        integer   :: i,j, tmp

        pivot = array(permutation((first+last)/2))

        i = first-1
        j = last+1

        ! Hoare partition scheme
        do
            do
                i = i+1
                if (array(permutation(i)) >= pivot) exit
            end do

            do
                j = j-1
                if (array(permutation(j)) <= pivot) exit
            end do

            if (i >= j) exit

            ! swap
            tmp            = permutation(j)
            permutation(j) = permutation(i)
            permutation(i) = tmp
        end do

        if (first < j)  call recursive_quicksort(array, first, j, permutation)
        if (j+1 < last) call recursive_quicksort(array, j+1, last, permutation)
    end subroutine recursive_quicksort
end module mod_sort
