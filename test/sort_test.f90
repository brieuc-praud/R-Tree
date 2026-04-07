! This test program ensures that the sort subroutine is able to sort a small list correctly.
program sort_test
    use iso_fortran_env, only: f64 => real64
    use mod_sort,        only: sort

    implicit none

    integer, dimension(*), parameter :: list   = [8, 1, 1, 3, 6, 1, 0, 3, 4, 2]
    integer, dimension(*), parameter :: sorted = [0, 1, 1, 1, 2, 3, 3, 4, 6, 8]
    
    integer :: permutation(size(list))
    integer :: i

    permutation(:) = [(i,i=1,size(list))]
    call sort(real(list,f64), permutation)

    if (any(list(permutation) /= sorted)) error stop 'sort failed'
end program sort_test
