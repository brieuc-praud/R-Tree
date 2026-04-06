! This test program generates two sets of N segments then does two checks:
! - if the R-Tree implementation finds the same number of intersections between the two sets than the brute force algorithm;
! - if the R-Tree implementation is actually faster than the brute force approach.
program test
    use iso_fortran_env, only: f64 => real64
    use mod_rtree,       only: t_r_tree, create, clean, search
    use mod_geometry,    only: segment_bounding_box, segments_intersect

    implicit none

    integer,   parameter :: N = 10000                  ! number of line segments to be generated in each set
    real(f64), parameter :: SEGMENT_LENGTH = 1.e-2_f64 ! maximal length of a line segment
    real(f64), parameter :: SPEEDUP = 1.e1             ! minimal speedup that the R-Tree should provide against the brute force

    real(f64),      dimension(:,:,:), allocatable :: segments_ref, segments_test, bbox
    type(t_r_tree)                                :: rtree
    integer                                       :: i

    type t_context
        real(f64), allocatable :: ref(:,:,:)
        real(f64), allocatable :: test(:,:,:)
        integer                :: count
        integer                :: i
    end type t_context

    call set_seed()

    ! Generate reference segments and store them in the R-Tree
    allocate(segments_ref(2,2,N))
    call random_segments(segments_ref)
    allocate(bbox, mold=segments_ref)
    do i=1,N
        bbox(:,:,i) = segment_bounding_box(segments_ref(:,:,i))
    end do
    call create(rtree, bbox)
    deallocate(bbox)

    ! Generate test segments
    allocate(segments_test(2,2,N))
    call random_segments(segments_test)
    allocate(bbox, mold=segments_test)
    do i=1,N
        bbox(:,:,i) = segment_bounding_box(segments_test(:,:,i))
    end do
    deallocate(bbox)

    ! Actual test
    call check_overlaps(rtree, segments_ref, segments_test)

    call clean(rtree)

    deallocate(segments_ref)
    deallocate(segments_test)

contains

    subroutine random_segments(segments)
        real(f64), intent(out) :: segments(:,:,:)

        call random_number(segments)
        segments(:,2,:) = segments(:,1,:) + SEGMENT_LENGTH*segments(:,2,:)
    end subroutine random_segments

    subroutine set_seed()
        integer :: n,i
        call random_seed(size=n)
        call random_seed(put=[(i,i=1,n)])
    end subroutine set_seed

    subroutine check_overlaps(rtree, ref, test)
        type(t_r_tree),                           intent(in) :: rtree
        real(f64),      dimension(:,:,:), target, intent(in) :: ref, test

        type(t_context) :: context
        integer         :: count
        integer         :: i,j

        real(f64) :: t1,t2,t3,t4

        ! Intersection count via R-Tree
        allocate(context%ref,  source=ref)
        allocate(context%test, source=test)
        context%count = 0
        call cpu_time(t1)
        do i=1,size(test,dim=3)
            context%i = i
            call search(rtree, segment_bounding_box(test(:,:,i)), overlap, context)
        end do
        call cpu_time(t2)

        ! Naive intersection count
        count = 0
        call cpu_time(t3)
        do i=1,size(test,dim=3)
            do j=1,size(ref,dim=3)
                if (segments_intersect(ref(:,:,j),test(:,:,i))) then
                    count = count + 1
                end if
            end do
        end do
        call cpu_time(t4)

        if (context%count /= count)  error stop 'R-Tree implementation did not match brute force algorithm.'
        if (SPEEDUP*(t2-t1) > t4-t3) error stop 'R-Tree implementation is too slow.'
    end subroutine check_overlaps

    pure subroutine overlap(id, context)
        integer,  intent(in)              :: id
        class(*), intent(inout), optional :: context

        select type (ctx => context)
        type is (t_context)
            if (segments_intersect(ctx%ref(:,:,id),ctx%test(:,:,ctx%i))) then
                ctx%count = ctx%count + 1
            end if
        end select
    end subroutine overlap

end program test
