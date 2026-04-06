! Simple AoS 2D R-Tree implementation.
! See Leutenegger, S. T., Lopez, M. A., & Edgington, J. "STR: A simple and efficient algorithm for R-tree packing", 1997.
module mod_rtree
    use iso_fortran_env, only: f64 => real64
    use mod_sort,        only: sort

    implicit none

    private
    public  :: t_r_tree, create, clean, print_tree, search

    integer, parameter :: NO_CHILD   = 0

    type t_node
        real(f64) :: box(2,2) ! bounding box [[xmin,xmax],[ymin,ymax]]
        integer   :: first    ! index of the first child (NO_CHILD for a leaf)
        integer   :: last     ! index of the last child (reference number for a leaf)
    end type t_node

    type t_r_tree
        type(t_node), dimension(:), allocatable :: nodes ! nodes and leafs forming the tree
        integer                                 :: root  ! index of the root (which is the last node element)
    end type

    ! Callback interface for the search subroutine.
    abstract interface
        pure subroutine p_callback(id, context)
            integer,  intent(in)              :: id
            class(*), intent(inout), optional :: context
        end subroutine p_callback
    end interface

contains
    ! Create a R-tree from a set of bounding boxes.
    pure subroutine create(rtree, bbox, group_size)
        type(t_r_tree),                   intent(out)          :: rtree ! tree to fill
        real(f64),      dimension(:,:,:), intent(in)           :: bbox  ! bounding boxes corresponding to the input objects
        integer,                          intent(in), optional :: group_size

        integer, allocatable :: permutation(:)
        integer              :: imin, imax, offset
        integer              :: n, r               ! notations from the article
        integer              :: i,j

        n = 8
        if (present(group_size)) n = group_size

        r = size(bbox,dim=3)

        ! count for allocation
        offset = r
        do while (r > 1)
            r = ceiling(real(r,f64)/n) 
            offset = offset + r
        end do
        allocate(rtree%nodes(offset))
        r = size(bbox,dim=3)

        ! compute the sorting permutation
        allocate(permutation(r))
        call STR(bbox, permutation, n)

        ! fill the leafs according to the sorting
        do i=1,r
            rtree%nodes(i)%box(:,:) = bbox(:,:,permutation(i))
            rtree%nodes(i)%first    = NO_CHILD
            rtree%nodes(i)%last     = permutation(i)
        end do
        deallocate(permutation)

        ! build the nodes
        offset = r
        do while (r > 1)
            do i=1,ceiling(real(r,f64)/n)
                imin = (i-1)*n+1 + offset-r
                imax = min(i*n,r) + offset-r  ! last box may not be full

                rtree%nodes(offset+i)%first = imin
                rtree%nodes(offset+i)%last  = imax

                rtree%nodes(offset+i)%box(:,:) = rtree%nodes(imin)%box
                do j=imin+1,imax
                    rtree%nodes(offset+i)%box(:,:) = bounding_box(rtree%nodes(offset+i)%box, rtree%nodes(j)%box)
                end do
            end do
            r = i-1
            offset = offset + r
        end do

        rtree%root = offset
    end subroutine create

    ! Sort-Tile-Recursive (STR)
    pure subroutine STR(bbox, permutation, n)
        real(f64), dimension(:,:,:), intent(in)  :: bbox
        integer,   dimension(:),     intent(out) :: permutation
        integer,                     intent(in)  :: n           ! group size

        integer,   dimension(:),   allocatable :: permutation_y
        real(f64), dimension(:,:), allocatable :: midpoint
        integer                                :: r, P, S       ! notations from the article
        integer                                :: i, imin, imax

        r = size(bbox,dim=3)
        allocate(midpoint(2,r))
        midpoint(:,:) = .5_f64*sum(bbox,dim=1)

        ! regular x-sort
        call sort(midpoint(1,:), permutation)
        midpoint(:,:) = midpoint(:,permutation)

        ! tiled y-sort
        P = ceiling(real(r,f64)/n)
        S = ceiling(sqrt(real(P,f64)))
        allocate(permutation_y(S*n))
        do i=1,S
            ! compute tile bounds
            imin = (i-1)*S*n + 1
            imax = min(i*S*n, size(midpoint,dim=2))
            ! sort the tile
            call sort(midpoint(2,imin:imax), permutation_y(1:imax-imin+1))
            permutation(imin:imax) = permutation(imin-1+permutation_y(1:imax-imin+1))
        end do
        deallocate(permutation_y)

        deallocate(midpoint)
    end subroutine STR

    ! Search in the R-tree structure.
    pure subroutine search(rtree, bbox, callback, context)
        type(t_r_tree),        intent(in)              :: rtree
        real(f64),             intent(in)              :: bbox(2,2)
        procedure(p_callback)                          :: callback
        class(*),              intent(inout), optional :: context

        call recursive_search(rtree, rtree%root, bbox, callback, context)
    contains
        pure recursive subroutine recursive_search(rtree, root, bbox, callback, context)
            type(t_r_tree),        intent(in)              :: rtree
            integer,               intent(in)              :: root
            real(f64),             intent(in)              :: bbox(2,2) ! bounding box of the input segment
            procedure(p_callback)                          :: callback
            class(*),              intent(inout), optional :: context

            logical :: is_leaf
            integer :: i

            if (.not.bounding_box_overlap(rtree%nodes(root)%box, bbox)) then
                return
            end if

            is_leaf = rtree%nodes(root)%first == NO_CHILD
            if (is_leaf) then
                call callback(rtree%nodes(root)%last, context) ! last is actually the reference index for a leaf
                return
            end if

            do i=rtree%nodes(root)%first, rtree%nodes(root)%last
                call recursive_search(rtree, i, bbox, callback, context)
            end do
        end subroutine recursive_search
    end subroutine search

    ! Print the input R-tree to stdout.
    recursive subroutine print_tree(rtree, root, level)
        character(*), parameter :: INDENT = repeat(" ",4)
        type(t_r_tree), intent(in)           :: rtree
        integer,        intent(in), optional :: root
        integer,        intent(in), optional :: level

        integer :: root_index
        integer :: recursion_level
        integer :: i

        root_index = rtree%root
        if (present(root)) root_index = root

        if (root_index == NO_CHILD) return

        recursion_level = 0
        if (present(level)) recursion_level = level

        write(*,'(A,A,2(F6.3),A,2(F6.3))') repeat(INDENT,recursion_level), &
           & " x:", rtree%nodes(root_index)%box(:,1), &
           & " y:", rtree%nodes(root_index)%box(:,2)

        if (rtree%nodes(root_index)%first == NO_CHILD) return
        do i=rtree%nodes(root_index)%first, rtree%nodes(root_index)%last
            call print_tree(rtree, i, recursion_level+1)
        end do
    end subroutine print_tree

    ! Clean the R-tree object.
    pure subroutine clean(rtree)
        type(t_r_tree), intent(inout) :: rtree

        deallocate(rtree%nodes)
    end subroutine clean

    ! Returns the enclosing bounding box from two bounding boxes
    ! This function assumes that the bounding boxes are ordered as [[xmin,xmax],[ymin,ymax]].
    pure function bounding_box(bbox1, bbox2)
        real(f64), dimension(2,2), intent(in) :: bbox1, bbox2
        real(f64), dimension(2,2)             :: bounding_box

        bounding_box(1,:) = min(bbox1(1,:),bbox2(1,:))
        bounding_box(2,:) = max(bbox1(2,:),bbox2(2,:))
    end function bounding_box

    ! Checks if two input bounding boxes overlap.
    ! This function assumes that the bounding boxes are ordered as [[xmin,xmax],[ymin,ymax]].
    pure logical function bounding_box_overlap(bbx1,bbx2)
        real(f64), dimension(2,2), intent(in) :: bbx1
        real(f64), dimension(2,2), intent(in) :: bbx2

        bounding_box_overlap = .true.

        if (any(bbx1(1,:) > bbx2(2,:) .or. bbx1(2,:) < bbx2(1,:))) then
            bounding_box_overlap = .false.
        end if
    end function bounding_box_overlap

end module mod_rtree
