! This module provides some geometric helper functions used for testing.
module mod_geometry
    use iso_fortran_env, only: f64 => real64

    implicit none

    private
    public  :: segments_intersect, segment_bounding_box

contains

    ! Checks if two segments intersect or not.
    pure logical function segments_intersect(ab,cd)
        real(f64), dimension(2,2), intent(in) :: ab,cd

        segments_intersect = (orient(ab(:,1),ab(:,2),cd(:,1))*orient(ab(:,1),ab(:,2),cd(:,2)) <= 0._f64) &
                            & .and. (orient(cd(:,1),cd(:,2),ab(:,1))*orient(cd(:,1),cd(:,2),ab(:,2)) <= 0._f64)
    end function segments_intersect

    ! Returns twice the signed area of the abc triangle.
    pure real(f64) function orient(a,b,c)
        real(f64), dimension(2), intent(in) :: a, b, c

        orient = ((a(1) - c(1))*(b(2) - c(2)) - (a(2) - c(2))*(b(1) - c(1)))
    end function orient

    ! Compute the bounding box from an input segment [[xA,yA],[xB,yB]].
    !The box is returned as [[xmin,xmax],[ymin,ymax]].
    pure function segment_bounding_box(segment) result(bounding_box)
        real(f64), intent(in) :: segment(2,2)
        real(f64)             :: bounding_box(2,2)

        integer :: j

        do j=1,2 ! axis x then y
            if (segment(j,1) <= segment(j,2)) then
                bounding_box(:,j) = segment(j,1:2:+1)
            else
                bounding_box(:,j) = segment(j,2:1:-1)
            end if
        end do
    end function segment_bounding_box

end module mod_geometry
