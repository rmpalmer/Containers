module item_class
    implicit none

    private

    type item
        integer :: value
      contains
        procedure, pass :: fancy_print
        procedure, pass :: less_than
        generic :: operator(<) => less_than
    end type item

  public :: item, new_item, fancy_print

  contains

    function less_than(a,b) result(r)
      class(item), intent(in) :: a,b
      logical :: r
      !
      r = (a%value < b%value)
      !
    end function less_than

    function new_item(v) result(new_instance)
      type(item), pointer :: new_instance
      integer, intent(in) :: v
      !
      allocate(new_instance)
      new_instance%value = v
      !
    end function new_item

    subroutine fancy_print(this)
      class(item), intent(inout) :: this
      !
      write(*,'(''@['',I0,'']@'')') this%value
      !
    end subroutine fancy_print

end module item_class
