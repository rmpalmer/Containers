module ll_class
    use item_class
    implicit none
    private

    type :: node
      type(item), pointer :: data
      type(node), pointer :: next => null()
    end type node

    type linked_list
        type(node), pointer :: head
        type(node), pointer :: curr
      contains
        procedure, pass :: next_item
        procedure, pass :: reset_list
        procedure, pass :: insert
        procedure, pass :: print
        procedure, pass :: iterate
    end type linked_list

    public :: linked_list, new_linked_list

  contains

    function next_item(this) result(p)
      class(linked_list), intent(inout) :: this
      type(item), pointer :: p
      if (associated(this%curr)) then
        p => this%curr%data
        if (associated(this%curr%next)) then
          this%curr => this%curr%next
        else
          nullify(this%curr)
        end if
      else
        nullify(p)
      end if
      !
    end function next_item

    subroutine reset_list(this)
      class(linked_list), intent(inout) :: this
      !
      if (associated(this%head)) this%curr => this%head
    end subroutine reset_list

    function new_linked_list() result(new_instance)
      type(linked_list) :: new_instance
      !
      nullify(new_instance%head)
      !
    end function new_linked_list

    subroutine insert(this, p)
      class(linked_list), intent(inout) :: this
      type(item), pointer, intent(in) :: p
      !
      type(node), pointer :: temp_ptr
      !
      allocate(temp_ptr)
      temp_ptr%data => p
      if (associated(this%head)) then
        temp_ptr%next => this%head
      end if
      this%head => temp_ptr
      call this%reset_list()
    end subroutine insert

    subroutine print(this)
      class(linked_list), intent(in) :: this
      !
      type(node), pointer :: temp_ptr => null()
      !
      if (associated(this%head)) then
        temp_ptr => this%head
        do
          write(*,*) temp_ptr%data%value
          if (associated(temp_ptr%next)) then
            temp_ptr => temp_ptr%next
          else
            exit
          end if
        end do
      end if
      !
    end subroutine print

    subroutine iterate(this, fun)
      class(linked_list), intent(in) :: this
      interface
         subroutine fun(ip)
           import :: item
           class(item), intent(inout) :: ip
         end subroutine fun
      end interface
      !
      type(node), pointer :: temp_ptr => null()
      !
      if (associated(this%head)) then
        temp_ptr => this%head
        do
          call fun(temp_ptr%data)
          if (associated(temp_ptr%next)) then
            temp_ptr => temp_ptr%next
          else
            exit
          end if
        end do
      end if
      !
    end subroutine iterate

end module ll_class
