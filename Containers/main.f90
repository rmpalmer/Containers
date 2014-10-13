program main

    use item_class
    use ll_class

    implicit none

    type(linked_list) :: my_list

    type(item), pointer :: ip

    write(*,*) 'Hello from Container Test'

    my_list = new_linked_list()
    call my_list%insert(new_item(17))
    call my_list%insert(new_item(55))
    call my_list%insert(new_item(42))
    call my_list%insert(new_item(13))
    call my_list%insert(new_item(7))
    call my_list%insert(new_item(23))
    call my_list%insert(new_item(35))
    call my_list%insert(new_item(18))
    call my_list%insert(new_item(17))
    call my_list%print()

    write(*,*) 'trying new loop'
    do
      ip => my_list%next_item()
      if (.not. associated(ip)) exit
      write(*,*) ip%value
    end do

    write(*,*) 'trying iterate'
    call my_list%iterate(fancy_print)

    write(*,*) 'Goodbye from Container Test'

end program main
