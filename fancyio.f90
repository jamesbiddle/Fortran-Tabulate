module FancyIO
  use Kinds
  implicit none

  interface Tabulate
     module procedure Tabulate_DP
     module procedure Tabulate_SP
     module procedure Tabulate_int
  end interface Tabulate
  
contains
  
  subroutine Tabulate_DP(data, file, access, column_labels, row_labels, sf, type)
    ! Input variables
    real(DP), dimension(:,:), intent(in) :: data
    character(len=*), intent(in) :: file
    character(len=*), optional :: access, type
    character(len=*), dimension(:), optional :: column_labels, row_labels
    integer, optional :: sf

    ! Local variables
    integer, parameter  :: id = 101
    real(DP), parameter :: upper_bound = 10.0d0**4, lower_bound = 10.0d0**(-4)
    integer, dimension(:), allocatable :: column_widths
    character(len=:), allocatable :: fchar_label, fchar_long, fchar_short
    character(len=:), allocatable :: access_, type_, pad
    character(len=32) :: fmt
    integer :: nrows, ncols
    integer :: col_label_length, row_label_length, long, short, pad_length
    integer :: sf_, head
    integer :: i, j

    ! Manage optional variables
    if(present(access)) then
       access_ = access
    else
       access_ = "sequential"
    end if

    if(present(type)) then
       type_ = type
    else
       type_ = "normal"
    end if

    select case(type_)
    case("normal")
       pad = '  '
    case("csv")
       pad = ', '
    case("fancy")
       pad = ' | '
    end select
    pad_length = len(pad)

    if(present(column_labels)) then
       col_label_length = len(column_labels(1))
    else
       col_label_length = 0
    end if

    if(present(row_labels)) then
       row_label_length = len(row_labels(1))
    else
       row_label_length = 0
    end if

    if(present(sf)) then
       sf_ = sf
    else
       sf_ = 8
    end if

    nrows = size(data(:,1))
    ncols = size(data(1,:))

    ! Assign column widths
    allocate(column_widths(ncols))
    long  = sf_ + 7
    short = sf_ + 2
    do i = 1,ncols
       if(any(abs(data(:,i)) >= upper_bound) .OR. any(abs(data(:,i)) <= lower_bound)) then
          ! Account for numbers to be written in scientific notation if >= 10000
          column_widths(i) = long
       else
          column_widths(i) = short
       end if

       if (column_widths(i) < col_label_length) then
          column_widths(i) = col_label_length
       end if
    end do
    allocate(character(len = col_label_length) :: fchar_label)
    allocate(character(len = short) :: fchar_short)
    allocate(character(len = long) :: fchar_long)

    open(id, file = file, access = access_, action = "write")

    ! Start with a blank line if appending
    if(access_ == "append") write(id,*)
    
    ! Print column headers
    if(present(column_labels)) then
       write(fmt, *) '(a', row_label_length + pad_length, ')'
       
       if(type == "fancy" .AND. row_label_length .NE. 0) then
          write(id, fmt, advance='no') pad
       else if(row_label_length .NE. 0) then
          write(id, fmt, advance='no') ''
       end if
       
       do i = 1, ncols
          write(fmt,*) '(a', col_label_length, ',a', column_widths(i) - col_label_length + pad_length, ')'
          write(id, fmt, advance='no') column_labels(i), pad
       end do

       ! Do the fancy header line
       if(type_ == "fancy") then
          write(id,*)
          if(row_label_length .NE. 0) then
             write(id,'(a)', advance = "no") repeat("-", row_label_length)//"-+-"
          end if
          
          do i = 1, ncols - 1
             write(id,'(a)', advance = "no") repeat("-", column_widths(i))//"-+-"          
          end do
          write(id,'(a)') repeat("-", column_widths(i))//"-|"          
       else
          write(id,*)
       end if
       
    end if

    ! Print row labels and data
    do i = 1, nrows
       if(present(row_labels)) then
          write(fmt, *) '(a', row_label_length, ',a', pad_length, ')'
          write(id, fmt, advance = 'no') row_labels(i), pad
       end if

       ! Print columns
       do j = 1, ncols
          ! Generate format specifier
          if (abs(data(i,j)) >= upper_bound .OR. abs(data(i,j)) <= lower_bound) then
             write(fmt,*) '(es', column_widths(j), '.', sf_, ')'
          else
             ! Figure out how many spaces go in front of the decimal
             if(abs(data(i,j)) < 1.0d0) then
                head = 1
             else if(mod(int(data(i,j)), 10) == int(data(i,j))) then
                head = 1
             else if(mod(int(data(i,j)), 100) == int(data(i,j))) then
                head = 2
             else if(mod(int(data(i,j)), 1000) == int(data(i,j))) then
                head = 3
             else
                head = 4
             end if
             write(fmt,*) '(f', column_widths(j), '.', sf_ - head, ')'
          end if

          ! Left adjust in case of variable number width
          if(column_widths(j) == col_label_length) then
             write(fchar_label, fmt) data(i,j)
             write(id, '(a)', advance = 'no') adjustl(fchar_label)
          else if(column_widths(j) == long) then
             write(fchar_long, fmt) data(i,j)
             write(id, '(a)', advance = 'no') adjustl(fchar_long)
          else
             write(fchar_short, fmt) data(i,j)
             write(id, '(a)', advance = 'no') adjustl(fchar_short)
          end if

          write(fmt,*) '(a', pad_length, ')'
          write(id, fmt, advance='no') pad
       end do

       write(id,*)
    end do

    deallocate(column_widths)
    close(id)

  end subroutine Tabulate_DP

  subroutine Tabulate_SP(data, file, access, column_labels, row_labels, sf, type)
    ! Input variables
    real(SP), dimension(:,:), intent(in) :: data
    character(len=*), intent(in) :: file
    character(len=*), optional :: access, type
    character(len=*), dimension(:), optional :: column_labels, row_labels
    integer, optional :: sf

    ! Local variables
    integer, parameter  :: id = 101
    real(SP), parameter :: upper_bound = 10.0**4, lower_bound = 10.0**(-4)
    integer, dimension(:), allocatable :: column_widths
    character(len=:), allocatable :: fchar_label, fchar_long, fchar_short
    character(len=:), allocatable :: access_, type_, pad
    character(len=32) :: fmt
    integer :: nrows, ncols
    integer :: col_label_length, row_label_length, long, short, pad_length
    integer :: sf_, head
    integer :: i, j

    ! Manage optional variables
    if(present(access)) then
       access_ = access
    else
       access_ = "sequential"
    end if

    if(present(type)) then
       type_ = type
    else
       type_ = "normal"
    end if

    select case(type_)
    case("normal")
       pad = '  '
    case("csv")
       pad = ', '
    case("fancy")
       pad = ' | '
    end select
    pad_length = len(pad)

    if(present(column_labels)) then
       col_label_length = len(column_labels(1))
    else
       col_label_length = 0
    end if

    if(present(row_labels)) then
       row_label_length = len(row_labels(1))
    else
       row_label_length = 0
    end if

    if(present(sf)) then
       sf_ = sf
    else
       sf_ = 8
    end if

    nrows = size(data(:,1))
    ncols = size(data(1,:))

    ! Assign column widths
    allocate(column_widths(ncols))
    long  = sf_ + 7
    short = sf_ + 2
    do i = 1,ncols
       if(any(abs(data(:,i)) >= upper_bound) .OR. any(abs(data(:,i)) <= lower_bound)) then
          ! Account for numbers to be written in scientific notation if >= 10000
          column_widths(i) = long
       else
          column_widths(i) = short
       end if

       if (column_widths(i) < col_label_length) then
          column_widths(i) = col_label_length
       end if
    end do
    allocate(character(len = col_label_length) :: fchar_label)
    allocate(character(len = short) :: fchar_short)
    allocate(character(len = long) :: fchar_long)

    open(id, file = file, access = access_, action = "write")

    ! Start with a blank line if appending
    if(access_ == "append") write(id,*)
    
    ! Print column headers
    if(present(column_labels)) then
       write(fmt, *) '(a', row_label_length + pad_length, ')'
       
       if(type == "fancy" .AND. row_label_length .NE. 0) then
          write(id, fmt, advance='no') pad
       else if(row_label_length .NE. 0) then
          write(id, fmt, advance='no') ''
       end if
       
       do i = 1, ncols
          write(fmt,*) '(a', col_label_length, ',a', column_widths(i) - col_label_length + pad_length, ')'
          write(id, fmt, advance='no') column_labels(i), pad
       end do

       ! Do the fancy header line
       if(type_ == "fancy") then
          write(id,*)
          if(row_label_length .NE. 0) then
             write(id,'(a)', advance = "no") repeat("-", row_label_length)//"-+-"
          end if
          
          do i = 1, ncols - 1
             write(id,'(a)', advance = "no") repeat("-", column_widths(i))//"-+-"          
          end do
          write(id,'(a)') repeat("-", column_widths(i))//"-|"          
       else
          write(id,*)
       end if
       
    end if

    ! Print row labels and data
    do i = 1, nrows
       if(present(row_labels)) then
          write(fmt, *) '(a', row_label_length, ',a', pad_length, ')'
          write(id, fmt, advance = 'no') row_labels(i), pad
       end if

       ! Print columns
       do j = 1, ncols
          ! Generate format specifier
          if (abs(data(i,j)) >= upper_bound .OR. abs(data(i,j)) <= lower_bound) then
             write(fmt,*) '(es', column_widths(j), '.', sf_, ')'
          else
             ! Figure out how many spaces go in front of the decimal
             if(abs(data(i,j)) < 1.0) then
                head = 1
             else if(mod(int(data(i,j)), 10) == int(data(i,j))) then
                head = 1
             else if(mod(int(data(i,j)), 100) == int(data(i,j))) then
                head = 2
             else if(mod(int(data(i,j)), 1000) == int(data(i,j))) then
                head = 3
             else
                head = 4
             end if
             write(fmt,*) '(f', column_widths(j), '.', sf_ - head, ')'
          end if

          ! Left adjust in case of variable number width
          if(column_widths(j) == col_label_length) then
             write(fchar_label, fmt) data(i,j)
             write(id, '(a)', advance = 'no') adjustl(fchar_label)
          else if(column_widths(j) == long) then
             write(fchar_long, fmt) data(i,j)
             write(id, '(a)', advance = 'no') adjustl(fchar_long)
          else
             write(fchar_short, fmt) data(i,j)
             write(id, '(a)', advance = 'no') adjustl(fchar_short)
          end if

          write(fmt,*) '(a', pad_length, ')'
          write(id, fmt, advance='no') pad
       end do

       write(id,*)
    end do

    deallocate(column_widths)
    close(id)
    
  end subroutine Tabulate_SP

  subroutine Tabulate_int(data, file, access, column_labels, row_labels, sf, type)
    ! Input variables
    integer, dimension(:,:), intent(in) :: data
    character(len=*), intent(in) :: file
    character(len=*), optional :: access, type
    character(len=*), dimension(:), optional :: column_labels, row_labels
    integer, optional :: sf

    ! Local variables
    integer, parameter  :: id = 101
    integer, dimension(:), allocatable :: column_widths
    character(len=:), allocatable :: ichar_label, ichar_num
    character(len=:), allocatable :: access_, type_, pad
    character(len=32) :: fmt
    integer :: nrows, ncols
    integer :: col_label_length, row_label_length, width, pad_length
    integer :: sf_, head
    integer :: i, j

    ! Manage optional variables
    if(present(access)) then
       access_ = access
    else
       access_ = "sequential"
    end if

    if(present(type)) then
       type_ = type
    else
       type_ = "normal"
    end if

    select case(type_)
    case("normal")
       pad = '  '
    case("csv")
       pad = ', '
    case("fancy")
       pad = ' | '
    end select
    pad_length = len(pad)

    if(present(column_labels)) then
       col_label_length = len(column_labels(1))
    else
       col_label_length = 0
    end if

    if(present(row_labels)) then
       row_label_length = len(row_labels(1))
    else
       row_label_length = 0
    end if

    if(present(sf)) then
       sf_ = sf
    else
       sf_ = 8
    end if

    nrows = size(data(:,1))
    ncols = size(data(1,:))

    ! Assign column widths
    allocate(column_widths(ncols))
    width = sf_ + 1

    if (width < col_label_length) then
       column_widths(:) = col_label_length
    else
       column_widths(:) = width
    end if

    allocate(character(len = col_label_length) :: ichar_label)
    allocate(character(len = width) :: ichar_num)

    open(id, file = file, access = access_, action = "write")

    if(access_ == "append") write(id,*)

    ! Print column headers
    if(present(column_labels)) then
       write(fmt, *) '(a', row_label_length + pad_length, ')'

       if(type == "fancy" .AND. row_label_length .NE. 0) then
          write(id, fmt, advance='no') pad
       else if(row_label_length .NE. 0) then
          write(id, fmt, advance='no') ''
       end if

       do i = 1, ncols
          write(fmt,*) '(a', col_label_length, ',a', column_widths(i) - col_label_length + pad_length, ')'
          write(id, fmt, advance='no') column_labels(i), pad
       end do

       ! Do the fancy header line
       if(type_ == "fancy") then
          write(id,*)
          if(row_label_length .NE. 0) then
             write(id,'(a)', advance = "no") repeat("-", row_label_length)//"-+-"
          end if

          do i = 1, ncols - 1
             write(id,'(a)', advance = "no") repeat("-", column_widths(i))//"-+-"          
          end do
          write(id,'(a)') repeat("-", column_widths(i))//"-|"          
       else
          write(id,*)
       end if

    end if

    ! Print row labels and data
    do i = 1, nrows
       if(present(row_labels)) then
          write(fmt, *) '(a', row_label_length, ',a', pad_length, ')'
          write(id, fmt, advance = 'no') row_labels(i), pad
       end if

       ! Print columns
       do j = 1, ncols
          write(fmt,*) '(i', column_widths(j), ')'

          ! Left adjust in case of variable number width
          if(column_widths(j) == col_label_length) then
             write(ichar_label, fmt) data(i,j)
             write(id, '(a)', advance = 'no') adjustl(ichar_label)
          else
             write(ichar_num, fmt) data(i,j)
             write(id, '(a)', advance = 'no') adjustl(ichar_num)
          end if

          write(fmt,*) '(a', pad_length, ')'
          write(id, fmt, advance='no') pad
       end do

       write(id,*)
    end do

    deallocate(column_widths)
    close(id)

  end subroutine Tabulate_int

end module FancyIO

module Kinds
  implicit none
  integer, parameter :: DP = kind(1.0d0)
  integer, parameter :: SP = kind(1.0)
  
end module Kinds
