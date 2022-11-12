module ringo_command_line
    use machina_vra
    use machina_string
    implicit none
    private

    !> save the user input command
    character(len=:), allocatable :: raw_cmd_input
    !> tokenized input command
    type(vra_char) :: cmd_tokens
    
contains


end module ringo_command_line