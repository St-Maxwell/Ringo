module ringo_born
    use ringo_env
    use machina_error
    implicit none
    private
    public :: ringo_launch

contains

    subroutine ringo_launch(input, output, nt, error)
        !> input file name
        character(len=*), intent(in) :: input
        !> output file name
        character(len=*), intent(in) :: output
        !> number of threads
        integer, intent(in) :: nt
        !> error handling
        type(error_t), intent(out) :: error
        ! local
        integer :: unit

        !> load input file to stirng
        call load_input_file(input, error)
        if (.has.error) return

        !> set global variables
        call open_std_output(output, error)
        if (.has.error) return

        call set_num_threads(nt)

        !> start timing
        call ringo_clock%start()

        !> print welcome message
        write(std_out,"(A)") ringo_welcome

    end subroutine ringo_launch

end module ringo_born
