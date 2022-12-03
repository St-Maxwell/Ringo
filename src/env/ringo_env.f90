module ringo_env
    use machina_string
    implicit none

    character(len=*), parameter :: ringo_version = &
        "Version:     0.0.1, alpha"//LF// &
        "Program:     ringo"//LF// &
        "Description: Ringo Is Not Gaussian/Orca"//LF// &
        "Home Page:   https://github.com/St-Maxwell/Ringo"//LF// &
        "License:     MIT"//LF

end module ringo_env
