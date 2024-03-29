[![](docs/images/M_history.gif)](https://urbanjost.github.io/M_history/fpm-ford/index.html)
# [M_history](https://urbanjost.github.io/M_history/man3.html)

A Fortran subroutine called redo(3f) may be used to give a line-mode
command history to interactive programs.

## Building

```bash
    git clone https://github.com/urbanjost/M_history.git
    cd M_history/src
    # change Makefile if not using one of the listed compilers
     
    # for gfortran
    make clean
    make F90=gfortran gfortran
     
    # for ifort
    make clean
    make F90=ifort ifort

    # for nvfortran
    make clean
    make F90=nvfortran nvfortran
```

This will compile the M_history module and build the example program 
in the app/ sub-directory using the source file M_history_standalone.f90.

## Supports FPM ![fpm](docs/images/fpm_logo.gif)
(registered at the [fpm(1) registry](https://github.com/fortran-lang/fpm-registry) )

Alternatively, download the github repository and build it with 
fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

```bash
     git clone https://github.com/urbanjost/M_history.git
     cd M_history
     fpm build
     fpm test
```

or just list it as a dependency in your fpm.toml project file.

```toml
     [dependencies]
     M_history        = { git = "https://github.com/urbanjost/M_history.git" }
```

## Documentation   ![docs](docs/images/docs.gif)

### User  
  + [M_history](https://urbanjost.github.io/M_history/M_history.3m_history.html) as HTML
  + [M_history](md/redo.3.md) as markdown
  + [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### Developer
  + [ford(1) output](https://urbanjost.github.io/M_history/fpm-ford/index.html).
<!--
  + [doxygen(1) output](https://urbanjost.github.io/M_history/doxygen_out/html/index.html).
-->
  + [github action status](docs/STATUS.md) 
