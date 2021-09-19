# M_history

A Fortran subroutine called redo(3f) may be used to give a line-mode
command history to interactive programs.

There is a stand-alone version in src-alt/ and an fpm-dependent version
in src/.

## BUILDING

```bash
    git clone https://github.com/urbanjost/M_history.git
    cd M_history/src-alt
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

## SUPPORTS FPM ![fpm](docs/images/fpm_logo.gif)
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

## DOCUMENTATION

### USER  
  + [M_history](https://urbanjost.github.io/M_history/M_history.html) as HTML
  + [M_history](md/redo.3.md) as markdown
  + [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### DEVELOPER
   + [ford(1) output](https://urbanjost.github.io/M_uuid/fpm-ford/index.html).
<!--
   + [doxygen(1) output](https://urbanjost.github.io/M_uuid/doxygen_out/html/index.html).
-->
   + [github action status](docs/STATUS.md) 
