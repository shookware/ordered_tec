# Ordered_tec for Fortran

Ordered_tec is a simple pack writing ordered data (structural data) to a binary tecplot file (.plt). Ordered_tec for Fortran is written in Fortran2003/2008. This package does not need any dependency, and it directly write binary files according to Tecplot binary data format.

## Required
* [BeFoR64](https://github.com/szaghi/BeFoR64), Base64 encoding/decoding library for FoRtran poor men
* [PENF](https://github.com/szaghi/PENF), Portability Environment for Fortran poor people
* [StringiFor](https://github.com/szaghi/StringiFor), Strings Fortran Manipulator with steroids

### Note on updates
The local third party libraries can be updated by the end-user by means of the provided update.sh or updata_cn.sh script, see install section for more details.
## Features

### Basic Features
* Support 1, 2 and 3 dimensional data
* Support multi-zone file
* Support file types including grid, solution and full
* Support solution time and strand Id for zone
* Support auxiliary data for file and zone (Developing)

### Hightlight Features
1. Auto recognition the data type, supporting three types of data(Float, Double, longInt)
2. Support exclude data with `Begin`, `End` and `Skip`
3. Support auto expanding data

## Usage

### Install
| [Download](#download) | [Update the third party libraries](#update) | [Build libraries](#build) | [Build toolkit](#toolkit)
<a name='download'></a>
#### Download
you can clone it by git.
<a name='update'></a>
#### Update the third party libraries
The third party libraries are adoped in this code. Then, it can be update by running the srcipt update.sh. It can automatically download the libraries and update the source codes. The user in china can use update_cn.sh for a faster downloading.
<a name='build'></a>
#### Build libraries
Building here support the GNU make. Then, you can type `make tec_lib` in the terminal for building it.
<a name='toolkit'></a>
#### Build toolkit (TODO)
Under developing
### Coding
The Tecplot file consists of zones and data are storage in zone. Zones describe different set of data in space or in time. All the numbers of data in different zones in one file are same. Auxiliary data is extra data attached to file or zone which can be seen and used in Tecplot.

There are three classes usually used included in the `ORDERED_TEC` module. They are `TEC_FILE`, `TEC_ZONE` and `TEC_DATA`.

To use the package, first you need some  module declaration.
```fortran
use ORDERED_TEC
```

Then you need to declare a `TEC_FILE` object and set its properties.
```fortran
type(tec_file) :: tecfile !< Define the TEC_FILE class object

tecfile = tec_file(name='filename.plt',  &
              &    path='.',             &
              &    title='FileTitle',    &
              &    filetype='GRID')
```
The `filetype` keyword here can be used to set the type of the tecplot file. They are `GRID`, `SOLUTION` and `FULL`. the filetype must be given as a character type. And `GRID`, `grid`, `Grid` are all acceptable.

The property `Variables` is a string array containing the variables' name which is required. You can add the variables to the file with the procedure `AddVar`.
```fortran
call tecfile%AddVar('x')
call tecfile%AddVar('y')
call tecfile%AddVar('z')
```
or
```fortran
call tecfile%AddVar(['x', 'y', 'z'])
```
or
```fortran
call tecfile%AddVar(['x', 'y'])
call tecfile%AddVar('z')
```

You can also add the Auxiliary properties optionally.
```fortran
call tecfile%AddAuxiliaryData('Re', '1.0e7')
call tecfile%AddAuxiliaryData('Ma', 7.0d0)
```

Then attach a `TEC_ZONE` object to the file and set its properties.
```fortran
call tecfile%AddZone(tec_zone('zone_1'))
tecfile%zones(1)%shape=[10,20,30]
!! x, y, z are the data array, the size of all data is 10 by 20 by 30
call tecfile%zones(1)%AddData(tec_data(x)) !auto recognition point type
call tecfile%zones(1)%AddData(tec_data(y))
call tecfile%zones(1)%AddData(tec_data(z)) !tec_data() is used to generate a tec_data object
```
For Adding Data, you can use array structure as adding variables.
```fortran
call tecfile%zones(1)%AddData([tec_data(x), &
                               tec_data(y), &
                               tec_data(z)]) !auto recognition point type
```
or
```fortran
call tecfile%zones(1)%AddData([tec_data(x), &
                               tec_data(y)]) !auto recognition point type
call tecfile%zones(1)%AddData(tec_data(z))
```

You can also set some other properties optionally.
```fortran
tecfile%zones(1)%zoneName = 'zone_1'
tecfile%zones(1)%strandID = 0
tecfile%zones(1)%solutionTime = 0.1
tecfile%zones(1)%begin = [1,2,1] !begin of data from 2 in second dimension
tec_file%zones(1)%end = [20,0,0]  !end of data offset 2 in first dimension
tec_file%zones(1)%skip = [2,1,1]  !skip to write data by 2 in first dimension
```

And write data.
```fortran
call tecfile%WritePlt()
```
### Auto Expanding Data Feature
You can generate a `TEC_DATA` object with a different rank array by expanding it
to a property rank.
For example, you can expand 1D array `x(:)` to 2D array `[x(:,j), j=1, jn]`:
```fortran
type(tec_data) :: xCoor
real(8) :: x(20)
xCoor=tec_data(x, expand='j', dim=30)
```
`TEC_DATA` object `Xcoor` here has a 20 by 30 array data. With
```fortran
type(tec_data) :: yCoor
real(8) :: y(30)
yCoor=tec_data(y, expand='i', dim=20) ! 20 by 30 data.
```
then, you can generate a 2D grid use `x` and `y` array conveniently.
```fortran
call tecfile%zone(1)%AddData([Xcoor, Ycoor])
```

For 3D grid, you can use:
```fortran
!! Generate a 20 by 30 by 40 grid
tecfile%zones(1)%shape=[20, 30, 40]
call tecfile%zones(1)%AddData(tec_data(x, expand='jk', dim=[30, 40]))
call tecfile%zones(1)%AddData([tec_data(y, expand='ik', dim=[20, 40]), &
                               tec_data(z, expand='ij', dim=[20, 30])])
```
### TODO
* [x] Automatically checking the data adding for constructing the zone rank and shape.
* [ ] Some toolkit such as converting a PLOT3D file to a binary tecplot file.
* [ ] Testing Cases.
* [ ] Documents
* [ ] log feature (Maybe if I have time.)
## Contribution
You can just comment in issues or contact me by [email](mailto:shookware@tju.edu.cn) to talk about your idea or demand. Thank you.

## License
[MIT License](https://opensource.org/licenses/MIT)
