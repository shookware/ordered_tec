#!/usr/bin/env bash
#
# File:        zoo_update.sh
#
# Description: Update the third library by means of fresh git clone of GitHub repositories
#              modified with @ref ZOO project.
# License:     GPL3+
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

# DEBUGGING
set -e
set -C # noclobber

# ZOO libraries list
#readonly ZAGHI_LIBRARIES=("BeFoR64" "FACE" "FLAP" "FLOw" "FiNeR" "FITTER" "forbear" "FORESEER" "FURY" "MORTIF" "PENF" "StringiFor" "VecFor" "VTKFortran")
readonly ZAGHI_LIBRARIES=("BeFoR64" "PENF")  #StringiFor must use a older commit if ifort is adopted.

# INTERNAL VARIABLES AND INITIALIZATIONS
readonly DOWNLOADS="downloads"
readonly SRC="src"
readonly ZAGHI="https://github.com/szaghi"
function clone () {
   # clone repository
   git clone $1/$2.git $DOWNLOADS/$2
}

function mv_lib () {
   # move library sources
   rm -rf $SRC/$1
   mkdir -p $SRC/$1
   mv $DOWNLOADS/$1/src/lib/* $SRC/$1/
}

rm -rf $DOWNLOADS
mkdir -p $DOWNLOADS

for lib in "${ZAGHI_LIBRARIES[@]}"
do
   echo "update $lib"
   clone $ZAGHI $lib
   mv_lib $lib
done

rm -rf $DOWNLOADS

exit 0
