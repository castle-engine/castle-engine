#
#  Copyright 2002-2011 Michalis Kamburelis.
#
#  This file is part of "Kambi's base Pascal units".
#
#  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with "Kambi's base Pascal units"; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

# This Makefile creates all XXX_%d.inc files based on ../XXX.inc files.
# I use these files as class templates.

ALL_TARGETS=dynarray_1.inc dynarray_2.inc dynarray_3.inc dynarray_4.inc \
  dynarray_5.inc dynarray_6.inc dynarray_7.inc dynarray_8.inc \
  dynarray_9.inc dynarray_10.inc \
  dynarray_11.inc dynarray_12.inc dynarray_13.inc dynarray_14.inc \
  dynarray_15.inc dynarray_16.inc dynarray_17.inc dynarray_18.inc \
  dynarray_19.inc dynarray_20.inc

.PHONY: all clean
all: $(ALL_TARGETS)
clean:
	rm -f $(ALL_TARGETS)

# Some notes about using sed: I'm assuming here that we have GNU sed.
SED_DO=sed --in-place --separate

# Use dynarray.inc to generate a copy dynarray_xxx.inc, with the same contents
# but all types and defines get the _xxx suffix.
# This way you can instantiate the dynarray template many times within the same unit
# (otherwise you would get conflicts on duplicate types like TDynArray).
dynarray_%.inc: ../dynarray.inc
	cp -f $< $@
	$(SED_DO) \
	  -e 's/\bTDynArray\b/TDynArray_'$*'/ig' \
	  -e 's/\bTDynArrayItem\b/TDynArrayItem_'$*'/ig' \
	  -e 's/\bPDynArrayItem\b/PDynArrayItem_'$*'/ig' \
	  -e 's/\bTDynArrayItemIsSmallerFunc\b/TDynArrayItemIsSmallerFunc_'$*'/ig' \
	  -e 's/\bTDynArrayItemIsSmallerFuncByObject\b/TDynArrayItemIsSmallerFuncByObject_'$*'/ig' \
	  -e 's/\bTInfiniteArray\b/TInfiniteArray_'$*'/ig' \
	  -e 's/\bPInfiniteArray\b/PInfiniteArray_'$*'/ig' \
	  -e 's/\bDYNARRAY_IS_STRUCT\b/DYNARRAY_'$*'_IS_STRUCT/ig' \
	  -e 's/\bDYNARRAY_IS_INIT_FINI_TYPE\b/DYNARRAY_'$*'_IS_INIT_FINI_TYPE/ig' \
	  -e 's/\bDYNARRAY_USE_EQUALITY_OP\b/DYNARRAY_'$*'_USE_EQUALITY_OP/ig' \
	  -e 's/\bDYNARRAY_USE_EQUALITY\b/DYNARRAY_'$*'_USE_EQUALITY/ig' \
	  -e 's/\bDYNARRAY_USE_EQUALITY_COMPAREMEM\b/DYNARRAY_'$*'_USE_EQUALITY_COMPAREMEM/ig' \
	  -e 's/\bDYNARRAY_IS_FUNCTION\b/DYNARRAY_'$*'_IS_FUNCTION/ig' \
	  -e 's/\bDYNARRAY_IS_FUNCTION_METHOD\b/DYNARRAY_'$*'_IS_FUNCTION_METHOD/ig' \
	  -e 's/\bDYNARRAY_USE_PLUSMINUS_OP\b/DYNARRAY_'$*'_USE_PLUSMINUS_OP/ig' \
	  $@
