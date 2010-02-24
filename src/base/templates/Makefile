#
#  Copyright 2002-2010 Michalis Kamburelis.
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

ALL_TARGETS=objectslist_1.inc objectslist_2.inc objectslist_3.inc objectslist_4.inc \
  objectslist_5.inc objectslist_6.inc objectslist_7.inc objectslist_8.inc \
  objectslist_9.inc objectslist_10.inc \
  dynarray_1.inc dynarray_2.inc dynarray_3.inc dynarray_4.inc \
  dynarray_5.inc dynarray_6.inc dynarray_7.inc dynarray_8.inc \
  dynarray_9.inc dynarray_10.inc \
  dynarray_11.inc dynarray_12.inc dynarray_13.inc dynarray_14.inc \
  dynarray_15.inc dynarray_16.inc dynarray_17.inc dynarray_18.inc \
  dynarray_19.inc dynarray_20.inc \
  dim2array_1.inc dim2array_2.inc dim2array_3.inc dim2array_4.inc \
  dim2array_5.inc dim2array_6.inc dim2array_7.inc dim2array_8.inc \
  dim2array_9.inc dim2array_10.inc \
  dim3array_1.inc dim3array_2.inc dim3array_3.inc dim3array_4.inc \
  dim3array_5.inc dim3array_6.inc dim3array_7.inc dim3array_8.inc \
  dim3array_9.inc dim3array_10.inc

.PHONY: all clean
all: $(ALL_TARGETS)
clean:
	rm -f $(ALL_TARGETS)

# Some notes about using sed: I'm assuming here that we have GNU sed.
SED_DO=sed --in-place --separate

# Ten skrypt bierze plik dynarray.inc i robi z niego plik dynarray_$1.inc
# ktorego tresc jest taka sama jak dynarray.inc ale typy (lub makra,
# zalezy jak na to patrzec) maja nazwy z _$1 np. zamiast TDynArray bedzie typ
# TDynArray_$1. W ten sposob mozemy uzyc jednoczesnie pliku dynarray.inc i
# dynarray_$1.inc w naszym programie aby zadeklarowac dwie tablice dynamiczne
# (co bylo naszym problemem pod Delphi, gdzie, nie mogac korzystac z makr,
# nie moglismy wiec uzyc dwa razy tego samego pliku dynarray.inc w jednym
# unicie).
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

objectslist_%.inc : ../objectslist.inc
	cp -f $< $@
	$(SED_DO) \
	  -e 's/\bTObjectsList\b/TObjectsList_'$*'/ig' \
	  -e 's/\bTObjectsListItem\b/TObjectsListItem_'$*'/ig' \
	  -e 's/\bTObjectsListIsSmallerFunction\b/TObjectsListIsSmallerFunction_'$*'/ig' \
	  $@

dim2array_%.inc: ../dim2array.inc
	cp -f $< $@
	$(SED_DO) \
	  -e 's/\bTDim2Array\b/TDim2Array_'$*'/ig' \
	  -e 's/\bTDim2ArrayItem\b/TDim2ArrayItem_'$*'/ig' \
	  -e 's/\bPDim2ArrayItem\b/PDim2ArrayItem_'$*'/ig' \
	  -e 's/\bDIM2ARRAY_IS_STRUCT\b/DIM2ARRAY_'$*'_IS_STRUCT/ig' \
	  -e 's/\bDIM2ARRAY_IS_INIT_FINI_TYPE\b/DIM2ARRAY_'$*'_IS_INIT_FINI_TYPE/ig' \
	  -e 's/\bDIM2ARRAY_RANGECHECKS\b/DIM2ARRAY_'$*'_RANGECHECKS/ig' \
	  $@

dim3array_%.inc: ../dim3array.inc
	cp -f $< $@
	$(SED_DO) \
	  -e 's/\bTDim3Array\b/TDim3Array_'$*'/ig' \
	  -e 's/\bTDim3ArrayItem\b/TDim3ArrayItem_'$*'/ig' \
	  -e 's/\bPDim3ArrayItem\b/PDim3ArrayItem_'$*'/ig' \
	  -e 's/\bDIM3ARRAY_IS_STRUCT\b/DIM3ARRAY_'$*'_IS_STRUCT/ig' \
	  -e 's/\bDIM3ARRAY_IS_INIT_FINI_TYPE\b/DIM3ARRAY_'$*'_IS_INIT_FINI_TYPE/ig' \
	  -e 's/\bDIM3ARRAY_RANGECHECKS\b/DIM3ARRAY_'$*'_RANGECHECKS/ig' \
	  $@
