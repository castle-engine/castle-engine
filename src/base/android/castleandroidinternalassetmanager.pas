{*
 * Copyright (C) 2010 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *}
 
{ @exclude Internal for the engine. }
unit CastleAndroidInternalAssetManager;

{$I castleconf.inc}

interface

uses ctypes;

type
  PAAssetManager = ^AAssetManager;
  AAssetManager = record end;

  AAssetDir = record end;

  AAsset = record end;

  PAAssetDir = ^AAssetDir;
  PAAsset = ^AAsset;
  Poff_t = ^coff_t;

  (* Available modes for opening assets  *)
const
  AASSET_MODE_UNKNOWN = 0;
  AASSET_MODE_RANDOM = 1;
  AASSET_MODE_STREAMING = 2;
  AASSET_MODE_BUFFER = 3;


(**
 * Open the named directory within the asset hierarchy.  The directory can then
 * be inspected with the AAssetDir functions.  To open the top-level directory,
 * pass in "" as the dirName.
 *
 * The object returned here should be freed by calling AAssetDir_close().
  *)
function AAssetManager_openDir(mgr: PAAssetManager; dirName: Pchar): PAAssetDir; cdecl; external;

(**
 * Open an asset.
 *
 * The object returned here should be freed by calling AAsset_close().
  *)
function AAssetManager_open(mgr: PAAssetManager; filename: Pchar; mode: cint): PAAsset; cdecl; external;

(**
 * Iterate over the files in an asset directory.  A NULL string is returned
 * when all the file names have been returned.
 *
 * The returned file name is suitable for passing to AAssetManager_open().
 *
 * The string returned here is owned by the AssetDir implementation and is not
 * guaranteed to remain valid if any other calls are made on this AAssetDir
 * instance.
  *)
function AAssetDir_getNextFileName(assetDir: PAAssetDir): Pchar; cdecl; external;

(**
 * Reset the iteration state of AAssetDir_getNextFileName() to the beginning.
  *)
procedure AAssetDir_rewind(assetDir: PAAssetDir); cdecl; external;

(**
 * Close an opened AAssetDir, freeing any related resources.
  *)
procedure AAssetDir_close(assetDir: PAAssetDir); cdecl; external;

(**
 * Attempt to read 'count' bytes of data from the current offset.
 *
 * Returns the number of bytes read, zero on EOF, or < 0 on error.
  *)
function AAsset_read(asset: PAAsset; buf: Pointer; count: csize_t): cint; cdecl; external;

(**
 * Seek to the specified offset within the asset data.  'whence' uses the
 * same constants as lseek()/fseek().
 *
 * Returns the new position on success, or (off_t) -1 on error.
  *)
function AAsset_seek(asset: PAAsset; offset: coff_t; whence: cint): coff_t; cdecl; external;

(**
 * Close the asset, freeing all associated resources.
  *)
procedure AAsset_close(asset: PAAsset); cdecl; external;

(**
 * Get a pointer to a buffer holding the entire contents of the assset.
 *
 * Returns NULL on failure.
  *)
function AAsset_getBuffer(asset: PAAsset): Pointer; cdecl; external;

(**
 * Report the total size of the asset data.
  *)
function AAsset_getLength(asset: PAAsset): coff_t; cdecl; external;

(**
 * Report the total amount of asset data that can be read from the current position.
  *)
function AAsset_getRemainingLength(asset: PAAsset): coff_t; cdecl; external;

(**
 * Open a new file descriptor that can be used to read the asset data.
 *
 * Returns < 0 if direct fd access is not possible (for example, if the asset is
 * compressed).
  *)
function AAsset_openFileDescriptor(asset: PAAsset; outStart, outLength: Poff_t): cint; cdecl; external;

(**
 * Returns whether this asset's internal buffer is allocated in ordinary RAM (i.e. not
 * mmapped).
  *)
function AAsset_isAllocated(asset: PAAsset): cint; cdecl; external;

implementation

end.
