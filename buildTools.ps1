cd tools/build-tool/ # first enter the build tool directory
Set-ExecutionPolicy Bypass -Scope Process
./castle-engine_compile.ps1


$LAZBuildPath = "D:\soft\fpcupdeluxe\lazarus\lazbuild.exe"

copy tools/build-tool/data/external_libraries/x86_64-win64/*.dll Bin/
copy tools/build-tool/data/external_libraries/x86_64-win64/openssl/*.dll Bin/
copy tools/build-tool/castle-engine.exe Bin/

& $LAZBuildPath "tools/castle-editor/castle_editor.lpr"
& $LAZBuildPath "tools/castle-curves/castle-curves.lpr"
& $LAZBuildPath "tools/image-to-pascal/image-to-pascal.lpr"
& $LAZBuildPath "tools/internal/x3d-nodes-to-pascal/x3d-nodes-to-pascal.lpr"
& $LAZBuildPath "tools/texture-font-to-pascal/texture-font-to-pascal.lpr"
& $LAZBuildPath "tools/to-data-uri/to-data-uri.lpr"

& $LAZBuildPath "../castle-view-image/castle-view-image.lpi"
& $LAZBuildPath "../view3dscene/view3dscene.lpr"
& $LAZBuildPath "../view3dscene/tovrmlx3d.lpr"




copy tools/castle-curves/castle-curves.exe Bin/
copy tools/castle-editor/castle-editor.exe Bin/
copy tools/image-to-pascal/image-to-pascal.exe Bin/
copy tools/internal/x3d-nodes-to-pascal/x3d-nodes-to-pascal.exe Bin/
copy tools/texture-font-to-pascal/texture-font-to-pascal.exe Bin/
copy tools/to-data-uri/to-data-uri.exe Bin/

copy ../castle-view-image/castle-view-image.exe Bin/
copy ../view3dscene/view3dscene.exe Bin/
copy ../view3dscene/tovrmlx3d.exe Bin/

pause