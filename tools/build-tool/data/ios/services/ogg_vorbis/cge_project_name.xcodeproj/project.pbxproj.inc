{ These strings will be pasted into the appropriate place of
  project.pbxproj in the final XCode project.

  TODO: We should make an XML format to define them,
  instead of include this file as Pascal sources.
  You should be able to modify these without
  recompiling the build tool, and adjusting
  the source code of ToolProject unit.
}

const
  Tremolo_IOS_EXTRA_PBXBuildFile =
'		02D2378C1F7AA7CB00B4CB35 /* README-castle-engine.txt in Resources */ = {isa = PBXBuildFile; fileRef = 02D2376B1F7AA7CB00B4CB35 /* README-castle-engine.txt */; };' + LineEnding +
'		02D2378D1F7AA7CB00B4CB35 /* bitwise.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D2376E1F7AA7CB00B4CB35 /* bitwise.c */; };' + LineEnding +
'		02D2378E1F7AA7CB00B4CB35 /* codebook.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D2376F1F7AA7CB00B4CB35 /* codebook.c */; };' + LineEnding +
'		02D2378F1F7AA7CB00B4CB35 /* dsp.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237731F7AA7CB00B4CB35 /* dsp.c */; };' + LineEnding +
'		02D237901F7AA7CB00B4CB35 /* floor0.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237741F7AA7CB00B4CB35 /* floor0.c */; };' + LineEnding +
'		02D237911F7AA7CB00B4CB35 /* floor1.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237751F7AA7CB00B4CB35 /* floor1.c */; };' + LineEnding +
'		02D237921F7AA7CB00B4CB35 /* floor_lookup.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237761F7AA7CB00B4CB35 /* floor_lookup.c */; };' + LineEnding +
'		02D237931F7AA7CB00B4CB35 /* framing.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237771F7AA7CB00B4CB35 /* framing.c */; };' + LineEnding +
'		02D237941F7AA7CB00B4CB35 /* info.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237781F7AA7CB00B4CB35 /* info.c */; };' + LineEnding +
'		02D237951F7AA7CB00B4CB35 /* mapping0.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D2377C1F7AA7CB00B4CB35 /* mapping0.c */; };' + LineEnding +
'		02D237961F7AA7CB00B4CB35 /* mdct.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D2377D1F7AA7CB00B4CB35 /* mdct.c */; };' + LineEnding +
'		02D237971F7AA7CB00B4CB35 /* misc.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237801F7AA7CB00B4CB35 /* misc.c */; };' + LineEnding +
'		02D237981F7AA7CB00B4CB35 /* res012.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237851F7AA7CB00B4CB35 /* res012.c */; };' + LineEnding +
'		02D237991F7AA7CB00B4CB35 /* vorbisfile.c in Sources */ = {isa = PBXBuildFile; fileRef = 02D237861F7AA7CB00B4CB35 /* vorbisfile.c */; };' + LineEnding +
'		02D2379A1F7AA7CB00B4CB35 /* tremolo.changelog.txt in Resources */ = {isa = PBXBuildFile; fileRef = 02D237881F7AA7CB00B4CB35 /* tremolo.changelog.txt */; };' + LineEnding +
'		02D2379B1F7AA7CB00B4CB35 /* tremolo.copying.txt in Resources */ = {isa = PBXBuildFile; fileRef = 02D237891F7AA7CB00B4CB35 /* tremolo.copying.txt */; };' + LineEnding +
'		02D2379C1F7AA7CB00B4CB35 /* tremolo.readme.txt in Resources */ = {isa = PBXBuildFile; fileRef = 02D2378A1F7AA7CB00B4CB35 /* tremolo.readme.txt */; };' + LineEnding +
'		02D2379D1F7AA7CB00B4CB35 /* tremolo.todo.txt in Resources */ = {isa = PBXBuildFile; fileRef = 02D2378B1F7AA7CB00B4CB35 /* tremolo.todo.txt */; };' + LineEnding +
'';

  Tremolo_IOS_EXTRA_PBXFileReference =
'		02D2376B1F7AA7CB00B4CB35 /* README-castle-engine.txt */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = text; path = "README-castle-engine.txt"; sourceTree = "<group>"; };' + LineEnding +
'		02D2376D1F7AA7CB00B4CB35 /* asm_arm.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = asm_arm.h; sourceTree = "<group>"; };' + LineEnding +
'		02D2376E1F7AA7CB00B4CB35 /* bitwise.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = bitwise.c; sourceTree = "<group>"; };' + LineEnding +
'		02D2376F1F7AA7CB00B4CB35 /* codebook.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = codebook.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237701F7AA7CB00B4CB35 /* codebook.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = codebook.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237711F7AA7CB00B4CB35 /* codec_internal.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = codec_internal.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237721F7AA7CB00B4CB35 /* config_types.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = config_types.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237731F7AA7CB00B4CB35 /* dsp.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = dsp.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237741F7AA7CB00B4CB35 /* floor0.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = floor0.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237751F7AA7CB00B4CB35 /* floor1.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = floor1.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237761F7AA7CB00B4CB35 /* floor_lookup.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = floor_lookup.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237771F7AA7CB00B4CB35 /* framing.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = framing.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237781F7AA7CB00B4CB35 /* info.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = info.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237791F7AA7CB00B4CB35 /* ivorbiscodec.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = ivorbiscodec.h; sourceTree = "<group>"; };' + LineEnding +
'		02D2377A1F7AA7CB00B4CB35 /* ivorbisfile.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = ivorbisfile.h; sourceTree = "<group>"; };' + LineEnding +
'		02D2377B1F7AA7CB00B4CB35 /* lsp_lookup.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = lsp_lookup.h; sourceTree = "<group>"; };' + LineEnding +
'		02D2377C1F7AA7CB00B4CB35 /* mapping0.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = mapping0.c; sourceTree = "<group>"; };' + LineEnding +
'		02D2377D1F7AA7CB00B4CB35 /* mdct.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = mdct.c; sourceTree = "<group>"; };' + LineEnding +
'		02D2377E1F7AA7CB00B4CB35 /* mdct.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = mdct.h; sourceTree = "<group>"; };' + LineEnding +
'		02D2377F1F7AA7CB00B4CB35 /* mdct_lookup.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = mdct_lookup.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237801F7AA7CB00B4CB35 /* misc.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = misc.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237811F7AA7CB00B4CB35 /* misc.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = misc.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237821F7AA7CB00B4CB35 /* ogg.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = ogg.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237831F7AA7CB00B4CB35 /* os.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = os.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237841F7AA7CB00B4CB35 /* os_types.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = os_types.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237851F7AA7CB00B4CB35 /* res012.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = res012.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237861F7AA7CB00B4CB35 /* vorbisfile.c */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.c; path = vorbisfile.c; sourceTree = "<group>"; };' + LineEnding +
'		02D237871F7AA7CB00B4CB35 /* window_lookup.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = window_lookup.h; sourceTree = "<group>"; };' + LineEnding +
'		02D237881F7AA7CB00B4CB35 /* tremolo.changelog.txt */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = text; path = tremolo.changelog.txt; sourceTree = "<group>"; };' + LineEnding +
'		02D237891F7AA7CB00B4CB35 /* tremolo.copying.txt */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = text; path = tremolo.copying.txt; sourceTree = "<group>"; };' + LineEnding +
'		02D2378A1F7AA7CB00B4CB35 /* tremolo.readme.txt */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = text; path = tremolo.readme.txt; sourceTree = "<group>"; };' + LineEnding +
'		02D2378B1F7AA7CB00B4CB35 /* tremolo.todo.txt */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = text; path = tremolo.todo.txt; sourceTree = "<group>"; };' + LineEnding +
'';

  Tremolo_IOS_EXTRA_PBXGroup =
'		02D2376A1F7AA7CB00B4CB35 /* tremolo */ = {' + LineEnding +
'			isa = PBXGroup;' + LineEnding +
'			children = (' + LineEnding +
'				02D2376B1F7AA7CB00B4CB35 /* README-castle-engine.txt */,' + LineEnding +
'				02D2376C1F7AA7CB00B4CB35 /* src */,' + LineEnding +
'				02D237881F7AA7CB00B4CB35 /* tremolo.changelog.txt */,' + LineEnding +
'				02D237891F7AA7CB00B4CB35 /* tremolo.copying.txt */,' + LineEnding +
'				02D2378A1F7AA7CB00B4CB35 /* tremolo.readme.txt */,' + LineEnding +
'				02D2378B1F7AA7CB00B4CB35 /* tremolo.todo.txt */,' + LineEnding +
'			);' + LineEnding +
'			path = tremolo;' + LineEnding +
'			sourceTree = "<group>";' + LineEnding +
'		};' + LineEnding +
'		02D2376C1F7AA7CB00B4CB35 /* src */ = {' + LineEnding +
'			isa = PBXGroup;' + LineEnding +
'			children = (' + LineEnding +
'				02D2376D1F7AA7CB00B4CB35 /* asm_arm.h */,' + LineEnding +
'				02D2376E1F7AA7CB00B4CB35 /* bitwise.c */,' + LineEnding +
'				02D2376F1F7AA7CB00B4CB35 /* codebook.c */,' + LineEnding +
'				02D237701F7AA7CB00B4CB35 /* codebook.h */,' + LineEnding +
'				02D237711F7AA7CB00B4CB35 /* codec_internal.h */,' + LineEnding +
'				02D237721F7AA7CB00B4CB35 /* config_types.h */,' + LineEnding +
'				02D237731F7AA7CB00B4CB35 /* dsp.c */,' + LineEnding +
'				02D237741F7AA7CB00B4CB35 /* floor0.c */,' + LineEnding +
'				02D237751F7AA7CB00B4CB35 /* floor1.c */,' + LineEnding +
'				02D237761F7AA7CB00B4CB35 /* floor_lookup.c */,' + LineEnding +
'				02D237771F7AA7CB00B4CB35 /* framing.c */,' + LineEnding +
'				02D237781F7AA7CB00B4CB35 /* info.c */,' + LineEnding +
'				02D237791F7AA7CB00B4CB35 /* ivorbiscodec.h */,' + LineEnding +
'				02D2377A1F7AA7CB00B4CB35 /* ivorbisfile.h */,' + LineEnding +
'				02D2377B1F7AA7CB00B4CB35 /* lsp_lookup.h */,' + LineEnding +
'				02D2377C1F7AA7CB00B4CB35 /* mapping0.c */,' + LineEnding +
'				02D2377D1F7AA7CB00B4CB35 /* mdct.c */,' + LineEnding +
'				02D2377E1F7AA7CB00B4CB35 /* mdct.h */,' + LineEnding +
'				02D2377F1F7AA7CB00B4CB35 /* mdct_lookup.h */,' + LineEnding +
'				02D237801F7AA7CB00B4CB35 /* misc.c */,' + LineEnding +
'				02D237811F7AA7CB00B4CB35 /* misc.h */,' + LineEnding +
'				02D237821F7AA7CB00B4CB35 /* ogg.h */,' + LineEnding +
'				02D237831F7AA7CB00B4CB35 /* os.h */,' + LineEnding +
'				02D237841F7AA7CB00B4CB35 /* os_types.h */,' + LineEnding +
'				02D237851F7AA7CB00B4CB35 /* res012.c */,' + LineEnding +
'				02D237861F7AA7CB00B4CB35 /* vorbisfile.c */,' + LineEnding +
'				02D237871F7AA7CB00B4CB35 /* window_lookup.h */,' + LineEnding +
'			);' + LineEnding +
'			path = src;' + LineEnding +
'			sourceTree = "<group>";' + LineEnding +
'		};' + LineEnding +
'';

  Tremolo_IOS_EXTRA_PBXGroup_MainGroup =
'				02D2376A1F7AA7CB00B4CB35 /* tremolo */,' + LineEnding +
'';

  Tremolo_IOS_EXTRA_PBXResourcesBuildPhase =
'				02D2379C1F7AA7CB00B4CB35 /* tremolo.readme.txt in Resources */,' + LineEnding +
'				02D2378C1F7AA7CB00B4CB35 /* README-castle-engine.txt in Resources */,' + LineEnding +
'				02D2379B1F7AA7CB00B4CB35 /* tremolo.copying.txt in Resources */,' + LineEnding +
'				02D2379D1F7AA7CB00B4CB35 /* tremolo.todo.txt in Resources */,' + LineEnding +
'				02D2379A1F7AA7CB00B4CB35 /* tremolo.changelog.txt in Resources */,' + LineEnding +
'';

  Tremolo_IOS_EXTRA_PBXSourcesBuildPhase =
'				02D237961F7AA7CB00B4CB35 /* mdct.c in Sources */,' + LineEnding +
'				02D2378E1F7AA7CB00B4CB35 /* codebook.c in Sources */,' + LineEnding +
'				02D237991F7AA7CB00B4CB35 /* vorbisfile.c in Sources */,' + LineEnding +
'				02D237901F7AA7CB00B4CB35 /* floor0.c in Sources */,' + LineEnding +
'				02D237931F7AA7CB00B4CB35 /* framing.c in Sources */,' + LineEnding +
'				02D237981F7AA7CB00B4CB35 /* res012.c in Sources */,' + LineEnding +
'				02D2378D1F7AA7CB00B4CB35 /* bitwise.c in Sources */,' + LineEnding +
'				02D237921F7AA7CB00B4CB35 /* floor_lookup.c in Sources */,' + LineEnding +
'				02D237971F7AA7CB00B4CB35 /* misc.c in Sources */,' + LineEnding +
'				02D237941F7AA7CB00B4CB35 /* info.c in Sources */,' + LineEnding +
'				02D237911F7AA7CB00B4CB35 /* floor1.c in Sources */,' + LineEnding +
'				02D2378F1F7AA7CB00B4CB35 /* dsp.c in Sources */,' + LineEnding +
'				02D237951F7AA7CB00B4CB35 /* mapping0.c in Sources */,' + LineEnding +
'';

  Tremolo_IOS_GCC_PREPROCESSOR_DEFINITIONS_DEBUG =
'				GCC_PREPROCESSOR_DEFINITIONS = (' + LineEnding +
'					"ONLY_C=1",' + LineEnding +
'					"DEBUG=1",' + LineEnding +
'					"$(inherited)",' + LineEnding +
'				);' + LineEnding +
'';

  Tremolo_IOS_GCC_PREPROCESSOR_DEFINITIONS_RELEASE =
'				GCC_PREPROCESSOR_DEFINITIONS = "ONLY_C=1";' + LineEnding +
'';
