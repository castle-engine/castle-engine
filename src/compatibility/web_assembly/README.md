# WebAssembly units to make it more compatible with other platforms

These units have been adjusted to work for FPC WebAssembly (Wasi/Wasm32).

- generics.collections have been adjusted by Kagamma some time ago.

    Note: They don't contain latest changes from FPC, constref->const. 
    
    TODO: They should be updated to latest FPC.

    TODO: Ideally, we should submit FPC issue to just make these units available in FPC.

- fcl-image, paszlib and pasjpeg have been adjusted by and3md on 2023-03, with minimal effort just to make them compile (we needed to comment out "goto").

    It is unclear how much they are actually functional, we just made them compile now.

    TODO: Ideally, we should submit FPC issue to just make these units available in FPC. Or... not use them. We could use browser API to read images?