// TODO temp until we decide on compatibility mode

{$ifdef KEEP_OLD_VECTOR_API}

{$define TVector2SingleList := TVector4List}
{$define TVector3SingleList := TVector4List}
{$define TVector4SingleList := TVector4List}

{$define TVector2Double := TVector2d}
{$define TVector2DoubleList := TVector2dList}
{$define PVector2Double := PVector2d}
{$define Vector2Double := Vector2d}

{$define TVector3Double := TVector3d}
{$define TVector3DoubleList := TVector3dList}
{$define PVector3Double := PVector3d}
{$define Vector3Double := Vector3d}

{$define TVector4Double := TVector4d}
{$define TVector4DoubleList := TVector4dList}
{$define PVector4Double := PVector4d}
{$define Vector4Double := Vector4d}

{$define TMatrix2Double := TMatrix2d}
{$define TMatrix2DoubleList := TMatrix2dList}
{$define PMatrix2Double := PMatrix2d}
{$define Matrix2Double := Matrix2d}

{$define TMatrix3Double := TMatrix3d}
{$define TMatrix3DoubleList := TMatrix3dList}
{$define PMatrix3Double := PMatrix3d}
{$define Matrix3Double := Matrix3d}

{$define TMatrix4Double := TMatrix4d}
{$define TMatrix4DoubleList := TMatrix4dList}
{$define PMatrix4Double := PMatrix4d}
{$define Matrix4Double := Matrix4d}

{$define TVector2Byte := TVector2b}
{$define TVector2ByteList := TVector2bList}
{$define PVector2Byte := PVector2b}
{$define Vector2Byte := Vector2b}
{$define TArray_Vector2Byte := TArray_Vector2b}
{$define PArray_Vector2Byte := PArray_Vector2b}

{$define TVector3Byte := TVector3b}
{$define TVector3ByteList := TVector3bList}
{$define PVector3Byte := PVector3b}
{$define Vector3Byte := Vector3b}
{$define TArray_Vector3Byte := TArray_Vector3b}
{$define PArray_Vector3Byte := PArray_Vector3b}

{$define TVector4Byte := TVector4b}
{$define TVector4ByteList := TVector4bList}
{$define PVector4Byte := PVector4b}
{$define Vector4Byte := Vector4b}
{$define TArray_Vector4Byte := TArray_Vector4b}
{$define PArray_Vector4Byte := PArray_Vector4b}

{$define TVector2Cardinal := TVector2card}
{$define TVector2CardinalList := TVector2cardList}
{$define PVector2Cardinal := PVector2card}
{$define Vector2Cardinal := Vector2card}
{$define TArray_Vector2Cardinal := TArray_Vector2card}
{$define PArray_Vector2Cardinal := PArray_Vector2card}

{$define TVector3Cardinal := TVector3card}
{$define TVector3CardinalList := TVector3cardList}
{$define PVector3Cardinal := PVector3card}
{$define Vector3Cardinal := Vector3card}

{$define TVector4Cardinal := TVector4card}
{$define TVector4CardinalList := TVector4cardList}
{$define PVector4Cardinal := PVector4card}
{$define Vector4Cardinal := Vector4card}

{$define TVector2Integer := TVector2int}
{$define TVector2IntegerList := TVector2intList}
{$define PVector2Integer := PVector2int}
{$define Vector2Integer := Vector2int}

{$define TVector3Integer := TVector3int}
{$define TVector3IntegerList := TVector3intList}
{$define PVector3Integer := PVector3int}
{$define Vector3Integer := Vector3int}

{$define TVector4Integer := TVector4int}
{$define TVector4IntegerList := TVector4intList}
{$define PVector4Integer := PVector4int}
{$define Vector4Integer := Vector4int}

{$define TVector2SmallInt := TVector2sint}
{$define TVector2SmallIntList := TVector2sintList}
{$define PVector2SmallInt := PVector2sint}
{$define Vector2SmallInt := Vector2sint}

{$define TVector4Pointer := TVector4ptr}

{$endif KEEP_OLD_VECTOR_API}
