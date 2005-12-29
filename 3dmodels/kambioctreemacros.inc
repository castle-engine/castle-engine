{ OCTREE_STEP_INTO_SUBNODES mechanism ----------------------------------------- }

{ kiedy robisz metode TTriangleOctreeNode ktora musi cos wykonac dla pewnych subnodes
  ktore zawieraja box3d to mozesz uzyc OCTREE_STEP_INTO_SUBNODES w
  nastepujacy sposob :
  - zdefiniuj proc lokalna
       procedure OCTREE_STEP_INTO_SUBNODES_PROC(
         subnode: TOctreeNode; var stop: boolean);
     Domyslnie stop = false, ustal stop = true aby przerwac
     wchodzenie w kolejne subnodes w tym momencie.
  - na poczatk proc w deklaracjach umiesc OCTREE_STEP_INTO_SUBNODES_DECLARE
  - gdzies w kodzie metody zainicjuj OSIS_Box
  - potem wywolaj makro OCTREE_STEP_INTO_SUBNODES
}

{$define OCTREE_STEP_INTO_SUBNODES_DECLARE:=
  var OSIS_b_low, OSIS_b_high: TOctreeSubnodeIndex;
      OSIS_Box: TBox3d;
      OSIS_Stop: boolean;
      OSIS_b_0, OSIS_b_1, OSIS_b_2: boolean;
  label OSIS_LoopEnd;
}
{$define OCTREE_STEP_INTO_SUBNODES:=

  SubnodesWithBox(OSIS_box, OSIS_b_low, OSIS_b_high);

  OSIS_Stop := false;
  for OSIS_b_0 := OSIS_b_low[0] to OSIS_b_high[0] do
   for OSIS_b_1 := OSIS_b_low[1] to OSIS_b_high[1] do
    for OSIS_b_2 := OSIS_b_low[2] to OSIS_b_high[2] do
    begin
     OCTREE_STEP_INTO_SUBNODES_PROC(
       TreeSubNodes[OSIS_b_0, OSIS_b_1, OSIS_b_2], OSIS_Stop);
     if OSIS_Stop then goto OSIS_LoopEnd;
    end;
  OSIS_LoopEnd:;
}