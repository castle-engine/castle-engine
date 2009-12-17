{ Define TRIANGLE_OCTREE_USE_MAILBOX to use mailboxes for TVRMLTriangleOctree.

  W ten sposob mozemy uniknac
  wielokrotnego sprawdzania przeciecia tego samego trojkata z tym samym
  promieniem na skutek tego ze jeden trojkat moze trafic do wielu lisci.

  Kiedys zamiast MailboxSavedTag mialem MailboxState (empty, ray lub segment)
  i MailboxVector1, MailboxVector2 (:TVector3Single) ktore razem okreslaly
  co jest zapamietane w Mailboxie za pomoca 6*SizeOf(Single) + SizeOf(enum) =
  = 7*4 = 28 bajtow. Teraz to wszystko zamienilem na MailboxSaveTag: Int64
  a wiec 8 bajtow a wiec istotna oszczednosc pamieci i przede wszystkim
  czasu zarzadzania mailboxem (czyli czasu porownywania "czy mozemy
  uzyc maiboxa" i czasu uaktualniania mailboxa).

  Patrz 3dmodels/rayhunter-demos/new-mailbox/new-mailbox-raport.txt :
  przeciêtny zysk czasowy ze zdefiniowania tego symbolu dla drzew
  o¶emkowych w rodzaju max-depth = 10 i leaf-capacity = 20 wynosi
  1.09 (tzn.  stary czas dzia³ania / nowy czas dzia³ania = 1.09) }
{$define TRIANGLE_OCTREE_USE_MAILBOX}

{ Define SHAPE_OCTREE_USE_MAILBOX to use in TVRMLShapeOctree the
  mailbox method.
  This helps for TVRMLShapeOctree with a lot of shapes duplicated in leaves.

  E.g. caste's gate_final.wrl speed --- more than 2 times speedup
  with rayhunter using OctreeDynamicCollisions! (from 12.87 to 5.20).
  Tests show that this helps for any model where shapes are duplicated > 1.

  For models where shapes are not duplicated (like alien_mirror.wrl),
  this makes a slight slowdown (from to 1.38 to 1.49), but I think that's
  acceptable. Trivial models are fast enough anyway. And note that ray-tracers
  are free to use OctreeVisibleTriangles, not based on shapes,
  since the scene is static there. This is really only for interactive usage
  with OctreeDynamicCollisions and such.
}
{$define SHAPE_OCTREE_USE_MAILBOX}
