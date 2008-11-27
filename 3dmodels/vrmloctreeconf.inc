{ zdefiniuj OCTREE_ITEM_USE_MAILBOX aby w TVRMLTriangle.*Collision uzywac
  wlasciwosci Mailbox* obiektow TVRMLTriangle. W ten sposob mozemy uniknac
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
{$define OCTREE_ITEM_USE_MAILBOX}
