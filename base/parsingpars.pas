{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Parsing command-line arguments.)

  Terminology:

  @definitionList(
    @itemLabel Parameter
    @item(Command-line parameters are given directly by the OS to our
      program. Implementation of this unit obtains them from @link(Parameters).)

    @itemLabel Option
    @item(Options are encoded by the user as parameters.
      Examples:

      @unorderedList(
        @item(Command-line

          @preformatted(  view3dscene --navigation Walk)

          passes two parameters (@code(@--navigation) and @code(Walk))
          for view3dscene, and these two parameters form one option:
          @code(@--navigation=Walk).)

       @item(Command-line

          @preformatted(  view3dscene -hv)

          passes one parameter (@code(-hv)) for view3dscene,
          and inside this parameter two options are encoded:
          @code(-h) and @code(-v).)
      )
    )
  )

  Pewne zalety uzywania w ogole funkcji do parsowania parametrow (w rodzaju
  getopts czy mojego ParsePars) ponad robieniem parsowania argumentow za kazdym
  razem recznie w programie:

  @unorderedList(
    @item(Trudniej sie pomylic, nie trzeba juz pisac tych
      wszystkich Parameters.Delete(i, 1), ParDeleteAndGetNext(i), Inc(i).)

    @item(Uwzglednianie takich rzeczy jak specjalny parametr '@--' jest juz
      automatyczne i nie powoduje komplikacji w kodzie programu.)

    @item(Automatycznie zglaszamy bledy "zla opcja", np. teraz
        @preformatted(  view3dscene --whatever scena.wrl)
      spowoduje blad "invalid long option @--whatever" podczas gdy wczesniej
      powodowal blad "expected one file name as parameter", bo wczesniej
      view3dscene widzial "@--whatever" po prostu jako kolejna nazwe pliku i
      wydawalo mu sie ze dostaje w parametrach dwie nazwy pliku.

      Oczywiscie teraz moze sie wydawac to niedogodnoscia, bo np. kiedys
      jesli miales plik o nazwie "@--scene-wrl" (a wiec plik ktorego nazwa zaczyna
      sie od "-" i wyglada jak opcja) to mogles go podac view3dscene i
      view3dscene nie uzna tego za opcje, tzn.
        @preformatted(  view3dscene --scene-wrl)
      zadziala ok. Natomiast teraz
      @preformatted(  view3dscene --scene-wrl)
      spowoduje blad "wrong long option --scene-wrl" i musisz zrobic tak jak
      w standardowych UNIXowych programach:
        @preformatted(  view3dscene -- --scene-wrl)
      Na dluzsza mete obecne rozwiazanie jest jednak duzo bardziej eleganckie:
      parametr "@--" powoduje ze w 1-znaczny sposob wszystko co jest za nim
      nie jest traktowane jako opcja, wiec np. jesli masz plik o nazwie
      "@--camera-radius", tzn. jego nazwa nie tylko wyglada na opcje ale tez _jest_
      rzeczywista opcja view3dscene to wczesniej nie bylo zupelnie zadnego
      sposobu zeby przekazac ten plik jako parametr bo view3dscene zawsze widzial
      @--camera-radius jako opcje. Teraz natomiast
        @preformatted(  view3dscene -- --camera-radius)
      dziala jak nalezy. To sa moze drobne subtelnosci (w koncu rzadko spotyka
      sie pliki o nazwach zaczynajacych sie na "-", ktore na dodatek sa
      rzeczywistymi opcjami programu) ale sprawiaja one ze na pierwszy rzut oka
      widac ze uzyta semantyka parametrow jest pelna i pozwala wyrazic doslownie
      wszystko co kiedykolwiek ktos moglby chciec wyrazic.)

    @item(Kombinowanie krotkich opcji (-abc) tez jest robione tutaj
      automatycznie, nie odbywa sie kosztem komplikacji kodu programu.)
  )

  Nazewnictwo: wszedzie w tym module, w komentarzach i w identyfikatorach
  (typow, nazw funkcji, stalych... wszystkiego) staram sie konsekwentnie
  uzywac okreslenia "parametr" na parametr podany w linii polecen
  przez usera (a dokladniej rzecz biorac, to sa stringi ktore zostaly nam
  przekazane przez program ktory nas wywolal) oraz "opcja" na jakas
  regule wedlug ktorej zamierzamy parsowac (czyli po prostu interpretowac,
  wszelkie dokladniejsze definicje parsera nie maja tu racji bytu)
  parametry. Np. wywolanie
    @preformatted(  view3dscene --camera-radius 0.5 plik.3ds)
  ma 4 parametry : parametr numer 0 to "view3dscene" (chociaz niekoniecznie,
  to zalezy od OSa, np. Windows wymusza postac parametru numer 0),
  potem "@--camera-radius", potem "0.5", potem "plik.3ds".
  Jezeli teraz zasam regule "jest jedna opcja, @--camera-radius, i ma ona
  jeden argument" to mozemy teraz zinterpretowac parametry i otrzymujemy
  wynik: napotkana jedna opcja @--camera-radius, jej argument to 0.5,
  a poza tym zostaja dwa parametry ktore nie zostaly wlaczone do zadnej
  opcji :
    @preformatted(  view3dscene plik.3ds)
  Zadaniem interfejsu tego modulu jest przede wszystkim zdefiniowac scisle
  "czym jest opcja" oraz "jaki jest zwiazek miedzy parametrami a opcjami".
  Zadaniem implementacji jest przeprowadzic odpowiednia konwersje :
  wez parametry, wez opcje, wyrzuc wynik (sparsowane paczki "opcja+zwiazane
  z nia argumenty") usuwajac zinterpretowane parametry z listy parametrow.
  Takie nazewnictwo jest konsekwentne z getopts i z Pascalowymi ParamStr/Count.
}

unit ParsingPars;

{$I kambiconf.inc}

interface

uses SysUtils, VectorMath, KambiUtils;

{$define read_interface}

{ Niniejsza funkcja stanowi odpowiednik znanych funkcji z rodziny getopts z libc,
    modulu GetOpts z FPC i innych. Zasadniczymi powodami dla ktorych chcialem
    napisac ta funkcje (zamiast uzywac czy przynajmniej opakowac jakas wersje
    getopts) bylo:
    - chcialem zeby ParsePars sygnalizowalo bledy przez wyjatki
    - chcialem podawac short options i long options w jednej tablicy
      (zamiast, jak w getopts, podawac short i long options osobno w osobnych
      parametrach funkcji; getLongOpts tak dzialaly bo chcialy byc mozliwie podobne
      do oryginalnych getopts ktore dzialaly tylko na short oprtions, mi na tym
      podobienstwie nie zalezalo)
    - chcialem uzywac @link(Parameters) z mojego KambiUtils zamiast ParamStr/ParamCount
      lub argv/argc. Wole moje @link(Parameters) bo one pozwalaja mi wygodnie usuwac
      opcje przez Parameters.Delete etc. (co nie jest mozliwe pod Param* i
      co jest mozliwe ale niewygodne (i nie pod kazdym Pascalem/platforma)
      z argv/c)
    - chcialem tez miec mozliwosc podawania argumentow dla opcji jako
      "required separate arguments" czyli 2 lub wiecej argumentow ktore musisz
      podac po kolei jako osobne parametry (przyklad zastosowania - podawanie
      wektorow 3d dla view3dscene i rayhuntera; w ten sposob shell robi za nas
      parsowanie tych wektorow, tzn. gdybysmy wczytywali te wektory jako
      "required" to user musialby je podawac jako --wektor '1 2 3' lub
      --wektor='1 2 3' i 1) user musialby je cytowac 2) my musielibysmy je
      parsowac; zgoda, to parsowanie to zadna robota i wymaganie od usera
      cytowania to tez nic strasznego, zreszta generalnie przychylam sie do
      zdania programow GNU ze podawanie opcji uzywajac '=' lepiej wiaze
      opcje z jej argumentami; ale w tej sytuacji pozwalam sobie na wyjatek
      po prostu dlatego ze zrobilem to juz wiele razy, takze programy Radiance'a
      biora w ten sposob wektory (jako 3 osobne parametry))

  Funkcja ParsePars parsuje opcje zawarte w parametrach @link(Parameters).
    Krotkie opcje moga byc podawane po kilka w jednym parametrze,
    poprzedzone "-" (ale tylko ostatnia opcja w takim wypadku moze miec argument).
    Kazda long option musi byc podana osobno i poprzedzona "--".
    Jezeli opcja ma argument <> oaNone to (na potrzeby przykladu, niech
      Short = 's', Long = 'long-option', argument jaki chce podac user to
      'Argument') :
      oaOptional oznacza ze argument moze byc podany jako
        --long-option=Argument
        -<short-options-without-args>s gdzie <short-options-without-args>
        to ciag literek oznaczajacych dowolne krotkie opcje ktore zostana
        potraktowane jakby nie mialy argumentow (a wiec musza byc oaNone
        lub oaOptional).
      oaRequired oznacza ze dwie dodatkowe postacie sa mozliwe :
        --long-option Argument
        -<short-options-without-args>s Argument
        No i mozliwe sa takze te same postacie co dla oaOptional.
        Obydwie postacie dla oaRequired oznaczaja obecnosc _dwoch_ parametrow 
        w @link(Parameters).
      oaRequired?Separate oznacza ze mozliwe sa nastepujace postacie :
        --long-option Argument1 Argument2 ... Argument?
        -<short-options-without-args>s Argument1 Argument2 ... Argument?
        Obydwie te postacie oznaczaja obecnosc ?+1 parametrow w
        @link(Parameters).

  Wszystkie parametry zaczynajace sie od "-" (razem z ew. argumentami ich opcji)
    zostana przez ta funkcje odczytane i poslane do OptionProc. Jezeli wystapi
    nieznana (np. '-x' gdzie 'x' nie ma nigdzie w Short) krotka opcja
    rzucimy wyjatek EInvalidShortOption, nieznana dluga opcja oznacza
    EInvalidLongOption,  argument podany z uzyciem "=" dla parametru oaNone
    lub oaRequired?Separate spowoduje EExcessiveOptionArgument,
    brak argumentu dla opcji oaRequired lub za malo argumentow dla
    oaRequired?Separate spowoduja EMissingOptionArgument.
    Bledny parametr bez zadnej opcji (ale zaczynajacy sie od "-")
    (w rodzaju '-=argument', '--=argument') => EInvalidParams.
  Tutaj od razu uwaga : opcja '-' bedzie pozostawiona w spokoju,
    tzn. nie spowoduje wyjatku EInvalidParams, nie moze zostac w zaden sposob
    przekazana do OptionProc i nie bedzie tez usunieta przez Parameters.Delete.
    Moznaby myslec ze skoro zapisy '-=argument' i '--=argument' traktujemy
    jako bledy ("empty option") to podobnie nalezaloby potraktowac '-',
    ale robimy tu wyjatek i traktujemy '-' jako zwykly parametr, nie-opcje,
    bo istnieje konwencja ze '-' uzyte w parametrach w miejsce nazwy pliku
    oznacza stdin lub stdout. Tak wiec obie wersje pustych parametrow, '-'
    i '--' (patrz nizej) traktujemy wyjatkowo.

  Specjalny parametr "--" bedzie przez ParsePars zawsze czytany i usuwany z
    Pars i bedzie oznaczal ze wszystkie nastepne parametry NIE sa
    parametrami Params, nawet jesli zaczynaja sie od znaku "-".
    W ten sposob, parametr "--" bedzie oznaczal dla ParsePars ze skonczyly
    sie juz parametry ktore ma parsowac i moze zakonczyc dzialanie (pamietaj
    ze wobec tego podanie DWOCH parametrow "--" "--" spowoduje ze drugi
    z nich pozostanie w Pars po wywolaniu ParsePars(); to jest _dobrze_,
    bo to umozliwia userowi podanie jako parametr do mojego programu nawet
    takiego stringa jak "--".)

  Opcje (razem ze swoimi ew. argumentami) (oraz pierwszy parametr "--")
  zostana usuniete z parametrow przez Parameters.Delete
  (nie jest zdefiniwane w ktorym momencie - przed wywolaniem OptionProc
  dla tego argumentu, po, czy moze nawet dopiero po wywolaniu wszystkich
  OptionProc ? To dlatego ze w przypadku gdy short options beda kombinowane
  i podawane w postaci jednego parametru to byloby raczej ze szkoda
  dla programisty polegac na jakimkolwiek schemacie usuwania parametrow;
  kod w srodku OptionProc nie moze w zaden sposob operowac (nawet czytac,
  skoro to w ktorym momencie ParsePars usunie sparsowane parametry nie jest
  zdefiniowane) parametrow w @link(Parameters), tym bardzi nie moze tez
  ich usuwac przez Parameters.Delete; nie moze tez wywolywac ParsePars jako ze
  ParsePars samo operuje na @link(Parameters); jest chyba jasne ze ParsePars
  jest NON-REENTRANT i nigdy nie bedzie reentrant bo samo kasowanie argumentow
  z @link(Parameters) nigdy nie bedzie reentrant).

  Pozostale parametry (nie zaczynajace sie od "-" i nie bedace zadnymi
  argumentami i nie bedace "--") nie beda ruszane. Parametry za pierwszym
  "--" nigdy nie beda ruszane. Po wywolaniu ParsePars powinienes wiec
  przegladnac @link(Parameters) i odczytac te pozostale "zwykle" argumenty
  programu.

  Notka: Parameters[0] nigdy nie jest ruszany ani czytany. ParsePars parsuje
  tylko parametry 1..Parameters.High.

  Specjalny przypadek : gdy ParseOnlyKnownLongOptions = true funkcja
  ParsePars dziala nieco inaczej.
  1) Wszystkie krotkie opcje sa ignorowane (tzn. parametry zaczynajace sie
     od '-' ale nie od '--' sa traktowane jako nieopcje, w zwiazku z czym
     wartosci Options[].Short sa kompletnie bez znaczenia)
  2) Wszystkie dlugie opcje ktore nie sa rozpoznane sa ignorowane.
     OptionProc jest wywolywane tylko dla znanych dlugich opcji.
     Np. jezeli w Options mamy specyfikacje dlugiej opcji --kot i nie mamy
     --pies to wywolanie ParsePars dla parametrow rownych
      zero --kot --pies
     (zero to paremetr zerowy, zawsze ignorowany, wiec bez znaczenia)
     to --kot zostanie rozpoznany, poslany do OptionProc i usuniety z
     parametrow przez Parameters.Delete natomiast --pies spokojnie zostanie potraktowany
     jakby w ogole nie byl zadna dluga opcja. Po zakonczeniu ParseProc
     parametry beda wiec wygladaly tak:
       zero --pies
     Uwaga: inne bledy przy podawaniu dlugich parametrow, np. --=argument
     (empty option) ciagle beda wychwytywane.
  3) Specjalny parametr '--' takze ciagle bedzie wychwytywany i oznaczac
     bedzie koniec opcji. Tyle ze tym razem nie bedziemy go kasowac.
  Do czego to sie moze przydac ? Mianowicie przydaje sie to gdy chcemy
    napisac cos w rodzaju TGLWindow.ParsePars ktore parsuje _niektore_
    znane sobie parametry (np. --geometry, --fullscreen) a inne parametry
    (jak np. --cam-pos dla view3dscene) zostawia w spokoju. W ten sposob
    parsowanie parametrow mozna rozbic wykonujac je w kilku zupelnie roznych
    miejscach, np. view3dscene parsuje parametry w czterech miejscach :
    najpierw w TGLWindow.ParsePars, potem wywoluje MultiNavigatorParsePars
    i LightsKindParsePars (wszystkie te trzy funkcje uzywaja ParsePars
    z ParseOnlyKnownLongOptions = true), potem samo wywoluje ParsePars
    z ParseOnlyKnownLongOptions = false (a wiec dopiero na samym koncu
    nieznane long options sa wychwycone).
  Takie podejscie ma jedno istotne niebezpieczenstwo: poniewaz wykonujac
    nie-ostateczne ParsePars (np. jak to w TGLWindow) nie znamy specyfikacji
    wszystkich opcji wiec omylkowo mozemy rozpoznac nasza opcje
    w srodku argumentow opcji view3dscene. Np. przyjmijmy ze
    view3dscene ma opcje --three-strings ktory ma oaRequired3Separate.
    Wywolanie
      view3dscene --three-strings --geometry 800x600 foo bar ala kot
    spowoduje sparsowanie i zinterpretowanie najpierw --geometry 800x600
    a potem --three-strings dostaje argumenty : foo, bar, ala.
    Podczas gdy powinno byc tak ze --three-strings dostaje 3 argumenty
    --geometry, 800x600 i foo a potem view3dscene dostaje zwykle parametry
    bar, ala, kot. Widzimy wiec ze bledna interpretacja parametrow nawet nie
    spowodowala bledow w zadnym ParsePars (nawet jesli na pewno --three-strings
    lub view3dscene zorientuja sie ze dostali glupoty zamiast argumentow).
    Ten problem dziala na podobnej zasadzie jak SReplacePatterns :
    nie mozesz wykonac SReplacePatters zamieniajac po kolei najpierw jeden
    substring w calym ciagu, potem drugi itd. Musisz zamieniac je wszystkie
    _naraz_. Polowicznym rozwiazaniem tego problemu jest uzywac
    tylko opcji z oaNone, oaOptional, a dla oaRequired radzic userom
    zeby uzywali postaci z "=" (to jest wlasnie zaleta tego zapisu ponad
    zapisywaniem argumentow w oddzielnych parametrach). Mozna tez zapewnic
    sobie zeby wszystkie argumenty jakich chca wszystkie opcje nie moga zaczynac
    sie od - i --, nie gwarantujemy sobie tym ze wychwycimy wszystkie bledy
    jakie user moze popelnic (byc moze zinterpretujemy cos nieprawidlowo
    i zaczniemy dzialac z glupimi ustawieniami) ale gwarantujemy sobie
    w ten sposob ze przynajmniej _jesli_ user poda _poprawne_ opcje
    to zinterpretujemy je dobrze.
  Nie mozna bylo tego samego zrobic dla krotkich opcji (i miec cos w rodzaju
    ParseOnlyKnownOptions) bo usuwanie ze srodka kombinowanych opcji
    powodowaloby dodatkowe problemy. Np. niech -a ma oaRequired a -b ma oaNone.
    Wywolanie pierwszego ParsePars ktore wie tylko o -a a potem drugiego
    ktore wie tylko o -b dla
      -ab argument
    zwroci -a z argumentem 'argument' a potem -b, podczas gdy powinno dac blad.
    Jest to problem podobny jak ten przedstawiony wyzej z dlugimi opcjami
    ale, w przeciwienstwie do tamtego, ten nie ma nawet polowicznego
    rozwiazania.
}

type
  EInvalidShortOption = class(EInvalidParams);
  EInvalidLongOption = class(EInvalidParams);
  EWrongOptionArgument = class(EInvalidParams);
  EExcessiveOptionArgument = class(EWrongOptionArgument);
  EMissingOptionArgument = class(EWrongOptionArgument);

  TOptionArgument = (
    { opcja nie moze miec zadnych argumentow }
    oaNone,
    { opcja moze ale nie musi pobierac argumentu; jezeli user chce podac jej argument,
      musi uzyc postaci --opcja=argument (lub -o=argument) }
    oaOptional,
    { opcja musi byc podana z argumentem; user moze ja podac jako --opcja=argument
      (lub -o=argument) (a wiec tak jak oaOptional) lub jako --opcja argument
      (lub -o argument) (a wiec tak jak podawalby hipotetyczne oaRequired1Separate) }
    oaRequired,
    { oaRequired?Separate oznacza ze opcja musi byc podana z ? argumentami podanymi
      jako osobne parametry; np. oaRequired3Separate oznacza ze opcje trzeba podac
      jako --opcja argument1 argument2 argument3 (lub -o argument1 argument2 argument3).

      Jak widac nie ma oaRequired1Separate - bo to nie byloby uzyteczne, mamy oaRequired
      ktore jest bardziej elastyczne niz byloby hipotetyczne oaRequired1Separate
      (bo oaRequired pozwala na zapis --opcja=argument).

      Jak widac tez nie mozemy uzyc dowolnej liczby w miejsce ? - to NIE jest ograniczenie
      wynikajace z zaprojektowania typu TOptionArgument (bo gdybym chcial moglbym zrobic
      tutaj po prostu oaRequiredSeparate i dodac pole RequiredSeparateArgs: Integer
      do rekordu TOption), to jest ograniczenie zwiazane z tym ze TParsedOption zawiera
      tablice SeparateArgs o stalym rozmiarze. Gdyby mozliwe bylo dowolne ? to tablica
      SeparateArgs musialaby byc dynamiczna a wiec zarzadzanie typem TParsedOption byloby
      bardziej skomplikowane (w praktyce, uczynilbym go klasa aby zapewnic mu inicjalizacje/
      finalizacje). A wydaje mi sie ze ta dodatkowa komplikacja nie jest warta zachodu -
      - przeciez raczej nie bedzie ci nigdy zalezalo na podaniu jakiejs duzej wartosci
      dla ? bo wtedy podawanie takiego parametru byloby dla usera arcy-niewygodne.

      W razie potrzeby mozna rozszerzyc ten typ o dodatkowe oaRequired?Separate, dopisujac
      kolejne oaRequired?Separate za ostatnim oaRequiredSeparateLast i zmieniajac wartosc
      stalej oaRequiredSeparateLast. }
    oaRequired2Separate,
    oaRequired3Separate,
    oaRequired4Separate,
    oaRequired5Separate,
    oaRequired6Separate,
    oaRequired7Separate,
    oaRequired8Separate,
    oaRequired9Separate
  );

  TOptionArguments = set of TOptionArgument;

const
  oaRequiredSeparateFirst = oaRequired2Separate;
  oaRequiredSeparateLast = oaRequired9Separate;

  RequiredSeparateFirstCount = 2;
  RequiredSeparateLastCount = RequiredSeparateFirstCount
    + Ord(oaRequiredSeparateLast) - Ord(oaRequiredSeparateFirst);

  OptionArgumentsRequiredSeparate: TOptionArguments =
    [oaRequiredSeparateFirst .. oaRequiredSeparateLast];

type
  TOptionSeparateArgument = oaRequiredSeparateFirst .. oaRequiredSeparateLast;
  TSeparateArgs = array[1..RequiredSeparateLastCount]of string;

const
  EmptySeparateArgs: TSeparateArgs = ('','','', '','','', '','','');

type
  { Gdy ta funkcja bedzie wywolywana z funkcji ParsePars to znaczenie parametrow bedzie
    nastepujace :

      OptionNum = bedzie numer opcji z tablicy Options (zero-based).

      HasArgument = false, jesli Options[ParamNum].Argument
      in [oaNone, oaRequired?Separate] lub (oaOptional i nie podano argumentu).
      true wpp. (a wiec gdy Options[ParamNum].Argument = oaRequired lub
      (oaOptional i podano argument).

      Argument = jesli HasArgument to jest to argument podany przy opcji,
      wpp. '' (notka: zwroc uwage ze nic nie przeszkadza userowi podac argument = ''.
      Dlatego wlasnie potrzebny jest parametr HasArgument zeby opcje
      oaOptional mogly rozpoznac czy user nie podal argumentu (HasArgument=false)
      czy tez podal argument rowny '' (HasArgument = true, Argument = '').

      SeparateArgs = jezeli Options[ParamNum].Argument in oaRequired?Separate to
      odpowiednia ilosc poczatkowych SeparateArgs bedzie usatwiona na poczatkowe
      argumenty, np. dla oaRequired2Separate powinienes odczytac argumenty
      z SeparateArgs[1] i SeparateArgs[2]. Wszystkie pozostale SeparateArgs
      (w powyzszym przykladzie, SeparateArgs[3] .. SeparateArgs[High(SeparateArgs)])
      beda rowne ''. Jesli Options[ParamNum].Argument nie byl rowny oaRequired?Separate
      to wszystkie SeparateArgs[] beda rowne '' (czyli SeparateArgs = EmptySeparateArgs).

      Data = OptionProcData podane do ParsePars
  }
  TOptionProc = procedure (OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);

  TOption = record
    { Short moze byc rowne ' ' (spacji), Long moze zawierac w sobie spacje
      na dowolnych pozycjach, to nie powoduje zadnych problemow,
      ale nie jest zalecane (raczej nie ulatwialoby to zycia userowi ktory
      musialby wprowadzic opcje ze spacjami w srodku).
      Zarowno Short jak i Long beda dopasowywane case-sensitive, '-a' i '-A' to
      rozne opcje. }

    { jezeli Short = #0 to nie istnieje krotka postac opcji.
      Short nie moze byc takze '-' lub '=' (to powodowaloby wiele potencjalnych
      niejednoznacznosci) }
    Short: Char;
    { jezeli Long = '' to nie istnieje dluga postac opcji.
      Long nie moze zawierac znaku '=' (to powodowaloby pewne niejednoznacznosci) }
    Long: string;
    Argument: TOptionArgument;
  end;
  POption = ^TOption;

  TDynArrayItem_2 = TOption;
  PDynArrayItem_2 = POption;
  {$define DYNARRAY_2_IS_STRUCT}
  {$define DYNARRAY_2_IS_INIT_FINI_TYPE}
  {$I DynArray_2.inc}
  TDynOptionArray = TDynArray_2;
  TOption_Array = TInfiniteArray_2;
  POption_Array = PInfiniteArray_2;

procedure ParsePars(
  Options: POption_Array; OptionsCount: Integer;
  OptionProc: TOptionProc; OptionProcData: Pointer;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif}); overload;
procedure ParsePars(
  const Options: array of TOption;
  OptionProc: TOptionProc; OptionProcData: Pointer;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif}); overload;
procedure ParsePars(
  Options: TDynOptionArray;
  OptionProc: TOptionProc; OptionProcData: Pointer;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif}); overload;

{ Jeszcze inna wersja ParsePars, o nieco innym interfejsie }

type
  TParsedOption = record
    OptionNum: Integer;
    HasArgument: boolean;
    Argument: string;
    SeparateArgs: TSeparateArgs;
  end;
  PParsedOption = ^TParsedOption;

  TDynArrayItem_1 = TParsedOption;
  PDynArrayItem_1 = PParsedOption;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$I DynArray_1.inc}
  TDynParsedOptionArray = TDynArray_1;

function ParsePars(
  const Options: array of TOption;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif})
  : TDynParsedOptionArray; overload;
function ParsePars(
  Options: POption_Array; OptionsCount: Integer;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif})
  : TDynParsedOptionArray; overload;

{ some simple helper utilities ---------------------------------------------- }

function OptionSeparateArgumentToCount(const v: TOptionSeparateArgument): Integer;
function SeparateArgsToVector3Single(const v: TSeparateArgs): TVector3Single;

const
  OnlyHelpOptions: array[0..1]of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    { dodajemy tutaj dodatkowo pusta opcje tylko po to zeby obejsc bug FPC 1.0.10
      pod Windowsem, patrz InfoWriteBug (w fpc.testy/ lub
      kambi_fpc_bugs/already_fixed/). To bedzie zdjete gdy przejde na FPC 2.0. }
    (Short:  #0; Long: ''; Argument: oaNone)
  );

  HelpOptionHelp =
    '  -h / --help           Print this help message and exit';
  VersionOptionHelp =
    '  -v / --version        Print the version number and exit';

{$undef read_interface}

implementation

{$define read_implementation}

{$ifdef FPC}
procedure Initialize(var v: TOption); overload;
begin
 FillChar(v, SizeOf(v), 0);
end;

procedure Finalize(var v: TOption); overload;
begin
 v.Long := '';
end;

procedure Initialize(var v: TParsedOption); overload;
begin
 FillChar(v, SizeOf(v), 0);
end;

procedure Finalize(var v: TParsedOption); overload;
var i: Integer;
begin
 v.Argument := '';
 for i := Low(TSeparateArgs) to High(TSeparateArgs) do v.SeparateArgs[i] := '';
end;
{$endif}

{$I DynArray_1.inc}
{$I DynArray_2.inc}

procedure ParsePars(const Options: array of TOption; OptionProc: TOptionProc;
  OptionProcData: Pointer; ParseOnlyKnownLongOptions: boolean);
begin
 ParsePars(@Options, High(Options)+1, OptionProc, OptionProcData,
   ParseOnlyKnownLongOptions);
end;

procedure ParsePars(Options: TDynOptionArray; OptionProc: TOptionProc;
  OptionProcData: Pointer; ParseOnlyKnownLongOptions: boolean);
begin
 ParsePars(Options.Items, Options.Count, OptionProc, OptionProcData,
   ParseOnlyKnownLongOptions);
end;

procedure SplitLongParameter(const s: string; var ParamLong: string;
  var HasArgument: boolean; var Argument: string; PrefixLength: Integer);
{ zadany s musi sie zaczynac od PrefixLength znakow ktore sa ignorowane
  (dla "prawdziwej" long option z definicji ParsePars PrefixLength musi byc
  2 i musza one byc rowne '--').
  Rozbija parametr na nazwe parametru (nie zawierajaca znaku '=', rozna od '',
    bedzie wyjatek EInvalidParams w tym rzadkim przypadku gdy
    s[PrefixLength+1] = '=' lub gdy string sie konczy po PrefixLength znakach)
  i Argument, tzn. jezeli s nie zawieral znaku '=' zwraca HasArgument
  =false i Argument = '', wpp. ParamLong to czesc zawarta pomiedzy '--' a '=',
  HasArgument = true, Argument to czesc za pierwszyn znakiem '='
  (w ten sposob sam Argument moze bez problemu zawierac znak '=').
  Przyklady:
    s = '--long-option' ->
      ParamLong = 'long-option', HasArgument = false, Argument = ''
    s = '--long-option=arg' ->
      ParamLong = 'long-option', HasArgument = true, Argument = 'arg'
    s = '--' ->
      EInvalidParams
    s = '--=arg' ->
      EInvalidParams
}
var p: Integer;
begin
 p := Pos('=', s);
 HasArgument := p <> 0;
 if HasArgument then
 begin
  ParamLong := CopyPos(s, PrefixLength+1, p-1);
  Argument := SEnding(s, p+1);
 end else
 begin
  ParamLong := SEnding(s, PrefixLength+1);
  Argument := '';
 end;

 if ParamLong = '' then
  raise EInvalidParams.Create('Invalid empty parameter "'+s+'"');
end;

procedure ParsePars(Options: POption_Array; OptionsCount: Integer; OptionProc: TOptionProc;
  OptionProcData: Pointer; ParseOnlyKnownLongOptions: boolean);

  function ParseLongParameter(const s: string; var HasArgument: boolean;
    var Argument: string): Integer;
  { s jest jakims parametrem ktory zaczyna sie od '--' i nie jest rowny '--'.
    Wyciaga z s-a opcje jaka reprezentuje (i zwraca jej numer w Params,
    zero-based), wyciaga tez zapisany razem z nia parametr i zwraca
    HasArgument i Argument (pamietaj ze wyciaga tylko argumenty dolaczone
    do opcji przy pomocy znaku "="; nie sprawdza tez w ogole czy HasArgument
    w jakis sposob zgadza sie z Options[result].Argument.).

    Jezeli ParseOnlyKnownLongOptions to moze zwrocic -1 aby zaznaczyc ze
    ten parametr nie reprezentuje zadnej znanej opcji (chociaz ciagle
    nieprawidlowe postacie w rodzaju --=argument czy --non-arg-option=argument
    beda oczywiscie powodowaly wyjatek.) }
  var ParamLong: string;
      i: Integer;
  begin
   SplitLongParameter(s, ParamLong, HasArgument, Argument, 2);
   for i := 0 to OptionsCount-1 do
    if Options[i].Long = ParamLong then
     begin result := i; Exit; end;

   if ParseOnlyKnownLongOptions then
    result := -1 else
    raise EInvalidLongOption.Create('Invalid long option "'+s+'"');
  end;

  function FindShortOption(c: char; const Parameter: string): Integer;
  { znajdz takie i ze Options[i].Short = c (i c <> #0).
    Jesli sie nie uda - wyjatek EInvalidshortOption.
    Parametr "Parameter" jest nam potrzebny
    _tylko_ zeby skomponowac ladniejszy (wiecej mowiacy) Message wyjatku,
    podany Parameter powinien byc parametrem w ktorym znalezlismy literke c. }
  const
    SInvalidShortOpt = 'Invalid short option character "%s" in parameter "%s"';
  begin
   if c = #0 then
    raise EInvalidShortOption.CreateFmt(SInvalidShortOpt, ['#0 (null char)', Parameter]);

   for result := 0 to OptionsCount-1 do
    if Options[result].Short = c then Exit;

   raise EInvalidShortOption.CreateFmt(SInvalidShortOpt, [c, Parameter]);
  end;

  function ParseShortParameter(const s: string; var HasArgument: boolean;
    var Argument: string; SimpleShortOptions: TDynIntegerArray): Integer;
  { s jest jakims parametrem zaczynajacym sie od '-' i nie bedacym '-'.
    Dziala tak jak ParseLongParameter tyle ze nigdy nie zwraca -1
    (podany s MUSI zawierac znany parametr).

    Ponadto do SimpleShortOptions dopisze ciag prostych opcji ktore zostaly
    podane razem z ostatnia opcja (czyli z opcja zwracana pod nazwa).
    Te proste opcje zostaly "skombinowane" razem z ostatnia opcja w
    jednym parametrze. W rezultacie nazywam je "prostymi" bo one nie moga
    miec argumentu - Options[].Argument tych opcji moze byc tylko oaNone
    lub oaOptional. Ta procedura NIE sprawdza ze to sie zgadza
    tak jak w ogole nie sprawdza zadnego Options[].Argument, takze
    dla ostatniej (zwracanej pod nazwa) opcji nie sprawdza - moze wiec
    zwrocic opcje oaNone z HasArgument albo oaRequired[*Separate] z
    not HasArgument.

    Ten kto uzywa tej opcji musi sprawdzic czy HasArgument ma sens ze
    zwrocona opcja. W przypadku oaRequired[*Separate] moze/musi odczytac
    dalsze parametry zeby postac argument/argumenty opcji.
    Zasada jest taka ze ta procedura zajmuje sie TYLKO parametrem s.
    Ona nie wchodzi na inne Parameters[], zreszta w ogole nie wie dla jakiego
    I zachodzi Parameters[I] = s.
  }
  var ParamShortStr: string;
      i: Integer;
  begin
   { evaluate ParamShortStr, HasArgument, Argument }
   SplitLongParameter(s, ParamShortStr, HasArgument, Argument, 1);

   { add to SimpleShortOptions }
   for i := 1 to Length(ParamShortStr)-1 do
    SimpleShortOptions.AppendItem( FindShortOption(ParamShortStr[i], s) );

   { evaluate result }
   result := FindShortOption(ParamShortStr[Length(ParamShortStr)], s);
  end;

var i, j, k, OptionNum: Integer;
    HasArgument: boolean;
    Argument, OptionName: string;
    SeparateArgs: TSeparateArgs;
    SimpleShortOptions: TDynIntegerArray;
begin
 i := 1;
 SimpleShortOptions := TDynIntegerArray.Create;
 try

  while i <= Parameters.High do
  begin
   if Parameters[i] = '--' then
   begin
    if not ParseOnlyKnownLongOptions then Parameters.Delete(i, 1);
    Break
   end;

   Assert(SimpleShortOptions.Length = 0);

   { evaluate OptionNum; Ustaw je na numer w Params jezeli Parameters[i] to opcja
     (w tym przypadku musisz tez ustalic OptionName), wpp. (jesli to nie opcja
     i mozemy ja pominac) ustal OptionNum na -1.

     Warunek Length(Parameters[i]) > 1 w linijce ponizej gwarantuje nam 
     ze parametr '-' uznamy za nie-opcje (zamiast np. powodowac wyjatek 
     "empty option") }
   OptionNum := -1;
   if SCharIs(Parameters[i], 1, '-') and (Length(Parameters[i]) > 1) then
   begin
    if SCharIs(Parameters[i], 2, '-') then
    begin
     OptionNum := ParseLongParameter(Parameters[i], HasArgument, Argument);
     if OptionNum <> -1 then OptionName := '--'+Options[OptionNum].Long;
    end else
    if not ParseOnlyKnownLongOptions then
    begin
     OptionNum := ParseShortParameter(Parameters[i], HasArgument, Argument, SimpleShortOptions);
     OptionName := '-'+Options[OptionNum].Short;
    end;
   end;

   { OptionNum = -1 oznacza ze z jakiegos powodu Parameters[i] jednak NIE przedstawia
     soba zadnej opcji i powinnismy postepowac dalej jakby Parameters[i] byl
     normalnym parametrem, nie-opcja. W praktyce bylo nam to potrzebne
     bo gdy ParseOnlyKnownLongOptions = true to fakt ze chcemy dany Parameters[i]
     mozemy czasem odkryc dosc pozno, np. bedac w wywolaniu ParseLongParameter. }

   if OptionNum <> -1 then
   begin
    { najpierw zajmij sie SimpleShortOptions }
    for k := 0 to SimpleShortOptions.Length-1 do
    begin
     if not (Options[SimpleShortOptions[k]].Argument in [oaNone, oaOptional]) then
      raise EMissingOptionArgument.Create('Missing argument for short option -'+
        Options[SimpleShortOptions[k]].Short +'; when combining short options only the last '+
        'option can have an argument');
     OptionProc(SimpleShortOptions[k], false, '', EmptySeparateArgs, OptionProcData);
    end;
    SimpleShortOptions.Length := 0;

    { teraz zajmij sie opcja OptionNum o nazwie OptionName }

    Parameters.Delete(i, 1);
    SeparateArgs := EmptySeparateArgs;

    { upewnij sie ze HasArgument ma dopuszczalna wartosc. Odczytaj argumenty
      podane jako osobne paranetry dla oaRequired i oaRequired?Separate. }

    if (Options[OptionNum].Argument = oaRequired) and (not HasArgument) then
    begin
     if i > Parameters.High then
      raise EMissingOptionArgument.Create('Missing argument for option '+OptionName);
     HasArgument := true;
     Argument := Parameters[i];
     Parameters.Delete(i, 1);
    end else
    if (Options[OptionNum].Argument = oaNone) and HasArgument then
     raise EExcessiveOptionArgument.Create('Excessive argument for option '+OptionName) else
    if Options[OptionNum].Argument in OptionArgumentsRequiredSeparate then
    begin
     if HasArgument then
      raise EExcessiveOptionArgument.CreateFmt('Option %s requires %d arguments, '+
        'you cannot give them using the form --option=argument, you must give '+
        'all the arguments as separate parameters', [OptionName,
        OptionSeparateArgumentToCount(Options[OptionNum].Argument) ]);

     for j := 1 to OptionSeparateArgumentToCount(Options[OptionNum].Argument) do
     begin
      if i > Parameters.High then
       raise EMissingOptionArgument.CreateFmt('Not enough arguments for option %s, '+
         'this option needs %d arguments but we have only %d', [OptionName,
         OptionSeparateArgumentToCount(Options[OptionNum].Argument), j-1]);
      SeparateArgs[j] := Parameters[i];
      Parameters.Delete(i, 1);
     end;
    end;

    OptionProc(OptionNum, HasArgument, Argument, SeparateArgs, OptionProcData);
   end else
    Inc(i);
  end;

 finally SimpleShortOptions.Free end;
end;

procedure ParseNextParam(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var ParsedArray: TDynParsedOptionArray absolute Data;
    LastItem: PParsedOption;
begin
 ParsedArray.IncLength;
 LastItem := ParsedArray.Pointers[ParsedArray.High];
 LastItem.OptionNum := OptionNum;
 LastItem.HasArgument := HasArgument;
 LastItem.Argument := Argument;
 LastItem.SeparateArgs := SeparateArgs;
end;

function ParsePars(
  Options: POption_Array; OptionsCount: Integer;
  ParseOnlyKnownLongOptions: boolean)
  : TDynParsedOptionArray;
begin
 result := TDynParsedOptionArray.Create;
 try
  ParsePars(Options, OptionsCount, ParseNextParam, result,
    ParseOnlyKnownLongOptions);
 except result.Free; raise end;
end;

function ParsePars(
  const Options: array of TOption; ParseOnlyKnownLongOptions: boolean)
  : TDynParsedOptionArray;
begin
 result := ParsePars(@Options, High(Options)+1, ParseOnlyKnownLongOptions);
end;

{ some simple helper utilities ---------------------------------------------- }

function OptionSeparateArgumentToCount(const v: TOptionSeparateArgument): Integer;
begin
 result := RequiredSeparateFirstCount + Ord(v) - Ord(oaRequiredSeparateFirst)
end;

function SeparateArgsToVector3Single(const v: TSeparateArgs): TVector3Single;
begin
 result[0] := StrToFloat(v[1]);
 result[1] := StrToFloat(v[2]);
 result[2] := StrToFloat(v[3]);
end;

end.
