### Instalacja BNFC

Nasze zajęcia będziemy prowadzić z użyciem narzędzia [BNFC](http://bnfc.digitalgrammars.com/). Narzędzie to pozwala na automatyzację generowania 

* wygodnej reprezentacji wewnętrznej programu,
* parsowania napisów na odpowiadające im wewnętrzne reprezentacje. 

###### Ćwiczenie

BNFC instalujemy za pomocą polecenia
```
# cabal install BNFC
```
W następnej kolejności należy dodać do ścieżki poszukiwania programów wykonywalnych miejsce, gdzie pojawił się program `bnfc`.
```
# export PATH=$HOME/.cabal/bin:$PATH
```
Warto też dodać to miejsce do `PATH` w skrypcie startowym powłoki systemowej (np. w pliku `.bashrc`).

### Gramatyki 

Pliki źródłowe programu BNFC mają rozszerzenie .cf. W dołączonym do lekcji [archiwum](https://moodle.mimuw.edu.pl/mod/resource/view.php?id=162339) znajduje się plik `While1.cf`, a w nim wpisana została część podanej na [wykładzie](https://www.mimuw.edu.pl/~tarlecki/teaching/semwer/slides/optiny.pdf) gramatyki:
```
e ::= N  |  x  |  e1 + e2  |  e1 * e2 | e1 − e2
```
Zapis tej gramatyki w BNFC różni się od powyższego zapisu. Oto główne różnice:

* Nazwa symbolu nieterminalnego `Expr` jest wieloliterowa. Mogłaby być jednoliterowa, ale w programach chcemy, aby identyfikatory niosły ze sobą trochę więcej informacji.
* Każdy symbol terminalny jest otoczony znakami cudzysłowu (np. `"+"`, `"*"`).
* Każdy przypadek gramatyki jest opisany w oddzielnym wierszu zakończonym znakiem `;`.
* Każdy przypadek gramatyki jest opatrzony dodatkową etykietą (np. `ENum`, `EVar` itd.)

Pełny zapis powyższej gramatyki w formacie BNFC ma zatem postać
```
ENum. Expr ::= Integer;
EVar. Expr ::= Ident;
EPlus. Expr ::= Expr "+" Expr;
EMul. Expr ::= Expr "*" Expr;
EMinus. Expr ::= Expr "-" Expr;
```
Warto tutaj nadmienić, że w BNFC istnieje pewien zestaw predefiniowanych kategorii syntaktycznych, dla których nie trzeba pisać osobnych definicji (no, chyba, że definicja dostarczona w programie nam nie pasuje). Tymi kategoriami są: `Integer`, `Double`, `Char`, `String`, `Ident`. Ich szczegółowe definicje można znaleźć w [dokumentacji programu BNFC](https://bnfc.readthedocs.io/en/latest/lbnf.html#abstract-syntax-conventions).

######  Ćwiczenie

Możemy teraz uruchomić program 
```
# bnfc -d -m While1.cf && make
```
po czym cieszyć się możliwością testowania uzyskanego narzędzia przetwarzania napisów na reprezentację wewnętrzną:
```
# echo "3+4" | While/Test
# echo "3+4+5" | While/Test
# echo "3+4*5+6" | While/Test
```
Uzyskane w ten sposób narzędzie ma jednak pewne wady. Zanim jednak zrozumiemy, na czym te wady polegają, zwróćmy uwagę na ich objawy. Po pierwsze wewnętrzna reprezentacja w ostatnim z powyższych przykładów jest nie taka, jaką byśmy chcieli mieć, a po drugie narzędzie BNFC podczas działania wypisuje na ekranie nieco niepokojącą linijkę
```
shift/reduce conflicts:  9
```
O tych wadach nieco więcej dowiemy się odpowiednio w następnych sekcjach *Drzewo składni abstrakcyjnej* i *Parsery LR(k)*.

### Drzewa składni abstrakcyjnej

Już na zajęciach z *Podstaw matematyki* na pierwszym roku studiów wspomniane było, że o wyrażeniach wygodnie jest myśleć jak o drzewach, których wierzchołki są etykietowane nazwami operacji, a potomkowie są ich argumentami. Na przykład wyrażenie `3+4` możemy sobie wyobrazić jako takie drzewo
```
  *
 / \
3   4
```
Patrząc w ten sposób, uzyskujemy bardzo wygodne wyidealizowane wyobrażenie tego, czym jest dane wyrażenie. Wyobrażenie to pozbawione jest różnych niejednoznaczności, jakie są związane z zapisem w postaci tekstu. 

Okazuje się, że gramatyki w dosyć naturalny sposób opisują takie drzewa. Z gramatykami związane są przecież drzewa wyprowadzenia. Wierzchołkom wewnętrznym takiego drzewa wygodnie możemy przypisać symbole nieterminalne, które są przekształcane na ciało konkretnego przypadku gramatyki. Z kolei zestaw symboli terminalnych w danym przypadku gramatyki stanowi dosyć wygodną reprezentację operacji, jaka ma być związana z danym wierzchołkiem. Nawiasem mówiąc, liście są też tutaj swego rodzaju „operacjami” – polegającymi na włożeniu napisu w wewnętrzną reprezentację wartości. Tę właśnie zależność wykorzystuje BNFC:
* symbole nieterminalne (np. `Expr`) służą do określania kategorii syntaktycznych, które mają swoją wyróżnioną reprezentację w implementacji, zwykle w postaci dedykowanego typu danych o odpowiedniej nazwie (np. `Expr`),
* konkretne produkcje (np. `EPlus. Expr ::= Expr "+" Expr;`) odpowiadają za generowanie konkretnych rodzajów węzłów-operacji (np. węzła "+").
W tym momencie jaśniejsze staje się, po co są te dodatkowe etykiety takie jak `EPlus` czy `ENum` – one służą do tego, żeby jakoś w kodzie nazwać stosowne wierzchołki drzewa. 

###### Ćwiczenie
Możemy przyjrzeć się teraz, jak wygląda kod definiujący wewnętrzną reprezentację  drzew wyrażeń arytmetycznych z naszego języka. Znajdziemy go w pliku `While/Abs.hs`. Ma on postać:
```
data Expr
    = ENum Integer
    | EVar Ident
    | EPlus Expr Expr
    | EMul Expr Expr
    | EMinus Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)
```
To jest w istocie opis typu drzew (czyli indukcyjnego typu danych) o nazwie `Expr` z wierzchołkami `ENum`, `EVar`, `EPlus`, `EMul` i `EMinus`. Wierzchołki `ENum` i `EVar` w swoim wnętrzu przechowują odpowiednio liczbę i identyfikator. Pozostałe zaś rodzaje wierzchołków prowadzą do dwóch wierzchołków potomnych, które są wyrażeniami. (Kodu od słówka `deriving` na razie nie wyjaśniamy, zrobimy to później).

(Zainteresowani mogą wygenerować kod w języku C:
```
# bnfc --c -m While1.cf
```
i zajrzeć do pliku `Absyn.h`, żeby się przekonać jak język C definiuje podobne drzewa za pomocą struktur i unii).

###### Zapis nawiasowy

Warto dodać, że drzewa dosyć naturalnie daje się jednoznacznie zapisać w notacji z użyciem nawiasów. Wystarczy po prostu wyrażenie każdego (właściwego) poddrzewa otoczyć nawiasami. Tego typu zapis znajdziemy w tym, co wypisywał nasz program testowy poniżej etykiety `[Abstract Syntax]`. I tak wyrażenie
```
EPlus (ENum 3) (ENum 4)
```
oznaczało drzewo dla wyrażenia `"3+4"`. Zwróćmy uwagę, że dla wyrażenia `"3 + 4 * 5 + 6"` dostaliśmy napis
```
EPlus (ENum 3) (EMul (ENum 4) (EPlus (ENum 5) (ENum 6)))
```
###### Ćwiczenie

Zobaczcie, jakiemu to drzewu odpowiada? Czy to jest prawidłowe drzewo?

### Parsery LR(k)

LR(k) to technika parsowania, w której parser przechodzi przez tekst wejściowy jeden raz i w trakcie tego przejścia rozpoznaje, czy tekst jest generowany przez odpowiednią gramatykę dla deterministycznego języka bezkontekstowego. W istocie ta technika polega na skonstruowaniu odpowiedniego automatu ze stosem równoważnego gramatyce i użyciu go do rozpoznania słowa oraz przy okazji wygenerowania drzewa składni abstrakcyjnej. Parsowanie odbywa się tutaj od lewa do prawa i od dołu drzewa składni do góry. Algorytm posługujący się automatem wykonuje dwa rodzaje operacji:

- SHIFT – to operacja wczytania kolejnego symbolu wejściowego i umieszczenie go na stosie,
- REDUCE – to operacja polegająca na spostrzeżeniu, że kilka symboli na stosie stanowi prawą stronę produkcji gramatyki i zastąpienie ich przez lewą stronę produkcji.

Liczba k w nazwie LR(k) oznacza maksymalną liczbę symboli, jakie trzeba wczytać, aby jednoznacznie określić, która z powyższych operacji ma zostać wykonana. 

Przyjrzyjmy się temu, jak mogłoby wyglądać parsowanie wyrażenia `"3+4*5"`, gdybyśmy przy naszej gramatyce zastosowali powyższe operacje SHIT i REDUCE.

1. Stos: `∅`;  do sparsowania: `3+4*5`; operacja: SHIFT (3)
2. Stos: `Integer(3)`; do sparsowania: `+4*5`; operacja: REDUCE (Expr ::= Integer)
3. Stos: `ENum(3)`; do sparsowania: `+4*5`; operacja: SHIFT (+)
4. Stos: `ENum(3), +`; do sparsowania: `4*5`; operacja: SHIFT (4)
5. Stos: `ENum(3), +, Integer(4)`; do sparsowania: `*5`; operacja: REDUCE (Expr ::= Integer)
6. Stos: `ENum(3), +, ENum(4)`; do sparsowania: `*5`; operacja: ?

O ile w dotychczasowych krokach nie mieliśmy wątpliwości, jaka operacja jest następna do wykonania, o tyle w punkcie 6. możemy się zastanowić, czy nie użyć operacji REDUCE i zastąpić 3 elementy na czubku stosu jednym nowym wyrażeniem

6. Stos: `ENum(3), +, ENum(4)`; do sparsowania: `*5`; operacja: REDUCE (Expr ::= Expr "+" Expr)

Czy też może wczytać kolejny symbol `*`

6. Stos: `ENum(3), +, ENum(4)`; do sparsowania: `*5`; operacja: SHIFT (*)

Oba te działania doprowadzą do pomyślnego ustalenia, że ciąg znaków `"3+4*5"` jest wyrażeniem:

6. Stos: `ENum(3), +, ENum(4)`; do sparsowania: `*5`; operacja: REDUCE (Expr ::= Expr "+" Expr)
7. Stos: `EPlus (ENum(3)) (ENum(4))`; do sparsowania: `*5`; operacja: SHIFT (*)
8. Stos: `EPlus (ENum(3)) (ENum(4)), *`; do sparsowania: `5`; operacja: SHIFT (5)
9. Stos: `EPlus (ENum(3)) (ENum(4)), *, Integer(5)`; do sparsowania: eof; operacja: REDUCE (Expr ::= Integer)
10. Stos: `EPlus (ENum(3)) (ENum(4)), *, ENum(5)`; do sparsowania: eof; operacja: REDUCE (Expr ::= Expr "*" Expr)
11. Stos: `EMul (EPlus (ENum(3)) (ENum(4))) (ENum(5))`; do sparsowania: eof; operacja: akceptuj

oraz w drugiej wersji:

6. Stos: `ENum(3), +, ENum(4)`; do sparsowania: `*5`; operacja: SHIFT (*)
7. Stos: `ENum(3), +, ENum(4), *`; do sparsowania: `5`; operacja: SHIFT (5)
8. Stos: `ENum(3), +, ENum(4), *, Integer(5)`; do sparsowania: `5`; operacja: REDUCE (Expr ::= Integer)
9. Stos: `ENum(3), +, ENum(4), *, ENum(5)`; do sparsowania: `5`; operacja: REDUCE (Expr ::= Expr "*" Expr)
10. Stos: `ENum(3), +, EMul (ENum(4)) (ENum(5))`; do sparsowania: `5`; operacja: REDUCE (Expr ::= Expr "+" Expr)
11. Stos: `EPlus (ENum(3)) (EMul (ENum(4)) (ENum(5)))`; do sparsowania: `5`; operacja: akceptuj

Jak widać dostajemy tutaj dwa różne drzewa składni
```
    *             +
   / \           / \
  +   5         3   *
 / \               / \
3   4             4   5
```
z których to drugie jest bardziej pożądane. Mamy tutaj zatem dwa problemy: po pierwsze w pierwotnej gramatyce ukryty jest pewien niedeterminizm dotyczący procesu parsowania, po drugie nie wszystkie drzewa składni, jakie opisuje nasza pierwotna gramatyka, odpowiadają naszym potrzebom i wymaganiom.

###### Ćwiczenie
Sprawdź, jakie drzewa składni tworzy skompilowany program `While/Test`. Zajrzyj do pliku `While/Par.info` i spróbuj się z niego dowiedzieć, jakie konfliktujące ze sobą operacje pojawiły się podczas generowania parsera. Spróbuj na podstawie eksperymentów zorientować się, jak zostały te konflikty rozwiązane.

### Gramatyka z priorytetami

Zauważyliśmy, że napisana w sposób bezpośredni gramatyka nie pozwala na parsowanie napisów zgodnie z obowiązującą powszechnie kolejnością wykonywania wyrażeń i prowadzi do niedeterminizmu w określaniu sposobu parsowania, który jest określany jako _konflikt shift/reduce_. Często stosowanym w praktyce rozwiązaniem jest wprowadzenie do gramatyki priorytetów parsowanych wyrażeń. W BNFC gramatyka wyrażeń z uwzględnieniem priorytetów wygląda tak (zob. plik `While2.cf`):
```
EPlus.   Expr0  ::= Expr0 "+" Expr1;
EMinus.  Expr0  ::= Expr0 "-" Expr1;
EMul.    Expr1 ::= Expr1 "*" Expr2; 
ENum.    Expr2 ::= Integer;
EVar.    Expr2 ::= Ident;
```
Dopisane do nazwy kategorii (tu `Expr`) numerki wskazują, jaki priorytet ma określona produkcja. Im wyższy priorytet, tym „chętniej” parser będzie wykonywał reduckję określonego rodzaju. W szczególności, widząc na wejściu symbol `*` i mając na stosie coś kategorii `Expr`, raczej wczyta (wykona SHIFT) ten symbol niż przekształci symbole ze szczytu stosu w wyrażenie (nie wykona REDUCE).

Dodatkowo w nowym pliku znajdziemy też produkcje z etykietą `_`:
```
_.       Expr2 ::= "(" Expr ")";
_.       Expr1 ::= Expr2;
_.       Expr0 ::= Expr1;
_.       Expr  ::= Expr0;
```
Produkcje z taką etykietą nie powodują powstania osobnych nazw dla oznaczonych przez nią rodzajów wyrażeń. Jest to bardzo uzasadnione dla wyrażenia otoczonego nawiasami – wyrażenie otoczone nawiasami nie powinno prowadzić do powstawania osobnego węzła w drzewie składni abstrakcyjnej. Wzięcie w nawiasy nie niesie za sobą konieczności wykonania jakiejś specjalnej operacji obliczeniowej w trakcie działania programu. Podobnie zmiana priorytetu wyrażenia na niższy nie wprowadza takiej operacji i też nie powinna prowadzić do powstania nowego węzła w drzewie składni abstrakcyjnej.

######  Ćwiczenie

Spróbuj stowrzyć pliki wykonywalne za pomocą opizu z pliku `While2.cf`. Sprawdź na kilku przykładach, jak tym razem wygląda parsowanie.

### Różne sposoby na wyrażenie tego samego

Czasami chcemy pozwolić korzystającemu z języka, aby zapisywał to samo znaczenie na wiele sposobów. Możemy chcieć na przykład skorzystać z bogactwa zestawu znaków UTF8 i pozwolić na zapisywanie nierówności jako `≤`, ale jednocześnie nie chcemy pozostawiać na lodzie biednych sierot (do których i ja należę), które nie umieją z klawiatury bezpośrednio takiego znaku wprowadzić i będą woleli staroświecki zapis nierówności jako `<=`. BNFC wspomaga takie działanie  przez wprowadzenie funkcji. Konstrukcję tę znajdziemy w pliku `While3.cf`:
```
BLeq.   BExpr1 ::= Expr "<=" Expr;
bleq.   BExpr1 ::= Expr "≤" Expr;
...
define bleq e1 e2 = BLeq e1 e2;
```
Widzimy tutaj etykietę `BLeq`, zaczynającą się z wielkiej litery. To oznacza, że BNFC dla tej produkcji wprowadzi osobny rodzaj węzła wewnętrznego drzewa składni abstrakcyjnej `BLeq`. Następnie w kodzie znajduje się etykieta `bleq`, zaczynająca się z małej litery. To oznacza, że BNFC nie wprowadzi tutaj nowego węzła drzewa składni, ale za to do stworzenia węzła odpowiadającego tej produkcji użyje funkcji, która nazywa się `bleq` i jest zdefiniowana w naszym pliku `.cf`. 

######  Ćwiczenie
Spróbuj poeksperymentować z parsowaniem, jakie wykonywane jest przez program `While/Test` wygenerowany z pliku `While3.cf`. Jakie masz obserwacje? Spróbuj zmodyfikować plik `While3.cf` i dodać do niego kilka produkcji z etykietą `BLeq` i `bleq`. Co się wtedy dzieje?

### Gramatyka dla pełnego Tiny

Plik `While4.cf` zawiera pełną gramatykę dla języka _Tiny_. Warto zwrócić tutaj uwagę na to, jak wygląda drzewo składni dla instrukcji sekwencji (czyli parsowanie w programach, których głównym operatorem jest `";"`). Warto sobie tutaj uświadomić, że operacja `;` jest operacją łączną. W przypadku operacji o tej własności często wymagane jest, aby wynik parsowania był łączny prawostronnie (większość wyrażenia znajduje się w drugim argumencie) 
```
  ;
 / \
a   ;
   / \
  b   ;
     / \
    c   d
```
lub lewostronnie (większość wyrażenia znajduje się w pierwszym argumencie):
```
      ;
     / \
    ;   d
   / \
  ;   c
 / \
a   b
```
W przypadku operatora sekwencjonowania instrukcji zwykle korzystniejsza jest łączność prawostronna, ale są operatory (np. implikacja), dla których właściwy jest inne rozwiązanie.

######  Ćwiczenie
Spróbuj poeksperymentować z parsowaniem operatora sekwencjonowania z łącznością lewostronną i prawostronną.

######  Skrócony `if`

Często w językach programowania pojawia się instrukcja `if` w wersji bez gałęzi `else`. Stosowna gramatyka dla języka Tiny z dodanym tego rodzaju wariantem parsowania znajduje się w pliku `While5.cf`. Jednak gramatyka ta, jak łatwo się przekonać, jest problematyczna. Rozwiązanie ich znajduje się w pliku `While6.cf`. 

W pliku `While6.cf` użyta została konstrukcja
```
internal SIf. Stmt2 ::= "if" BExpr "then" Stmt "else" Stmt;
```
która powoduje wprowadzenie osobnego rodzaju wierzchołka drzewa składni abstrakcyjnej (tutaj jest nim `SIf`), ale nie wprowadza do parsera bezpośrednio reguł redukcji, które w innym wypadku tutaj zostały wprowadzone. W naszym przypadku zastosowanie tego rozwiązania nie jest konieczne, ale czasami składnia docelowa używa tylko szczególnych form pewnego ogólnego wzorca, a czasami po prostu chcemy mieć w drzewie składni określone węzły, ale żadne wyrażenie ze składni „wpisywanej przez programistę” takich węzłów nie generuje (więcej o tym na następnych zajęciach).

######  Ćwiczenie
Jakie problemy stwarza gramatyka z pliku `While5.cf`? 
Jak zmieni się parsowanie, gdy w pliku `While6.cf` w przypadkach dla instrukcji `if` na końcu zamiast `Stmt1` damy `Stmt`? Dlaczego zmiana `Stmt` po `"then"` we wszystkich produkcjach na `Stmt1` nie rozwiąże problemów, jakie się w tym miejscu pojawiają?

### Dwa słowa o lekserze i jego powiązaniu z parserem

Częścią całego pakietu obsługującego parsowanie jest moduł zwany _lekserem_. Moduł ten grupuje znaki podawane na wejściu w tak zwane _leksemy_, czyli pewne większe zgrupowania znaków, które dadzą się rozpoznać za pomocą wyrażeń regularnych. Typowo leksemami są słowa kluczowe, literały liczbowe (to trochę co innego niż liczby), identyfikatory i symbole operatorów. Potem sam parser już operuje na niepodzielnym tokenie (np. `then`), a nie na ciągu znaków (np. `"then"`). 

W BNFC występuje bardzo naturalne usprawnienie, mianowicie w pliku definiującym lekser `While/Lex.x` słowa kluczowe są umieszczone w drzewie poszukiwań binarnych (zob. `resWords`), a następnie w pliku definiującym parser `While/Par.y` tekstowe reprezentacje tokenów (`%token`) są identyfikowane jako elementy tego drzewa. W pliku `While/Par.y` łatwo też zidentyfikujemy poszczególne produkcje gramatyki oraz tworzone przez nie drzewa składni abstrakcyjnej, np.:
```
  : Ident ':=' Expr { While.Abs.SAssgn $1 $3 }
```
mówi, że dla ciągu tokenów, który rozpoznaliśmy jako: identyfikator, znak przypisania, wyrażenie; budujemy drzewo składni abstrakcyjnej, którego typ jest zdefiniowany w module `While.Abs` i którego węzeł nazywa się `SAssgn`. Drzewo to ma dwóch potomków: zawartości pierwszego potomka bierze się z zawartości pierwszego elementu produkcji (`$1`), a zawartość drugiego z trzeciego elementu produkcji (`$3`). Zawartość drugiego elementu produkcji (czyli symbolu `:=`) nie służy do tworzenia żadnych poddrzew.

###### Ćwiczenie

Spróbuj zidentyfikować fragmenty pliku `While/Lex.x`  oraz pliku `While/Par.y`, które rozumiesz i te, których nie rozumiesz. 

### Materiały dodatkowe

* Więcej o parsowaniu LR(k) można dowiedzieć się ze [slajdów dra Marcina Benke](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2014/Notatki/11lr.pdf)
* Przystępny wykład o parsowaniu LR(k) znajduje się na [angielskojęzycznej Wikipedii](https://en.wikipedia.org/wiki/LR_parser)
* O parsowaniu zstępującym, ze szczególnym uwzględnieniem parsowania LL(k) – innym ważnym podejściu do parsowania programów – można dowiedzieć się ze [slajdów dra Marcina Benke](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2014/Notatki/10ll.pdf)
