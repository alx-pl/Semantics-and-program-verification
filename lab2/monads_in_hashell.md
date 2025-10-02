##### Ćwiczenie

Rozpakuj pliki z archiwum [Monady.tgz](https://moodle.mimuw.edu.pl/mod/resource/view.php?id=162342). Zorientuj się z zawartości rozpakowanego katalogu.

### Motywacja

W praktyce programistycznej często pojawia się problem taki, że wykonujemy przetwarzanie danych nie bezpośrednio na wartościach, ale na wartościach umieszczonych w jakimś szerszym kontekście (np. zmieniamy zawartość paska komunikatów w aplikacji okienkowej). Typowo taka operacja wymaga trzech kroków:

1. wypakowania interesującej nas wartości z kontekstu,
2. wykonania właściwego przekształcenia,
3. zapakowania przetworzonej wartości w punkcie kontekstu, z którego wyjęta była pierwotna wartość.

Tego typu działanie wymaga często wielokrotnego powtarzania tego samego kodu w punktach 1 i 3 powyżej. W Haskellu istnieje możliwość ukrycia tych operacji poprzez odpowiednie wprowadzenie tzw. monad.

### Monada IO

Przy programowaniu w Haskellu najłatwiej zetknąć się z monadą `IO`. Za pomocą tej monady możemy wykonywać operacje w kontekście operacji wejścia-wyjścia. Typowym przykładem funkcji, która korzysta z tej monady jest funkcja `main`:
```
main :: IO ()                                  -- Informujemy, że `main` jest bezparametrową funkcją o wyniku w typie `IO ()`
main = do                                      -- Definiujemy funkcję `main`, podając jej ciało po znaku =
                                               -- Słówko `do` oznacza, że poniżej znajduje się ciąg operacji monadycznych
  args <- getArgs                              -- Pobierz listę napisów będących argumentami wywołania i umieść wynik w zmiennej `args`
  case args of                                 -- Sprawdź, co zawiera lista
    ["--help"] -> usage                        -- Dla jednoelementowej listy `["--help]"` wypisz możliwe postaci wywołania (`usage`)
    []         -> getContents >>= run 2 pStmt  -- Dla pustej listy pobież zawartość standardowego wejścia (`getContents`)
                                               -- i na pobranej wartości zastosuj funkcję parsującą instrukcje Tiny
    "-s":fs    -> mapM_ (runFile 0 pStmt) fs   -- Jak wyżej, ale zamiast standardowego wejścia użyj pliku z `fs`
    fs         -> mapM_ (runFile 2 pStmt) fs   -- Jak wyżej, ale wypisuj trochę więcej informacji
```
Powyższy kod przedstawia funkcję `main` z programu *While/Test.hs*, którego używaliśmy na poprzednich zajęciach. Funkcja ta jest bezparametrowa i daje wynik w monadycznym typie `IO ()`, czyli monadzie `IO`, przechowującej na potrzeby bieżącego obliczenia wartość jednoelementowego typu `()`.

Monada `IO` jest przykładem monady, w której kontekst, o którym pisaliśmy, nie jest zwykłą strukturą danych, ale czymś nieco innym. Główna różnica, jaką tu mamy, to fakt, że kontekst wewnątrz tej monady może się zmieniać niezależnie od obliczeń, jakie wykonuje nasz program. W istocie należy się spodziewać, że każde odwołanie do tej monady oznacza, iż odwołujemy się do innego stanu tego kontekstu.

Operacje, których często używamy, pracując z monadą `IO` to:

* `putChar :: Char -> IO ()`
* `putStr :: String -> IO ()`
* `putStrLn :: String -> IO ()`
* `print :: Show a => a -> IO ()`
&nbsp;
* `getChar :: IO Char`
* `getLine :: IO String`
* `getContents :: IO String`
&nbsp;
* `readFile :: FilePath -> IO String`
* `writeFile :: FilePath -> String -> IO ()`

### Monady niskopoziomowo

Monady w Haskellu przenoszą na grunt tego języka matematyczne pojęcie *monady*, a ściślej mówiąc, tzw. *trójki Kleisliego* ([kilka słów historii](https://en.wikipedia.org/wiki/Monad_(functional_programming)#History)). W skład tej trójki wchodzą trzy elementy:

* Funktor *F : Types -> M( Types )*, który wkłada światy pochodzące z kategorii *Types* (w Haskellu z kategorii typów danych) do kategorii *M(Types)*, „wewnątrz monady *M*”. Przykładem takiego funktora jest `IO`, który wkłada dowolny typ danych Haskella, np. `()` lub `String` w ten sam typ w kontekście interakcji wejścia/wyjścia.
* Operacja *unit : D -> M(D)*, która dla dowolnego elementu typu danych *D* ze świata *Types* (czyli dla dowolnego typu) daje ten element zapakowany w kontekst monady *M(D)*. W Haskellu ta operacja jest nazywana `return` i ma typ
```
          return :: Monad m => a -> m a     -- Przy założeniu, że `m` jest monadą, daną typu `a` przekształć w tę samą daną umieszczoną
                                            -- w kontekście monady `m`, czyli w typie `m a`
```
* Operacja *bind : M(D) -> (D -> M(E)) -> M(E)*, która element danych typu *D* umieszczony w kontekście monady *M* przekształca w element danych typu *E* umieszczony w kontekście monady *M*. Przekształcenie, jakie jest tutaj wykonywane, jest określone za pomocą funkcji typu *D -> M(E)*. Operacja ta wykonuje wspomniane na początku tej lekcji wypakowywanie danej z kontekstu, a następnie zapakowywanie wyniku. W Haskellu ta operacja jest wprowadzona za pomocą operatora o symbolu `>>=` i ma typ
```
          (>>=) :: Monad m => m a -> (a -> m b) -> m b
```
Na co dzień powyższy operator jest nazywany *operatorem bind*.

##### Ćwiczenie

Przyjrzyj się kodowi w pliku *Monad0.hs*. Obecna tam funkcja `main` ma treść
```
main :: IO ()
main =
    getContents >>= \v -> putStrLn ("> " ++ v )
```
Kod ten pobiera zawartość standardowego wejścia i wypisuje je na na ekran, poprzedzając napisem `"> "`. Spróbuj dodać do tego kodu funkcję `return`. Jakie problemy wywoła dodanie `return "Ala ma kota"`?

### Notacja monadyczna z `do`

Bezpośrednie wielokrotne stosowanie operatora `>>=` może być niewygodne. Dlatego wprowadzona została wygodna notacja usprawniająca takie działanie. Kod powyższej funkcji `main` zapisany w tej notacji wygląda tak:
```
main :: IO ()
main = do
    v <- getContents
    putStrLn ("> " ++ v )
```
W ogólności ciąg operacji
```
val >>= \v1 -> op1(v1) >>= \v2 -> op2(v1, v2) >>= ... >>= \vn -> opn(v1,...,vn)
```
możemy zapisać w notacji `do` jako
```
  do { v1 <- val; v2 <- op1(v1); v3 <- op2(v1, v2); ...; vn <- opn-1(v1,...,vn-2); opn(v1,...,vn) }
```
lub z odpowiednim wcięciem i bez średników jako
```
  do 
    v1 <- val
    v2 <- op1(v1)
    v3 <- op2(v1, v2)
    ...
    vn <- opn-1(v1,...,vn-2)
    opn(v1,...,vn)
```


### Jak zdefiniować własną monadę?

Zwykle wpakowywanie i wypakowywanie czegoś do/z kontekstu zależy od konkretnego zastosowania. Dlatego warto umieć samemu stworzyć monadę, którą następnie można będzie efektywnie wykorzystać w swoim programie. Nabędziemy tej umiejętności na przykładzie. 

Wyobraźmy sobie, że w jakiejś części kodu chcemy używać danych w dwóch wersjach, bez podniesionej flagi i z podniesioną flagą. Naturalne jest w tym momencie wprowadzenie typu, który oprócz interesującej nas wartości zawiera wartość flagi. Często wykonywane operacje na tym typie – bez zmiany wartości flagi – można wyrazić zgrabnie w notacji monadycznej.

##### Ćwiczenie

Przyjrzyj się zawartości pliku *Monad3.hs*. W pliku tym mamy definicję indukcyjnego typu danych:
```
data Flagged a = FData a Bool
  deriving (Eq,Show)
```
(dopisek `deriving (Eq,Show)` oznacza, że dla tego typu będziemy korzystać z predefiniowanej równości, odpowiada za to nazwa `Eq`, oraz predefiniowanego sposobu wypisywania wartości tego typu, odpowiada za to nazwa `Show`). To jest właśnie typ, dla którego określmy operacje monadyczne.

We wcześniejszych wersjach Haskella monadę wprowadzało się bezpośrednio za pomocą definicji
```
instance Monad Flagged where
  return d = FData d False
  (FData d b) >>= f = f d
```
Definicja ta informuje nas, że typ `Flagged` traktowany jest od teraz jako funktor przekształcający dowolny typ w typ wewnątrz monady o tej samej nazwie (tzn. `Flagged`). Dalej widzimy naturalne definicje operacji `return` i operatora `>>=`.

Jednak wieloletnie doświadczenie w korzystaniu z monad podpowiedziało twórcom Haskella, że w ogromnej większości przypadków tworzenie monady wiąże się z utworzeniem dwóch dodatkowych tworów: instancji funktora i instancji struktury aplikacyjnej. W związku z tym, że monadę w naturalny sposób definiuje się z użyciem funktora i struktury aplikacyjnej, uznano, że do stworzenia monady konieczne jest najpierw zdefiniowanie typu danych jako funktora, a następnie jako struktury aplikacyjnej. Dlatego przed powyższym kodem wprowadzającym monadę musimy dodać następujący naturalny kod:
```
instance Functor Flagged where
  fmap f (FData d b) = FData (f d) b

instance Applicative Flagged where
  pure = \x -> FData x False
  (FData f b) <*> (FData d b') = FData (f d) (b || b') 
```
Zdefiniowaliśmy tutaj:

* Funkcję `fmap :: Functor f => (a -> b) -> f a -> f b`, która dostając przekształcenie danych typu `a` w dane typu `b` umie wykonać to samo przekształcenie ale „wewnątrz” typu funktorialnego. W naszym przypadku będzie to wewnątrz typu `Flagged`.
* Funkcję `pure :: Applicative f => a -> f a`, która ma funkcjonalność podobną do funkcji `return`, ale działa na potrzeby API klasy `Applicative`. 
* Operator infiksowy `(<*>) :: Applicative f => f (a -> b) -> f a -> f b`, który jeśli mamy umieszczoną w kontekście aplikacyjnym funkcję `(a -> b)` i mamy jakąś daną typu `a` też umieszczoną w kontekście aplikacyjnym, to możemy uzyskać wynik zastosowania tej operacji do tej danej umieszczony w kontekście aplikacyjnym.

W ten sposób uzbrojeni możemy zdefiniować sobie jakiś ciąg obliczeń wewnątrz monady `Flagged`:
```
someComputations :: Flagged String -> Flagged String
someComputations d =
  do
    v <- d
    w <- return (v ++ "FFF")
    raiseF w
```
Powyżej doklejamy do napisu wewnątrz monady ciąg `"FFF"`, a następnie podnosimy flagę.

##### Ćwiczenie

Spróbuj wykonać więcej obliczeń z użyciem tej monady. Zwróć uwagę, jak znajomość typów `return` i `>>=` pomaga Ci zrozumieć błędy wypisywane przez kompilator.

#### Dodatkowe operacje, które są dostępne w każdej monadzie:

* `(>>) :: Monad m => m a -> m b -> m b`

#### Warto wiedzieć, że monadami są...

* Typ `Maybe`
* Typ `List`


