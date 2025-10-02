##### Ćwiczenie

Rozpakuj pliki z archiwum [SmallStep.tgz](https://moodle.mimuw.edu.pl/mod/resource/view.php?id=162344). Zorientuj się z zawartości rozpakowanego katalogu. Zajrzyj do pliku *Makefile*, aby zorientować się, co i jak będzie kompilowane.

### Pierwsze zetknięcie z semantyką małych kroków

Obejrzyjmy sobie program *Interpreter.hs*. Znajduje się w nim implementacja semantyki języka Tiny literalnie naśladująca semantykę podaną na wykładzie. Funkcja `main` pobiera dane ze standardowego wejścia i przekazuje je do funkcji `compute`. Ta z kolei sprawdza, czy wynik parsowania programu jest prawidłowy, a gdy taki jest wywołuje funkcję `closure`. Funkcja `closure` dokonuje rekurencyjnie domknięcia przechodniego relacji przekształcającej program zgodnie z jego semantyką, czyli wykonuje obliczenie kolejnych konfiguracji
```
γ0, γ1, . . . , γi, γi+1, . . . ,
```
takich że `γi ⇒ γi+1`. Sama relacja przepisywania jednej konfiguracji w następną jest wykonywana w funkcji `step`. Funkcja ta dokonuje przekształcenia abstrakcyjnego drzewa składni zgodnie z opisem wynikającym z semantyki. Obliczanie wartości wyrażeń arytmetycznych i boolowskich odbywa się w funkcjach `eE` i `eB` zgodnie z opisami tych funkcji, jakie były na wykładzie.

##### Ćwiczenie

Przyjrzyj się kodowi programu *Interpreter.hs*. Zwróć uwagę na sposób zdefiniowania typów funkcji `closure`, `step`, `eE`, `eB`. Przyjrzyj się, jak definiowane są poszczególne przypadki obsługiwane przez te funkcje. Zwróć uwagę na wykorzystanie typu `Either` do skonstruowania sumy rozłącznej zbiorów i wykorzystanie do budowania wartości takiej sumy konstruktorów `Left` i `Right`. Zwróć uwagę, co się dzieje, gdy następuje odwołanie do zmiennej, której wartość nie została wpisana do wartości typu `State`.

### Małe kroki konsekwentnie

Ideę przetwarzania obliczanego programu za pomocą małych kroków przekształcających format programu można rozszerzyć na wszystkie kategorie syntaktyczne (czyli przekształcać małymi krokami nie tylko instrukcje programu, ale też wyrażenia arytmetyczne i wyrażenia boolowskie). Zapisaną w ten sposób semantykę języka Tiny znajdziesz w programie *Interpreter2.hs*. 

##### Ćwiczenie

Przyjrzyj się kodowi programu *Interpreter2.hs*. Zwróć uwagę na sposób zdefiniowania funkcji `stepE` oraz `stepB`. Zwróć uwagę, jak funkcje te są wywoływane w funkcji `step`. Jakie istotne zmiany w funkcji `step` się pojawiły?

### Małe kroki i obsługa błędów

Przedstawione powyżej semantyki wykorzystywały rozwiązanie, w którym odwołanie do zmiennej, której nie ma w stanie, działa tak, jakby ta zmienna miała wartość 0. Można jednak mieć inne podejście do takiej sytuacji i uznać, że odwołanie do zmiennej, której nie ma w stanie, oznacza błąd. Taka wersja semantyki Tiny znajduje się w pliku *Interpreter3.hs*. 

##### Ćwiczenie

Przyjrzyj się kodowi programu *Interpreter3.hs*. Zwróć uwagę jak odwołanie do wartości zmiennej różni się od tego, co widać w pliku *Interpreter2.hs*. Zwróć uwagę, jak użyto monady `Maybe` do uproszczenia propagacji błędów.

##### Ćwiczenie

Współnie z całą grupą zajęciową napiszcie semantykę małych kroków dla języka o gramatyce:
```
S ::= x := e | skip | S1; S2 | if b then S1 else S2 | while b do S | repeat S until b | for x:=e1 to e2 do S | do e times S | do S while b
b ::= true | false | e1 ≤ e2 | ¬b | b1 ∧ b2
e ::= N | x | e1 + e2 | e1 ∗ e2 | e1 − e2
```
