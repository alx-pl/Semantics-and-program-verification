##### Ćwiczenie

Rozpakuj pliki z archiwum [Denotational1.tgz](https://moodle.mimuw.edu.pl/mod/resource/view.php?id=162347). Zorientuj się z zawartości rozpakowanego katalogu. Zajrzyj do pliku *Makefile*, aby zorientować się, co i jak będzie kompilowane.

#### Semantyka denotacyjna

Semantyka denotacyjna polega na definiowaniu funkcji, która wartościom z kategorii syntaktycznych przypisuje wartości semantyczne. W pliku *Interpreter.hs* znajduje się zapis takiej semantyki w języku Haskell. Warto porównać użycie obecnej tam funkcji `iS` z użyciem funcji `step` we wcześniejszych wersjach programu (zob. zajęcia dotyczące semantyki małych kroków). Warto porównać też definicję tej funkcji z definicją funkcji `step` we wcześniejszych wersjach.

#### Semantyka while.

Rozważmy na tablicy jak powinna działać semantyka pętli while:
I ⟦ while b do I ⟧ = X
gdzie X to taka funkcja State → State, że zachodzi:

X = 𝜆  s → if B⟦ b ⟧ s then X (I⟦ I ⟧ s) else s

Teraz pytanie czy to już specyfikuje jednoznacznie semantykę while? No nie, bo jest to równanie na X. Rozważmy dwa przypadki:

* I<sub>0</sub> == while true do skip
* I<sub>1</sub> == while x > 0 do x := x - 1

W przypadku instrukcji I<sub>0</sub> równanie na X przybiera postać:

X = 𝜆  s → if true then X ( I⟦ skip ⟧ s ) else skip = 𝜆 s → X s = X

Ile funkcji częściowych X : State → State spełnia to równanie? Otóż KAŻDA! W szczególności funkcja która każdy stan mapuje na taki w którym wszystkie zmienne mają wartość 42 też jest dobrym rozwiązaniem. A która jest „prawdziwą” semantyką tego while? Otóż funkcja pusta.

Co to dla nas znaczy? Szukając tego „prawdziwego” rozwiązania równania na while możemy co prawda dodać tam „samospełniające” się przepowiednie (jak to z 42), ale ta prawdziwa semantyka powinna zawierać tylko te wartości które są w jakiś sposób uzasadnione. Czyli powinna być w jakimś sensie „minimalna”.

#### Porządek na funkcjach
Powiemy, że dla dwóch funkcji częściowych f, g: X → Y zachodzi f ≤ g jeśli one są w sobie zawarte jako relacje, czyli dom(f) ≤ dom(g) oraz g|<sub>dom(f)</sub> = f. Najmniejszy element tego porządku to funkcja pusta, elementy maksymalne to funkcje całkowite. Można o tym porządku myśleć jak o porządku wiedzy: student pierwszego roku nie wie nic, a potem się uczy, jeden student majoryzuje drugiego jeśli zna odpowiedzi na przynajmniej te pytania co pierwszy i na dodatek te odpowiedzi są zgodne.

#### Czy istnieje najmniejsza funkcja spełniająca równanie na while?

Zapiszmy to równanie alternatywnie, że X = F(X), gdzie F(X) = 𝜆 s → if B⟦ b ⟧ s then X (I⟦ I ⟧ s) else s. To czego szukamy to punkt stały F, który na dodatek jest NAJMNIEJSZYM z tych punktów stałych. To spróbujmy go aproksymować:

* X<sub>0</sub> = ∅
* X<sub>n+1</sub> = F(X<sub>n</sub>)

Skoro X<sub>1</sub> ≥ X<sub>0</sub> (bo X<sub>0</sub> to element najmniejszy) to indukcyjnie X<sub>2</sub> = F(X<sub>1</sub>) ≥ F(X<sub>0</sub>) = X<sub>1</sub> itd. --- korzystamy tu z monotoniczności F.

Niech X<sub>oo</sub> = ⋃ X<sub>n</sub>. Wtedy oczywiście X<sub>oo</sub> ≥ X<sub>n</sub> dla każdego n. Czy X<sub>oo</sub>  jest punktem stałym F? Otóż tak, bo F jest ciągła - sprawdza swój argument tylko na skończenie wielu wartościach, w naszym przypadku na jednej (s' = I ⟦ I ⟧ s). Więc jeśli F( X<sub>oo</sub>  ) ma przyjąć jakąś wartość to już któryś skończony F (X<sub>n</sub>) = X<sub>n+1</sub> ją przyjmie.

Dodatkowo, jeśli F(Y) = Y, to indukcyjnie X<sub>0</sub> ≤ Y więc X<sub>1</sub>  ≤ Y, itp, więc X<sub>oo</sub>  ≤ Y, więc spośród punktów stałych F nasz X<sub>oo</sub>  jest NAJMNIEJSZY.

##### Ćwiczenie
Zobaczyć jakie mają dziedziny i jakie wartości kolejne aproksymacje X<sub>0</sub>, X<sub>1</sub>, itp dla while w I<sub>1</sub>  powyżej.

// podpowiedź: dom(X<sub>n+1</sub>) = { s ∊ State | s(x) ≤ n }

##### Umowa
Wszystkie funkcje które napiszemy (bez stwierdzeń w stylu "nie należy do dziedziny", albo "dla każdego zmiennej ...") są monotoniczne i ciągłe. Dla takich funkcji oznaczamy Fix(F) ów najmniejszy punkt stały. Więc finalnie, poprawne zapisanie semantyki while to:

I ⟦ while b do I ⟧ = Fix(F)
gdzie F to taka funkcja (State → State) → (State → State), że zachodzi:
F(X) = 𝜆 s → if B⟦ b ⟧ s then X (I⟦ I ⟧ s) else s

Tutaj nie ma już żadnej rekurencji, definiujemy najpierw F (kompozycjonalnie względem b oraz I) a następnie aplikujemy do tego operację Fix.

#### W Haskellu

W Haskellu nie musimy wprost używać operatora Fix, natomiast najlepiej byłoby wyróżnić miejsce użycia tego operatora, np. pisząc właśnie

I ⟦ while b do I ⟧ = X where
X = 𝜆 s → if B⟦ b ⟧ s then X (I⟦ I ⟧ s) else s

gdzie równanie na X to jest miejsce użycia Fix.

##### Ćwiczenie

Współnie z całą grupą zajęciową napiszcie semantykę denotacyjną dla języka o gramatyce:
```
S ::= x := e | skip | S1; S2 | if b then S1 else S2 | while b do S | repeat S until b | for x:=e1 to e2 do S | do e times S | do S while b
b ::= true | false | e1 ≤ e2 | ¬b | b1 ∧ b2
e ::= N | x | e1 + e2 | e1 ∗ e2 | e1 − e2
```
