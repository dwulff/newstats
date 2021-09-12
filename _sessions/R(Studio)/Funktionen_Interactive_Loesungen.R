### R Skript zu "Funktionen"
### Kurs "Einführung in die moderne Datenanalyse mit R"
### Datum: August 2020
### Autor: The R Bootcamp


### Jede Aktion wird durch eine Funktion ausgeführt ----------------------------

# Generiere einen Vektor, der die Elemente 1,2 und 3 enthält und nenne ihn x
x <- c(1, 2, 3)
x <- 1:3

# Wie viele (sichtbaren) Funktionen wurden hierbei aufgerufen?
#> 2; im ersten Fall `<-` und c(), und im zweiten Fall `<-` und `:`

# Zeige den Vektor x in der Konsole an
x

# Wie viele (sichtbaren) Funktionen wurden hierbei aufgerufen?

### Funktionen haben help files ------------------------------------------------

# Um zu lernen wie eine Funktion funktioniert, können wir das help file aufrufen.
# Schaue dir das help file der "sum()" Funktion an.
?sum
help(sum)

# Welches sind die wichtigsten Abschnitte eines help files (Achtung subjektiv und 
# vom jeweiligen help file abhängig)
#> Description: Gibt einen kurzen Überblick über das Ziel der Funktion.
#> Usage: Zeigt die Funktion mit allen Argumenten und, falls vorhanden, den
#>        dazugehörigen Defaults.
#> Arguments: Gibt eine genauere Beschreibung der Argumente
#> Details: Detailiertere Informationen über die Funktion und die Methoden, die 
#>          angewendet werden. Hier gibt es sehr grosse Unterschiede im Detailgrad.
#>          Es kommt ganz auf den Autor der Funktion an, wie genau alles
#>          beschrieben wird.
#> Value: Beschreibung des Outputs einer Funktion.
#> Examples: Beispiele über die Verwendung der Funktion. Kann zum Teil etwas
#>           kryptisch sein.


### Funktionen haben (normalerweise) Argumente ---------------------------------

# Was ist ein Argument?
#> Ein Argument ist ein Objekt, welches einer Funktion übergeben wird, um damit 
#> eine Aktion auszuführen.

# Die "sum()" Funktion berechnet den Mittelwert eines Objekts. Benutze sie um
# die Summe aller Elemente in x zu berechnen
sum(x)

# Im help file vorhin haben wir gesehen, dass sum ein Argument "na.rm" hat;
# weshalb funktioniert "sum(x)" obwohl wir das Argument nirgends angeben?
#> na.rm hat einen Default (TRUE). Falls das Argument ausgelassen wird, wird
#> einfach dieser Wert verwendet.

# Füge ein NA an der vierten Position des Vektors x hinzu
#> Wie meistens in R gibt es hier mehrere Möglichkeiten, nämlich:
x <- c(x, NA)
x[4] <- NA
x[length(x) + 1] <- NA

# Berechne nun nochmals die Summe von x. Was ist das Ergebnis und warum?
sum(x)
#> NA, da na.rm = FALSE der Default ist, werden die NAs nicht ausgeschlossen

# Wiederhole die Berechnung der letzten Aufgabe, so dass das Resultat 6 ergibt
sum(x, na.rm = TRUE)

### Die Klasse des Inputs muss mit der vorgegebenen Klasse übereinstimmen ------

# Teste die Klasse von x. Was bedeuetet das Ergebnis?
class(x)
#> x ist ein numerischer Vektor, d.h. mit ihm können z.B. Berechnungen angestellt
#> werden.

# Füge ein "a" an der fünften Position von x hinzu
x[5] <- "a"

# Teste die Klasse von x. Hat sich etwas geändert? Weshalb?
class(x)
#> x hat neu die Klasse character, d.h. die numerischen Elemente wurden auch zu
#> characters umgewandelt. Dies entspricht der Hierarchie der Type coersion in R:
#> Folgende Klassen werden falls nötig in die nächst höhere Klasse umgewandelt
#> logical -> numeric (integer -> double) -> character

# Berechne nochmals die Summe von x. Was passiert und weshalb
sum(x, na.rm = TRUE)
#> Wir erhalten eine Fehlermeldung, da die Klasse des Inputs nicht mit der Klasse
#> des Outputs übereinstimmt.

# Plotte den Vektor x mit der plot() Funktion. Was ist passiert?
plot(x)
#> Die Plot Funktion hat den character Vektor offenbar in einen numeric Vektor
#> umgewandelt

# Erstelle eine matrix mit zwei Spalten und den Werten von 1 bis 10. Nenne sie y
y <- matrix(1:10, ncol = 2)

# Überprüfe, ob y tatsächlich die Klasse Matrix hat
class(y)

# Berechne die Summe der Matrix
sum(y)

# Die sum() Funktion hat die Summe über alle Zellen der Matrix berechnet. Was wenn
# wir nur die Summe pro Zeile wollen?
sum(y[1,])
sum(y[2,])
#> ... Diese Art ist aber sehr umständlicher. Einfacher geht es mit rowSums()
rowSums(y)

