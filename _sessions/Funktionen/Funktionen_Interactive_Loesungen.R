### R Skript zu "Funktionen"
### Kurs "Einführung in die moderne Datenanalyse mit R"
### Datum: Februar 2020
### Autor: The R Bootcamp


### Jede Aktion wird durch eine Funktion ausgeführt ----------------------------

# Generiere einen Vektor, der die Elemente 1,2 und 3 enthält und nenne ihn my_vec.
my_vec <- c(1, 2, 3)
my_vec <- 1:3

# Wie viele (sichtbaren) Funktionen wurden hierbei aufgerufen?
#> 2; im ersten Fall `<-` und c(), und im zweiten Fall `<-` und `:`

# Zeige den Vektor my_vec in der Konsole an.
my_vec


### Funktionen haben help files ------------------------------------------------

# Um zu lernen wie eine Funktion funktioniert, können wir das help file aufrufen.
# Schaue dir das help file der "sum()" Funktion an.
?sum
help(sum)

# Welches sind die wichtigsten Abschnitte eines help files (Achtung subjektiv und 
# vom jeweiligen help file abhängig).
#> Description: Gibt einen kurzen Überblick über das Ziel der Funktion.
#> Usage: Zeigt die Funktion mit allen Argumenten und, falls vorhanden, den
#>        dazugehörigen Defaults.
#> Arguments: Gibt eine genauere Beschreibung der Argumente.
#> Details: Detailiertere Informationen über die Funktion und die Methoden, die 
#>          angewendet werden. Hier gibt es sehr grosse Unterschiede im Detailgrad.
#>          Es kommt ganz auf den Autor der Funktion an, wie genau alles
#>          beschrieben wird.
#> Value: Beschreibung des Outputs einer Funktion.
#> Examples: Beispiele über die Verwendung der Funktion. Kann zum Teil etwas
#>           kryptisch sein.


### Funktionen haben (normalerweise) Argumente ---------------------------------

# Die "sum()" Funktion berechnet den Mittelwert eines Objekts. Benutze sie um
# die Summe aller Elemente in my_vec zu berechnen.
sum(my_vec)

# Füge mit Hilfe des folgenden Codes ein NA am Ende des Vektors my_vec hinzu.
my_vec <- c(my_vec, NA)

# Berechne nun nochmals die Summe von my_vec. Was ist das Ergebnis und warum?
sum(my_vec)
#> NA, da na.rm = FALSE der Default ist, werden die NAs nicht ausgeschlossen.

# Wiederhole die Berechnung der letzten Aufgabe, so dass das Resultat 6 ergibt.
sum(my_vec, na.rm = TRUE)

### Die Klasse des Inputs muss mit der vorgegebenen Klasse übereinstimmen ------

# Teste die Klasse von my_vec.
class(my_vec)
#> my_vec ist ein numerischer Vektor, d.h. mit ihm können z.B. Berechnungen angestellt
#> werden.

# Ersetze mit Hilfe des folgenden Codes das NA in my_vec durch ein "a".
my_vec[4] <- "a"

# Teste die Klasse von my_vec. Weshalb hat sich die Klasse geändert?
class(my_vec)
#> my_vec hat neu die Klasse character, d.h. die numerischen Elemente wurden auch zu
#> characters umgewandelt. Dies entspricht der Hierarchie der Type coersion in R:
#> Folgende Klassen werden falls nötig in die nächst höhere Klasse umgewandelt
#> logical -> numeric (integer -> double) -> character

# Berechne nochmals die Summe von my_vec. Was passiert und weshalb?
sum(my_vec, na.rm = TRUE)
#> Wir erhalten eine Fehlermeldung, da die Klasse des Inputs nicht mit der Klasse
#> des Outputs übereinstimmt.

