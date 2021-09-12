### R Skript zu "Assignments"
### Kurs "Einführung in die moderne Datenanalyse mit R"
### Datum: August 2020
### Autor: The R Bootcamp


### <- kreiert Objekte ----------------------------

# Was ist Funktion, was ist Objekt
eins_zwei_drei <- c(1, 2, 3) # Object links, Funktion rechts, Objekt als Input zur Funktion

# Funktioniert das?
c(4, 5, 6) -> vier_fünf_sechs # Ja, der Pfeil funktioniert in beide Seite

# Funktioniert das?
sieben_acht_neun = c(7, 8, 9) # Ja, = ist identisch mit <- (Pfeil nach links)

### <- verändert Objekte ----------------------------

# Ändert sich hier das Objekt?
eins_zwei_drei + 10 # Nein

# Ändert sich hier das Objekt?
eins_zwei_drei_plus10 <- eins_zwei_drei + 10 # Nein, ein neues wird kreiert 

# Was kommt hier raus
was_ist_das <- eins_zwei_drei + vier_fünf_sechs # schlicht die jeweils die Summe der Einträge an den verschiedenen Positionen

# Was kommt hier raus
was_ist_das <- eins_zwei_drei + vier_fünf_sechs * sieben_acht_neun # selbe Prinzip wie eben, element-wise operation

# Was steckt hinter dem Objekt
was_ist_das # probiert es aus

### Objekt <- Funktion ----------------------------

# Wo ist das Objekt? Und wo die Funktion?
eins_zwei_drei + 10 # Die Funktion ist das +, die Objekte, welche als Input dienen, sind links und rechts

# Wo ist das Objekt? Und wo die Funktion?
eins_zwei_drei # die Funktion print() wird im Hintergrund ausgeführt

