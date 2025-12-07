---
editor_options: 
  markdown: 
    wrap: 72
---

## ShinyDWD

### Agronomy group CAU Kiel

#### Disclaimer

Die App Shiny-DWD ermöglicht es, für einen durch eine Karte wählbaren
Ort innerhalb Deutschlands aufbereitete Wetterdaten in täglicher Auflösung ab 1990 zu erhalten. Diese beruhen auf
einer Interpolation  DWD-Daten ( https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/ ). 
Hierzu muss der Ort über einen Klick auf die Karte ausgewählt werden. 
Die Aktualisierung der Daten erfolgt in unregelmäßigen Abständen.

Der Algorithmus der App sucht für jeden der Parameter die 3 nächstgelegenen verfügbaren Werte aus den Daten des DWD. Hierbei werden nur Stationen mit einer
Höhendifferenz kleiner als 150m zum gewählten Ort berücksichtigt. Eine Korrektur der Messdaten für Höhendifferenzen erfolgt ansonsten nicht.
Aus diesen Daten wird über inverse Distanzwichtung ein Wert für die gewählte Koordinate ermittelt. Die Strahlungsdaten werden dabei aus den
Sonnenscheindauerwerten des DWD über eine empirische Regression zwischen den Sonnenscheinstundendauern und der Relation gemessener zu
astronomisch möglichen Globalstrahlungswerten abgeleitet.
Für die Niederschlagsdaten wird der jeweils nächstverfügbare Messwert ohne Interpolation verwendet. Hierzu werden die 3 nächstgelegenen Stationen des erweiterten
Niederschlagsmessnetzes des DWD mit herangezogen.
Weitere Parameter werden berechnet und können grafisch dargestellt werden.

Die App berucht im Wesentlichen auf Funktionen des R-Pakets "weatherfunctions", welches unter 
https://github.com/AgronomyKiel/Weatherfunctions für Testzwecke verfügbar ist.
Kontakt: kage@pflanzenbau.uni-kiel.de
