# Zugkontrolle
Ermöglicht die Steuerung einer Modelleisenbahn über einen Raspberry Pi.
Die Funktion der Pins wird mit Datentypen realisiert, welche zur Laufzeit erstellt werden können.
Je nach Datentyp stehen einem so registrierten Pin vorgefertigte Aktionen (z.B. Schalten einer Weiche) zur Verfügung.
Einige Beispielschaltpläne sind NOCH NICHT ERSTELLT.

## Zugtyp
Unterstützte Zugtypen sind (analoge) __Märklin__- und __Lego__-Modelleisenbahnen. Bei beiden erfolgt die Stromzufuhr über eine leitende Schiene.
Der Hauptunterschied besteht darin, wie ein Umdrehen einer Lokomotive erfolgt:
    Bei __Märklin__-Eisenbahnen führt eine Fahrspannung von __24V__ (im Gegensatz zur normalen Betriebsspannung __<=16V__) zu einem Umdrehen aller auf der Schiene befindlichen Lokomotiven.
    Bei __Lego__-Eisenbahnen gibt die _Polarität_ der Spannung die Richtung vor.
Außerdem gibt es bei __Lego__-Eisenbahnen keine automatischen Weichen, weshalb ein Schalten z.B. über einen Servo-Motor realisiert werden muss.

## Bahngeschwindigkeit
Eine Bahngeschwindigkeit regelt die Geschwindigkeit von allen Zügen auf den zugehörigen Gleisen, bzw. deren Fahrtrichtung.
Dazu wird ein PWM-Signal erzeugt um ausgehend von einer Maximal-Spannung eine effektiv geringere Fahrspannung zu erzeugen.

Bei __Märklin__-Modellbahnen wird __1__ Pin benötigt. Die Maximalspannung sollte __24V__ (Umdreh-Spannung) betragen.
Bei __Lego__-Modellbahnen werden __2__ Pins benötigt. Je ein Pin kümmert sich dabei um Geschwindigkeit und Fahrtrichtung.
    Die Maximalspannung bei der Geschwindigkeit hängt vom Modell ab.
    Bei der letzen Version mit leitenden schienen sollte sie __9V__ betragen.

## Streckenabschnitt
Ein Streckenabschnitt regelt, welche Gleis-Abschnitte mit Strom versorgt werden. So können Abstellgleise abgeschaltet werden, ohne eine eigene Bahngeschwindigkeit zu benötigen.

## Weiche
Weichen und Kreuzungen, bei denen die Fahrtrichtung geändert werden kann.

Bei __Märklin__-Modellbahnen wird pro Richtung __1__ Pin benötigt.
Bei __Lego__-Modellbahnen ist ein Umschalten über einen Servo-Motor angedacht. Es werden nur __2__ Richtungen unterstützt und __1__ Pin benötigt.

## Kupplung
Eine Kupplung ist eine Schiene bei der Zug-Elemente (Lokomotive/Wagon) voneinander getrennt werden können. Es wird __1__ Pin benötigt.

__Anmerkung:__
    Mir sind keine Kupplungsschienen für __Lego__-Modellbahnen bekannt.

## Wegstrecke
Eine Wegstrecke ist eine Zusammenfassung mehrer Teilelemente, wobei Weichen eine eindeutige Richtung zugewiesen wurde.
Eine mögliche Anwendung ist das fahren von/auf ein Abstellgleis.

Wegstrecken unterstützen sämtliche Funktionen ihrer Elemente, welche immer auf einmal ausgeführt werden.
Weichen können dabei nur auf ihre festgelegte Richtung eingestellt werden.

## Plan
Ein Plan ist eine Aneinanderreihung von Aktionen vorher erstellter StreckenObjekte und Wartezeiten.
Beim ausführen eines Plans werden diese nacheinander aufgerufen.

# Installation
Zur Installation wird stack empfohlen. Durch Aufruf von "stack install" wird eine Kopie der Executable im Unterordner "./bin" erstellt.

Nachdem die Installation der Pakete "gtk3" und "lens" eine Installation des "Cabal"-Pakets vorraussetzen, welches sehr lange dauert (bei mir ~1 Tag) wird eine Installation ohne beide Pakete unterstützt.
Dazu muss der Installations-Befehl erweitert werden um die flag gui auf false zu setzten. Der neue Installationsbefehl lautet somit:
    "stack install --flag Zugkontrolle:-gui"
Eine Verwendung des GTK-UI ist dann natürlich nicht mehr möglich.
Ein möglicher Arbeitsablauf ist dann Erstellen der Repräsentation z.B. auf Cip-Rechner über das GTK-UI, speichern und kopieren in einer json-Datei und anschließendes Ausführen mit Cmd-UI auf dem Raspberry Pi.

## Installation von WiringPi
Unter Raspian ist standardmäßig eine Version von wiringpi installiert.
Um die neueste Version zu installieren ist es zu empfehlen die Installationsanweisungen unter http://wiringpi.com/download-and-install/ zu berücksichtigen.

## Installation von GTK+
Um  das GTK-UI zu verwenden muss natürlich GTK+ installiert werden.
Dazu ist am besten die Anleitung auf der Website zu befolgen: https://www.gtk.org/download/index.php
    Linux/Raspian:
        Falls es nicht schon installiert ist (bei Raspian der Fall) ist eine Installation über den verwendeten packet manager vermutlich das einfachste.
        Bei Verwendung von apt-get ist der Befehl:
        "sudo apt-get install libgtk-3-dev"
    Windows MSYS2:
        In dieser muss natürlich gtk3 installiert sein (pacman -S mingw-w64-x86_64-gtk3).
        Wenn man keine selbst geplegte MSYS2-Installation wünscht kann man die von stack mitgebrachte verwenden.
        Die Installation von gtk3 erfolgt dann über "stack exec -- pacman -S mingw-w64-x86_64-gtk3"

# Ausführen des Programms
Zum Ausführen werden Root-Rechte benötigt, weil sonst nicht alle Funktionen der WiringPi-Bibliothek zur Verfügung stehen.
Auf Linux-Systemen mit ARM-Architektur bricht das Programm sonst direkt mit einer Fehlermeldung ab.

Nachdem auf nicht-RasperryPi-Systemen sämtliche IO-Funktionen des WiringPi-Moduls durch "return ()" ersetzt wurden ist das dort natürlich nicht notwendig.

Wenn das Programm unter Windows nicht startet, bzw. mit dll-Fehlern abbricht (Fehlermeldungen werden bei Start über Powershell nicht angezeigt) muss der Ordner der MSYS2-Installation weiter vorne im Path stehen.
Bei einer eigenen MSYS2-Installation ist das normalerweise: "C:\msys64\mingw64\bin".
Für die von stack mitgelieferte Version ist der Pfad normalerweise: "\~\AppData\Local\Programs\stack\x86_64-windows\msys2-20180531\mingw64\bin\"
Falls das immer nocht nicht hilft (bei "stack exec ..." normalerweise der Fall) muss die "zlib1.dll" durch die neuere aus dem msys-Ordner ersetzt werden.
Durch den Befehl "stack exec -- where zlib1.dll" werden alle im Pfad befindlichen in Reihenfolge aufgelistet.
Alle vor der im MSYS2-Ordner befindlichen müssen mit dieser überschrieben werden.
Im Normalfall (bei Ausführung über stack exec) betrifft das eine Datei: "~\AppData\Local\Programs\stack\x86_64-windows\ghc-8.2.2\mingw\bin\zlib1.dll\zlib1.dll"

## Unterstütze Kommandozeilen-Parameter
-p|--print
    Wenn diese Flag gesetzt ist werden die Ausgaben der Raspberry Pi Ausgänge (Pins) nicht als Ausgang verwendet.
    Es wird stattdessen eine Konsolenausgabe erzeugt.
    Die Flag ist vor allem zum Testen auf anderen Systemen gedacht.
--ui=Cmd|GTK
    Auswahl der Benutzer-Schnittstelle (Standard: GTK).
    Bei Installation mit "--flag Zugkontrolle:-gui" wird immer das Cmd-UI verwendet.
-lDATEI|--load=DATEI
    Versuche den in DATEI gespeicherten Zustand zu laden.
    Wenn die Datei nicht existiert/das falsche Format hat wird ohne Fehlermeldung mit einem leeren Zustand gestartet.
