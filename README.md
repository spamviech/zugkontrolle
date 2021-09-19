# Zugkontrolle

Ermöglicht die Steuerung einer Modelleisenbahn über einen Raspberry Pi.  
Die Funktion der Pins wird mit Datentypen realisiert, welche zur Laufzeit erstellt werden können.  
Je nach Datentyp stehen einem so registrierten Pin vorgefertigte Aktionen
(z.B. Schalten einer Weiche) zur Verfügung.
Einige Beispielschaltpläne sind __NOCH NICHT ERSTELLT__.

## Bedienen

### Modus: Bauen

Neue Gleise werden durch hereinziehen erstellt.
Sie erhalten automatisch den aktuellen Streckenabschnitt.
Wenn die Checkbox gesetzt ist führt ein Klick auf ein Gleis _ohne_ Bewegen der Maus
ebenfalls zu einem zuweisen des aktuellen Streckenabschnittes.

Gleise können über einfaches Drag&Drop bewegt werden.
Wird ein Gleis aus dem angezeigten Bereich gezogen (Maus beim loslassen außerhalb des Canvas)
wird das Gleis gelöscht.

Ein Doppelklick auf ein weichen-artiges Gleis öffnet einen Dialog zum einstellen
der Anschlüsse zur Richtungs-Steuerung.

Ein Doppelklick auf eine Gerade oder Kurve soll einen Dialog zum einstellen der Anschlüsse
für einen Kontakt öffnen (nicht implementiert).

Ändern des angezeigten Bereiches (Bewegen/Drehen,Zoomen) ist aktuell
nur über die Knöpfe in der oberen Leiste möglich.

### Modus: Fahren

Geschwindigkeiten können in der linken Seitenleiste eingestellt werden.

Weichen können über einen Klick auf das Gleis gestellt werden.

Streckenabschnitte können über einen Klick auf eine zugehörige Gerade oder Kurve an/ausgeschaltet werden.

Ändern des angezeigten Bereiches (Bewegen/Drehen,Zoomen) ist aktuell
nur über die Knöpfe in der oberen Leiste möglich.

## Begriffe

### Zugtyp

Unterstützte Zugtypen sind (analoge) __Märklin__- und __Lego__-Modelleisenbahnen. Bei beiden erfolgt die Stromzufuhr über eine leitende Schiene.
Der Hauptunterschied besteht darin, wie ein Umdrehen einer Lokomotive erfolgt:

- Bei __Märklin__-Eisenbahnen führt eine Fahrspannung von __24V__ (im Gegensatz zur normalen Betriebsspannung __<=16V__) zu einem Umdrehen aller auf der Schiene befindlichen Lokomotiven.
- Bei __Lego__-Eisenbahnen gibt die _Polarität_ der Spannung die Richtung vor.
    Außerdem gibt es bei __Lego__-Eisenbahnen keine automatischen Weichen,
    weshalb eine Schaltung selbst gebaut werden muss.
    Ein Beispiel ist [in diesem Youtube-Video zu sehen](https://www.youtube.com/watch?v=h-5FmGfYzRs).

### Geschwindigkeit

Eine Geschwindigkeit regelt die Geschwindigkeit von allen Zügen auf den zugehörigen Gleisen,
bzw. deren Fahrtrichtung.
Dazu wird ein PWM-Signal erzeugt um ausgehend von einer Maximal-Spannung eine effektiv geringere Fahrspannung zu erzeugen.

Bei __Märklin__-Modellbahnen wird __1__ Pin benötigt. Die Maximalspannung sollte __24V__ (Umdrehen-Spannung) betragen.
Bei __Lego__-Modellbahnen werden __2__ Pins benötigt.
Je ein Pin kümmert sich dabei um Geschwindigkeit und Fahrtrichtung.
    Die Maximalspannung bei der Geschwindigkeit hängt vom Modell ab.
    Bei der letzten Version mit leitenden schienen sollte sie __9V__ betragen.

### Streckenabschnitt

Ein Streckenabschnitt regelt, welche Gleis-Abschnitte mit Strom versorgt werden.
So können Abstellgleise abgeschaltet werden, ohne eine eigene Bahngeschwindigkeit zu benötigen.

### Weiche

Weichen und Kreuzungen, bei denen die Fahrtrichtung geändert werden kann.
Es wird ein Anschluss pro Richtung benötigt.

## Geplant

### Kontakt (nicht implementiert)

Ein Kontakt ist ein Eingangssignal. Es wird durch einen Zug ausgelöst und ist hauptsächlich für den
automatischen Betrieb (Plan) interessant.

Soll ein PCF8574Port verwenden werden wird der zugehörige InterruptPin benötigt.

### Plan (nicht implementiert)

Ein Plan ist eine Aneinanderreihung von Aktionen vorher erstellter StreckenObjekte und Wartezeiten.
Beim ausführen eines Plans werden diese nacheinander aufgerufen.

## Installation

Für jede Release-Version wird eine vorkompilierte binary als Asset auf [github](https://github.com/spamviech/Zugkontrolle/releases) angeboten.
Damit es ausgeführt werden kann ist vermutlich ein Anpassen der Berechtigungen notwendig:
`chmod +x /pfad/zu/binary/zugkontrolle-version`

Zur Installation aus dem Quellcode wird cargo empfohlen.  
Nach Installation aller Abhängigkeiten (siehe Unten) kann durch den Aufruf von
`cargo build --release` eine Executable in einem Unterordner von `./target` erstellt werden.

### Installation via rustup

Zur Installation von rustup wird empfohlen die Anleitung auf der
[zugehörigen Website](https://rustup.rs/) zu befolgen.
Im Moment lautet der Befehl
    `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`

### Installation von libxkbcommon

Obwohl angeblich nichts zusätzlich installiert wurde war der folgende Befehl notwendig
    `sudo apt install libxkbcommon-dev`

## Ausführen des Programms

Sobald sie als ausführbar markiert ist kann die binary über die Kommandozeile
(mit gewünschten Parametern) gestartet werden.

Alternativ lautet der Befehl zum Ausführen über cargo `cargo run -- release`.  
Zusätzliche Kommandozeilen-Parameter (siehe Unten) müssen getrennt durch `--` übergeben werden.

### Unterstütze Kommandozeilen-Parameter

- -h | --help  
    Zeige den Hilfstext an. Dieser wird automatisch erzeugt, wodurch Teile davon auf englisch sind.
- --version  
    Zeige die aktuelle Version an.
- -z ZUGTYP | --zugtyp ZUGTYP
    Starte mit dem übergebenen Zugtyp. Vorhandene Gleise und Geschwindigkeiten unterscheiden sich.
    Aktuell sind `Märklin` und `Lego` unterstützt.
- -p DATEI | --pfad DATEI  
    Versuche den in DATEI gespeicherten Zustand zu laden.
    Wenn die Datei nicht existiert/das falsche Format hat wird mit Fehlermeldung
    und einem leeren Zustand gestartet.
- -m MODUS | --modus MODUS
    Bestimme den Modus bei Programmstart.
    Aktuell sind `Bauen` und `Fahren` unterstützt.
- -z ZOOM | --zoom ZOOM
    Bestimme den Zoom bei Programmstart.
- -x X | --x X
    Bestimme den x-Wert der linken oberen Ecke.
- -y Y | --y >
    Bestimme den y-Wert der linken oberen Ecke.
- -w WINKEL | --winkel WINKEL
    Bestimme den Winkel bei Programmstart.
- --verbose
    Zeige zusätzliche (Debug) Informationen in der Kommandozeile an.

### Starten durch ziehen einer Datei auf die binary

Wird nur ein Kommandozeilenargument übergeben wird versucht dieses als Datei zu öffnen und zu laden.

- Unter `Windows` entspricht dies dem ziehen (drag-and-drop) einer Datei auf die Executable.
- Unter `Linux` (nautilus window manager) ist ein Start über ziehen auf die Binary nicht möglich.
    Stattdessen muss eine .desktop-Datei erstellt werden, die dass Verhalten unterstützt.

    Eine .desktop-Datei kann folgendermaßen aussehen:

    ```.desktop
    [Desktop Entry]
    Type=Application
    Terminal=false
    Name[en_EN]=Zugkontrolle
    Exec=sh -c "/home/pi/Desktop/Zugkontrolle-bin/Zugkontrolle %f"
    ```

  Die Anleitung stammt aus folgenden Quellen:

  - [Nautilus drag-and-drop](https://askubuntu.com/questions/52789/drag-and-drop-file-onto-script-in-nautilus)
  - [aktueller Ordner in .desktop Datei](https://stackoverflow.com/a/56202419)
