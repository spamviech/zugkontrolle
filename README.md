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

### Kontakt

Ein Kontakt ist ein Eingangssignal. Es wird durch einen Zug ausgelöst und ist hauptsächlich für den
automatischen Betrieb (Plan) interessant.

Soll ein PCF8574Port verwenden werden wird der zugehörige InterruptPin benötigt.

## Geplant

### Plan (nicht implementiert)

Ein Plan ist eine Aneinanderreihung von Aktionen vorher erstellter StreckenObjekte und Wartezeiten.
Beim ausführen eines Plans werden diese nacheinander aufgerufen.

## Installation

Für jede Release-Version wird eine vorkompilierte binary als Asset auf
[github](https://github.com/spamviech/Zugkontrolle/releases) angeboten.
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

- `-h` | `--hilfe` | `--help`
    Zeige den Hilfstext an.
- `-v` | `--version`
    Zeige die aktuelle Version an.
- `-z ZUGTYP` | `--zugtyp ZUGTYP`
    Starte mit dem übergebenen Zugtyp. Vorhandene Gleise und Geschwindigkeiten unterscheiden sich.
    Aktuell sind `Märklin` und `Lego` unterstützt.
- `-p DATEI` | `--pfad DATEI`
    Versuche den in DATEI gespeicherten Zustand zu laden.
    Wenn die Datei nicht existiert/das falsche Format hat wird mit Fehlermeldung
    und einem leeren Zustand gestartet.
- `-m MODUS` | `--modus MODUS`
    Bestimme den Modus bei Programmstart.
    Aktuell sind `Bauen` und `Fahren` unterstützt.
- `-z ZOOM` | `--zoom ZOOM`
    Bestimme den Zoom bei Programmstart.
- `-x X` | `--x X`
    Bestimme die x-Koordinate der linken oberen Ecke.
- -`y Y` | `--y Y`
    Bestimme die y-Koordinate der linken oberen Ecke.
- `-w WINKEL` | `--winkel WINKEL`
    Bestimme den Dreh-Winkel bei Programmstart.
- `--[kein-]i2c0_1`
    (De)aktiviere den I2C channel auf pins 2 und 3 (bus 0 oder 1)
- `--[kein-]i2c3`
    (De)aktiviere den I2C channel auf pins 4 und 5 (bus 3)
- `--[kein-]i2c4`
    (De)aktiviere den I2C channel auf pins 8 und 9 (bus 4)
- `--[kein-]i2c5`
    (De)aktiviere den I2C channel auf pins 12 und 13 (bus 5)
- `--[kein-]i2c6`
    (De)aktiviere den I2C channel auf pins 22 und 23 (bus 6)
- `--verbose`
    Zeige zusätzliche (Debug) Informationen in der Kommandozeile an.
- `-l` | `--[keine-]log_datei`
    Speichere Log-Informationen zusätzlich in einer Datei.

### Starten durch ziehen einer Datei auf die binary

Wird nur ein Kommandozeilenargument übergeben wird versucht dieses als Datei zu öffnen und zu laden.

- Unter `Windows` entspricht dies dem ziehen (drag-and-drop) einer Datei auf die Executable.
- Unter `Linux` (nautilus window manager) ist ein Start über ziehen auf die Binary nicht möglich.
    Stattdessen muss eine .desktop-Datei erstellt werden, die das Verhalten unterstützt.

    Eine .desktop-Datei kann folgendermaßen aussehen:

    ```.desktop
    [Desktop Entry]
    Type=Application
    Terminal=false
    Name[en_EN]=Zugkontrolle
    Exec=sh -c "/home/pi/zugkontrolle/zugkontrolle %f"
    ```

  Die Anleitung stammt aus folgenden Quellen:

  - [Nautilus drag-and-drop](https://askubuntu.com/questions/52789/drag-and-drop-file-onto-script-in-nautilus)
  - [aktueller Ordner in .desktop Datei](https://stackoverflow.com/a/56202419)

## Abhängigkeiten

Sofern die desktop-variante des Raspberry Pi OS installiert wurde, sollte alles nötige vorhanden sein.

Falls z.B. aus Größenbeschränkung der SD-Karte nur die lite-variante installiert wurde,
kann man nach Installation der folgenden Pakete ebenfalls in ein Desktop-environment booten.
Das sollte (minimal) weniger Speicherplatz benötigen.

```sh
sudo apt install xserver-xorg raspberrypi-ui-mods lightdm
```

## Aktivieren zusätzlicher I2C-Busse

Durch hinzufügen folgender Zeilen in `/boot/config.txt` werden ab dem nächsten boot zusätzliche I2C-Busse (3-6) verfügbar sein.
Bei raspi4 verwenden diese Hardware-Funktionalität, ansonsten wird das I2C-Signal über Software erzeugt.

```txt
# Enable additional i2c busses
# https://www.instructables.com/Raspberry-PI-Multiple-I2c-Devices/
# https://www.raspberrypi.com/documentation/computers/config_txt.html
# https://www.raspberrypi.com/documentation/computers/configuration.html#part3.1
[pi4]
dtoverlay=i2c3
dtoverlay=i2c4
dtoverlay=i2c5
dtoverlay=i2c6
[pi3]
dtoverlay=i2c-gpio,bus=6,i2c_gpio_sda=22,i2c_gpio_scl=23
dtoverlay=i2c-gpio,bus=5,i2c_gpio_sda=12,i2c_gpio_scl=13
dtoverlay=i2c-gpio,bus=4,i2c_gpio_sda=8,i2c_gpio_scl=9
dtoverlay=i2c-gpio,bus=3,i2c_gpio_sda=4,i2c_gpio_scl=5
gpio=22,23,12,13,8,9,4,5=pu
[pi2]
dtoverlay=i2c-gpio,bus=6,i2c_gpio_sda=22,i2c_gpio_scl=23
dtoverlay=i2c-gpio,bus=5,i2c_gpio_sda=12,i2c_gpio_scl=13
dtoverlay=i2c-gpio,bus=4,i2c_gpio_sda=8,i2c_gpio_scl=9
dtoverlay=i2c-gpio,bus=3,i2c_gpio_sda=4,i2c_gpio_scl=5
gpio=22,23,12,13,8,9,4,5=pu
[pi1]
dtoverlay=i2c-gpio,bus=6,i2c_gpio_sda=22,i2c_gpio_scl=23
dtoverlay=i2c-gpio,bus=5,i2c_gpio_sda=12,i2c_gpio_scl=13
dtoverlay=i2c-gpio,bus=4,i2c_gpio_sda=8,i2c_gpio_scl=9
dtoverlay=i2c-gpio,bus=3,i2c_gpio_sda=4,i2c_gpio_scl=5
gpio=22,23,12,13,8,9,4,5=pu
[all]
```
