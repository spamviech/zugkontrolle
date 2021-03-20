# Zugkontrolle

Ermöglicht die Steuerung einer Modelleisenbahn über einen Raspberry Pi.  
Die Funktion der Pins wird mit Datentypen realisiert, welche zur Laufzeit erstellt werden können.  
Je nach Datentyp stehen einem so registrierten Pin vorgefertigte Aktionen (z.B. Schalten einer Weiche) zur Verfügung.
Einige Beispielschaltpläne sind __NOCH NICHT ERSTELLT__.

## Begriffe

### Zugtyp

Unterstützte Zugtypen sind (analoge) __Märklin__- und __Lego__-Modelleisenbahnen. Bei beiden erfolgt die Stromzufuhr über eine leitende Schiene.
Der Hauptunterschied besteht darin, wie ein Umdrehen einer Lokomotive erfolgt:
    Bei __Märklin__-Eisenbahnen führt eine Fahrspannung von __24V__ (im Gegensatz zur normalen Betriebsspannung __<=16V__) zu einem Umdrehen aller auf der Schiene befindlichen Lokomotiven.
    Bei __Lego__-Eisenbahnen gibt die _Polarität_ der Spannung die Richtung vor.
Außerdem gibt es bei __Lego__-Eisenbahnen keine automatischen Weichen, weshalb ein Schalten z.B. über einen Servo-Motor realisiert werden muss.

### Bahngeschwindigkeit

Eine Bahngeschwindigkeit regelt die Geschwindigkeit von allen Zügen auf den zugehörigen Gleisen, bzw. deren Fahrtrichtung.
Dazu wird ein PWM-Signal erzeugt um ausgehend von einer Maximal-Spannung eine effektiv geringere Fahrspannung zu erzeugen.

Bei __Märklin__-Modellbahnen wird __1__ Pin benötigt. Die Maximalspannung sollte __24V__ (Umdrehen-Spannung) betragen.
Bei __Lego__-Modellbahnen werden __2__ Pins benötigt.
Je ein Pin kümmert sich dabei um Geschwindigkeit und Fahrtrichtung.
    Die Maximalspannung bei der Geschwindigkeit hängt vom Modell ab.
    Bei der letzten Version mit leitenden schienen sollte sie __9V__ betragen.

### Streckenabschnitt

Ein Streckenabschnitt regelt, welche Gleis-Abschnitte mit Strom versorgt werden. So können Abstellgleise abgeschaltet werden, ohne eine eigene Bahngeschwindigkeit zu benötigen.

### Weiche

Weichen und Kreuzungen, bei denen die Fahrtrichtung geändert werden kann.

Bei __Märklin__-Modellbahnen wird pro Richtung __1__ Anschluss benötigt.
Bei __Lego__-Modellbahnen ist ein Umschalten über einen Servo-Motor angedacht. Es werden nur __2__ Richtungen unterstützt und __1__ Pin benötigt.

### Kupplung

Eine Kupplung ist eine Schiene bei der Zug-Elemente (Lokomotive/Wagon) voneinander getrennt werden können. Es wird __1__ Anschluss benötigt.

__Anmerkung:__
    Mir sind keine Kupplungsschienen für __Lego__-Modellbahnen bekannt.

### Kontakt

Ein Kontakt ist ein Eingangssignal. Es wird durch einen Zug ausgelöst und ist hauptsächlich für den
automatischen Betrieb (Plan) interessant.

Soll ein PCF8574Port verwenden werden wird der zugehörige InterruptPin benötigt.
Es wird empfohlen den zugehörigen PCF8574 nicht gleichzeitig für Output zu verwenden.

### Wegstrecke

Eine Wegstrecke ist eine Zusammenfassung mehrerer Teilelemente, wobei Weichen eine eindeutige Richtung zugewiesen wurde.
Eine mögliche Anwendung ist das fahren von/auf ein Abstellgleis.

Wegstrecken unterstützen sämtliche Funktionen ihrer Elemente, welche immer auf einmal ausgeführt werden.
Weichen können dabei nur auf ihre festgelegte Richtung eingestellt werden.
Bei Kontakten wird auf ein beliebiges Signal gewartet.

### Plan

Ein Plan ist eine Aneinanderreihung von Aktionen vorher erstellter StreckenObjekte und Wartezeiten.
Beim ausführen eines Plans werden diese nacheinander aufgerufen.

## Installation

Zur Installation aus dem Quellcode wird cargo empfohlen.  
Nach Installation aller Abhängigkeiten (siehe Unten) kann durch den Aufruf von `cargo build` eine Executable in einem Unterordner von `./target` erstellt werden.

Alternativ wird für jede Release-Version eine vorkompilierte binary im Unterordner `bin` angeboten.

### Installation via rustup

Zur Installation von rustup wird empfohlen die Anleitung auf der
[zugehörigen Website](https://rustup.rs/) zu befolgen.
Im Moment lautet der Befehl
    `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`

### Swap-Datei erstellen

TODO: überprüfen ob es mit rust auch notwendig ist

```sh
    sudo dphys-swapfile swapoff   # disable swap
    sudo nano /etc/dphys-swapfile # and set 'CONF_SWAPSIZE' to 2048
    sudo dphys-swapfile setup     # refresh with new settings
    sudo dphys-swapfile swapon    # re-enable swap
```


#### libtinfo-dev

TODO: überprüfen ob es mit rust auch notwendig ist

```sh
sudo apt-get install libtinfo-dev
```

### Installation von pigpio

Zur Ansteuerung der GPio-Pins wird [pigpio](http://abyz.me.uk/rpi/pigpio/download.html) verwendet.
Der Befehlt zur Installation über apt lautet

```sh
sudo apt-get update
sudo apt-get install pigpio python-pigpio python3-pigpio
```

### Installation von GTK+

Um  das GTK-UI zu verwenden muss natürlich GTK+ (Version 3) installiert werden.
Dazu ist am besten die Anleitung auf [der Gtk-Website](https://www.gtk.org/download/index.php) zu befolgen.

* Linux/Raspbian:
    Falls nicht schon installiert, ist eine Installation über den verwendeten paket manager vermutlich das einfachste.
    Bei Verwendung von apt ist der Befehl: `sudo apt install libgtk-3-dev`

    gobject-introspection: `sudo apt install gobject-introspection`
    
    Things not preinstalled on Ubuntu:
    pkg-config: `sudo apt install pgk-config`
    girepository: `sudo apt install libgirepository1.0-dev`

* Windows:
    Die Installation erfolgt über __MSYS2__.  
    Der Installationsbefehl lautet `pacman -S mingw-w64-x86_64-gtk3`.
        gtk4 (evtl. für spätere Versionen): `pacman -S mingw64/mingw-w64-x86_64-gtk4`
    
    gobject-introspection:`pacman -S mingw64/mingw-w64-x86_64-gobject-introspection`


    Wenn man keine selbst gepflegte MSYS2-Installation wünscht kann man die von stack mitgebrachte verwenden.
    Die Installation von gtk3 erfolgt dann über `stack exec -- pacman -S mingw-w64-x86_64-gtk3`

TODO: evtl. umstellen auf gtk4 (geplant)

#### haskell-gi Voraussetzugen

TODO: auch bei rust benötigt?

haskell-gi requires certain development libraries to be present.
```sh
sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```

[Source](https://github.com/haskell-gi/haskell-gi/blob/master/README.md#installation)


## Ausführen des Programms

Zum Ausführen kann wieder cargo verwendet werden.  
Der Befehl lautet `cargo run`.  
Zusätzliche Kommandozeilen-Parameter (siehe Unten) müssen getrennt durch `--` übergeben werden.

Alternativ kann natürlich direkt die von `cargo build` erzeugte binary gestartet werden.

### Starten des pigpio daemon

Um eine Ausführung ohne root-Rechte zu ermöglichen wird `pigpiod` verwendet.
Mehr Informationen dazu auf der [pigpio Website](http://abyz.me.uk/rpi/pigpio/pigpiod.html).
Der Befehl zum starten lautet

```sh
sudo pigpiod
````

### Unterstütze Kommandozeilen-Parameter

* -h | --help  
    Zeige den Hilfstext an. Dieser wird automatisch erzeugt, wodurch Teile davon auf englisch sind.
* -v | --version  
    Zeige die aktuelle Version an.
* -p | --print  
    Wenn diese Flag gesetzt ist werden die Ausgaben der Raspberry Pi Ausgänge (Pins) nicht als Ausgang verwendet.
    Es wird stattdessen eine Konsolenausgabe erzeugt.  
    Diese Flag ist vor allem zum Testen auf anderen Systemen gedacht.
* --ui=Cmd | GTK  
    Auswahl der Benutzer-Schnittstelle (Standard: GTK).
    Bei Installation mit "--flag Zugkontrolle:-gui" wird immer das Cmd-UI verwendet.
* -lDATEI | --load=DATEI  
    Versuche den in DATEI (im `yaml`-Format) gespeicherten Zustand zu laden.
    Wenn die Datei nicht existiert/das falsche Format hat wird ohne Fehlermeldung mit einem leeren Zustand gestartet.
* --pwm=HardwarePWM | SoftwarePWM  
    Gebe an, welche PWM-Funktion bevorzugt verwendet wird (Standard: SoftwarePWM).
    Nachdem nur das Einstellen der hardware-basierten PWM-Funktion Root-Rechte benötigt werden diese bei Verwendung von `--pwm=SoftwarePWM` nicht benötigt.
* --sprache=Deutsch | Englisch  
    Wähle die verwendete Sprache. Ein Wechsel ist nur durch einen Neustart möglich.
* --seiten=Einzelseiten | Sammelseiten  
    Soll im Gtk-UI jede Kategorie eine eigene Seite bekommen?  
    Vor allem auf kleineren Bildschirmen ist die Option `--seiten=Einzelseiten` zu empfehlen.

### Starten durch ziehen einer Datei auf die binary

Wird nur ein Kommandozeilenargument übergeben wird versucht dieses als Datei zu öffnen und zu laden.

* Unter `Windows` entspricht dies dem ziehen (drag-and-drop) einer Datei auf die Executable.
* Unter `Linux` (nautilus window manager) ist ein Start über ziehen auf die Binary nicht möglich.
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

  * [Nautilus drag-and-drop](https://askubuntu.com/questions/52789/drag-and-drop-file-onto-script-in-nautilus)
  * [aktueller Ordner in .desktop Datei](https://stackoverflow.com/a/56202419)
