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

Zur Installation aus dem Quellcode wird stack empfohlen.  
Nach Installation aller Abhängigkeiten (siehe Unten) kann durch den Aufruf von `stack build` eine Executable in einem Unterordner von `./.stack-work` erstellt werden.
Durch Aufruf von `stack install` wird eine Kopie der Executable im Unterordner "./bin" erstellt.

Nachdem die Installation der Pakete "gtk3" und "lens" eine Installation des "Cabal"-Pakets
vorraussetzen, welches sehr lange dauert (bei mir ~1 Tag) wird eine Installation ohne beide
Pakete unterstützt.   Dazu muss der Installations-Befehl erweitert werden um die flag gui auf false
zu setzten. Der neue Installationsbefehl lautet somit:
    `stack install --flag Zugkontrolle:-gui`  
Eine Verwendung des GTK-UI ist dann natürlich nicht mehr möglich.  
Ein möglicher Arbeitsablauf ist dann Erstellen der Repräsentation auf einem Desktop-Rechner, speichern
und  kopieren in einer yaml-Datei und anschließendes Ausführen mit Cmd-UI auf dem Raspberry Pi.

Alternativ wird für jede Release-Version eine vorkompilierte binary im Unterordner `bin` angeboten.

### Installation via cabal (ab Zugkontrolle 1.3)
ghc-8.10.1 auf arm-Architekturen funktioniert nicht problemlos, so dass eine Installation via stack momentan (2.6.2020) nicht möglich ist.
Für eine Installation kann stattdessen cabal-install (über apt-get erhältlich; Version 2.2 at time of writing) verwendet werden.

Nach Installation aller benötigten Programme und Developer Bibliotheken (siehe unten) ist mein workflow:
* Eingabe von `cabal configure`
* `cabal install <Paketname>` für alle fehlenden Pakete
* wiederholen, bis keine Fehler mehr auftreten
* `cabal build`
* binary finden (weiß noch nicht wie)

### Installation von stack

Zur Installation von stack wird empfohlen die Anleitung auf der
[zugehörigen Website](https://docs.haskellstack.org/en/stable/install_and_upgrade/) zu befolgen.
Im moment lautet der Befehl
    `curl -sSL https://get.haskellstack.org/ | sh`

Leider bricht die neueste stack-Version beim kompilieren mit einer Fehlermeldung wegen fehlender
haddock-binary ab. Als Abhilfe wird
[Version 1.9.3 in Zugkontrolle-Ressourcen](https://github.com/spamviech/ZugkontrolleRessourcen)
bereitgestellt.

Nach Version 2.1.3 werden keine arm-binaries mehr angeboten, weshalb auch davon eine in o.g. repository bereitgestellt wird.
Ab Zugkontrolle 1.3 wird diese stack-Version benötigt, da 1.9.3 den benötigten Resolver nicht findet.

### Swap-Datei erstellen

Wird ein kompilieren inklusive des Gtk-UI gewünscht reicht der Arbeitsspeicher eines RaspberryPi v3B+
leider nicht aus. Deswegen muss eine swap-Datei erstellt werden:

```sh
    sudo dphys-swapfile swapoff   # disable swap
    sudo nano /etc/dphys-swapfile # and set 'CONF_SWAPSIZE' to 2048
    sudo dphys-swapfile setup     # refresh with new settings
    sudo dphys-swapfile swapon    # re-enable swap
```

#### LLVM 3.9

ghc-8.2.2 benötigt weiterhin LLVM 3.9 um zu funktionieren.
Der Download ist von der [LLVM Seite](https://releases.llvm.org/download.html#3.9.1) möglich.
Ich empfehle die dort vorhandenen binaries (armv7a Linux) herunterzuladen und zu entpacken:

```sh
    tar -xf clang+llvm-3.9.1-armv7a-linux-gnueabihf.tar.xz
    cd clang+llvm-3.9.1-armv7a-linux-gnueabihf/
    sudo mkdir /usr/lib/llvm-3.9
    sudo mkdir /usr/lib/llvm-3.9/bin
    sudo mv bin/* /usr/lib/llvm-3.9/bin/
    sudo mkdir /usr/lib/llvm-3.9/include
    sudo mv include/* /usr/lib/llvm-3.9/include/
    sudo mkdir /usr/lib/llvm-3.9/lib
    sudo mv lib/* /usr/lib/llvm-3.9/lib/
    sudo mkdir /usr/lib/llvm-3.9/libexec
    sudo mv libexec/* /usr/lib/llvm-3.9/libexec/
    sudo mkdir /usr/lib/llvm-3.9/share
    sudo mv share/* /usr/lib/llvm-3.9/share/
```

Jetzt muss noch sichergestellt werden, dass stack/ghc die erzeugten Dateien auch findet.
Dazu kann die PATH-Variable ergänzt werden (export PATH=/usr/lib/llvm-3.9/bin:$PATH).
Alternativ können folgende Zeilen zur `config.yaml` hinzugefügt werden.

```yaml
extra-path:
- /usr/lib/llvm-3.9/bin
```

Alternativ ist es möglich LLVM aus dem Quellcode zu kompilieren.
Leider brach das bei mir nach langer Wartezeit mit einer Fehlermeldung ab, so dass ich hier nur die
Befehle aus der Anleitung wiedergeben kann.
Es wird cmake benötigt: [LLVM CMake Anleitung](https://www.llvm.org/docs/CMake.html)

```sh
    sudo apt-get install cmake
    mkdir mybuilddir
    cd mybuilddir
    cmake path/to/llvm/source/root
    cmake --build .
    cmake --build . --target install
```

#### libtinfo-dev

Wer jetzt `stack build` eingibt wird festellen, dass ghc mit einer Fehlermeldung beim ersten
TemplateHaskell-Modul (Zug.UI.Gtk.Klassen) abstürzt. Ohne TemplateHaskell würde die Fehlermeldung
beim linken auftreten. Um das Problem zu lösen muss das Paket `libtinfo-dev` installiert werden:

```sh
sudo apt-get install libtinfo-dev
```

#### llvm-9
ghc 8.10.1 verwendet llvm Version 9. Eine Installation ist über apt-get möglich.
```sh
sudo apt-get install llvm-9
```

#### ghc 8.10.1
```sh
wget http://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-armv7-deb9-linux.tar.xz
tar -xf ghc-8.10.1-armv7-deb9-linux.tar.gz
cd ghc-8.10.1
./configure CONF_CC_OPTS_STAGE2="-marm -march=armv7-a" CFLAGS="-marm -march=armv7-a"
sudo make install
```
Die von stack produzierte Binary funktioniert nicht, weshalb eine globale ghc-Version installiert werden muss.
(Probleme mit autoconf, siehe configure-Optionen)

#### Relevanter Blog-Post

Viele der Informationen wurden aktualisiert anhand
[dieses Blog-Posts](https://svejcar.dev/posts/2019/09/23/haskell-on-raspberry-pi-4/).
Sollten sie nicht mehr aktuell sein ist es ratsam, nach aktuelleren Anleitungen für dann aktuelle
RaspberryPi-Versionen zu suchen.

[Neuerer Bog-Post](https://www.haskell.org/ghc/blog/20200515-ghc-on-arm.html)

### Installation von WiringPi

Unter Raspbian ist standardmäßig eine Version von wiringpi installiert.  
Um die neueste Version zu installieren ist es zu empfehlen die Installationsanweisungen auf [der wiringPi-Seite](http://wiringpi.com/download-and-install/) zu berücksichtigen.

### Installation von GTK+

Um  das GTK-UI zu verwenden muss natürlich GTK+ (Version 3) installiert werden.
Dazu ist am besten die Anleitung auf [der Gtk-Website](https://www.gtk.org/download/index.php) zu befolgen.

* Linux/Raspbian:
    Falls nicht schon installiert, ist eine Installation über den verwendeten paket manager vermutlich das einfachste.
    Bei Verwendung von apt-get ist der Befehl: `sudo apt-get install libgtk-3-dev`
* Windows:
    Die Installation erfolgt über __MSYS2__.  
    Der Installationsbefehl lautet `pacman -S mingw-w64-x86_64-gtk3`.

    Wenn man keine selbst gepflegte MSYS2-Installation wünscht kann man die von stack mitgebrachte verwenden.
    Die Installation von gtk3 erfolgt dann über `stack exec -- pacman -S mingw-w64-x86_64-gtk3`

#### Probleme bei neueren Versionen (Windows)

Bei aktuellen Versionen unter Windows schlägt das kompilieren der `cairo`- und `gtk3`-Bibliotheken fehl.
Als Workaround werden alte Versionen der __MSYS2__-Pakete in
[Zugkontrolle-Ressourcen](https://github.com/spamviech/ZugkontrolleRessourcen) angeboten.  
Diese werden über folgenden Befehl installiert: `pacman -U <Dateiname>`  
Zum Glück ist nach erstmaligem Kompilieren ein updaten auf die neueste Version wieder möglich.

#### haskell-gi Voraussetzugen
haskell-gi requires certain development libraries to be present.
```sh
sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```

[Source](https://github.com/haskell-gi/haskell-gi/blob/master/README.md#installation)

#### Weitere Abhängigkeiten
```sh
sudo apt-get install happy
```

## Ausführen des Programms

Zum Ausführen kann wieder stack verwendet werden.  
Der Befehl lautet `stack exec Zugkontrolle`.  
Zusätzliche Kommandozeilen-Parameter (siehe Unten) müssen getrennt durch `--` übergeben werden.

Bei Verwenden der Flag `--pwm=HardwarePWM` werden Root-Rechte benötigt, weil sonst nicht alle notwendigen Funktionen der WiringPi-Bibliothek zur Verfügung stehen.
Auf Linux-Systemen mit ARM-Architektur (Raspberry Pi) bricht das Programm sonst direkt mit einer Fehlermeldung ab.  
Nachdem auf nicht-RaspberryPi-Systemen sämtliche IO-Funktionen des WiringPi-Moduls durch "return ()" ersetzt wurden ist das dort natürlich nicht notwendig.

Alternativ kann natürlich direkt die von `stack install` erzeugte binary gestartet werden.

### GTK-Probleme mit stack und Windows

Wenn das Programm unter Windows nicht startet, bzw. mit dll-Fehlern abbricht (Fehlermeldungen werden bei Start über Powershell nicht angezeigt) muss der Ordner der MSYS2-Installation weiter vorne im Path stehen.

* Bei einer eigenen MSYS2-Installation ist das normalerweise: `C:\msys64\mingw64\bin`.
* Für die von stack mitgelieferte Version ist der Pfad normalerweise: `\~\AppData\Local\Programs\stack\x86_64-windows\msys2-20180531\mingw64\bin\`

Falls das immer noch nicht hilft (bei `stack exec ...` normalerweise der Fall) muss die `zlib1.dll` durch die neuere aus dem msys-Ordner ersetzt werden.  
Durch den Befehl `stack exec -- where zlib1.dll` werden alle im Pfad befindlichen in Reihenfolge aufgelistet.
Alle vor der im MSYS2-Ordner befindlichen müssen mit dieser überschrieben werden.

Im Normalfall (bei Ausführung über stack exec) betrifft das eine Datei: `~\AppData\Local\Programs\stack\x86_64-windows\ghc-8.2.2\mingw\bin\zlib1.dll\zlib1.dll`

### Probleme beim kompilieren von glib/pango/gtk3 (Windows/MSYS2)

Bei neueren Versionen von gtk3/glib2 treten Fehler der folgenden Art auf:

```stack
pango       > C:/msys64/mingw64/include/glib-2.0/glib/gspawn.h:76: (column 22) [FATAL]
pango       >   >>> Syntax error!
pango       >   The symbol `__attribute__' does not fit here.
```

Als Lösung werden alte Versionen der MSYS2-Pakete im Ordner `gtk3` mitgeliefert.
Der Befehl zum installieren lautet:
    `pacman -U <Dateiname>`

Nach kompilieren der o.g. Pakete muss ein Update durchgeführt werden, da es sonst zu dll-Problemen kommt.
Evtl. ist das bei einer frischen MSYS2-Installation nicht notwendig.

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
