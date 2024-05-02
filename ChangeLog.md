# Changelog for Zugkontrolle

## Unreleased changes

- Füge fehlende schließende Klammer in der `Display`-Implementierung für Input/OutputAnschluss hinzu.

## 5.0.0

- Aufteilen in mehrere crates. Dazu waren ein paar Anpassungen notwendig:
  - Neuer `KnopfThema`-trait um Canvas-basierte Darstellungen über das `Thema` zu parametrisieren.
  - Invertiere parameter + implementierender Typ für den `KnopfNachricht`-Trait.
- Weitere breaking API Änderungen:
  - Entferne verbleibende 2-suffixe.
  - Behebe clippy-lints, die eine public API-Anpassung benötigen (z.B. needless_pass_by_value).
  - Entferne `WinkelGradmaß`. Ein Konstruktor für `Winkel` ist ausreichend.
  - Entferne Trigonometrie-Trait. Verwende statt dessen Methoden auf dem `Winkel`-Typ.
  - Entferne Modal-Zustand. Verwende stattdessen Nachrichten & Konstruktor-Parameter.
  - Füge padding beim bewegen-Widget hinzu.
  - Zugkontrolle-Parameter `S` muss zusätzlich `Display` implementieren.
  - Anpassen der `Nachricht`-Typen.
  - Umbenennen `SteuerungAktualisierenFehler::Deserialisieren` -> `Reservieren`.
  - Umbenennen `Ergebnis::FehlerMitErsatzwert` -> `WertMitWarnungen`.
- Dunkles Thema. Dazu wurden u.a. die `zeichnen` Methode des `Cache` angepasst.
- Verwende PickList für Modus-Auswahl.
- Verwende `rfd` zur Datei-Auswahl beim Speichern und Laden.
- Zeige Schließen-Knopf wieder am Ende im Lizenzen-Widget an.
- Anpassen der Widget-Anordnung der Kopf-Leiste.
- Erlaube (nachträgliches) Anpassen des Interrupt-Pins eines Pcf8574.
- Verwende Slider auch für KonstanteSpannung-Geschwindigkeiten.
- Lizenzen werden direkte über `include_str` eingebunden.
- Verbesserungen im `Gleise`-canvas:
  - Touch-Bewegungen aktualisieren nicht mehr die gespeicherte Maus-Position.
  - Zeige Gleise direkt nach dem hinzufügen an.
  - Korrigiere Offset bei gedrehtem Canvas.
  - Anpassen des Mauszeigers, wenn er über einem Gleis ist.
  - Anpassen des Mauszeigers, wenn er über einem `Knopf` ist und dadurch ein Gleis hinzugefügt wurde.
  - Bevorzuge genau angeklickte Gleise, bevor der Toleranz-Bereich berücksichtigt wird.
- Ersetzte Debug-Ausgaben (die durch clippy erkannt werden) für Fehler durch Display-Ausgabe über `thiserror::Error`
- Aktualisiere auf iced v0.12

## 4.0.3

- Knöpfe zeigen wieder an, wenn sich der Mauszeiger über ihnen befindet.
- Bewegen- und Drehen-Widgets reagieren auf Touch-Events.
- Zeichne diagonale Richtungen für das Bewegen-Widget.

## 4.0.2

- Drehen-Widget passt den Mauszeiger an, wenn der Mauszeiger auf den Knopf zeigt, oder er gezogen wird.
- Bewegen-Widget passt den Mauszeiger an, wenn der Mauszeiger auf aktive Elemente zeigt.
- Verwende async_io für die async-Funktion `Nachricht::nach_sleep`.
- Korrigiere Zeichen-Reihenfolge der `Gleise::draw`-Methode.
  Es werden z.B. wieder beide Seiten von überlappenden Verbindungen angezeigt.
- Färbe den Hintergrund von überlappenden Verbindungen mit Gehalten-Beteiligung erneut ein.

## 4.0.1

- Gleise-Canvas hört auf Touch-Eingaben. Es können mehrere Gleise gleichzeitig bewegt werden.
- Modal-Widget unterstützt passthrough_events, die immer vom Underlay verarbeitet werden,
  selbst wenn das Overlay aktuell angezeigt wird.

## 4.0.0

- neues Feature "raspi", ersetzt bisherige target-spezifische Logik
- Kontakte werden auf dem Canvas als Kreise angezeigt
- Kontakte können für Geraden und Kurven per Doppelklick eingestellt werden
- Zusätzliche "trailing" Bytes werden beim Laden nicht mehr ignoriert. Stattdessen kommt es zu einem Fehler.
- Neues ID-basiertes Speicherformat.
  - Weiterhin sehr stark an wirklich verwendete Datenstrukturen angelehnt.
  - Gleis-Definitionen kommen nur noch ein mal vor.
- verbessere build-scripts:
  - neue targets: 64-bit raspi, x86_64 linux
  - erkenne automatisch host target-triple
  - binaries enthalten immer target-triple + version
  - lese name+version aus Cargo.toml
  - ausführen unabhängig vom aktuellen Arbeitsverzeichnis möglich
  - stelle sicher ziel-Ordner existiert, bevor scp ausgeführt wird
  - config.ini um Einstellungen lokal zu überschreiben
  - deploy.py "bearbeitet" nur aktivierte targets
  - neues "raspi"-feature wird automatisch anhand des targets gesetzt
- refactoring:
  - verschiebe MitRichtung nach steuerung::weiche
  - verwende &str statt &String für Zeichne::name_und_beschreibung und MitName::name
  - extrahiere application::Nachricht (+ Hilfsgrößen) und AuswahlZustand in eigene Module
  - verwende From-implementierungen zur Konvertierung der Auswahl-Nachrichten.
  - entferne die meisten `pub use` re-exports
  - entferne lifetime vom MitSteuerung
  - verschiebe \*Serialisiert-Definitionen für Sammel-Strukturen nach gleis::gleise::de_serialisieren::v4
  - Neue Implementierung für Ids (jetzt zahlen-basiert).
  - AuswahlZustand verwendet Id als ersten Parameter (analog zu allen anderen Datenstrukturen)
- aktualisiere iced auf Version 0.10.0 (+ aktualisiere weitere dependencies)
- pcf8574::Port::als_(input|output) geben immer den Port zurück,
  selbst wenn es beim Initialisieren einen Fehler gab (z.B. pcf8574 nicht angeschlossen).
- (Input|Output)Anschluss::reserviere gibt FehlerMitErsatzwert zurück,
  wenn initialisieren nicht erfolgreich war (z.B. pcf8574 nicht angeschlossen).
- entferne RwLock aus Gleise-Struktur, um auftretende Deadlocks zu beheben
- ersetzte Mutex in Steuerung-Struktur, um Deadlocks bei gleichzeitigem neuzeichnen zu beheben
- erstelle FlatMap, um mehrere Nachrichten zurückgeben zu können
- ermögliche Auswahl des I2C-Busses bei Anschlüssen
- alle I2C-Busse sind standard-mäßig deaktiviert
- neue Gleise erscheinen direkt an der richtigen Position
- Anschluss-Auswahl für Weichen/Kreuzungen wird immer richtig geschlossen
- Schriftgröße von Beschreibungen skaliert nun ebenfalls
- gebe Empfänger Id-Parameter, falls mehrere benötigt werden
- Reserviere hat jetzt zusätzlich RefArg und MutRefArg als (veränderliche) Referenz-Argumente für reserviere
- Anpassen der Fehler-Enums
- Erlaube einfaches bearbeiten vorhandener Streckenabschnitte und Geschwindigkeiten

## 3.0.0

- Fehlermeldung bei Anschlüsse anpassen schließt nicht mehr das Dialog-Fenster
- Zeige Fließend-Status von Streckenabschnitten über Transparenz an
- Verwende einen Slider anstelle von Buttons zum Skalieren
- Nach erfolgreichem Speichern wird der Knopf temporär Grün gefärbt
- Aktuelle Richtung einer Weiche wird über Transparenz der Füllung angezeigt
- Neue Kommandozeilen-Argumente zoom, x, y, winkel
- Verbesserte Widgets zum einstellen von Position und Rotation
- Erlaube entfernen von Weichen-Steuerung
- Bisherige Anschlüsse werden beim Anpassen von z.B. Weichen berücksichtigt
- Bei fehlgeschlagenem Laden wird immer ein leerer Zustand erzeugt
- Fehlermeldungen enthalten falls möglich den betroffenen Anschluss
- Länger dauernde Aktionen (z.B. Weiche schalten) werden in einem eigenen Thread ausgeführt
- Bei Drag&Drop wird ein einrasten vor loslassen des Gleises angezeigt
- Klicks müssen nicht mehr aufs Pixel genau das Gleis treffen
- Verwende selbst-geschriebene Bibliothek für Kommandozeilen-Argumente,
    inklusiver Anpassung der erlaubten Argumente
- Anpassen des Speicher-Formats.
  Ein Laden von mit Version 2.0.X erzeugten Dateien ist weiterhin möglich.
- Bibliothek: erlaube assoziieren von Streckenabschnitten mit einer Geschwindigkeit
- Bibliothek: erlaube Verwendung anderer I2c-Kanäle für Pcf8574-Anschlüsse
- Bibliothek: Zugtyp kein Typ-Parameter mehr, sonder ein echter Typ gespeichert im Gleise-Zustand
  - Dadurch Spurweite als zusätzlicher Parameter vieler Funktionen
  - Leiter als Parameter, sofern notwendig (z.B. Geschwindigkeit)
  - Teil des Speicherformats
  - Ermöglicht im Prinzip nachträgliches hinzufügen neuer Gleise zu einem Zugtyp
- Bibliothek:
  Ehemalige Konstanten bei Funktionen in `steuerung::geschwindigkeit` und `steuerung::weiche`
  sind jetzt Argumente.
  Sie werden für das GUI im Zugtyp gespeichert.
- Bibliothek: Viele fehlenden docstrings ergänzt.
- Bibliothek: GUI an neue iced-version mit state-less widgets angepasst
- Bugfix: Bei fehlgeschlagenem Einstellen der Geschwindigkeit wird das UI nicht verändert
- Refactoring: Aufsplitten von großen Modulen (application.rs, gleise.rs)
- Refactoring: Verwende AnyId statt GleisId\<Z\>.
    Ids unterschiedlicher Gleise sind jetzt wirklich unabhängig
- Refactoring: Alle Module sind public
- Refactoring: Module umbenannt
  - serde.rs -> de_serialisieren.rs
  - anchor.rs -> verbindung.rs
  - polarity.rs -> polarität.rs
  - maps.rs -> daten.rs
  - args.rs -> argumente.rs
  - button.rs -> knopf.rs
- Refactoring: Module neu sortiert
  - zugkontrolle::application::gleis -> zugkontrolle::gleis
  - zugkontrolle::application::typen -> zugkontrolle::typen
  - zugkontrolle::farbe -> zugkontrolle::typen::farbe
  - Untermodule entsprechend mit-verschoben
  - gleise::de_serialisieren ist jetzt ein Untermodul von gleise::daten
- Refactoring: diverse Methoden-namen übersetzt
- Refactoring: diverse Fehler-Typen umbenannt Error->Fehler
- Refactoring: diverse Typen umbenannt Status->Zustand (`state` ist `Zustand`, nicht `Status`)
- Refactoring: diverse Nachrichten-Typen umbenannt Message->Nachricht
- Refactoring: diverse Verbindungs-Typen umbenannt
  - AnchorName->VerbindungName
  - AnchorPoints->Verbindungen
- Refactoring: Typ umbenannt GleiseMaps -> GleiseDaten
    Dazu Änderung der internen Repräsentation auf einen RTree
- Refactoring: Typen umbenannt:
  - Grabbed -> Gehalten
  - Args -> Argumente
  - args::Zugtyp -> ZugtypArgument
  - Button -> Knopf
- Refactoring: GleisId neu gedacht
- Refactoring: Anpassung diverser Traits an neue Anforderungen
- Refactoring: GleiseMap-Trait (jetzt DatenAuswahl) ist nicht mehr public.
    Stattdessen werden explizite Methoden für jeden Typ bereitgestellt.
- Crate umbenannt: zugkontrolle-derive -> zugkontrolle-macros
- dependencies aktualisiert

## 2.0.1

- Checkbox zum automatischen Festlegen von Streckenabschnitten startet deaktiviert
- Geschwindigkeit wird nach umdrehen auch als 0 angezeigt
- Neues Kommandozeilen-Argument: `--verbose`

## 2.0.0

- Neuschreiben in rust
- Anzeige & Steuerung über Gleisplan, kein Cmd-UI mehr
- Doppelklick für festlegen der Richtungs-Anschlüsse einer Weiche/Kreuzung
- Streckenabschnitt erstellen über eigenes Untermenü
- Zuweisen zu Gleisen über Erstellen/Klick nach Auswahl
- Geschwindigkeiten erstellen über eigenes Untermenü
- Lego-Weichen werden auf Raspi-Seite genauso bedient wie Märklin-Weichen
- Streckenabschnitte schalten über Klick auf assoziierte Gerade/Kurve
- Schalten von Weichen/Kreuzungen über Klick auf das jeweilige Gleis
- Speichern über ein binary Format
- Kontakte können aktuell nicht erstellt werden
- Aktuell ist nur deutsche Sprache unterstützt
- Wegstrecken/Pläne sind aktuell nicht unterstützt

## 1.2.0.1

- Wartezeit zwischen i2c-Befehlen hinzugefügt.
    PCF8574 sollten nicht mehr z.B. bei "einstellen" einer Wegstrecke den falschen Wert erhalten.

## 1.2.0.0

- Neues StreckenObjekt: Kontakt
    Ein Kontakt repräsentiert ein Eingangsignal, typischerweise eine Kontakt-Schiene.
    Es kann gewartet werden, bis bei einem Kontakt ein Signal eingeht.
    Eine Wegstrecke wartet, bis bei einem ihrer Kontakte ein Signal eingeht.
    Im Zuge dessen wurden Sammelseiten neu sortiert.
- Gtk-UI: Plan & Märklin-Weiche Hinzufügen in ScrollbaresWidget angezeigt.
- Gtk-UI: Anschlüsse werde gruppiert in einem Expander angezeigt.
- Gtk-UI: Dummy-Fenster vergrößert, damit der Title lesbar ist.
- Gtk-UI: Bearbeiten von StreckenObjekten ist möglich.
    Das alte StreckenObjekt wird dabei nicht entfernt.
- Gtk-UI: AssistantHinzufügen wird nur erstellt, wenn er wirklich benötigt wird.
    Dadurch sollte die Startzeit verkürzt werden.
- Gtk-UI: Pläne synchronisieren nun ebenfalls andere Widgets.

## 1.1.0.0

- Support für PCF8574 hinzugefügt
- MVar durch TVar/TMVar ersetzt, LinkedMVar entfernt
- unqualifizierter Import wird immer explizit angegeben
- GADTs verwendet um Zugtyp-Mischformen zu vermeiden
- Ein Plan kann andere Pläne ausführen (inklusive rekursivem Aufruf von sich selbst)
- Aktuelle Version über Data.Version.Version-Datentyp gespeichert (ausgelesen aus Paths_Zugkontrolle)
- Bei Märklin-Zugtyp ist eine alternative BahngeschwindigkeitVariante möglich.
    Zwei Pins, einer für Fahrstrom (<=16V), einer für Umdrehen-Strom (25V).
- Sprachwechsel möglich, ohne das Programm neu starten zu müssen.
- `yaml` anstelle von `json` als Speicherformat verwendet.
    Alte Speicherdateien können weiterverwendet werden.
- Cmd-UI: ..Unbekannt/..StatusAnfrage-Konstruktoren in AnfrageFortsetzung-Datentyp ausgelagert
- Gtk-UI: Das Fenster sollte beim Programmstart nicht mehr zu klein sein.
- Gtk-UI: Fenster wird bei Programmstart maximiert.
- GTK-UI: Verwendung von HPaned/VPaned um mehrere Kategorien gleichzeitig anzuzeigen (optional).
- GTK-UI: Jeder Plan erhält seine eigene ProgressBar.
- Gtk.UI: Wegstrecken-Elemente werden scrollbar angezeigt (weiterhin in einem Expander versteckt).
- Gtk-UI, Hinzufügen: Eingabefelder für Namen werden nicht mehr zurückgesetzt.
- Gtk-UI, Hinzufügen: Alle bisher gewählten Aktionen können durch "Zurücksetzen" entfernt werden.
- Gtk-UI: Widgets werden synchronisiert.
    Wird z.B. der Strom einer Wegstrecke auf fließend gesetzt passiert das auch bei der Anzeige
    aller enthaltenen Streckenabschnitte.
- Tests entfernt. Wurden nicht gepflegt, somit ist deren Nicht-Existenz die ehrlichere Lösung.

## 1.0.0.15

- Bugfix: Cmd-UI erkennt wieder, wann ein Plan ausgeführt wird (war invertiert).
- Cmd-UI: Bei gesperrtem Plan kann gewartet werden, bis eine Ausführung wieder möglich ist.
    Man muss nicht erst ins Hauptmenü zurückkehren.

## 1.0.0.14

- Fließend-Value (HIGH/LOW) ist jetzt eine Eigenschaft jedes Objekts.  
    Das zugehörige Kommandozeilenargument `--fließend` wurde entsprechend entfernt.
- SEQueue umbenannt in Warteschlange
- Pläne können jetzt abgebrochen werden
- Pläne können nur ausgeführt werden, wenn kein Pins bei einem bereits ausgeführtem Plan verwendet wird.

## 1.0.0.13

- Bugfix (Cmd-UI): Zeile zum Unterstreichen erscheint nun auch unter dem Titel und ist lang genug
- Umdrehen erhält eine automatische Wartezeit nach der Umdrehen-Aktion
- Englische Sprache hinzugefügt. Auswahl über die Kommandozeilen-Option `--sprache=Englisch`.
- Durch ziehen einer .json-Datei auf die Executable wird versucht diese bei Programmstart zu laden.
- SoftwarePWM-Frequenz auf 500Hz erhöht.

## 1.0.0.12

- Bugfix (GTK-UI): Objekte werden nur vollständig und nicht nur graphisch entfernt.
- Bugfix (GTK-UI): Wegstrecken sorgen auch alleine dazu,  dass betroffene Aktionen angezeigt werden.
- Verbliebene englische Namen, die nicht an bestehende Funktionen angelehnt sind ins deutsche übersetzt.

## 1.0.0.11

- Bugfix (GTK-UI): Weichen werden beim Plan erstellen nur dann angezeigt, wenn sie diese haben
    und nicht umgekehrt
- GTK-UI:
  - Beim Plan erstellen werden Bahngeschwindigkeiten und Wegstrecken zum Umdrehen
      nur bei passendem Zugtyp angezeigt
  - Laden dreht nicht mehr die Reihenfolge um
- Aktionen einer Wegstrecke (z.B. Weichen einstellen) laufen nun parallel ab
- Bei Verwenden von --pwm=SoftwarePWM werden keine Root-Rechte benötigt
- Laden schlägt bei fehlerhaften Teilstück nicht komplett fehl.
    Es werden nun alle korrekt formatierten Teilestücke geladen.
- Zum Laden werden immer deutsche Namen (unabhängig von Language.hs) verwendet.
    Einmal erzeugte json-Dateien können somit unabhängig von der Sprache verwendet werden.
- Beim Hilfe-Text werden unterstützte Optionen angezeigt.
- Flag -v|--version hinzugefügt
- Bugfix: getPWMValue reagiert nun richtig auf eine reduzierte PWMRange
- PWMValue skaliert nur quadratisch mit dem Eingabewert.
    Dadurch skaliert der Eingabewert linear zur Effektivspannung.

## 1.0.0.10

- HIGH/Low vertauscht (unverändert für PWM-Output)
    kann per Kommandozeilenargument `--fließend=\<Value\>` eingestellt werden
- SoftwarePWM kann auf allen Pins per Kommandozeile "--pwm=SoftwarePWM" erzwungen werden

## 1.0.0.9

- Zeit zum stellen einer Weiche reduziert
- Bugfix: LinkedMVar merkt sich Änderungen wieder

## 1.0.0.8

- SEQueue-Funktionen umbenannt
- LinkedMVar:
  - Update-Aktion nicht mehr als Argument übergeben
  - Hinzufügen einer zusätzlichen Update-Aktion erzeugt keine neue LinkedMVar,
      sondern verändert die aktuelle LinkedMVar

## 1.0.0.7

- Plan erstellen:
  - Letztes Element kann entfernt werden
  - Fahrtrichtung kann für Umdrehen mit Lego-Weichen angegeben werden.
  - GTK-UI: Aktionen eines Plans werden in einem ScrolledWindow innerhalb eines Expanders angezeigt

## 1.0.0.6

- Bugfix: Beim erstellen eines Plans mit dem Cmd-UI wurde die Error-Funktion aufgerufen.

## 1.0.0.5

- Bugfix: Beim Hinzufügen einer Wegstrecke wurde immer die erste Richtung einer Weiche ausgewählt.
- Lego-Weiche/Bahngeschwindigkeit von Cmd-UI unterstützt.
- Lego-Weiche/Bahngeschwindigkeit von GTK-UI unterstützt.

## 1.0.0.4

- GTK-UI:
  - nameEntry erhält PlaceholderText (wird gezeigt, wenn Entry leer und nicht fokussiert)
- Erlaube Kompilation ohne GTK-UI:
    gtk3- und lens-Pakete setzen Cabal-Packet voraus, dessen Installation sehr lange
      (und viel Arbeitsspeicher) braucht.
    Wer nicht so lange warten will kann auf das Kommandozeilen-basiertes UI zurückgreifen.
    Installation ohne GTK-UI erfolgt durch den Befehl "stack build --flag Zugkontrolle:-gui"

## 1.0.0.3

- GTK-UI:
  - vBoxen beim hinzufügen von Wegstrecke & Plan skalieren
  - vBoxen beim hinzufügen von Plan scrollbar
  - Bugfix: RadioButtons für Richtung einer Weiche beim Hinzufügen einer Wegstrecke hängen jetzt zusammen
  - CheckBox immer am Anfang beim Hinzufügen einer Wegstrecke
  - Bugfix: Hinzufügen-Wegstrecke-Knopf ist nicht permanent ausgegraut
  - PinSpinBox skaliert nicht mehr horizontal
  - Beim Hinzufügen wird das Namen-Entry fokussiert
- CMD-UI:
  - Bei endlicher Anzahl werden akzeptierte Eingaben angezeigt
  - Fehlerhafte Eingabe wird in roter Farbe gemeldet.

## 1.0.0.2

- Bugfix Hinzufügen Plan (Einstellen)
- GTK-UI: Typ nicht mehr explizit erwähnt, kleiner Abstand nach Name hinzugefügt
- Linux-Version bricht ab, wenn sie nicht mit Root-Rechten ausgeführt wird.

## 1.0.0.1

- Immer Fehlschlagende FromJSON-Parser entfernt. Zusätzliche Funktion bei Laden-Befehl verwendet.
- Anfangs-Status für GTK-UI hinzugefügt.

## 1.0.0.0

- Erste Version
- Noch ohne UI-Unterstützung für Lego-Bahngeschwindigkeiten und -Weichen
