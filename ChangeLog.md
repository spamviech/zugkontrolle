# Changelog for Zugkontrolle
1.0.0.0:
- Erste Version
- Noch ohne UI-Unterstützung für Lego-Bahngeschwindigkeiten und -Weichen
1.0.0.1:
- Immer Fehlschlagende FromJSON-Parser entfernt. Zusätzliche Funktion bei Laden-Befehl verwendet.
- Anfangs-Status für GTK-UI hinzugefügt.
1.0.0.2:
- Bugfix Hinzufügen Plan (Einstellen)
- GTK-UI: Typ nicht mehr explizit erwähnt, kleiner Abstand nach Name hinzugefügt
- Linux-Version bricht ab, wenn sie nicht mit Root-Rechten ausgeführt wird.
1.0.0.3:
- GTK-UI:
    - vBoxen beim hinzufügen von Wegstrecke & Plan skalieren
    - vBoxen beim hinzufügen von Plan scrollbar
    - Bugfix: RadioButtons für Richtung einer Weiche beim Hinzufügen einer Wegstrecke hängen jetzt zusammmen
    - CheckBox immer am Anfang beim Hinzufügen einer Wegstrecke
    - Bugfix: Hinzufügen-Wegstrecke-Knopf ist nicht permanent ausgegraut
    - PinSpinBox skaliert nicht mehr horizontal
    - Beim Hinzufügen wird das Namen-Entry fokusiert
- CMD-UI:
    - Bei endlicher Anzahl werden akzeptierte Eingaben angezeigt
    - Fehlerhafte Eingabe wird in roter Farbe gemeldet.
1.0.0.4:
- GTK-UI:
    - nameEntry erhält PlaceholderText (wird gezeigt, wenn Entry leer und nicht fokusiert)
- Erlaube Kompilation ohne GTK-UI:
    gtk3- und lens-Packete setzen Cabal-Packet vorraus, dessen Installation sehr lange (und viel Arbeitsspeicher) braucht.
    Wer nicht so lange warten will kann auf das Kommandozeilen-basiertes UI zurückgreifen.
    Installation ohne GTK-UI erfolgt durch den Befehl "stack build --flag Zugkontrolle:-gui"
1.0.0.5:
- Bugfix: Beim Hinzufügen einer Wegstrecke wurde immer die erste Richtung einer Weiche ausgewählt.
- Lego-Weiche/Bahngeschwindigkeit von Cmd-UI unterstützt.
- Lego-Weiche/Bahngeschwindigkeit von GTK-UI unterstützt.
1.0.0.6:
- Bugfix: Beim erstellen eines Plans mit dem Cmd-UI wurde die Error-Funktion aufgerufen.
1.0.0.7:
- Plan erstellen:
    Letztes Element kann entfernt werden
    Fahrtrichtung kann für Umdrehen mit Lego-Weichen angegeben werden.
    GTK-UI: Aktionen eines Plans werden in einem ScrolledWindow innerhalb eines Expanders angezeigt
## Unreleased changes