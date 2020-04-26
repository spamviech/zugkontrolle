{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Zug.UI.Gtk.StreckenObjekt.WSWidgets (WSWidgets(), wegstreckePackNew) where

import qualified Data.Aeson as Aeson
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung
       (StreckenObjekt(..), BahngeschwindigkeitKlasse(..), BahngeschwindigkeitContainer(..)
      , StreckenabschnittKlasse(..), StreckenabschnittContainer(..), WeicheContainer(..)
      , KupplungKlasse(..), KupplungContainer(..), KontaktKlasse(..), KontaktContainer(..))
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.StreckenObjekt.BGWidgets ()
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen (WegstreckenElement(..), PlanElement(..))
import Zug.UI.Gtk.StreckenObjekt.KOWidgets ()
import Zug.UI.Gtk.StreckenObjekt.KUWidgets ()
import Zug.UI.Gtk.StreckenObjekt.STWidgets ()
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen (Kategorie(..), KategorieText(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp (WidgetsTyp(..))

instance Kategorie (WSWidgets z) where
    kategorie :: KategorieText (WSWidgets z)
    kategorie = KategorieText Language.wegstrecken

-- | 'Wegstrecke' mit zugehörigen Widgets
data WSWidgets (z :: Zugtyp) =
    WSWidgets
    { ws :: Wegstrecke z
    , wsWidget :: Gtk.Frame
    , wsFunktionBox :: Gtk.Box
    , wsHinzPL :: WegstreckePlanHinzufügenWidget z
    , wsTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , wsTVarEvent :: TVar EventAusführen
    , wsScaleGeschwindigkeit :: Maybe Gtk.HScale
    , wsAuswahlFahrstrom :: Maybe (AuswahlWidget Word8)
    , wsAuswahlFahrtrichtung :: Maybe (AuswahlWidget Fahrtrichtung)
    , wsToggleButtonStrom :: Maybe Gtk.ToggleButton
    }
    deriving (Eq)

-- | Widget zum Hinzufügen einer 'Wegstrecke' zu einem 'Plan'
data WegstreckePlanHinzufügenWidget (z :: Zugtyp) =
    WegstreckePlanHinzufügenWidget
    { bahngeschwindigkeit :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , bahngeschwindigkeitPwm :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , bahngeschwindigkeitKonstanteSpannung :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , streckenabschnitt :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , kupplung :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , wegstrecke :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    }
    deriving (Eq)

instance MitWidget (WSWidgets z) where
    erhalteWidget :: WSWidgets z -> Gtk.Widget
    erhalteWidget = erhalteWidget . wsWidget

instance MitWidget (GeschwindigkeitPhantom WSWidgets g z) where
    erhalteWidget :: GeschwindigkeitPhantom WSWidgets g z -> Gtk.Widget
    erhalteWidget (GeschwindigkeitPhantom wsWidgets) = erhalteWidget wsWidgets

instance (PlanElement (WSWidgets z)) => WidgetsTyp (WSWidgets z) where
    type ObjektTyp (WSWidgets z) = Wegstrecke z

    erhalteObjektTyp :: WSWidgets z -> Wegstrecke z
    erhalteObjektTyp = ws

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (WSWidgets z) m) => WSWidgets z -> m ()
    entferneWidgets wsWidgets@WSWidgets {wsTVarSprache} = do
        DynamischeWidgets {vBoxWegstrecken} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxWegstrecken wsWidgets
        entferneHinzufügenPlanWidgets wsWidgets
        liftIO $ atomically $ writeTVar wsTVarSprache Nothing

    boxButtonEntfernen :: WSWidgets z -> Gtk.Box
    boxButtonEntfernen = wsFunktionBox

    tvarSprache :: WSWidgets z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = wsTVarSprache

    tvarEvent :: WSWidgets z -> TVar EventAusführen
    tvarEvent = wsTVarEvent

instance WidgetsTyp (ZugtypEither WSWidgets) where
    type ObjektTyp (ZugtypEither WSWidgets) = ZugtypEither Wegstrecke

    erhalteObjektTyp :: ZugtypEither WSWidgets -> ZugtypEither Wegstrecke
    erhalteObjektTyp = mapZugtypEither ws

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (ZugtypEither WSWidgets) m)
                    => ZugtypEither WSWidgets
                    -> m ()
    entferneWidgets (ZugtypMärklin wsWidgets) = entferneWidgets wsWidgets
    entferneWidgets (ZugtypLego wsWidgets) = entferneWidgets wsWidgets

    boxButtonEntfernen :: ZugtypEither WSWidgets -> Gtk.Box
    boxButtonEntfernen (ZugtypMärklin wsWidgets) = boxButtonEntfernen wsWidgets
    boxButtonEntfernen (ZugtypLego wsWidgets) = boxButtonEntfernen wsWidgets

    tvarSprache :: ZugtypEither WSWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (ZugtypMärklin wsWidgets) = tvarSprache wsWidgets
    tvarSprache (ZugtypLego wsWidgets) = tvarSprache wsWidgets

    tvarEvent :: ZugtypEither WSWidgets -> TVar EventAusführen
    tvarEvent (ZugtypMärklin wsWidgets) = tvarEvent wsWidgets
    tvarEvent (ZugtypLego wsWidgets) = tvarEvent wsWidgets

instance (PlanElement (WSWidgets z)) => WidgetsTyp (GeschwindigkeitPhantom WSWidgets g z) where
    type ObjektTyp (GeschwindigkeitPhantom WSWidgets g z) = GeschwindigkeitPhantom Wegstrecke g z

    erhalteObjektTyp
        :: GeschwindigkeitPhantom WSWidgets g z -> GeschwindigkeitPhantom Wegstrecke g z
    erhalteObjektTyp (GeschwindigkeitPhantom WSWidgets {ws}) = GeschwindigkeitPhantom ws

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (GeschwindigkeitPhantom WSWidgets g z) m)
                    => GeschwindigkeitPhantom WSWidgets g z
                    -> m ()
    entferneWidgets (GeschwindigkeitPhantom wsWidgets) = entferneWidgets wsWidgets

    boxButtonEntfernen :: GeschwindigkeitPhantom WSWidgets g z -> Gtk.Box
    boxButtonEntfernen (GeschwindigkeitPhantom wsWidgets) = boxButtonEntfernen wsWidgets

    tvarSprache :: GeschwindigkeitPhantom WSWidgets g z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (GeschwindigkeitPhantom wsWidgets) = tvarSprache wsWidgets

    tvarEvent :: GeschwindigkeitPhantom WSWidgets g z -> TVar EventAusführen
    tvarEvent (GeschwindigkeitPhantom wsWidgets) = tvarEvent wsWidgets

instance PlanElement (WSWidgets 'Märklin) where
    foldPlan
        :: Lens.Fold (WSWidgets 'Märklin) (Maybe (ButtonPlanHinzufügen (WSWidgets 'Märklin)))
    foldPlan =
        Lens.folding
        $ (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (WSWidgets 'Märklin) r)
              => Wegstrecke 'Märklin
              -> Lens.Fold r (BoxPlanHinzufügen (WSWidgets 'Märklin))
    boxenPlan _wsWidgets =
        Lens.folding
        $ (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            , vBoxHinzufügenPlanWegstreckenKupplungMärklin
            , vBoxHinzufügenPlanWegstreckenMärklin]

instance PlanElement (WSWidgets 'Lego) where
    foldPlan :: Lens.Fold (WSWidgets 'Lego) (Maybe (ButtonPlanHinzufügen (WSWidgets 'Lego)))
    foldPlan =
        Lens.folding
        $ (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (WSWidgets 'Lego) r)
              => Wegstrecke 'Lego
              -> Lens.Fold r (BoxPlanHinzufügen (WSWidgets 'Lego))
    boxenPlan _wsWidgets =
        Lens.folding
        $ (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            , vBoxHinzufügenPlanWegstreckenKupplungLego
            , vBoxHinzufügenPlanWegstreckenLego]

instance PlanElement (ZugtypEither WSWidgets) where
    foldPlan :: Lens.Fold (ZugtypEither WSWidgets) (Maybe (ButtonPlanHinzufügen (ZugtypEither WSWidgets)))
    foldPlan =
        Lens.folding
        $ ausZugtypEither
        $ map (fmap widgetHinzufügenZugtypEither)
        . (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (ZugtypEither WSWidgets) r)
              => ZugtypEither Wegstrecke
              -> Lens.Fold r (BoxPlanHinzufügen (ZugtypEither WSWidgets))
    boxenPlan (ZugtypMärklin _wegstrecke) =
        Lens.folding
        $ map widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            , vBoxHinzufügenPlanWegstreckenKupplungMärklin
            , vBoxHinzufügenPlanWegstreckenMärklin]
    boxenPlan (ZugtypLego _wegstrecke) =
        Lens.folding
        $ map widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            , vBoxHinzufügenPlanWegstreckenKupplungLego
            , vBoxHinzufügenPlanWegstreckenLego]

instance StreckenObjekt (WSWidgets z) where
    anschlüsse :: WSWidgets z -> Set Anschluss
    anschlüsse = anschlüsse . ws

    erhalteName :: WSWidgets z -> Text
    erhalteName = erhalteName . ws

instance Aeson.ToJSON (WSWidgets z) where
    toJSON :: WSWidgets z -> Aeson.Value
    toJSON = Aeson.toJSON . ws

instance BahngeschwindigkeitKlasse (GeschwindigkeitPhantom WSWidgets) where
    geschwindigkeit :: (I2CReader r m, PwmReader r m, MonadIO m)
                    => GeschwindigkeitPhantom WSWidgets 'Pwm z
                    -> Word8
                    -> m ()
    geschwindigkeit
        (GeschwindigkeitPhantom WSWidgets {ws, wsScaleGeschwindigkeit, wsTVarEvent})
        wert = do
        eventAusführen wsTVarEvent $ geschwindigkeit (GeschwindigkeitPhantom ws) wert
        case wsScaleGeschwindigkeit of
            (Just scaleGeschwindigkeit) -> liftIO
                $ ohneEvent wsTVarEvent
                $ Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
            Nothing -> pure ()

    fahrstrom :: (I2CReader r m, MonadIO m)
              => GeschwindigkeitPhantom WSWidgets 'KonstanteSpannung z
              -> Word8
              -> m ()
    fahrstrom (GeschwindigkeitPhantom WSWidgets {ws, wsAuswahlFahrstrom, wsTVarEvent}) wert = do
        eventAusführen wsTVarEvent $ fahrstrom (GeschwindigkeitPhantom ws) wert
        case wsAuswahlFahrstrom of
            (Just auswahlFahrstrom)
                -> liftIO $ ohneEvent wsTVarEvent $ setzeAuswahl auswahlFahrstrom wert
            Nothing -> pure ()

    umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m)
             => GeschwindigkeitPhantom WSWidgets g 'Märklin
             -> m ()
    umdrehen (GeschwindigkeitPhantom WSWidgets {ws, wsTVarEvent}) =
        eventAusführen wsTVarEvent $ umdrehen $ GeschwindigkeitPhantom ws

    fahrtrichtungEinstellen :: (I2CReader r m, PwmReader r m, MonadIO m)
                            => GeschwindigkeitPhantom WSWidgets g 'Lego
                            -> Fahrtrichtung
                            -> m ()
    fahrtrichtungEinstellen
        (GeschwindigkeitPhantom WSWidgets {ws, wsAuswahlFahrtrichtung, wsTVarEvent})
        wert = do
        eventAusführen wsTVarEvent $ fahrtrichtungEinstellen (GeschwindigkeitPhantom ws) wert
        case wsAuswahlFahrtrichtung of
            (Just auswahlFahrtrichtung)
                -> liftIO $ ohneEvent wsTVarEvent $ setzeAuswahl auswahlFahrtrichtung wert
            Nothing -> pure ()

instance StreckenabschnittKlasse (WSWidgets z) where
    strom :: (I2CReader r m, MonadIO m) => WSWidgets z -> Strom -> m ()
    strom WSWidgets {ws, wsToggleButtonStrom, wsTVarEvent} wert = do
        eventAusführen wsTVarEvent $ strom ws wert
        case wsToggleButtonStrom of
            (Just toggleButtonStrom) -> liftIO
                $ ohneEvent wsTVarEvent
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (wert == Fließend)]
            Nothing -> pure ()

instance KupplungKlasse (WSWidgets z) where
    kuppeln :: (I2CReader r m, MonadIO m) => WSWidgets z -> m ()
    kuppeln WSWidgets {ws, wsTVarEvent} = eventAusführen wsTVarEvent $ kuppeln ws

instance KontaktKlasse (WSWidgets z) where
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => WSWidgets z -> m ()
    warteAufSignal WSWidgets {ws, wsTVarEvent} = eventAusführen wsTVarEvent $ warteAufSignal ws

instance (WegstreckeKlasse (Wegstrecke z)) => WegstreckeKlasse (WSWidgets z) where
    einstellen :: (I2CReader r m, PwmReader r m, MonadIO m) => WSWidgets z -> m ()
    einstellen WSWidgets {ws, wsTVarEvent} = eventAusführen wsTVarEvent $ einstellen ws

instance (ZugtypKlasse z) => BahngeschwindigkeitContainer (WSWidgets z) where
    enthalteneBahngeschwindigkeiten
        :: WSWidgets z -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = enthalteneBahngeschwindigkeiten . ws

instance StreckenabschnittContainer (WSWidgets z) where
    enthalteneStreckenabschnitte :: WSWidgets z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = enthalteneStreckenabschnitte . ws

instance (ZugtypKlasse z) => WeicheContainer (WSWidgets z) where
    enthalteneWeichen :: WSWidgets z -> Set (ZugtypEither Weiche)
    enthalteneWeichen = enthalteneWeichen . ws

instance KupplungContainer (WSWidgets z) where
    enthalteneKupplungen :: WSWidgets z -> Set Kupplung
    enthalteneKupplungen = enthalteneKupplungen . ws

instance KontaktContainer (WSWidgets z) where
    enthalteneKontakte :: WSWidgets z -> Set Kontakt
    enthalteneKontakte = enthalteneKontakte . ws

-- | 'Wegstrecke' darstellen
wegstreckePackNew
    :: forall m z.
    ( ObjektGuiReader m
    , MonadIO m
    , ZugtypKlasse z
    , PlanElement (WSWidgets z)
    , WegstreckeKlasse (Wegstrecke z)
    )
    => Wegstrecke z
    -> MStatusGuiT m (WSWidgets z)
wegstreckePackNew
    wegstrecke@Wegstrecke
    {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen} = do
    objektReader <- ask
    statusVar <- erhalteStatusVar :: MStatusGuiT m StatusVarGui
    dynamischeWidgets@DynamischeWidgets {vBoxWegstrecken} <- erhalteDynamischeWidgets
    (wsTVarSprache, wsTVarEvent) <- liftIO $ do
        wsTVarSprache <- newTVarIO $ Just []
        wsTVarEvent <- newTVarIO EventAusführen
        pure (wsTVarSprache, wsTVarEvent)
    let justTVarSprache = Just wsTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    let [boxBGPwm, boxBGKonstanteSpannung, boxBG, boxStreckenabschnitt, boxKupplung, boxWegstrecke] =
            dynamischeWidgets ^.. boxenPlan wegstrecke
    hinzufügenPlanWidgetBGPwm
        <- if any (ausGeschwindigkeitEither $ (== Pwm) . verwendetPwm) wsBahngeschwindigkeiten
            then Just <$> hinzufügenWidgetPlanPackNew boxBGPwm wegstrecke wsTVarSprache
            else pure Nothing
    hinzufügenPlanWidgetBGKonstanteSpannung <- if any
        (ausGeschwindigkeitEither $ (== KonstanteSpannung) . verwendetPwm)
        wsBahngeschwindigkeiten
        then Just <$> hinzufügenWidgetPlanPackNew boxBGKonstanteSpannung wegstrecke wsTVarSprache
        else pure Nothing
    hinzufügenPlanWidgetBG <- if null wsBahngeschwindigkeiten
        then pure Nothing
        else Just <$> hinzufügenWidgetPlanPackNew boxBG wegstrecke wsTVarSprache
    hinzufügenPlanWidgetST <- if null wsStreckenabschnitte
        then pure Nothing
        else Just <$> hinzufügenWidgetPlanPackNew boxStreckenabschnitt wegstrecke wsTVarSprache
    hinzufügenPlanWidgetKU <- if null wsKupplungen
        then pure Nothing
        else Just <$> hinzufügenWidgetPlanPackNew boxKupplung wegstrecke wsTVarSprache
    hinzufügenPlanWidgetWS <- if null wsWeichenRichtungen
        then pure Nothing
        else Just <$> hinzufügenWidgetPlanPackNew boxWegstrecke wegstrecke wsTVarSprache
    let hinzufügenPlanWidget =
            WegstreckePlanHinzufügenWidget
            { bahngeschwindigkeitPwm = hinzufügenPlanWidgetBGPwm
            , bahngeschwindigkeitKonstanteSpannung = hinzufügenPlanWidgetBGKonstanteSpannung
            , bahngeschwindigkeit = hinzufügenPlanWidgetBG
            , streckenabschnitt = hinzufügenPlanWidgetST
            , kupplung = hinzufügenPlanWidgetKU
            , wegstrecke = hinzufügenPlanWidgetWS
            }
    -- Widget erstellen
    (frame, expander, vBoxExpander, functionBox) <- liftIO $ do
        frame <- boxPackWidgetNewDefault vBoxWegstrecken Gtk.frameNew
        vBox <- containerAddWidgetNew frame $ Gtk.vBoxNew False 0
        namePackNew vBox wegstrecke
        expander <- boxPackWidgetNewDefault vBox $ Gtk.expanderNew Text.empty
        vBoxExpander <- containerAddWidgetNew expander $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
        functionBox <- boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
        pure (frame, expander, vBoxExpander, functionBox)
    verwendeSpracheGui justTVarSprache
        $ \sprache -> Gtk.set expander [Gtk.expanderLabel := Language.wegstreckenElemente sprache]
    (wsScaleGeschwindigkeit, wsAuswahlFahrstrom, wsAuswahlFahrtrichtung) <- if null
        wsBahngeschwindigkeiten
        then pure (Nothing, Nothing, Nothing)
        else do
            boxPackWidgetNewDefault vBoxExpander
                $ labelSpracheNew justTVarSprache
                $ Language.bahngeschwindigkeiten
                <:> fromJust (foldl appendName Nothing wsBahngeschwindigkeiten)
            maybeScale <- if any
                (ausGeschwindigkeitEither $ (== Pwm) . verwendetPwm)
                wsBahngeschwindigkeiten
                then Just
                    <$> hScaleGeschwindigkeitPackNew
                        functionBox
                        (GeschwindigkeitPhantom wegstrecke)
                        wsTVarEvent
                        statusVar
                else pure Nothing
            let geschwindigkeitenKonstanteSpannung = catKonstanteSpannung wsBahngeschwindigkeiten
            maybeAuswahlFahrstrom <- if null geschwindigkeitenKonstanteSpannung
                then pure Nothing
                else fmap Just
                    $ auswahlFahrstromPackNew
                        functionBox
                        (GeschwindigkeitPhantom wegstrecke)
                        (maximum
                         $ fromIntegral . length . fahrstromAnschlüsse
                         <$> geschwindigkeitenKonstanteSpannung)
                        wsTVarSprache
                        wsTVarEvent
                        statusVar
            eitherFahrtrichtungWidget <- case zuZugtypEither wegstrecke of
                (ZugtypMärklin wsMärklin) -> Left
                    <$> buttonUmdrehenPackNew
                        functionBox
                        (GeschwindigkeitPhantom wsMärklin
                         :: GeschwindigkeitPhantom Wegstrecke 'Pwm 'Märklin)
                        wsTVarSprache
                        wsTVarEvent
                        statusVar
                (ZugtypLego wsLego) -> Right
                    <$> auswahlFahrtrichtungEinstellenPackNew
                        functionBox
                        (GeschwindigkeitPhantom wsLego
                         :: GeschwindigkeitPhantom Wegstrecke 'Pwm 'Lego)
                        wsTVarSprache
                        wsTVarEvent
                        statusVar
            pure (maybeScale, maybeAuswahlFahrstrom, rightToMaybe eitherFahrtrichtungWidget)
    wsToggleButtonStrom <- if null wsStreckenabschnitte
        then pure Nothing
        else do
            boxPackWidgetNewDefault vBoxExpander
                $ labelSpracheNew justTVarSprache
                $ Language.streckenabschnitte
                <:> fromJust (foldl appendName Nothing wsStreckenabschnitte)
            Just
                <$> toggleButtonStromPackNew
                    functionBox
                    wegstrecke
                    wsTVarSprache
                    wsTVarEvent
                    statusVar
    unless (null wsWeichenRichtungen) $ void $ do
        boxPackWidgetNewDefault vBoxExpander
            $ labelSpracheNew justTVarSprache
            $ Language.weichen <:> fromJust (foldl (\acc (weiche, richtung) -> Just
                                                    $ fromJust (appendName acc weiche)
                                                    <°> richtung) Nothing wsWeichenRichtungen)
        boxPackWidgetNewDefault functionBox
            $ buttonNewWithEventLabel justTVarSprache Language.einstellen
            $ eventAusführen wsTVarEvent
            $ flip runReaderT objektReader
            $ ausführenStatusVarAktion (Einstellen wegstrecke) statusVar
    unless (null wsKupplungen) $ void $ do
        boxPackWidgetNewDefault vBoxExpander
            $ labelSpracheNew justTVarSprache
            $ Language.kupplungen <:> fromJust (foldl appendName Nothing wsKupplungen)
        buttonKuppelnPackNew functionBox wegstrecke wsTVarSprache wsTVarEvent
    let wsWidgets =
            WSWidgets
            { ws = wegstrecke
            , wsWidget = frame
            , wsFunktionBox = erhalteBox functionBox
            , wsHinzPL = hinzufügenPlanWidget
            , wsTVarSprache
            , wsTVarEvent
            , wsScaleGeschwindigkeit
            , wsAuswahlFahrstrom
            , wsAuswahlFahrtrichtung
            , wsToggleButtonStrom
            }
    buttonEntfernenPackNew wsWidgets $ entfernenWegstrecke $ zuZugtypEither wsWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OWegstrecke $ zuZugtypEither wsWidgets
    pure wsWidgets
    where
        appendName :: (StreckenObjekt o) => Maybe (Sprache -> Text) -> o -> Maybe (Sprache -> Text)

        -- Maybe necessary here, because otherwise (compare strings) this would lead to O(n!) runtime
        appendName Nothing objekt = Just $ const $ erhalteName objekt
        appendName (Just acc) objekt = Just $ acc <^> erhalteName objekt

        fahrstromAnschlüsse :: Bahngeschwindigkeit 'KonstanteSpannung z -> NonEmpty Anschluss
        fahrstromAnschlüsse
            MärklinBahngeschwindigkeitKonstanteSpannung {bgmkFahrstromAnschlüsse} =
            bgmkFahrstromAnschlüsse