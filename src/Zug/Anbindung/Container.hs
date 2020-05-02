{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Zug.Anbindung.Container
  ( BahngeschwindigkeitContainer(..)
  , StreckenabschnittContainer(..)
  , WeicheContainer(..)
  , KupplungContainer(..)
  , KontaktContainer(..)
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Zug.Anbindung.Bahngeschwindigkeit (Bahngeschwindigkeit())
import Zug.Anbindung.Kontakt (Kontakt())
import Zug.Anbindung.Kupplung (Kupplung())
import Zug.Anbindung.Streckenabschnitt (Streckenabschnitt())
import Zug.Anbindung.Wegstrecke (Wegstrecke(..))
import Zug.Anbindung.Weiche (Weiche())
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(zuZugtypEither)
                , GeschwindigkeitVariante(..), GeschwindigkeitEither(..)
                , GeschwindigkeitKlasse(zuGeschwindigkeitEither), GeschwindigkeitPhantom(..))

-- | Typen, die 'Bahngeschwindigkeit'en enthalten können.
class BahngeschwindigkeitContainer b where
    -- | Alle enthaltenen Bahngeschwindigkeiten.
    enthalteneBahngeschwindigkeiten
        :: b -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))

instance ( BahngeschwindigkeitContainer (a 'Pwm z)
         , BahngeschwindigkeitContainer (a 'KonstanteSpannung z)
         ) => BahngeschwindigkeitContainer (GeschwindigkeitEither a z) where
    enthalteneBahngeschwindigkeiten
        :: GeschwindigkeitEither a z
        -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten (GeschwindigkeitPwm a) = enthalteneBahngeschwindigkeiten a
    enthalteneBahngeschwindigkeiten (GeschwindigkeitKonstanteSpannung a) =
        enthalteneBahngeschwindigkeiten a

instance (BahngeschwindigkeitContainer (a 'Märklin), BahngeschwindigkeitContainer (a 'Lego))
    => BahngeschwindigkeitContainer (ZugtypEither a) where
    enthalteneBahngeschwindigkeiten
        :: ZugtypEither a -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten (ZugtypMärklin a) = enthalteneBahngeschwindigkeiten a
    enthalteneBahngeschwindigkeiten (ZugtypLego a) = enthalteneBahngeschwindigkeiten a

instance (BahngeschwindigkeitContainer (a z))
    => BahngeschwindigkeitContainer (GeschwindigkeitPhantom a g z) where
    enthalteneBahngeschwindigkeiten
        :: GeschwindigkeitPhantom a g z
        -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten (GeschwindigkeitPhantom a) = enthalteneBahngeschwindigkeiten a

instance (GeschwindigkeitKlasse g, ZugtypKlasse z)
    => BahngeschwindigkeitContainer (Bahngeschwindigkeit g z) where
    enthalteneBahngeschwindigkeiten
        :: Bahngeschwindigkeit g z
        -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = Set.singleton . zuZugtypEither . zuGeschwindigkeitEither

instance BahngeschwindigkeitContainer Streckenabschnitt where
    enthalteneBahngeschwindigkeiten
        :: Streckenabschnitt -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = const Set.empty

instance BahngeschwindigkeitContainer (Weiche z) where
    enthalteneBahngeschwindigkeiten
        :: Weiche z -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = const Set.empty

instance BahngeschwindigkeitContainer Kupplung where
    enthalteneBahngeschwindigkeiten
        :: Kupplung -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = const Set.empty

instance BahngeschwindigkeitContainer Kontakt where
    enthalteneBahngeschwindigkeiten
        :: Kontakt -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = const Set.empty

instance (ZugtypKlasse z) => BahngeschwindigkeitContainer (Wegstrecke z) where
    enthalteneBahngeschwindigkeiten
        :: Wegstrecke z -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = Set.map zuZugtypEither . wsBahngeschwindigkeiten

-- | Typen, die 'Streckenabschnitt'e enthalten können.
class StreckenabschnittContainer s where
    -- | Alle enthaltenen 'Streckenabschnitt'e.
    enthalteneStreckenabschnitte :: s -> Set Streckenabschnitt

instance ( StreckenabschnittContainer (a 'Pwm z)
         , StreckenabschnittContainer (a 'KonstanteSpannung z)
         ) => StreckenabschnittContainer (GeschwindigkeitEither a z) where
    enthalteneStreckenabschnitte :: GeschwindigkeitEither a z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte (GeschwindigkeitPwm a) = enthalteneStreckenabschnitte a
    enthalteneStreckenabschnitte (GeschwindigkeitKonstanteSpannung a) =
        enthalteneStreckenabschnitte a

instance (StreckenabschnittContainer (a 'Märklin), StreckenabschnittContainer (a 'Lego))
    => StreckenabschnittContainer (ZugtypEither a) where
    enthalteneStreckenabschnitte :: ZugtypEither a -> Set Streckenabschnitt
    enthalteneStreckenabschnitte (ZugtypMärklin a) = enthalteneStreckenabschnitte a
    enthalteneStreckenabschnitte (ZugtypLego a) = enthalteneStreckenabschnitte a

instance (StreckenabschnittContainer (a z))
    => StreckenabschnittContainer (GeschwindigkeitPhantom a g z) where
    enthalteneStreckenabschnitte :: GeschwindigkeitPhantom a g z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte (GeschwindigkeitPhantom a) = enthalteneStreckenabschnitte a

instance StreckenabschnittContainer (Bahngeschwindigkeit g z) where
    enthalteneStreckenabschnitte :: Bahngeschwindigkeit g z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = const Set.empty

instance StreckenabschnittContainer Streckenabschnitt where
    enthalteneStreckenabschnitte :: Streckenabschnitt -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = Set.singleton

instance StreckenabschnittContainer (Weiche z) where
    enthalteneStreckenabschnitte :: Weiche z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = const Set.empty

instance StreckenabschnittContainer Kupplung where
    enthalteneStreckenabschnitte :: Kupplung -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = const Set.empty

instance StreckenabschnittContainer Kontakt where
    enthalteneStreckenabschnitte :: Kontakt -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = const Set.empty

instance StreckenabschnittContainer (Wegstrecke z) where
    enthalteneStreckenabschnitte :: Wegstrecke z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = wsStreckenabschnitte

-- | Typen, die 'Weiche'n enthalten können.
class WeicheContainer w where
    -- | Alle enthaltenen Weichen.
    enthalteneWeichen :: w -> Set (ZugtypEither Weiche)

instance (WeicheContainer (a 'Pwm z), WeicheContainer (a 'KonstanteSpannung z))
    => WeicheContainer (GeschwindigkeitEither a z) where
    enthalteneWeichen :: GeschwindigkeitEither a z -> Set (ZugtypEither Weiche)
    enthalteneWeichen (GeschwindigkeitPwm a) = enthalteneWeichen a
    enthalteneWeichen (GeschwindigkeitKonstanteSpannung a) = enthalteneWeichen a

instance (WeicheContainer (a 'Märklin), WeicheContainer (a 'Lego))
    => WeicheContainer (ZugtypEither a) where
    enthalteneWeichen :: ZugtypEither a -> Set (ZugtypEither Weiche)
    enthalteneWeichen (ZugtypMärklin a) = enthalteneWeichen a
    enthalteneWeichen (ZugtypLego a) = enthalteneWeichen a

instance (WeicheContainer (a z)) => WeicheContainer (GeschwindigkeitPhantom a g z) where
    enthalteneWeichen :: GeschwindigkeitPhantom a g z -> Set (ZugtypEither Weiche)
    enthalteneWeichen (GeschwindigkeitPhantom a) = enthalteneWeichen a

instance WeicheContainer (Bahngeschwindigkeit g z) where
    enthalteneWeichen :: Bahngeschwindigkeit g z -> Set (ZugtypEither Weiche)
    enthalteneWeichen = const Set.empty

instance WeicheContainer Streckenabschnitt where
    enthalteneWeichen :: Streckenabschnitt -> Set (ZugtypEither Weiche)
    enthalteneWeichen = const Set.empty

instance (ZugtypKlasse z) => WeicheContainer (Weiche z) where
    enthalteneWeichen :: Weiche z -> Set (ZugtypEither Weiche)
    enthalteneWeichen = Set.singleton . zuZugtypEither

instance WeicheContainer Kupplung where
    enthalteneWeichen :: Kupplung -> Set (ZugtypEither Weiche)
    enthalteneWeichen = const Set.empty

instance WeicheContainer Kontakt where
    enthalteneWeichen :: Kontakt -> Set (ZugtypEither Weiche)
    enthalteneWeichen = const Set.empty

instance (ZugtypKlasse z) => WeicheContainer (Wegstrecke z) where
    enthalteneWeichen :: Wegstrecke z -> Set (ZugtypEither Weiche)
    enthalteneWeichen = Set.map (zuZugtypEither . fst) . wsWeichenRichtungen

-- | Typen, die 'Kupplung'en enthalten können.
class KupplungContainer k where
    -- | Alle enthaltenen Kupplungen.
    enthalteneKupplungen :: k -> Set Kupplung

instance (KupplungContainer (a 'Pwm z), KupplungContainer (a 'KonstanteSpannung z))
    => KupplungContainer (GeschwindigkeitEither a z) where
    enthalteneKupplungen :: GeschwindigkeitEither a z -> Set Kupplung
    enthalteneKupplungen (GeschwindigkeitPwm a) = enthalteneKupplungen a
    enthalteneKupplungen (GeschwindigkeitKonstanteSpannung a) = enthalteneKupplungen a

instance (KupplungContainer (a 'Märklin), KupplungContainer (a 'Lego))
    => KupplungContainer (ZugtypEither a) where
    enthalteneKupplungen :: ZugtypEither a -> Set Kupplung
    enthalteneKupplungen (ZugtypMärklin a) = enthalteneKupplungen a
    enthalteneKupplungen (ZugtypLego a) = enthalteneKupplungen a

instance (KupplungContainer (a z)) => KupplungContainer (GeschwindigkeitPhantom a g z) where
    enthalteneKupplungen :: GeschwindigkeitPhantom a g z -> Set Kupplung
    enthalteneKupplungen (GeschwindigkeitPhantom a) = enthalteneKupplungen a

instance KupplungContainer (Bahngeschwindigkeit g z) where
    enthalteneKupplungen :: Bahngeschwindigkeit g z -> Set Kupplung
    enthalteneKupplungen = const Set.empty

instance KupplungContainer Streckenabschnitt where
    enthalteneKupplungen :: Streckenabschnitt -> Set Kupplung
    enthalteneKupplungen = const Set.empty

instance KupplungContainer (Weiche z) where
    enthalteneKupplungen :: (Weiche z) -> Set Kupplung
    enthalteneKupplungen = const Set.empty

instance KupplungContainer Kupplung where
    enthalteneKupplungen :: Kupplung -> Set Kupplung
    enthalteneKupplungen = Set.singleton

instance KupplungContainer Kontakt where
    enthalteneKupplungen :: Kontakt -> Set Kupplung
    enthalteneKupplungen = const Set.empty

instance KupplungContainer (Wegstrecke z) where
    enthalteneKupplungen :: Wegstrecke z -> Set Kupplung
    enthalteneKupplungen = wsKupplungen

-- | Typen, die 'Kontakt'e enthalten können.
class KontaktContainer k where
    -- | Alle enthaltenen Kontakte.
    enthalteneKontakte :: k -> Set Kontakt

instance (KontaktContainer (a 'Pwm z), KontaktContainer (a 'KonstanteSpannung z))
    => KontaktContainer (GeschwindigkeitEither a z) where
    enthalteneKontakte :: GeschwindigkeitEither a z -> Set Kontakt
    enthalteneKontakte (GeschwindigkeitPwm a) = enthalteneKontakte a
    enthalteneKontakte (GeschwindigkeitKonstanteSpannung a) = enthalteneKontakte a

instance (KontaktContainer (a 'Märklin), KontaktContainer (a 'Lego))
    => KontaktContainer (ZugtypEither a) where
    enthalteneKontakte :: ZugtypEither a -> Set Kontakt
    enthalteneKontakte (ZugtypMärklin a) = enthalteneKontakte a
    enthalteneKontakte (ZugtypLego a) = enthalteneKontakte a

instance (KontaktContainer (a z)) => KontaktContainer (GeschwindigkeitPhantom a g z) where
    enthalteneKontakte :: GeschwindigkeitPhantom a g z -> Set Kontakt
    enthalteneKontakte (GeschwindigkeitPhantom a) = enthalteneKontakte a

instance KontaktContainer (Bahngeschwindigkeit g z) where
    enthalteneKontakte :: Bahngeschwindigkeit g z -> Set Kontakt
    enthalteneKontakte = const Set.empty

instance KontaktContainer Streckenabschnitt where
    enthalteneKontakte :: Streckenabschnitt -> Set Kontakt
    enthalteneKontakte = const Set.empty

instance KontaktContainer (Weiche z) where
    enthalteneKontakte :: Weiche z -> Set Kontakt
    enthalteneKontakte = const Set.empty

instance KontaktContainer Kupplung where
    enthalteneKontakte :: Kupplung -> Set Kontakt
    enthalteneKontakte = const Set.empty

instance KontaktContainer Kontakt where
    enthalteneKontakte :: Kontakt -> Set Kontakt
    enthalteneKontakte = Set.singleton

instance KontaktContainer (Wegstrecke z) where
    enthalteneKontakte :: Wegstrecke z -> Set Kontakt
    enthalteneKontakte = wsKontakte
