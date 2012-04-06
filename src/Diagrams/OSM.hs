{-# LANGUAGE FlexibleContexts #-}
module Diagrams.OSM 
       (
         buildOsm
       , osmToDiagram
       , OsmStyle(..)
       , defaultOsmStyle
       ) where

import Control.Applicative
import qualified Data.Map as M
import Data.Lens.Common
import Diagrams.Prelude
import Data.Geo.OSM
import Data.Geo.OSM.BoundOption
import Data.Maybe

-- TODO: add options
data OsmStyle = OsmStyle

defaultOsmStyle :: OsmStyle
defaultOsmStyle = OsmStyle

buildOsm :: (Backend b R2, Renderable (Path R2) b)
         => OsmStyle -> FilePath -> IO (Either String (Diagram b R2))
buildOsm s f = do
  osms <- readOsmFile f
  return $ case osms of
    [] -> Left "Failed to process OSM file."
    _  -> Right (osmToDiagram s osms)

data OsmMap = OsmMap 
              { 
                nodes :: M.Map String P2
              , ways  :: M.Map String [String]
              }
                       
instance Monoid OsmMap where
  mempty = OsmMap mempty mempty
  a `mappend` b = OsmMap (nodes a `mappend` nodes b) (ways a `mappend` ways b) 
  
-- I'm pretty sure there is a much better way to do the bounds...
data OsmBounds = OsmBoundsEmpty
               | OsmBounds
                 {
                   minLat :: Double
                 , minLon :: Double
                 , maxLat :: Double
                 , maxLon :: Double
                 }

instance Monoid OsmBounds where
  mempty = OsmBoundsEmpty
  OsmBoundsEmpty `mappend` a = a
  a `mappend` OsmBoundsEmpty = a
  (OsmBounds ax ay ax' ay') `mappend` (OsmBounds bx by bx' by') = 
    OsmBounds (min ax bx) (min ay by) (max ax' bx') (max ay' by')
  

osmToDiagram :: (Backend b R2, Renderable (Path R2) b) 
                => OsmStyle -> [OSM] -> Diagram b R2
osmToDiagram s os = boundView v d # reflectY # rotateBy (1/4) # scaleX 0.8
  where 
    e = const mempty
    f = foldChildren e e e e e (mconcat . map g)
    g r = foldNodeWayRelation r handleNode handleWay e
    d = osmMapToDiagram s . mconcat . map (f . getL childrenL) $ os
    
    v :: OsmBounds
    v = mconcat . map (b . getL boundsL) $ os
    b :: BoundOption -> OsmBounds
    b = foldBoundOption handleBound handleBounds mempty

handleBounds = OsmBounds <$> read . getL minlatL
                         <*> read . getL minlonL 
                         <*> read . getL maxlatL
                         <*> read . getL maxlonL
                         
handleBound = mempty -- TODO: handle this option

boundView :: (Backend b R2) => OsmBounds -> Diagram b R2 -> Diagram b R2
boundView (OsmBounds x y x' y') = view (p2 (x,y)) (r2 (x'-x,y'-y))

handleNode n = OsmMap (M.singleton
                       (getL idL $ n) 
                       (p2 (read . getL latL $ n, read . getL lonL $ n)))
               mempty
               
handleWay w = OsmMap mempty (M.singleton
                             (getL idL w)
                             (map (getL refL) . getL ndL $ w))

osmMapToDiagram :: (Backend b R2, Renderable (Path R2) b)
                   => OsmStyle -> OsmMap -> Diagram b R2
osmMapToDiagram s (OsmMap ns ws) = M.fold f mempty ws
  where
    f ids d = d <> mkDiagram s (catMaybes . map (`M.lookup` ns) $ ids)
    
mkDiagram :: (Backend b R2, Renderable (Path R2) b) 
             => OsmStyle -> [P2] -> Diagram b R2
mkDiagram s ps = fromVertices ps # stroke # lw 0.0001