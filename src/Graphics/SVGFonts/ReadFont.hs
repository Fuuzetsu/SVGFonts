{-# LANGUAGE DeriveGeneric #-}
module Graphics.SVGFonts.ReadFont
       ( bbox_dy
       , bbox_lx, bbox_ly
       , getGlyphPath
       , underlinePosition
       , underlineThickness

       , horizontalAdvance
       , kernAdvance
       , Kern(..)

       , PreparedFont(..)
       , loadFont
       , validateFont
       ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char                       (isSpace)
import           Data.List                       (intersect, sortBy, lookup)
import           Data.List.Split                 (splitOn, splitWhen)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust, fromMaybe, isNothing,
                                                  maybeToList, maybe, mapMaybe)
import           Data.Tuple.Select
import qualified Data.Vector                     as V
import           Diagrams.Path
import           Diagrams.Prelude                hiding (font)
import           Text.XML.Expat.Proc
import           Text.XML.Expat.Tree

import           Graphics.SVGFonts.CharReference (charsFromFullName)
import           Graphics.SVGFonts.ReadPath      (PathCommand (..),
                                                  pathFromByteString)

import           GHC.Generics                    (Generic)
import           Data.Serialize                  (Serialize)
import           Data.Vector.Serialize           ()

-- | This type contains everything that a typical SVG font file produced
--   by fontforge contains.
data PreparedFont n = PreparedFont
  { fontDataGlyphs                 :: SvgGlyphs n
  , fontDataKerning                :: Kern n
  , fontDataBoundingBox            :: [n]
  , fontDataFileName               :: String
  , fontDataUnderlinePos           :: n
  , fontDataUnderlineThickness     :: n
  , fontDataOverlinePos            :: Maybe n
  , fontDataOverlineThickness      :: Maybe n
  , fontDataStrikethroughPos       :: Maybe n
  , fontDataStrikethroughThickness :: Maybe n
  , fontDataHorizontalAdvance      :: n
  , fontDataFamily                 :: String
  , fontDataStyle                  :: String
  , fontDataWeight                 :: String
  , fontDataVariant                :: String
  , fontDataStretch                :: String
  , fontDataSize                   :: Maybe String
  , fontDataUnitsPerEm             :: n
  , fontDataPanose                 :: String
  , fontDataSlope                  :: Maybe n
  , fontDataAscent                 :: n
  , fontDataDescent                :: n
  , fontDataXHeight                :: n
  , fontDataCapHeight              :: n
  , fontDataAccentHeight           :: Maybe n
  , fontDataWidths                 :: Maybe String
  , fontDataHorizontalStem         :: Maybe n
    -- ^ This data is not available in some fonts (e.g. Source Code Pro)
  , fontDataVerticalStem           :: Maybe n
    -- ^ This data is not available in some fonts (e.g. Source Code Pro)
  , fontDataUnicodeRange           :: String
  , fontDataRawKernings            :: [(String, [String], [String], [String], [String])]
  , fontDataIdeographicBaseline    :: Maybe n
  , fontDataAlphabeticBaseline     :: Maybe n
  , fontDataMathematicalBaseline   :: Maybe n
  , fontDataHangingBaseline        :: Maybe n
  , fontDataVIdeographicBaseline   :: Maybe n
  , fontDataVAlphabeticBaseline    :: Maybe n
  , fontDataVMathematicalBaseline  :: Maybe n
  , fontDataVHangingBaseline       :: Maybe n
  } deriving (Generic)

-- | Open an SVG-Font File and extract the data
parseFont :: (Read n, RealFloat n) => FilePath -> BSL.ByteString -> PreparedFont n
parseFont basename contents = readFontData fontElement basename
  where
    xml :: UNode String
    (xml, Nothing) = parse defaultParseOptions contents

    fontElement :: UNode String
    Just fontElement = findElement "font" xml

-- | Read font data from an XML font element.
readFontData :: (Read n, RealFloat n) => UNode String -> String -> PreparedFont n
readFontData fontElement basename = PreparedFont
  { fontDataGlyphs      = Map.fromList glyphs
  , fontDataKerning     = Kern
    { kernU1S = transformChars u1s
    , kernU2S = transformChars u2s
    , kernG1S = transformChars g1s
    , kernG2S = transformChars g2s
    , kernK = kAr
    }
  , fontDataBoundingBox = parsedBBox
  , fontDataFileName    = basename
  , fontDataUnderlinePos       = fontface `readAttr` "underline-position"
  , fontDataUnderlineThickness = fontface `readAttr` "underline-thickness"
  , fontDataHorizontalAdvance  = fontHadv
  , fontDataFamily     = readString fontface "font-family" ""
  , fontDataStyle      = readString fontface "font-style" "all"
  , fontDataWeight     = readString fontface "font-weight" "all"
  , fontDataVariant    = readString fontface "font-variant" "normal"
  , fontDataStretch    = readString fontface "font-stretch" "normal"
  , fontDataSize       = fontface `readStringM` "font-size"
  , fontDataUnitsPerEm = fontface `readAttr` "units-per-em"
  , fontDataSlope      = fontface `readAttrM` "slope"
  , fontDataPanose     = readString fontface "panose-1" "0 0 0 0 0 0 0 0 0 0"
  , fontDataAscent     = fontface `readAttr` "ascent"
  , fontDataDescent    = fontface `readAttr` "descent"
  , fontDataXHeight    = fontface `readAttr` "x-height"
  , fontDataCapHeight  = fontface `readAttr` "cap-height"
  , fontDataAccentHeight = fontface `readAttrM` "accent-height"
  , fontDataWidths  = fontface `readStringM` "widths"
  , fontDataHorizontalStem = fontface `readAttrM` "stemh"
  , fontDataVerticalStem   = fontface `readAttrM` "stemv"
  , fontDataUnicodeRange = readString fontface "unicode-range" "U+0-10FFFF"
  , fontDataRawKernings = rawKerns
  , fontDataIdeographicBaseline   = fontface `readAttrM` "ideographic"
  , fontDataAlphabeticBaseline    = fontface `readAttrM` "alphabetic"
  , fontDataMathematicalBaseline  = fontface `readAttrM` "mathematical"
  , fontDataHangingBaseline       = fontface `readAttrM` "hanging"
  , fontDataVIdeographicBaseline  = fontface `readAttrM` "v-ideographic"
  , fontDataVAlphabeticBaseline   = fontface `readAttrM` "v-alphabetic"
  , fontDataVMathematicalBaseline = fontface `readAttrM` "v-mathematical"
  , fontDataVHangingBaseline      = fontface `readAttrM` "v-hanging"
  , fontDataOverlinePos            = fontface `readAttrM` "overline-position"
  , fontDataOverlineThickness      = fontface `readAttrM` "overline-thickness"
  , fontDataStrikethroughPos       = fontface `readAttrM` "strikethrough-position"
  , fontDataStrikethroughThickness = fontface `readAttrM` "strikethrough-thickness"
  }
  where
    findAttr :: String -> UNode String -> Maybe String
    findAttr k = lookup k . getAttributes

    readAttr :: (Read a) => UNode String -> String -> a
    readAttr e attr = fromJust $ fmap read $ findAttr attr e

    readAttrM :: (Read a) => UNode String -> String -> Maybe a
    readAttrM e attr = fmap read $ findAttr attr e

    -- | @readString e a d@ : @e@ element to read from; @a@ attribute to read; @d@ default value.
    readString :: UNode String -> String -> String -> String
    readString e attr d = fromMaybe d $ findAttr attr e

    readStringM :: UNode String -> String -> Maybe String
    readStringM e attr = findAttr attr e

    fontHadv = fromMaybe ((parsedBBox!!2) - (parsedBBox!!0)) -- BBox is used if there is no "horiz-adv-x" attribute
                         (fmap read (findAttr "horiz-adv-x" fontElement) )
    fontface = fromJust $ findElement "font-face" fontElement -- there is always a font-face node
    bbox     = readString fontface "bbox" ""
    parsedBBox :: Read n => [n]
    parsedBBox = map read $ splitWhen isSpace bbox

    glyphElements = findChildren "glyph" fontElement
    kernings = findChildren "hkern" fontElement

    glyphs = map glyphsWithDefaults glyphElements

    -- monospaced fonts sometimes don't have a "horiz-adv-x="-value , replace with "horiz-adv-x=" in <font>
    glyphsWithDefaults g = (charsFromFullName $ fromMaybe gname (findAttr "unicode" g), -- there is always a name or unicode
                             ( gname
                             , fromMaybe fontHadv (fmap read (findAttr "horiz-adv-x" g))
                             , d_val
                             -- Inline outline map in the PreparedFont:
                             -- this way if a glyph is used once, it
                             -- can be re-used without the (high) cost
                             -- of reparsing and recalculating it. The
                             -- saving grace is laziness.
                             , (\cmds -> mconcat $ commandsToTrails cmds [] zero zero zero)
                               <$> pathFromByteString (BS.pack d_val)
                             )
                           )

      where gname = fromMaybe "" (findAttr "glyph-name" g)
            d_val = fromMaybe mempty $! findAttr "d" g


--        return $

    u1s         = map (fromMaybe "") $ map (findAttr "u1")  kernings
    u2s         = map (fromMaybe "") $ map (findAttr "u2")  kernings
    g1s         = map (fromMaybe "") $ map (findAttr "g1")  kernings
    g2s         = map (fromMaybe "") $ map (findAttr "g2")  kernings
    ks          = map (fromMaybe "") $ map (findAttr "k")   kernings
    kAr     = V.fromList (map read ks)

    rawKerns = fmap getRawKern kernings
    getRawKern kerning =
      let u1 = splitWhen (==',') $ fromMaybe "" $ findAttr "u1" $ kerning
          u2 = splitWhen (==',') $ fromMaybe "" $ findAttr "u2" $ kerning
          g1 = splitWhen (==',') $ fromMaybe "" $ findAttr "g1" $ kerning
          g2 = splitWhen (==',') $ fromMaybe "" $ findAttr "g2" $ kerning
          k  = fromMaybe "" $ findAttr "k" $ kerning
      in (k, g1, g2, u1, u2)

    transformChars chars = Map.fromList $ map ch $ multiSet $
                                          map (\(x,y) -> (x,[y])) $ sort fst $ concat $ indexList chars
    ch (x,y) | null x = ("",y)
             | otherwise = (x,y)

    indexList u = addIndex (map (splitWhen isColon) u) -- ie ["aa,b","c,d"] to [["aa","b"],["c","d"]]
    isColon = (== ',')                             -- to [("aa",0),("b",0)],[("c",1), ("d",1)]

    addIndex qs = zipWith (\x y -> (map (\z -> (z,x)) y)) [0..] qs
    sort f xs = sortBy (\x y -> compare (f x) (f y) ) xs

    multiSet [] = []
    multiSet (a:[]) = [a] -- example: [("n1",[0]),("n1",[1]),("n2",[1])] to [("n1",[0,1]),("n2",[1])]
    multiSet (a:b:bs) | fst a == fst b = multiSet ( (fst a, (snd a) ++ (snd b)) : bs)
                      | otherwise = a : (multiSet (b:bs))



-- | \[ (unicode, (glyph_name, horiz_advance, ds, glyph_outline)) \]
type SvgGlyphs n = Map.Map String (String, n, String, Either String (Path V2 n))


-- | Horizontal advance of a character consisting of its width and spacing, extracted out of the font data
horizontalAdvance :: String -> PreparedFont n -> n
horizontalAdvance ch fontD = maybe (fontDataHorizontalAdvance fontD) sel2 char
  where char = (Map.lookup ch (fontDataGlyphs fontD))

-- | See <http://www.w3.org/TR/SVG/fonts.html#KernElements>
--
-- Some explanation how kerning is computed:
--
-- In Linlibertine.svg, there are two groups of chars: e.g.
-- \<hkern g1=\"f,longs,uni1E1F,f_f\" g2=\"parenright,bracketright,braceright\" k=\"-37\" />
-- This line means: If there is an f followed by parentright, reduce the horizontal advance by -37 (add 37).
-- Therefore to quickly check if two characters need kerning assign an index to the second group (g2 or u2)
-- and assign to every unicode in the first group (g1 or u1) this index, then sort these tuples after their
-- name (for binary search). Because the same unicode char can appear in several g1s, reduce this 'multiset',
-- ie all the (\"name1\",0) (\"name1\",1) to (\"name1\",[0,1]).
-- Now the g2s are converted in the same way as the g1s.
-- Whenever two consecutive chars are being printed try to find an
-- intersection of the list assigned to the first char and second char
data Kern n = Kern
  { kernU1S :: Map.Map String [Int]
  , kernU2S :: Map.Map String [Int]
  , kernG1S :: Map.Map String [Int]
  , kernG2S :: Map.Map String [Int]
  , kernK   :: V.Vector n
  } deriving (Show, Generic)

instance Serialize n => Serialize (Kern n)

-- | Change the horizontal advance of two consective chars (kerning)
kernAdvance :: RealFloat n => String -> String -> Kern n -> Bool -> n
kernAdvance ch0 ch1 kern u |     u && not (null s0) = (kernK kern) V.! (head s0)
                           | not u && not (null s1) = (kernK kern) V.! (head s1)
                           | otherwise = 0
  where s0 = intersect (s kernU1S ch0) (s kernU2S ch1)
        s1 = intersect (s kernG1S ch0) (s kernG2S ch1)
        s sel ch = concat (maybeToList (Map.lookup ch (sel kern)))

-- > import Graphics.SVGFonts.ReadFont
-- > textWH0 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "SPACES" lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH1 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "are sometimes better." lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH2 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "But too many chars are not good." lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH = textWH0 # alignBL === strutY 0.3 === textWH1 === strutY 0.3 === textWH2 # alignBL
-- > textW0 = (rect 3 1) # alignBL <> ( (textSVG_ $ TextOpts "HEADLINE" lin INSIDE_W KERN False 3 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd ) # alignBL
-- > textW1 = (rect 10 1) # alignBL <> ( (textSVG_ $ TextOpts "HEADLINE" lin INSIDE_W KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd ) # alignBL
-- > textW = textW0 # alignBL ||| strutX 1 ||| textW1 # alignBL
-- > textH0 = (rect 10 1) # alignBL <> ((textSVG_ $ TextOpts "Constant font size" lin INSIDE_H KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textH1 = (rect 3 1) # alignBL <> ((textSVG_ $ TextOpts "Constant font size" lin INSIDE_H KERN False 3 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textH = textH0 # alignBL === strutY 0.5 === textH1 # alignBL

-- > import Graphics.SVGFonts.ReadFont
-- > textHADV = (textSVG_ $ TextOpts "AVENGERS" lin INSIDE_H HADV False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd

-- > import Graphics.SVGFonts.ReadFont
-- > textKern = (textSVG_ $ TextOpts "AVENGERS" lin INSIDE_H KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd


-- | Difference between highest and lowest y-value of bounding box
bbox_dy :: RealFloat n => PreparedFont n -> n
bbox_dy fontData = (bbox!!3) - (bbox!!1)
  where bbox = fontDataBoundingBox fontData -- bbox = [lowest x, lowest y, highest x, highest y]

-- | Lowest x-value of bounding box
bbox_lx :: PreparedFont n -> n
bbox_lx fontData   = (fontDataBoundingBox fontData) !! 0

-- | Lowest y-value of bounding box
bbox_ly :: PreparedFont n -> n
bbox_ly fontData   = (fontDataBoundingBox fontData) !! 1

-- | Position of the underline bar
underlinePosition :: PreparedFont n -> n
underlinePosition fontData = fontDataUnderlinePos fontData

-- | Thickness of the underline bar
underlineThickness :: PreparedFont n -> n
underlineThickness fontData = fontDataUnderlineThickness fontData

-- | Read font data from font file.
loadFont :: (Read n, RealFloat n) => FilePath -> IO (PreparedFont n)
loadFont filename = do
  s <- BSL.readFile filename
  let basename = last $ init $ concat (map (splitOn "/") (splitOn "." filename))
  return $! parseFont basename s

-- | Parse every glyph in a font and find any errors, if any.
--
-- @[(glyph, errorMessage)]@
validateFont :: PreparedFont n -> [(String, String)]
validateFont = mapMaybe f . Map.toList . fontDataGlyphs
  where
    -- This should be OK and should not force outline computation!
    -- We're only forcing the Either constructor and we don't inspect
    -- the content of the outline, i.e. we're forcing the parsing
    -- only.
    f (ch, (_, _, ds, Left s)) = Just (ch, s)
    f _ = Nothing

-- | Extract (and potentially first calculate) the 'Path' for the
-- specified glyph.
--
-- Nothing: no glyph in font or error during parsing
getGlyphPath :: PreparedFont n
             -> String -- ^ Glaph
             -> Maybe (Path V2 n)
getGlyphPath fd k = Map.lookup k (fontDataGlyphs fd) >>= \r -> case r of
  (_, _, _, Right p) -> Just p
  _ -> Nothing

commandsToTrails ::RealFloat n => [PathCommand n] -> [Segment Closed V2 n] -> V2 n -> V2 n -> V2 n -> [Path V2 n]
commandsToTrails [] _ _ _ _ = []
commandsToTrails (c:cs) segments l lastContr beginPoint -- l is the endpoint of the last segment
      | isNothing nextSegment = (translate beginPoint (pathFromTrail . wrapTrail  . closeLine $ lineFromSegments segments)) :
                  ( commandsToTrails cs [] (l ^+^ offs) (contr c) (beginP c) ) -- one outline completed
      | otherwise = commandsToTrails cs (segments ++ [fromJust nextSegment])
                                           (l ^+^ offs) (contr c) (beginP c)   -- work on outline
  where nextSegment = go c
        offs = maybe zero segOffset nextSegment
        (x0,y0) = unr2 offs
        (cx,cy) = unr2 lastContr -- last control point is always in absolute coordinates
        beginP ( M_abs (x,y) ) = r2 (x,y)
        beginP ( M_rel (x,y) ) = l ^+^ r2 (x,y)
        beginP _ = beginPoint
        contr ( C_abs (_x1,_y1,x2,y2,x,y) ) = r2 (x0+x - x2, y0+y - y2 ) -- control point of bezier curve
        contr ( C_rel (_x1,_y1,x2,y2,x,y) ) = r2 (   x - x2,    y - y2 )
        contr ( S_abs (x2,y2,x,y) )         = r2 (x0+x - x2, y0+y - y2 )
        contr ( S_rel (x2,y2,x,y) )         = r2 (   x - x2,    y - y2 )
        contr ( Q_abs (x1,y1,x,y) ) = r2 (x0+x - x1, y0+y - y1 )
        contr ( Q_rel (x1,y1,x,y) ) = r2 (   x - x1,    y - y1 )
        contr ( T_abs (_x,_y) )     = r2 (2*x0 - cx, 2*y0 - cy )
        contr ( T_rel (x,y) )       = r2 (   x - cx,    y - cy )
        contr ( L_abs (_x,_y) ) = r2 (x0, y0)
        contr ( L_rel (_x,_y) ) = r2 ( 0,  0)
        contr ( M_abs (_x,_y) ) = r2 (x0, y0)
        contr ( M_rel (_x,_y) ) = r2 ( 0,  0)
        contr ( H_abs _x ) = r2 (x0, y0)
        contr ( H_rel _x ) = r2 ( 0, y0)
        contr ( V_abs _y ) = r2 (x0, y0)
        contr ( V_rel _y ) = r2 (x0,  0)
        contr ( Z ) = r2 (0, 0) -- to get rid of warnings
        contr ( A_abs ) = r2 (0, 0) -- to get rid of warnings
        contr ( A_rel ) = r2 (0, 0) -- to get rid of warnings

        straight' = straight . r2
        bezier3' point1 point2 point3 = bezier3 (r2 point1) (r2 point2) (r2 point3)

        go ( M_abs (_x,_y) ) = Nothing
        go ( M_rel (_x,_y) ) = Nothing
        go ( L_abs (x,y) ) = Just $ straight' (x0+x, y0+y)
        go ( L_rel (x,y) ) = Just $ straight' (x, y)
        go ( H_abs x) = Just $ straight' (x0 + x, y0)
        go ( H_rel x) = Just $ straight' (x, 0)
        go ( V_abs y) = Just $ straight' (x0, y0 + y)
        go ( V_rel y) = Just $ straight' (0, y)
        go ( C_abs (x1,y1,x2,y2,x,y) ) = Just $ bezier3' (x0+x1, y0+y1) (x0+x2,y0+y2) (x0+x,y0+y)
        go ( C_rel (x1,y1,x2,y2,x,y) ) = Just $ bezier3' (x1, y1) (x2, y2) (x, y)
        go ( S_abs (      x2,y2,x,y) ) = Just $ bezier3' (cx, cy) (x0+x2, y0+y2) (x0+x, y0+y)
        go ( S_rel (      x2,y2,x,y) ) = Just $ bezier3' (cx, cy) (x2, y2) (x, y)
        go ( Q_abs (x1,y1,x,y) ) = Just $ bezier3' (x0 + x1, y0 + y1) (x0 + x, y0 + y) (x0 + x, y0 + y)
        go ( Q_rel (x1,y1,x,y) ) = Just $ bezier3' (x1, y1) (x, y) (x, y)
        go ( T_abs (x,y) ) = Just $ bezier3' (cx, cy) (x0 + x, y0 + y) (x0 + x, y0 + y)
        go ( T_rel (x,y) ) = Just $ bezier3' (cx, cy) (x, y) (x, y)
        go ( Z ) = Nothing
        go ( A_abs ) = Nothing
        go ( A_rel ) = Nothing
