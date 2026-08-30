{-|
Module      : Bot.Handler.Typing.Typst
Description : Typst document generation for typing rank snapshots
Stability   : experimental
-}

module Bot.Handler.Typing.Typst
  ( typstDocument
  , typstString
  )
where

import Bot.Prelude
import qualified Data.Char as Char
import qualified Data.Text as Text

typstDocument :: Text -> [[Text]] -> Text
typstDocument title rows =
  Text.unlines
    [ "#let table-width = " <> tableWidth rows
    , "#set page(width: table-width + 36pt, height: auto, margin: 18pt)"
    , "#set text(font: (\"Droid Sans Fallback\", \"Noto Sans\", \"DejaVu Sans\"), size: 8pt)"
    , "#align(center)[#text(" <> typstString title <> ", size: 16pt, weight: \"bold\")]"
    , "#v(8pt)"
    , "#block(width: table-width)["
    , "#table("
    , "  columns: " <> tableColumns rows <> ","
    , "  inset: 3pt,"
    , "  stroke: rgb(\"d8dee9\"),"
    , "  fill: (_, y) => if y == 0 { rgb(\"edf2f7\") } else if calc.rem(y, 2) == 0 { rgb(\"fbfbfb\") } else { white },"
    , "  align: center + horizon,"
    , cells
    , ")"
    , "]"
    ]
  where
    cells =
      Text.intercalate ",\n"
        [ "  " <> typstCell cell
        | row <- rows
        , cell <- row
        ]

tableColumns :: [[Text]] -> Text
tableColumns rows =
  case maybe 0 length (viaNonEmpty head rows) of
    14 -> "(36pt, 72pt, 42pt, 112pt, 48pt, 48pt, 48pt, 42pt, 64pt, 38pt, 52pt, 52pt, 112pt, 92pt)"
    10 -> "(36pt, 104pt, 34pt, 58pt, 48pt, 48pt, 54pt, 58pt, 52pt, 116pt)"
    n  -> "(" <> Text.intercalate ", " (replicate n "auto") <> ")"

tableWidth :: [[Text]] -> Text
tableWidth rows =
  case maybe 0 length (viaNonEmpty head rows) of
    14 -> "828pt"
    10 -> "608pt"
    _  -> "auto"

typstCell :: Text -> Text
typstCell cell =
  "text(" <> typstString cell <> ")"

typstString :: Text -> Text
typstString text =
  "\"" <> Text.concatMap escapeStringChar text <> "\""

escapeStringChar :: Char -> Text
escapeStringChar = \case
  '\\' -> "\\\\"
  '"'  -> "\\\""
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c
    | Char.isControl c -> " "
    | otherwise -> Text.singleton c
