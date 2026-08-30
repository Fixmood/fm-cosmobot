module Main (main) where

import Bot.Handler.Typing.Typst
import Bot.Prelude
import Bot.System.Typst.CLI (typstOutputFileName)
import Bot.System.Typst.Types (TypstOutputFormat (TypstOutputPNG))
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "typing"
      [ testCase "rank cells are emitted as Typst string content" testRankCellsUseTextStrings
      , testCase "Typst strings escape only string syntax" testTypstStringEscapesStringSyntax
      , testCase "different rank documents render to different output names" testDifferentRankDocumentsUseDifferentOutputNames
      ]

testRankCellsUseTextStrings :: IO ()
testRankCellsUseTextStrings = do
  let document = typstDocument "2026-05-14锦标赛成绩" [["用户名", "输入法"], ["魔然@rime-mkm", "[#foo] $x_1$"]]
  assertBool "keeps @ inside a string literal" ("text(\"魔然@rime-mkm\")" `Text.isInfixOf` document)
  assertBool "does not emit markup cells that can parse @ as a label reference" (not ("[魔然@rime-mkm]" `Text.isInfixOf` document))
  assertBool "keeps markup-like text inside a string literal" ("text(\"[#foo] $x_1$\")" `Text.isInfixOf` document)

testTypstStringEscapesStringSyntax :: IO ()
testTypstStringEscapesStringSyntax =
  typstString "a\\b\"c\n下一行" @?= "\"a\\\\b\\\"c\\n下一行\""

testDifferentRankDocumentsUseDifferentOutputNames :: IO ()
testDifferentRankDocumentsUseDifferentOutputNames = do
  let championship =
        typstDocument
          "2026-06-06锦标赛成绩"
          [ ["排名", "用户名", "速度"]
          , ["1", "锦标赛用户", "123.45"]
          ]
      tiger =
        typstDocument
          "2026-06-06虎杯成绩"
          [ ["排名", "用户名", "VIP", "速度", "击键", "码长", "打词率", "时间", "键准", "输入法"]
          , ["1", "虎杯用户", "2", "236.75", "9.42", "2.39", "54.96%", "02:48", "89.38%", "虎码"]
          ]
  assertBool
    "expected distinct output file names"
    (typstOutputFileName TypstOutputPNG championship /= typstOutputFileName TypstOutputPNG tiger)
