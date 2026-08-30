{-|
Module      : Bot.Agent.Tools.Skills
Description : Agent skill tools
Stability   : experimental
-}

module Bot.Agent.Tools.Skills
  ( loadSkillTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Skills as Skills
import Bot.Prelude

loadSkillTool :: Skills.Skills :> es => Tool (Eff es)
loadSkillTool =
  withDescription "Load the full instructions for an available skill by name."
  $ tool "load_skill"
      (requiredText "name" "Skill name advertised in the system prompt.")
      \name ->
        toolText . fromMaybe "Skill not found." <$> Skills.loadSkill name
