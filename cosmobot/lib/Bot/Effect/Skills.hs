{-|
Module      : Bot.Effect.Skills
Description : Startup skill metadata capability facade
Stability   : experimental
-}

module Bot.Effect.Skills
  ( Skills
  , skillsSystemPrompt
  , loadSkill
  , reloadSkills
  , runSkills
  )
where

import Bot.Prelude
import qualified Bot.Skills as SkillsStore
import qualified Effectful.Prim.IORef as IORef

data Skills :: Effect where
  SkillsSystemPrompt :: Skills m Text
  LoadSkill :: Text -> Skills m (Maybe Text)
  ReloadSkills :: Skills m ()

type instance DispatchOf Skills = Dynamic

skillsSystemPrompt :: Skills :> es => Eff es Text
skillsSystemPrompt =
  send SkillsSystemPrompt

loadSkill :: Skills :> es => Text -> Eff es (Maybe Text)
loadSkill =
  send . LoadSkill

reloadSkills :: Skills :> es => Eff es ()
reloadSkills =
  send ReloadSkills

runSkills
  :: (IOE :> es, Prim :> es)
  => SkillsStore.SkillsConfig
  -> Eff (Skills : es) a
  -> Eff es a
runSkills cfg action = do
  promptRef <- IORef.newIORef =<< SkillsStore.loadSkillsPrompt cfg
  interpret (\_ -> \case
    SkillsSystemPrompt -> (.systemPrompt) <$> IORef.readIORef promptRef
    LoadSkill name -> SkillsStore.skillContent name <$> IORef.readIORef promptRef
    ReloadSkills -> IORef.writeIORef promptRef =<< SkillsStore.loadSkillsPrompt cfg
    ) action
