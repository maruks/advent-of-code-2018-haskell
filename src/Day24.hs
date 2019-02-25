{-# LANGUAGE RecordWildCards #-}

module Day24
  (
    solution1, solution2, boostedTeamWins, Damage (..), Team (..), Group (..)
  ) where

import Data.List as List
import Data.Set as Set
import Data.Ord as Ord
import Data.Map.Strict as Map

data Damage = Cold | Bludgeoning | Fire | Radiation | Slashing deriving (Eq, Show, Read, Ord)

data Team = TeamA | TeamB deriving (Eq, Show, Read, Ord)

type GroupId = String

data Group = Group {name :: GroupId,
                    units :: Int,
                    hitPoints :: Int,
                    weak :: Set Damage,
                    immune :: Set Damage,
                    damageType :: Damage,
                    team :: Team,
                    damage :: Int,
                    initiative :: Int} deriving (Eq, Show)

effectivePower :: Group -> Int
effectivePower Group{..} = units * damage

instance Ord Group where
  compare g1@Group{initiative = i1} g2@Group{initiative = i2} =
    let cEffPow = compare (effectivePower g1) (effectivePower g2)
        cIn = compare i1 i2
    in cEffPow <> cIn

damageAmount :: Group -> Group -> Int
damageAmount attacker@Group{..} Group{weak = w, immune = i}
  | Set.member damageType w = 2 * effectivePower attacker
  | Set.member damageType i = 0
  | otherwise = effectivePower attacker

compareTargets :: Group -> Group -> Group -> Ordering
compareTargets attacker@Group{..} defender1@Group{initiative = i1} defender2@Group{initiative = i2} =
  let dmgAmount = damageAmount attacker
      cDmg = compare (dmgAmount defender1) (dmgAmount defender2)
      cEffPow = compare (effectivePower defender1) (effectivePower defender2)
      cIn = compare i1 i2
  in cDmg <> cEffPow <> cIn

selectTarget :: Group -> [Group] -> Maybe GroupId
selectTarget attacker [] = Nothing
selectTarget attacker defenders =
  let defender = List.maximumBy (compareTargets attacker) defenders
  in case damageAmount attacker defender of
    0 -> Nothing
    _ -> Just (name defender)

enemyGroups :: Group -> [Group] -> [Group]
enemyGroups Group{..} = List.filter (\Group{team = t, units = u} -> t /= team && u > 0)

selectTargets' :: [Group] -> Map GroupId Group -> [(GroupId, GroupId)]
selectTargets' [] _ = []
selectTargets' (x:xs) defenders =
  case selectTarget x $ enemyGroups x (Map.elems defenders) of
    Just defender -> (name x, defender) : selectTargets' xs (Map.delete defender defenders)
    Nothing -> selectTargets' xs defenders

dealDamage :: Group -> Group -> Group
dealDamage attacker defender@Group{..} =
  let amount = damageAmount attacker defender
      unitsKilled = amount `div` hitPoints
  in defender {units = max 0 (units - unitsKilled)}

selectTargets :: Map GroupId Group -> Map GroupId GroupId
selectTargets group = Map.fromList $ selectTargets' (List.sortBy (comparing Down) $ Map.elems group) group

performAttacks :: [GroupId] -> Map GroupId Group -> Map GroupId GroupId -> Map GroupId Group
performAttacks [] groups _ = groups
performAttacks (x:xs) groups targets =
  case targets !? x of
    Nothing -> performAttacks xs groups targets
    Just targetName -> let nextGroups = Map.adjust (dealDamage (groups ! x)) targetName groups
                       in performAttacks xs nextGroups targets

combatResult :: Map GroupId Group -> Map GroupId Group
combatResult groups =
  let attackers = List.map name $ List.sortOn (Down . initiative) $ Map.elems groups
      targets = selectTargets groups
      nextGroups = Map.filter (\Group{..} -> units > 0) $ performAttacks attackers groups targets
  in if Map.null targets || groups == nextGroups
     then groups
     else combatResult nextGroups

groupsMap :: [Group] -> Map GroupId Group
groupsMap groups = Map.fromList $ List.zip (name <$> groups) groups

solution1 :: [Group] -> Int
solution1 groups =
  List.sum $ units <$> result
  where result = combatResult $ groupsMap groups

boostedTeamWins :: Int -> Team -> [Group] -> (Bool, Int)
boostedTeamWins boost team_ groups =
  let boosted = List.map (\g@Group{..} -> if team == team_
                                          then g {damage = damage + boost}
                                          else g) groups
      result = combatResult $ groupsMap boosted
      alive = Map.elems result
  in (List.all (\Group{team=t} -> t == team_) alive, List.sum $ units <$> alive)

solution2 :: [Group] -> Int
solution2 groups =
  let results = List.map (\b -> boostedTeamWins b TeamA groups) [0..]
  in snd $ head $ List.dropWhile (not . fst) results
