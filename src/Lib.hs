module Lib
    ( someFunc,
    allocateRoles,
    writeGameInfo
    ) where


import GHC.IO.Encoding
import Data.Map (Map)
import qualified Data.Map as Map

import System.Random
import Data.List
import Control.Monad
import System.Directory

mkRands = mapM (randomRIO.(,)0 ). enumFromTo 1. pred

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l = let (a,b) = splitAt i l in a++c:(drop 1 b)

swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i,j) xs | i==j = xs
                   | otherwise = replaceAt j (xs!!i) $ replaceAt i (xs!!j) xs

knuthShuffle :: [a] -> IO [a]
knuthShuffle xs =
  liftM (foldr swapElems xs. zip [1..]) (mkRands (length xs))


type Info = Map Int [String]
type Players = [Role]
type CardsOnTable = [Role]

randomChoice :: [a] -> IO a
randomChoice xs = do
    items <- (knuthShuffle xs)
    return (head items)


swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt a b list
    | a > b = swapElementsAt b a list
    | otherwise = left ++ [list !! b] ++ middle ++ [list !! a] ++ right
            where
                left = (take a list)
                middle = (drop (a + 1) (take b list))
                right = (drop (b + 1) list)


changeNthElement :: [a] -> Int -> a -> [a]
changeNthElement items index newItem =
    (take index items) ++ [ newItem ] ++ (drop (index + 1) items)


data Role = Doppelganger | Werewolf | Minion | Mason | Seer 
    | Robber | Troublemaker | Drunk | Insomniac | Hunter | Tanner | Villager deriving (Eq)


instance Show Role where
    show Doppelganger = "化身幽灵"
    show Werewolf = "狼人"
    show Minion = "爪牙"
    show Mason = "守夜人"
    show Seer = "预言家"
    show Robber = "强盗"
    show Troublemaker = "捣蛋鬼"
    show Drunk = "酒鬼"
    show Insomniac = "失眠者"
    show Hunter = "猎人"
    show Tanner = "皮匠"
    show Villager = "村民"

getAllRoles :: Int -> [Role]
getAllRoles numOfPlayers = case numOfPlayers of
    3 -> [Werewolf, Villager, Seer, Minion, Drunk, Robber]
    4 -> [Werewolf, Villager, Seer, Minion, Drunk, Troublemaker, Robber]
    5 -> [Werewolf] ++ (replicate 2 Villager) ++ [Seer, Minion, Drunk, Troublemaker, Robber]
    6 -> (replicate 2 Werewolf) ++ (replicate 2 Villager) ++ [Seer, Minion, Drunk, Troublemaker, Robber]
    7 -> (replicate 2 Werewolf) ++ (replicate 2 Villager) ++ [Seer, Minion, Drunk, Troublemaker, Robber, Insomniac]
    8 -> (replicate 2 Werewolf) ++ (replicate 2 Villager) ++ [Seer, Minion, Drunk, Troublemaker, Robber] ++ (replicate 2 Mason)
    9 -> (replicate 2 Werewolf) ++ (replicate 2 Villager) ++ [Seer, Minion, Drunk, Troublemaker, Robber] ++ (replicate 2 Mason) ++ [Doppelganger]
    10 -> (replicate 2 Werewolf) ++ (replicate 2 Villager) ++ [Seer, Minion, Drunk, Troublemaker, Robber, Insomniac] 
        ++ (replicate 2 Mason) ++ [Doppelganger]


findAll :: Eq a => [a] -> a -> [Int]
findAll items element = map (\item -> case item of (index, item ) -> index)
    (filter
    (\item -> case item of (index, item) -> item == element)
    (zip[0..] items))


enumerateCards :: [a] -> Int -> [(Int, a)]
enumerateCards cards excludeId = (filter
            (\item -> case item of (index, card) -> index /= excludeId)
            (zip [0..] cards))


wereWolfAction :: Role -> Players -> CardsOnTable -> IO (Info)
wereWolfAction doppelgangerNewIdentity originalPlayers cardsOnTable
    | nWerewolves == 1 = do
        cards <- (knuthShuffle cardsOnTable)
        let card = (head (filter (\item -> item /= Werewolf) cards))
            in return $ Map.fromList [
                (head werewolves,
                ["你是场上唯一的狼人，桌上的一张身份牌是 " ++ (show card)])]
    | nWerewolves > 1 = do
        let pairs = [(p1, p2) | p1 <- werewolves, p2 <- werewolves, p1 /= p2]
            in return $ Map.unionsWith (++) (map (
                \item -> case item of
                    (p1, p2) -> Map.fromList[
                        (p1, ["玩家 " ++ (show p2) ++ " 是狼人"])])
            pairs)
    | otherwise = return (Map.empty :: Info)
    where
        werewolves = sort (let werewolves = (findAll originalPlayers  Werewolf)
                            in if doppelgangerNewIdentity == Werewolf
                                then werewolves ++ (findAll originalPlayers Doppelganger)
                                else werewolves)
        nWerewolves = length(werewolves)


seerAction :: Int -> Players -> CardsOnTable -> IO (Info)
seerAction seerId players cardsOnTable = do
    idx <- randomRIO (0 :: Int, 1 :: Int)
    if idx == 1
        then
            let cards = (enumerateCards players seerId)
            in do
                (i, card) <- (randomChoice cards)
                return $ Map.fromList [
                    (seerId,
                    ["你查看了场上一名玩家的身份，玩家 " ++ (show i) ++ " 是 " ++ (show card)])]
        else do
            shuffledCards <- (knuthShuffle cardsOnTable)
            return $ Map.unionsWith (++) (map
                (\item -> Map.fromList [(seerId,
                            ["桌上的一张身份牌是 " ++ (show item)])])
                (take 2 shuffledCards)
                )


robberAction :: Int -> Players -> CardsOnTable -> IO (Players, Info)
robberAction robberId players cardsOnTable = do
    (i, card) <- (randomChoice cards)
    return $ ((swapElementsAt robberId i players),
        Map.fromList [
        (robberId,
        ["玩家 " ++ (show i) ++ " 是 " ++ (show card) ++
        ", 你与该玩家交换了身份"])
        ])
    where cards = (enumerateCards players robberId)


drunkAction :: Int -> Players -> CardsOnTable -> IO (Players, CardsOnTable, Info)
drunkAction drunkId players cardsOnTable = do
    idx <- randomRIO (0 :: Int, (length cardsOnTable) - 1)
    let newIdentity = cardsOnTable !! idx
        origianlIdentity = players !! drunkId
        in return $ (
            (changeNthElement players drunkId newIdentity),
            (changeNthElement cardsOnTable idx origianlIdentity),
            Map.fromList [
                (drunkId,
                ["你交换了你的身份牌与与桌上的一张身份牌"])
            ]
        )


troublemakerAction :: Int -> Players -> CardsOnTable -> IO (Players, Info)
troublemakerAction troublemakerId players cardsOnTable = do
    let cards = (enumerateCards players troublemakerId)
        in do
            shuffledCards <- (knuthShuffle cards)
            let [(p1, role1), (p2, role2)] = (take 2 shuffledCards)
                in return $ (
                    (changeNthElement
                        (changeNthElement players p1 role2)
                        p2 role1),
                    Map.fromList [
                        (troublemakerId,
                        ["你交换了玩家 " ++ (show p1)
                        ++ " 和玩家 " ++ (show p2) ++ " 的身份牌"])])

minionAction :: Role -> Int -> Players -> Info
minionAction doppelgangerNewIdentity minionId players
    | nWerewolves > 0 = Map.fromList (
            [(minionId,
            (map (\p -> "玩家 " ++ (show p) ++ " 是狼人") werewolves))
            ])
    | otherwise = Map.fromList [
            (minionId, ["场上没有狼人"])]
    where
        werewolves = sort (let werewolves = (findAll players  Werewolf)
                            in if doppelgangerNewIdentity == Werewolf
                                then werewolves ++ (findAll players Doppelganger)
                                else werewolves)
        nWerewolves = length(werewolves)


masonAction :: Role -> Players -> Info
masonAction doppelgangerNewIdentity players
    | nMasons > 1 =
        let pairs = [(p1, p2) | p1 <- masons, p2 <- masons, p1 /= p2]
            in Map.unionsWith (++) (map (
                \item -> case item of
                    (p1, p2) -> Map.fromList [
                        (p1, ["玩家 " ++ (show p2) ++ " 是守夜人"])])
                    pairs)
    | nMasons == 1 = Map.fromList [
        ((head (findAll players Mason)),
        ["场上没有其他玩家是守夜人"])]
    | otherwise = Map.empty :: Info
    where
        masons = sort (let masons = (findAll players Mason)
                        in if doppelgangerNewIdentity == Mason
                            then masons ++ (findAll players Doppelganger)
                            else masons)
        nMasons = (length masons)


doppelgangerAction :: Int -> Players -> CardsOnTable -> IO (Role, Players, CardsOnTable, Info)
doppelgangerAction doppelgangerId players cardsOnTable = do
    (i, card) <- (randomChoice (filter (
        \item -> case item of
            (id, role) -> id /= doppelgangerId
        ) (zip [0..] players)))
    let firstInfo = Map.fromList [
            (doppelgangerId,
            ["玩家 " ++ (show i) ++ " 的身份是 " ++ (show card) ++ ", 你获得了该玩家的身份"])]
        in
        case card of
            Seer -> (seerAction doppelgangerId players cardsOnTable) >>= (
                \info -> return (card, players, cardsOnTable, (Map.unionWith (++) firstInfo info))
                )
            Minion -> return $ (card, players, cardsOnTable, (Map.unionWith (++)
                firstInfo
                (Map.fromList [
                    (doppelgangerId,
                    (map (\p -> "玩家 " ++ (show p) ++ " 是狼人")
                    (findAll players Werewolf)))])))
            Robber -> (robberAction doppelgangerId players cardsOnTable) >>= (
                        \item -> case item of
                            (newPlayers, secondInfo) -> return (
                                (card, newPlayers, cardsOnTable,
                                (Map.unionWith (++) firstInfo secondInfo))))
            Troublemaker -> (troublemakerAction doppelgangerId players cardsOnTable) >>= (
                \item -> case item of
                    (newPlayers, secondInfo) -> return (
                        (card, newPlayers, cardsOnTable,
                        (Map.unionWith (++) firstInfo secondInfo))))
            Drunk -> (drunkAction doppelgangerId players cardsOnTable) >>= (
                \item -> case item of
                    (newPlayers, newCardsOnTable, secondInfo) -> return (
                        (card, newPlayers, newCardsOnTable,
                        (Map.unionWith (++) firstInfo secondInfo))))
            otherwise ->
                return (card, players, cardsOnTable, firstInfo)

insomniacAction :: Int -> Players -> Info
insomniacAction insomniacId players =
    Map.fromList [
        (insomniacId,
        ["你当前的身份是 " ++ (show role)])
    ]
    where role = players !! insomniacId

allocateRoles :: Int -> IO (Players, CardsOnTable)
allocateRoles numOfPlayers = roles >>= (\cards -> return (splitAt numOfPlayers cards))
    where roles = (knuthShuffle (getAllRoles numOfPlayers))


initialInfo :: Players -> Info
initialInfo players =
    Map.fromList (map
    (\item -> case item of
        (index, card) -> (index, ["你的初始身份是 " ++ (show card)]))
    (zip [0..] players))

nightActions :: Players -> CardsOnTable -> IO (Players, CardsOnTable, Info)
nightActions originalPlayers cardsOnTable = do

    (doppelgangerNewIdentity, players, cardsOnTable, info1) <-
        case (elemIndex Doppelganger originalPlayers) of
        Just index -> (doppelgangerAction index originalPlayers cardsOnTable)
        Nothing -> return (Doppelganger, originalPlayers, cardsOnTable, Map.empty :: Info)
    putStrLn("化身幽灵操作完毕")
    putStrLn((show (doppelgangerNewIdentity, players, cardsOnTable)))

    info2 <- case (elemIndex Werewolf originalPlayers) of
        Just _ -> (wereWolfAction doppelgangerNewIdentity originalPlayers cardsOnTable)
        Nothing -> return (Map.empty :: Info)

    info3 <- case (elemIndex Minion originalPlayers) of
        Just minionId -> return (minionAction doppelgangerNewIdentity minionId players)
        Nothing -> return (Map.empty :: Info)
    
    info4 <- case (elemIndex Mason originalPlayers) of
        Just _ -> return (masonAction doppelgangerNewIdentity originalPlayers)
        Nothing -> return (Map.empty :: Info)

    info5 <- case (elemIndex Seer originalPlayers) of
        Just seerId -> (seerAction seerId players cardsOnTable)
        Nothing -> return (Map.empty :: Info)
    
    (players, info6) <- case (elemIndex Robber originalPlayers) of
        Just robberId -> (robberAction robberId players cardsOnTable)
        Nothing -> return (players, Map.empty :: Info)
    putStrLn("强盗操作完毕")
    putStrLn((show (players, cardsOnTable)))

    (players, info7) <- case (elemIndex Troublemaker originalPlayers) of
        Just troublemakerId -> (troublemakerAction troublemakerId players cardsOnTable)
        Nothing -> return (players, Map.empty :: Info)
    putStrLn("捣蛋鬼操作完毕")
    putStrLn((show (players, cardsOnTable)))

    (players, cardsOnTable, info8) <- case (elemIndex Drunk originalPlayers) of
        Just drunkId -> (drunkAction drunkId players cardsOnTable)
        Nothing -> return (players, cardsOnTable, Map.empty :: Info)
    putStrLn("酒鬼操作完毕")
    putStrLn((show (players, cardsOnTable)))

    info9 <- case (elemIndex Insomniac originalPlayers) of
        Just insomniacId -> return (insomniacAction insomniacId players)
        Nothing -> return (Map.empty :: Info)
    
    info10 <- if doppelgangerNewIdentity == Insomniac then
                    case (elemIndex Doppelganger originalPlayers) of
                        Just insomniacId -> return (insomniacAction insomniacId players)
                else return (Map.empty :: Info)

    return (players, cardsOnTable, Map.unionsWith (++) [
        (initialInfo originalPlayers),
        info1, info2, info3, info4, info5, info6, info7, info8, info9, info10
        ])


printInfo :: Info -> String
printInfo info = (intercalate "\n"
    (map (\playerId -> case Map.lookup playerId info of
                Just items -> "玩家 " ++ (show playerId) ++ " 的信息：\n" ++
                    (intercalate "\n" items) ++ "\n")
                [0..(n - 1)]
                ))
    where n = Map.size info


writeInfo :: FilePath -> Info -> IO [()]
writeInfo directoryPath info =
    (mapM (\item -> case item of
        (i, text) -> (writeFile
                        (directoryPath ++ "/玩家" ++ (show i) ++ ".txt")
                        text))
        (zip [0..(n - 1)] playerInfoList))
    where
        n = Map.size info
        playerInfoList = (map
                (\playerId -> case Map.lookup playerId info of
                    Just items -> (intercalate "\n" items))
                    [0..(n - 1)])


writeGlobalInfo :: FilePath -> (Players, CardsOnTable) -> (Players, CardsOnTable) -> IO ()
writeGlobalInfo directoryPath (originalPlayers, originalCardsOnTable) (players, cardsOnTable) =
    let
        originalPlayersText = (intercalate "\n" (map (\item -> case item of
            (playerId, player) -> "玩家 " ++ (show playerId) ++ " 的初始身份是 " ++
                (show player)) (zip [0..] originalPlayers)))
        originalCardsOnTableText = (intercalate "\n" (map (\card ->
                "初始状态下，桌上的一张牌为 " ++ (show card)) originalCardsOnTable))
        currentPlayersText = (intercalate "\n" (map (\item -> case item of
            (playerId, player) -> "行动结束时，玩家 " ++ (show playerId) ++ " 的身份是 " ++
                (show player)) (zip [0..] players)))
        currentCardsOnTableText = (intercalate "\n" (map (\card ->
                "行动结束时，桌上的一张牌为 " ++ (show card)) cardsOnTable))
        werewolvesText = "场上的狼人为 " ++ (intercalate ", "
            (map (\item -> case item of
                (playerId, _) -> "玩家 " ++ (show playerId))
            (filter (\item -> case item of
                (playerId, player) -> player == Werewolf) (zip [0..] originalPlayers))))
        in
            writeFile
                (directoryPath ++ "/游戏信息（游戏结束前请勿打开）.txt")
                (originalPlayersText ++ "\n\n" ++ originalCardsOnTableText ++ "\n" ++
                "\n" ++ currentPlayersText ++ "\n\n" ++ currentCardsOnTableText ++ "\n" ++ werewolvesText)


writeGameInfo :: Int -> Int -> IO [()]
writeGameInfo nPlayers nGames =
    mapM (\i -> do
            setLocaleEncoding utf8
            (originalPlayers, originalCardsOnTable) <- (allocateRoles nPlayers)
            (players, cardsOnTable, info) <- (nightActions originalPlayers originalCardsOnTable)
            (createDirectoryIfMissing True (folderName i))
            (writeInfo (folderName i) info)
            (writeGlobalInfo (folderName i)
                (originalPlayers, originalCardsOnTable) (players, cardsOnTable))) [1..nGames]
    where folderName = (\i -> "一夜狼人杀（" ++ (show nPlayers) ++ "）人局/游戏" ++ (show i))


someFunc :: IO ()
someFunc = do
    setLocaleEncoding utf8
    setStdGen (mkStdGen 56)
    (originalPlayers, originalCardsOnTable) <- (allocateRoles 8)
    
    putStrLn("original players: ")
    putStrLn((show originalPlayers))
    putStrLn("original cards on the table: ")
    putStrLn((show originalCardsOnTable))
    
    (players, cardsOnTable, info) <- (nightActions originalPlayers originalCardsOnTable)
    
    putStrLn("")
    putStrLn("current players: ")
    putStrLn((show players))
    putStrLn("current cards on the table: ")
    putStrLn((show cardsOnTable))
    putStrLn("")
    putStrLn((printInfo info))
    
