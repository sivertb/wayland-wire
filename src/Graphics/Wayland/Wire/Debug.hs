module Graphics.Wayland.Wire.Debug
    ( MessageType (..)
    , ppMsg
    )
where

import Data.List
import Graphics.Wayland.Protocol
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import Text.Printf

data MessageType = MsgReq | MsgEvt
    deriving (Eq, Enum, Show)

liftMaybe :: String -> Maybe a -> Either String a
liftMaybe err = maybe (Left err) Right

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ []     = Nothing
safeIndex 0 (x:_ ) = Just x
safeIndex n (_:xs) = safeIndex (n - 1) xs

getEvtName :: Interface -> OpCode -> Either String String
getEvtName iface op =
    eventName
    <$> liftMaybe "No such event" (safeIndex (fromIntegral $ unOpCode op) (ifaceEvents iface))

getReqName :: Interface -> OpCode -> Either String String
getReqName iface op =
    reqName
    <$> liftMaybe "No such request" (safeIndex (fromIntegral $ unOpCode op) (ifaceRequests iface))

getFuncName :: MessageType -> Interface -> OpCode -> Either String String
getFuncName MsgReq = getReqName
getFuncName MsgEvt = getEvtName

-- | Pretty prints a message argument.
ppArg :: (ObjId -> Maybe Interface) -> MsgArg -> Either String String
ppArg lf arg =
    case arg of
      ArgInt    i          -> return $ show i
      ArgWord   w          -> return $ show w
      ArgEnum   w          -> return $ show w
      ArgFixed  f          -> return . show $ fixedToDouble f
      ArgFd     fd         -> return $ show fd
      ArgString Nothing    -> return "str none"
      ArgString (Just str) -> return $ show str
      ArgNew    Nothing    -> return "new none"
      ArgNew    (Just oid) -> return $ printf "new %i" (unNewId oid)
      ArgArray  ws         -> return $ printf "[%s]" (intercalate ", " (map show ws))
      ArgObject Nothing    -> return "object none"
      ArgObject (Just oid) ->
          printf "object %s(%i)" . ifaceName
          <$> liftMaybe "Could not find object" (lf oid)
          <*> pure (unObjId oid)

-- | Pretty prints a message.
ppMsg :: (ObjId -> Maybe Interface) -> MessageType -> Message -> Either String String
ppMsg lf msgType msg = do
    iface    <- liftMaybe "Could not find object" . lf $ msgObj msg
    funcName <- getFuncName msgType iface $ msgOp msg
    args     <- mapM (ppArg lf) (msgArgs msg)

    return $ printf "%s@%i.%s(%s)" (ifaceName iface) (unObjId $ msgObj msg) funcName (intercalate ", " args)
