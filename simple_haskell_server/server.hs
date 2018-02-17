import Network.CoAP.Server
import Network.CoAP.Transport
import Network.Socket
import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import System.IO
import Numeric.Limits

-- withSocketsDo:: IO a -> IO a
-- listenOn:: PortID -> IO Socket
-- accept:: Socket -> IO (Handle, HostName, PortNumber)
-- forkIO:: IO () -> IO ThreadId

type CalibratedEmpty = Double
type ServerAdress = Endpoint
type State = (ServerAdress, CalibratedEmpty)
type MState = MVar State

main:: IO ()
main = do
  serverThread
  
--  clientThread




serverThread = withSocketsDo $ do
--    sock <- socket AF_INET6 Datagram defaultProtocol
    sock <- socket AF_INET Datagram defaultProtocol
--    bind sock (SockAddrInet6 5683 0 iN6ADDR_ANY 0)
    bind sock (SockAddrInet 5683 iNADDR_ANY)
    mstate <- newMVar  (initialServerAdress, maxValue) 
    server <- createServer (createUDPTransport sock) $ requestHandler mstate
    runServer server

initialServerAdress:: Endpoint
--initialServerAdress = (SockAddrInet6 80 0 (0,0,0,1) 0) 
initialServerAdress = (SockAddrInet 80 (0x7f000001)) --127.0.0.1 BigEndian 



findPath :: [Option] -> B.ByteString
findPath [] = B.empty
findPath (option:options) =
  let restPath = findPath options
      sep      = if B.null restPath then B.empty else B.pack "/"
   in case option of
        UriPath value -> B.append value (B.append sep restPath)
        _             -> findPath options


requestHandler :: MState -> RequestHandler
requestHandler mstate req@((Request method  _  _  _), _) = 
  case method of 
    GET    -> handleGet    mstate req
    POST   -> handlePost   mstate req
    PUT    -> error ("Unsupported Method:PUT")
    DELETE -> error ("Unsupported Method:DELETE")


handleGet :: MState -> RequestHandler
handleGet mstate req@(request,_) = 
  case path request of    
    ".well-known/core" -> handleHelp req
    "help"             -> handleHelp req
    "serverAdress"     -> handleGetServerAdress mstate req
    "isEmpty"          -> handleIsEmpty mstate req
    "gps"              -> handleGetGPS req
    "calibrated"       -> handleCalibrated mstate req
    _                  -> return $ Response Content [ContentFormat ApplicationJson] $ B.pack <$> Just ("{\"error\":\"Unknown path in GET: " ++ show (path request) ++ "\"}") 

handlePost :: MState -> RequestHandler
handlePost mstate req@(request,_) =
  case path request of
    "calibrate"    -> handleCalibrate mstate req -- full
    "serverAdress" -> handleSetServerAdress mstate req
    _              -> return $ Response Content [ContentFormat ApplicationJson] $ B.pack <$> Just ("{\"error\":\"Unknown path in POST: " ++ show (path request) ++ "\"}")


handleHelp :: RequestHandler
handleHelp = const $ return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"help\":\"GET serverAdress; GET isEmpty; GET gps; GET Calibrated; POST calibrate 'false'; POST calibrate 'true'; POST serverAdress 'new_ipv4_serveradress'\"}"))))

handleIsEmpty :: MState -> RequestHandler
handleIsEmpty mstate = const $ do
  emptyness <- readFile "emptyness_file"
  (_,cali) <- readMVar mstate
  return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"isEmpty\":\"" ++ show (read emptyness < cali) ++ "\"}"))))

handleCalibrated :: MState -> RequestHandler
handleCalibrated mstate = const $ do
  (_,cali) <- readMVar mstate
  return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"isEmpty\":\"" ++ show cali ++ "\"}"))))

handleGetServerAdress :: MState -> RequestHandler
handleGetServerAdress mstate = const $ do
  (serverAdress,_) <- readMVar mstate
  return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"getServerAdress\":\"" ++ show serverAdress ++ "\"}"))))

handleGetGPS :: RequestHandler
handleGetGPS = const $ return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"getGPS\":\"TODO\"}"))))

handleCalibrate :: MState -> RequestHandler
handleCalibrate mstate ((Request _ _ payload _),_) = case B.unpack <$> payload of
  Nothing      ->  return $ Response Content [ContentFormat ApplicationJson] $ B.pack <$> Just ("{\"error\":\"calibrate was send without payload\"}")
  Just "false" -> do 
      (adr,_) <- readMVar mstate
      swapMVar mstate (adr,maxValue)
      return (Response Content [ContentFormat ApplicationLinkFormat] (Nothing))
  Just "true"  -> do  
      cali <- readFile "emptyness_file"
      (adr,_) <- readMVar mstate
      swapMVar mstate (adr,read cali)
      return (Response Content [ContentFormat  ApplicationLinkFormat] (Nothing))
  _            -> return $ Response Content [ContentFormat ApplicationJson] $ B.pack <$> Just ("{\"error\":\"calibrate payload was neither 'true' nor 'false'\"}")

handleSetServerAdress :: MState -> RequestHandler
handleSetServerAdress mstate ((Request _ _ payload _),_) = case B.unpack <$> payload of
  Nothing      -> return $ Response Content [ContentFormat ApplicationJson] $ B.pack <$> Just ("{\"error\":\"No payload given for POST serverAdress\"}")
  Just s       -> do
      (a,c) <- readMVar mstate
      swapMVar mstate (a,c) --(read s, c)
      return (Response Content [ContentFormat ApplicationLinkFormat] (Nothing))


path :: Request -> String 
path (Request _ options _ _) = B.unpack (findPath options)



