import Network.CoAP.Server
import Network.CoAP.Transport
import Network.Socket
import Network.CoAP.Client
import qualified Network.URI as URI
import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import System.IO
import Numeric.Limits
import qualified Data.List as DL

-- withSocketsDo:: IO a -> IO a
-- listenOn:: PortID -> IO Socket
-- accept:: Socket -> IO (Handle, HostName, PortNumber)
-- forkIO:: IO () -> IO ThreadId

type CalibratedValue = Double
type ServerAdress = Endpoint
type Observers = [Endpoint]
data State = ST { adr::ServerAdress, calEmpty::CalibratedValue, calFull::CalibratedValue, obs::Observers}
type MState = MVar State

main:: IO ()
main = serverThread
  
notifierThread mstate = do
    threadDelay 2000000
    st <- readMVar mstate
    emptyness <- readFile "emptyness_file"
    if calEmpty st <= read emptyness then do
      notifieAllObs $ obs st
      notifierThread mstate
    else
      notifierThread mstate

notifieAllObs [] = return ()
notifieAllObs (o:obs) = do
    putStrLn "im in notifieAll"
    let request = Request { requestMethod = GET
                        , requestOptions = []
                        , requestPayload = Nothing
                        , requestReliable = True }
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock o --coudn't bind sender endpoint of the observer request
    let transport = createUDPTransport sock
    client <- createClient transport
    let uriStr = URI.parseURI "/observe"
    case uriStr of
        Nothing -> return ()
        Just uri -> do 
            doRequest client uri request
            return ()




serverThread = withSocketsDo $ do
--    sock <- socket AF_INET6 Datagram defaultProtocol
    sock <- socket AF_INET Datagram defaultProtocol
--    bind sock (SockAddrInet6 5683 0 iN6ADDR_ANY 0)
    bind sock (SockAddrInet 5683 iNADDR_ANY)
    mstate <- newMVar  (ST initialServerAdress maxValue minValue []) 
--    forkIO $ notifierThread mstate
    server <- createServer (createUDPTransport sock) $ requestHandler mstate
    runServer server

initialServerAdress:: Endpoint
-- initialServerAdress = (SockAddrInet6 80 0 (0,0,0,1) 0) 
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
requestHandler mstate req@(request, _) = 
  case requestMethod request of 
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
    "weight"           -> handleWeight mstate req
    "isEmpty"          -> handleIsEmpty mstate req
    "gps"              -> handleGetGPS req
    "calibrated"       -> handleCalibrated mstate req
    "observe"          -> handleAddObserver mstate req
    "stop_observing"   -> handleDeleteObserver mstate req
    _                  -> return $ jsonResponse ("{\"error\":\"Unknown path in GET: " ++ show (path request) ++ "\"}") 


handlePost :: MState -> RequestHandler
handlePost mstate req@(request,_) =
  case path request of
    "calibrate"    -> handleCalibrate mstate req -- full
    "serverAdress" -> handleSetServerAdress mstate req
    _              -> return $ jsonResponse ("{\"error\":\"Unknown path in POST: " ++ show (path request) ++ "\"}")


handleAddObserver :: MState -> RequestHandler
handleAddObserver mstate (_,endpoint) = do
    st <- readMVar mstate
    if elem endpoint (obs st) then 
      return $ jsonResponse "{\"observer\":\"you are already observing\"}"
    else do
      let newSt = st{obs = endpoint : obs st}
      swapMVar mstate newSt
      return $ jsonResponse "{\"observer\":\"you are now observing\"}"


handleDeleteObserver :: MState -> RequestHandler
handleDeleteObserver mstate (_,endpoint) = do
    st <- readMVar mstate
    if elem endpoint (obs st) then do
      let newSt = st{obs = DL.delete endpoint (obs st)}
      return $ jsonResponse "{\"observer\":\"you don't observe anymore\"}"
    else return $ jsonResponse "{\"observer\":\"you weren't observing anyways\"}"


handleHelp :: RequestHandler
handleHelp = const $ return $ jsonResponse ("{\"help\":\"" ++ 
    "GET serverAdress;"++
    " GET isEmpty;"++
    " GET gps;"++
    " GET calibrated;"++
    " GET weight;"++
    " POST calibrate 'false';"++
    " POST calibrate 'true';"++
    " POST serverAdress 'new_ipv4_serveradress'\"}")

handleIsEmpty :: MState -> RequestHandler
handleIsEmpty mstate = const $ do
  emptyness <- readFile "emptyness_file"
  st <- readMVar mstate
  return $ jsonResponse ("{\"isEmpty\":\"" ++ show (read emptyness < calEmpty st) ++ "\"}")

handleWeight :: MState -> RequestHandler
handleWeight mstate = const $ do
  emptyness <- readFile "emptyness_file"
  st <- readMVar mstate
  let weightProcent = 100 * ((read emptyness) - (calEmpty st))  / (calFull st - calEmpty st)
  return $ jsonResponse ("{\"weight\":\"" ++ show weightProcent ++ "\"}")


handleCalibrated :: MState -> RequestHandler
handleCalibrated mstate = const $ do
  st <- readMVar mstate
  return $ jsonResponse ("{\"calibrated\":\"" ++ show (calEmpty st, calFull st) ++ "\"}")

handleGetServerAdress :: MState -> RequestHandler
handleGetServerAdress mstate = const $ do
  st <- readMVar mstate
  return $ jsonResponse ("{\"getServerAdress\":\"" ++ show (adr st) ++ "\"}")

handleGetGPS :: RequestHandler
handleGetGPS = const $ return $ jsonResponse ("{\"getGPS\":\"TODO\"}")

handleCalibrate :: MState -> RequestHandler
handleCalibrate mstate ((Request _ _ payload _),_) = case B.unpack <$> payload of
  Nothing      ->  return $ jsonResponse ("{\"error\":\"calibrate was send without payload\"}")
  Just "false" -> do 
      cali <- readFile "emptyness_file"
      st <- readMVar mstate
      let newSt = st{calFull = read cali}
      swapMVar mstate newSt
      return voidResponse
  Just "true"  -> do  
      cali <- readFile "emptyness_file"
      st <- readMVar mstate
      let newSt = st{ calEmpty = read cali}
      swapMVar mstate newSt
      return voidResponse
  _            -> return $ jsonResponse ("{\"error\":\"calibrate payload was neither 'true' nor 'false'\"}")

handleSetServerAdress :: MState -> RequestHandler
handleSetServerAdress mstate ((Request _ _ payload _),_) = case B.unpack <$> payload of
  Nothing      -> return $ jsonResponse ("{\"error\":\"No payload given for POST serverAdress\"}")
  Just s       -> do
      st <- readMVar mstate
      swapMVar mstate st --(read s, c)
      return voidResponse


path :: Request -> String 
path = B.unpack . findPath . requestOptions


jsonResponse :: String -> Response
jsonResponse = Response Content [ContentFormat ApplicationJson] . Just . B.pack 


voidResponse :: Response
voidResponse = Response Content [ContentFormat ApplicationLinkFormat] Nothing
