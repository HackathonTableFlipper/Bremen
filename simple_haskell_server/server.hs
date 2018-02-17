import Network.CoAP.Server
import Network.CoAP.Transport
import Network.Socket
import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import System.IO

-- withSocketsDo:: IO a -> IO a
-- listenOn:: PortID -> IO Socket
-- accept:: Socket -> IO (Handle, HostName, PortNumber)
-- forkIO:: IO () -> IO ThreadId

-- runStateT :: StateT s m a -> s -> m (a, s) 
-- evalStateT :: Monad m => StateT s m a -> s -> m a 
-- evalStateT m s = liftM fst (runStateT m s)



main:: IO a
main = withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bind sock (SockAddrInet6 5002 0 iN6ADDR_ANY 0)
    server <- createServer (createUDPTransport sock) requestHandler
    runServer server


findPath :: [Option] -> B.ByteString
findPath [] = B.empty
findPath (option:options) =
  let restPath = findPath options
      sep      = if B.null restPath then B.empty else B.pack "/"
   in case option of
        UriPath value -> B.append value (B.append sep restPath)
        _             -> findPath options


requestHandler :: RequestHandler
requestHandler req@(request, endpoint) = do
  let options = requestOptions request
  
  let path = B.unpack (findPath options)
  case path of
    "calibrate" -> handleCore req
    "test"             -> handleTest req
    "separate"         -> handleSeparate req
    "seg1/seg2/seg3"   -> handleSeg req
    "query"            -> handleQuery req
    "location-query"   -> handleLocationQuery req
    "multi-format"     -> handleMultiFormat req
    "validate"         -> handleValidate req
    "create1"          -> handleCreate1 req
    _                  -> error ("Unknown path " ++ show path) 
 
msg:: IO String
msg = readFile "HTTP"


http = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
