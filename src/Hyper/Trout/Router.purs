module Hyper.Trout.Router
       ( RoutingError(..)
       , class Router
       , route
       , router
       ) where

import Prelude
import Data.HTTP.Method as Method
import Type.Trout.Record as Record
import Control.Bind.Indexed (idiscard)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Indexed (ibind, ipure)
import Data.Array (elem, filter, null, uncons)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Lazy (force)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, lookup, snd)
import Foreign.Object (Object)
import Foreign.Object (lookup) as F
import Hyper.Conn (Conn)
import Hyper.ContentNegotiation (AcceptHeader, NegotiationResult(..), negotiateContent, parseAcceptHeader)
import Hyper.Middleware (Middleware, lift')
import Hyper.Request (class Request, class ReadableBody, getRequestData, readBody)
import Hyper.Response (contentType, closeHeaders, respond, writeStatus, class Response, class ResponseWritable, ResponseEnded, StatusLineOpen)
import Hyper.Status (Status (Status), statusBadRequest, statusMethodNotAllowed, statusNotAcceptable, statusNotFound, statusOK)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Type.Trout as Trout
import Type.Trout (type (:<|>), type (:=), type (:>), ReqBody, Capture, CaptureAll, QueryParam, QueryParams, Lit, Raw)
import Type.Trout.ContentType (class AllMimeRender, class MimeParse, allMimeRender, mimeParse)
import Type.Trout.PathPiece (class FromPathPiece, fromPathPiece)

type Method' = Either Method CustomMethod

type RoutingContext = { path :: Array String
                      , query :: Array (Tuple String (Maybe String))
                      , method :: Method'
                      }

data RoutingError
  = HTTPError { status :: Status
              , message :: Maybe String
              }

type Handler r = Either RoutingError r

derive instance genericRoutingError :: Generic RoutingError _

instance eqRoutingError :: Eq RoutingError where
  eq = genericEq

instance showRoutingError :: Show RoutingError where
  show = genericShow

class Router e h r | e -> h, e -> r where
  route :: Proxy e -> RoutingContext -> h -> Handler r

orHandler :: forall r. Handler r -> Handler r -> Handler r
orHandler h1 h2 =
  case h1 of
    Left err1 ->
      case h2 of
        -- The Error that's thrown depends on the errors' HTTP codes.
        Left err2 -> throwError (selectError err1 err2)
        Right handler -> pure handler
    Right handler -> pure handler
  where
    fallbackStatuses = [statusNotFound, statusMethodNotAllowed]
    selectError (HTTPError errL) (HTTPError errR) =
      case Tuple errL.status errR.status of
        Tuple  s1 s2
          | s1 `elem` fallbackStatuses && s2 == statusNotFound -> HTTPError errL
          | s1 /= statusNotFound && s2 `elem` fallbackStatuses -> HTTPError errL
          | otherwise -> HTTPError errR

instance routerAltNamed :: ( Router t1 h1 out
                           , Router t2 (Record h2) out
                           , IsSymbol name
                           , Cons name h1 h2 hs
                           )
                           => Router (name := t1 :<|> t2) (Record hs) out where
  route _ context handlers =
    route (Proxy :: Proxy t1) context (Record.get name handlers)
    `orHandler`
    route (Proxy :: Proxy t2) context (Record.delete name handlers)
    where
      name = SProxy :: SProxy name

instance routerNamed :: ( Router t h out
                        , IsSymbol name
                        , Cons name h () hs
                        )
                        => Router (name := t) (Record hs) out where
  route _ context handlers =
    route (Proxy :: Proxy t) context (Record.get (SProxy :: SProxy name) handlers)

instance routerLit :: ( Router e h out
                      , IsSymbol lit
                      )
                      => Router (Lit lit :> e) h out where
  route _ ctx r =
    case uncons ctx.path of
      Just { head, tail } | head == expectedSegment ->
        route (Proxy :: Proxy e) ctx { path = tail} r
      Just _ -> throwError (HTTPError { status: statusNotFound
                                      , message: Nothing
                                      })
      Nothing -> throwError (HTTPError { status: statusNotFound
                                       , message: Nothing
                                       })
    where expectedSegment = reflectSymbol (SProxy :: SProxy lit)

instance routerCapture :: ( Router e h out
                          , FromPathPiece v
                          )
                          => Router (Capture c v :> e) (v -> h) out where
  route _ ctx r =
    case uncons ctx.path of
      Nothing -> throwError (HTTPError { status: statusNotFound
                                       , message: Nothing
                                       })
      Just { head, tail } ->
        case fromPathPiece head of
          Left err -> throwError (HTTPError { status: statusBadRequest
                                            , message: Just err
                                            })
          Right x -> route (Proxy :: Proxy e) ctx { path = tail } (r x)


instance routerCaptureAll :: ( Router e h out
                             , FromPathPiece v
                             )
                             => Router (CaptureAll c v :> e) (Array v -> h) out where
  route _ ctx r =
    case traverse fromPathPiece ctx.path of
      Left err -> throwError (HTTPError { status: statusBadRequest
                                        , message: Just err
                                        })
      Right xs -> route (Proxy :: Proxy e) ctx { path = [] } (r xs)


instance routerQueryParam :: ( IsSymbol k
                             , Router e h out
                             , FromPathPiece t
                             )
                          => Router (QueryParam k t :> e) (Maybe t -> h) out where
  route _ ctx r =
    let k = reflectSymbol (SProxy :: SProxy k)
        v = map (fromMaybe "") $ lookup k $ ctx.query in
    case fromPathPiece <$> v of
      Nothing -> go Nothing
      Just (Right v') -> go (Just v')
      Just (Left err) -> throwError (HTTPError { status: statusBadRequest
                                               , message: Just err
                                               })
    where go = route (Proxy :: Proxy e) ctx <<< r


instance routerQueryParams :: ( IsSymbol k
                              , Router e h out
                              , FromPathPiece t
                              )
                           => Router (QueryParams k t :> e) (Array t -> h) out where
  route _ ctx r =
    let k = reflectSymbol (SProxy :: SProxy k)
        v = map (fromMaybe "" <<< snd) $ filter ((==) k <<< fst) $ ctx.query in
    case traverse fromPathPiece v of
      Right v' -> go v'
      Left err -> throwError (HTTPError { status: statusBadRequest
                                        , message: Just err
                                        })
    where go = route (Proxy :: Proxy e) ctx <<< r


routeEndpoint :: forall e r method
                  . IsSymbol method
                 => Proxy e
                 -> RoutingContext
                 -> r
                 -> SProxy method
                 -> Either RoutingError r
routeEndpoint _ context r methodProxy = do
  unless (null context.path) $
    throwError (HTTPError { status: statusNotFound
                          , message: Nothing
                          })

  let expectedMethod = Method.fromString (reflectSymbol methodProxy)
  unless (expectedMethod == context.method) $
    throwError (HTTPError { status: statusMethodNotAllowed
                          , message: Just ("Method "
                                           <> show context.method
                                           <> " did not match "
                                           <> show expectedMethod
                                           <> ".")
                          })
  pure r

getAccept :: Object String -> Either String (Maybe AcceptHeader)
getAccept m =
  case F.lookup "accept" m of
    Just a -> Just <$> parseAcceptHeader a
    Nothing -> pure Nothing

instance routerAltMethod :: ( IsSymbol method
                            , Router (Trout.Method method reqBody body ct) (Record hs) out
                            , Router methods (Record hs) out
                            )
                        => Router
                           (Trout.Method method reqBody body ct :<|> methods)
                           (Record hs)
                           out
  where
  route _ context handlers =
    route (Proxy :: Proxy (Trout.Method method reqBody body ct)) context handlers
    `orHandler`
    route (Proxy :: Proxy methods) context handlers

instance routerMethodWithoutRequestBody :: ( Monad m
                         , Request req m
                         , Response res m r
                         , ResponseWritable r m b
                         , IsSymbol method
                         , AllMimeRender body ct b
                         , Cons method (ExceptT RoutingError m body) hs' hs
                         )
                     => Router
                        (Trout.Method method Void body ct)
                        (Record hs)
                        (Middleware
                        m
                        { request :: req, response :: (res StatusLineOpen), components :: c}
                        { request :: req, response :: (res StatusLineOpen), components :: c}
                        (Either RoutingError (Tuple MediaType b)))
  where
  route proxy context handlers = do
    let handler = lift' (runExceptT (Record.get (SProxy :: SProxy method) handlers))
          `ibind` makeResponse (Proxy :: Proxy b) (Proxy :: Proxy ct)
    routeEndpoint proxy context handler (SProxy :: SProxy method)
    where bind = ibind

-- NOTE: Current implementation doesn't read Content-Type request header.
--       We actually should change ctReq according to that request header.
instance routerMethodWithRequestBody :: ( Monad m
                         , Request req m
                         , ReadableBody req m reqBody
                         , Response res m r
                         , ResponseWritable r m b
                         , IsSymbol method
                         , MimeParse reqBody ctReq reqBodyParsed
                         , AllMimeRender body ct b
                         , Cons method ( reqBodyParsed -> ExceptT RoutingError m body) hs' hs
                         )
                     => Router
                        (Trout.Method method (ReqBody reqBody ctReq) body ct)
                        (Record hs)
                        (Middleware
                        m
                        { request :: req, response :: (res StatusLineOpen), components :: c}
                        { request :: req, response :: (res StatusLineOpen), components :: c}
                        (Either RoutingError (Tuple MediaType b)))
  where
  route proxy context handlers = do
    let handler = do
          reqBody <- readBody
          r <- lift' $ runExceptT do
            case mimeParse (Proxy :: Proxy ctReq) reqBody of
                Right reqBodyParsed -> Record.get (SProxy :: SProxy method) handlers reqBodyParsed
                Left emsg -> do
                  let status = Status { code: 422, reasonPhrase: "Unprocessable Entity" }
                  throwError (HTTPError { status, message: Just emsg })
          makeResponse (Proxy :: Proxy b) (Proxy :: Proxy ct) r
    routeEndpoint proxy context handler (SProxy :: SProxy method)
    where bind = ibind

makeResponse
  :: forall m req res r body ct b c
   . Monad m
  => Request req m
  => Response res m r
  => ResponseWritable r m b
  => AllMimeRender body ct b
  => Proxy b
  -> Proxy ct
  -> Handler body
  -> (Middleware
        m
        { request :: req, response :: (res StatusLineOpen), components :: c} 
        { request :: req, response :: (res StatusLineOpen), components :: c}
        (Handler (Tuple MediaType b)))
makeResponse _ _ r =
  case r of
    Left err -> ipure $ Left err
    Right body -> do
      { headers } <- getRequestData
      case getAccept headers of
          Left _ -> ipure $ Left $ HTTPError { status: statusBadRequest, message: Nothing }
          Right parsedAccept ->
            case negotiateContent parsedAccept (allMimeRender (Proxy :: Proxy ct) body) of
              Match negotiationResult -> ipure $ Right negotiationResult
              Default negotiationResult -> ipure $ Right negotiationResult
              NotAcceptable _ -> ipure $ Left $ HTTPError { status: statusNotAcceptable, message: Nothing }
  where bind = ibind

instance routerRaw :: IsSymbol method
                   => Router
                      (Raw method)
                      (Middleware
                       m
                       { request :: req
                       , response :: (res StatusLineOpen)
                       , components :: c
                       }
                       { request :: req
                       , response :: (res ResponseEnded)
                       , components :: c
                       }
                       Unit)
                      (Middleware
                       m
                       { request :: req
                       , response :: (res StatusLineOpen)
                       , components :: c
                       }
                       { request :: req
                       , response :: (res ResponseEnded)
                       , components :: c
                       }
                       Unit)
                      where
  route proxy context r =
    routeEndpoint proxy context r (SProxy :: SProxy method)


instance routerResource :: ( Router methods h out
                           )
                        => Router (Trout.Resource methods) h out where
  route proxy = route (Proxy :: Proxy methods)


router
  :: forall s r resources m req res c b
   . Monad m
  => Request req m
  => Response res m r
  => ResponseWritable r m b
  => Router s resources (Middleware
                         m
                         (Conn req (res StatusLineOpen) c)
                         (Conn req (res StatusLineOpen) c)
                         (Either RoutingError (Tuple MediaType b)))
  => Proxy s
  -> resources
  -> (Status
      -> Maybe String
      -> Middleware
         m
         (Conn req (res StatusLineOpen) c)
         (Conn req (res ResponseEnded) c)
         Unit)
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
router site handler onRoutingError = do
  handler'
  -- Run the routing to get a handler.
  -- route (Proxy :: Proxy s) ctx handler
  -- Then, if successful, run the handler, possibly also generating an HTTPError.
  -- # either catch runHandler
  where
    context { parsedUrl, method } =
      let parsedUrl' = force parsedUrl in
      { path: parsedUrl'.path
      , query: either (const []) identity parsedUrl'.query
      , method: method
      }
    catch (HTTPError { status, message }) =
      onRoutingError status message

    handler' ∷ Middleware
               m
               (Conn req (res StatusLineOpen) c)
               (Conn req (res ResponseEnded) c)
               Unit
    handler' = do
      ctx <- context <$> getRequestData
      case route site ctx handler of
        Left err → catch err
        Right h → do
          eres <- h
          case eres of
              Left err → catch err
              Right (Tuple ct rendered) → do 
                writeStatus statusOK
                contentType ct
                closeHeaders
                respond rendered

    bind = ibind
    discard = idiscard
