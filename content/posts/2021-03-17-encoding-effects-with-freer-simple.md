+++
title = "Encoding Effects using freer-simple"
date = 2021-03-17

[taxonomies]
tags = ["haskell", "fp", "functional", "type", "level", "programming", "effects", "effects system", "effects encoding", "effect", "encoding", "type level programming", "types"]

+++

## Introduction

In previous posts I have [written](/posts/2019-03-17-tagless-final-haskell.html) about how encode **Effects** (e.g. IO, Database access and Caching) using a **Tagless Final** approach.
On that article, the idea was to show how to organize a program which can have different *Effects* and compose and combine them in a single program. I've also explored the *technique* of **Tagless Final** for representing the encoding and [**mtl**](https://hackage.haskell.org/package/mtl) in particular to *handle or interpret* effects.

## Context

The majority of Industry Software Solutions nowadays cannot escape to be connected to an external Service or System. Although there are some cases where our Solutions does not need connecting to an external service, the program still needs some kind of monitoring, metrics and logs before being shipped into a production environment. Taking these into consideration, it seems inevitable to deal with side effects in our language. Therefore **Effectful computations** or **Algebraic Effect Handlers** is a very interesting and important topic for me, because it encodes **Effects** as a ***first-class*** citizen in our *FP Programs*.

Everyone of us, as **Functional Programming Developer** want to deal only with *pure* functions and we don't want to deal with *side effects*. But *pure FP* can happen only on small portions of our software solutions in the Industry and at the end we need to deal with *side effects*.

As I've explained in my previous article there are several ways to encode **Effects** in order to write our program with ***pure functions*** and write the interpretation or ***side effects computations*** to specific *Effect* in a decouple way. In such a way we end up with a **Pure Program** that could be run or interpret later with **Side effects computations**. This **powerful abstraction** allow us to *reason* about our programs as we do with *pure functions* but at the same time having the ability to run it in other *non-pure* contexts.

After trying different approach and libraries for encoding and handling **effects** in production systems, I would like to explain my experience using [*freer-simple*](https://hackage.haskell.org/package/freer-simple) which is one of my favourite and also the one that I am currently using.


## freer-simple

[*freer-simple*](https://hackage.haskell.org/package/freer-simple) is an **Effect System** library based on some of the famous papers of <cite>Oleg Kiselyov et al.[1]</cite> about **Free Monads** and **Extensible Effects**. The features that are pointed out from the library doc it self are:

- Efficient Effect system
- Some implementations of common MTL Monads
- Components for defining Custom Effects

One key feature that is not explicitly mentioned on the library doc is that **Effect** encoding is done at **Type level** using and *Extensible Open Union Type* (a type-indexed coproduct of functors) as it is clearly explain [here](http://okmij.org/ftp/Haskell/extensible/exteff.pdf). This means that the unpeel of the different *Effectful* layers are being done in compile time and at the same time it is constant with respect to the size of the Union.

Another important aspect of the implementation of the library is that it used a single **Monad** which is `Eff r a`, avoiding the need of stacking multiple Monad scaping to the quadratic $$O(n^2)$$ instance definitions problem in **Monad Transformers**. The common question that might arise here is: How the **effects** are handle if they are not stacked and are orthogonal? The answer is using **Coroutines**: *"a computation sends a request and suspends, waiting for a reply; a handler waits for a request, handles what it can, and resumes the client"*[2]. This is implemented with Continuation Monad.


[1]:http://okmij.org/ftp/Haskell/extensible/more.pdf
[2]:http://okmij.org/ftp/Haskell/extensible/exteff.pdf

Lets see some examples on how to use it. For that propose I am going to use a real example that I had to do but i am going to change some names in order to preserve privacy and details of the solution behind.

## freer-simple by Example

### Use Case

The use case was the following:

```shell
As an user I want to upload some personal documents
to the system in order to complete my identification process.
```
This is also known as ***KYC (Known your Customer)*** process. So, lets assume that we need to run this kind of process and identify the user, asking for some personal documentation to verify them. In that case we need to be able to provide some mean to the user that is already registered in our System to allow them upload those documents.

Lets assume **w.l.o.g.** that our Solution is going to use **AWS S3** as a Storage Provider for the documentation and **PostgreSQL** as a Relational Database for storing related data to that user and document. We are going to need that DB in order to store the **reference or document path to S3** in order to be able to recover that specific document later if it is requested by the user or by other means.

On the other hand we are going to expose this capability as a *REST API* endpoint and we are going to use [**servant**](https://hackage.haskell.org/package/servant) for that matter.

### Program

First lets create the main algorithm of our program that should do the following:

1. Received the Requested Document `ByteString` with some metadata associated in order to identify the user
2. If the user does not exist in our system (Not registered yet) throw an error.
3. If the user is registered, upload the document to S3
4. If the upload was successful, store the S3 reference to our DB
5. Return this reference to the user

In Haskell lingua this should be like this

```haskell
import           Control.Monad.Freer.Error as FE

uploadDocument :: Members '[DocStorage, DataAccess, Logging, AppError] eff
               => Document
               -> Eff eff DocRef
uploadDocument doc =
  ifM
    (userExists (doc^.document.userId))
    ( do
       ref <- uploadDoc doc
       logInfo ("Document was uploaded in " <> show ref)
       setDocumentUploaded doc ref
       return ref
    )
    $ FE.throwError USER_NOT_FOUND

```

Lets start analyzing this code. `Members '[DocStorage, DataAccess, Logging, AppError] eff` *Constraint* is indexed by an **Open Union Type** `[DocStorage, DataAccess, Logging, AppError]` whose *Coproduct* indicates what are the possible effects that `eff` can encode into. This is telling to the compiler that our `Eff eff a` are *constraining* by this **Open Union Type** and we can interleave any of those *Effects* throughout all the computation, without needing any lifting because we are not stacking *Monads* as in **Monad Transformer** approach.

We need to see now all the Effects that are involved in the **Open Union Type** which in this case are: `[DocStorage, DataAccess, Logging, AppError]`.

### Effects

In **freer-simple** library there is a tool set for defining *Custom Effects* as well as some already provided common effects that can be found in **Monad Transformer** libraries like **mtl**. In my example I am using both kind of effects, already built in effects that are packed with it *Handlers* and *Custom* Effects that we need to build and provider the specific *Handler* for them.

- *Custom* effects in my example are: `DocStorage`, `DataAccess` and `Logging`. Last one could have been implemented using `Writer` effect provided by the library but I prefer something custom in order to interpret that effect using [co-log](https://hackage.haskell.org/package/co-log) library.

- *Built-in* effect in this example is `AppError`

#### Custom Effects

In Custom Effects you need to provide the definition of the **GADT** which describes the Algebra of your Effect and the function that introduces or `send` that Algebra to the `Eff r a` *Monad* whose indexed **Open Union Type** is contained in. With this function we are injecting our *Algebra* into that index list.

> NOTE: It is important to point out that **freer-simple** provides [Template Haskell](https://wiki.haskell.org/Template_Haskell) tooling for auto-generate the `send` injecting
functions for us, but I've preferred to be a little more boilerplate for educational purpose.

#### DocStorage Effect
```haskell
import Control.Monad.Freer as F

data DocStorage r where
  UploadDocument  :: Document -> DocStorage DocRef

uploadDoc :: Member DocStorage effs => Document -> Eff effs DocRef
uploadDoc = F.send . UploadDocument
```
#### DataAccess Effect
```haskell
import Control.Monad.Freer as F

data DataAccess r where
  UserExists          :: UserId -> DataAccess Bool
  SetDocumentUploaded :: Document -> DocRef -> DataAccess ()

userExists :: Member DataAccess effs => UserId -> Eff effs Bool
userExists = F.send . UserExists

setDocumentUploaded :: Member DataAccess effs => Document -> DocRef -> Eff effs ()
setDocumentUploaded doc = F.send . SetDocumentUploaded doc
```
#### Logging Effect
```haskell
import Control.Monad.Freer as F

data Logging r where
  LogInfo   :: Text -> Logging ()
  LogError  :: Text -> Logging ()

logInfo :: Member Logging effs => Text -> Eff effs ()
logInfo = F.send . LogInfo

logError :: Member Logging effs => Text -> Eff effs ()
logError = F.send . LogError
```
#### Built-In Effects
In the case of *built-in* effects the effort is minimum because the Effect and handler is already provided by the library and we only need to define some aliases, but only if we want as in my case.

```haskell
{-# LANGUAGE PatternSynonyms #-}

import Control.Monad.Freer.Error

data Err
  = NotFound Text
  | InternalError Text
  | Forbidden Text
  | BadRequest Text
  deriving (Eq, Show)

pattern ERROR_UPLOADING_DOCUMENT
      , DOCUMENT_NOT_FOUND
      , USER_NOT_FOUND
     :: Err
pattern ERROR_UPLOADING_DOCUMENT   = InternalError "Document uploading error"
pattern DOCUMENT_NOT_FOUND         = NotFound      "Document was not found on the Storage"
pattern USER_NOT_FOUND             = NotFound      "User was not found on the system"

type AppError = Error Err
```
In this case the library already provide a **Higher Order Type** which is `newtype Error e r` which Value Constructor is `Error :: e -> Error e r`, therefore its kind is `* -> * -> *`. Our alias is `type AppError = Error Err` whose kind is `* -> *` which coincide with the Kind that is contain by the Indexed **Open Union Type**


```shell
λx.x> import Control.Monad.Freer
λx.x> :k Eff
Eff :: [* -> *] -> * -> *
λx.x>
```
### Handlers or Interpreters
The next step is to provide some interpretation for those effects, we can have either *pure* interpretations which is going to be mainly used for Testing where we need to be deterministic, or **side effects** interpretations which is going to be connecting to Database, APIs, etc.

Lets see some example of *side effects* interpretations for this effects presented above.

#### Logging Interpreter

```haskell

import Colog as C
import Control.Monad.Freer as F
import Servant

runColog :: forall effs a. (LastMember Handler effs)
             => Eff (Logging ': effs) a
             -> Eff effs a
runColog =
    interpretM $
        \case
            LogInfo  msg -> liftIO $ withColog I msg
            LogError msg -> liftIO $ withColog E msg

withColog :: Severity -> Text -> IO ()
withColog s = usingLoggerT (cmap fmtMessage logTextStdout) . C.log s
```
The important thing to point out here is that in `runColog` interpretation we are receiving the Effect that is being waited by this Handler, as we explain in the [introduction to the library](#freer-simple). Because of this, the *Effect* that is the first parameter passed to this interpreter is only indexed by `Logging` Algebra plus some possible rest of Algebras. This is Sum Type is indicated by the use of *Cons* `(':)` at Type level, finally returning that possible rest of effects without the `Logging` Algebra inside. This is a great and powerful semantic because we are stating by its types that we are taking of `Logging` Algebra and that is not going to be part anymore of the *Open Union* after this interpreter runs.

The `LastMember Handler effs` Constraint indicates that the last effect in the Stack is going to be `Handler`, since on top of this we are running a Servant API which is the last Monadic computation. This is not breaking any abstraction at all because this is the interpreter and not the Algebra, and we know that this interpreter is going to be use only by our *Servant* API. We can write other interpreters if we are in other context different that Servant.

#### Database Interpreter
```haskell

import Database.PostgreSQL.Simple as D
import Database.PostgreSQL.Simple.Transaction as T
import Database.PostgreSQL.Simple.SqlQQ

runPostgresql :: forall effs a. (LastMember Handler effs,  Members '[ AppError, Logging] effs)
              => Conf
              -> Eff (DataAccess ': effs) a
              -> Eff effs a
runPostgresql conf =
    interpret $
        \case
            UserExists userId -> callUserExists conf userId
            SetDocumentUploaded docToUpload docRef  -> callSetDocumentUploaded conf docToUpload docRef

callUserExists :: (LastMember Handler eff, Members '[AppError, Logging] eff)
                        => Conf -> Text -> Eff eff Bool
callUserExists conf userId = do
  logInfo "Calling for to check if user exist in the Database"
  conn <- liftIO $ connectPostgreSQL (conf^.dbConnection . L.to encodeUtf8)
  [Only i] <- liftIO $ D.query
                        conn
                        [sql|
                            SELECT 1
                            FROM users
                            where user_id = ?'
                        |]
                        userId
   return $ (i == 1)

-- callSetDocumentUploaded is similar but with an D.execute statement

```
> NOTE: Some details of the implementation is not provided here.

Important thing here to remark is that we can interleave other *Effects* in the stack in this interpretation without needing any lifting or complex process. We are just saying that in the same context we have other effects like `AppError` or `Logging` and we can use those Algebras that are going to be interpret at some point in time.

Another important aspect is that `Conf` object can be easily introduce with a `Reader` that is already provided by **freer-simple** and we can have something implicit instead of explicit. I wanted to leave it as explicit just for the purpose of the example, but in practice I am using it with `Reader` and it looks like this:

```haskell

import Control.Monad.Freer.Reader as RE
import Database.PostgreSQL.Simple as D
import Database.PostgreSQL.Simple.Transaction as T
import Database.PostgreSQL.Simple.SqlQQ

runPostgresql :: forall effs a. (LastMember Handler effs,  Members '[ Conf, AppError, Logging] effs)
              => Eff (DataAccess ': effs) a
              -> Eff effs a
runPostgresql conf =
    interpret $
        \case
            UserExists userId -> callUserExists userId
            SetDocumentUploaded docToUpload docRef  -> callSetDocumentUploaded docToUpload docRef

callUserExists :: (LastMember Handler eff, Members '[Conf, AppError, Logging] eff)
                        => Text -> Eff eff Bool
callUserExists userId = do
  conf <- RE.ask
  logInfo "Calling for to check if user exist in the Database"
  conn <- liftIO $ connectPostgreSQL (conf^.dbConnection . L.to encodeUtf8)
  [Only i] <- liftIO $ D.query
                        conn
                        [sql|
                            SELECT 1
                            FROM users
                            where user_id = ?'
                        |]
                        userId
   return $ (i == 1)

-- callSetDocumentUploaded is similar but with an D.execute statement

```

#### AWS S3 Interpreter

```haskell
import Control.Lens as L
import Control.Monad.Trans.AWS as A
import Control.Monad.Catch as MC
import Network.AWS.S3

runS3Storage :: forall effs a. (LastMember S.Handler effs,  Members '[ AppError, Logging, S.Handler] effs)
             => Conf
             -> Eff (DocStorage ': effs) a
             -> Eff effs a
runS3Storage conf =
    interpret $
        \case
            UploadDocument doc -> uploadFile conf doc

uploadFile :: (LastMember S.Handler eff, Members '[AppError, Logging, S.Handler] eff)
           => Conf -> Document -> Eff eff DocRef
uploadFile conf doc = do
    env <- getEnv conf
    let bucketName = conf^.bucketName
    let key = objectKey conf doc
    result <- liftIO . MC.try . runResourceT . runAWST env . A.send . putObject' bucketName key $ doc
    either toInternalError (const (key^._ObjectKey. L.to (pure . DocRef))) result

    where
        putObject' bucketName key doc' = putObject (BucketName bucketName) key (doc'^.dtuContent . L.to toBody)
                                         & set poContentType (doc'^.document^?contentType)


```
> NOTE: Some details of the implementation is not provided here.

### Run Interpreters

Finally we need to have the Continuation Monad running with all the Handlers waiting for their requests.

```haskell
runEffects :: Conf -> Eff '[ DocStorage, DataAccess, Logging, AppError, Handler ] a -> Handler a
runEffects conf = (either toServantError return =<<) .
                    runM .
                    runError .
                    runColog .
                    runPostgresql conf .
                    runS3Storage conf

toServantError :: MonadError ServerError m => Err -> m a
toServantError (BadRequest msg)    = throwError err400 { errBody = toSL msg }
toServantError (InternalError msg) = throwError err500 { errBody = toSL msg }
toServantError (NotFound msg)      = throwError err404 { errBody = toSL msg }
toServantError (Forbidden msg)     = throwError err403 { errBody = toSL msg }
```
Then we have our *Program* and our *Interpretation*, therefore it is a matter of composing them:

```haskell
run :: Conf -> Document -> Handler DocRef
run conf = runEffects conf . uploadDocument
```
## Test

As we are going to see now, implementing **Mocks** and Testing our *Effectful Programs* in a deterministic manner is straightforward and easy.

```haskell

data Boundaries = Boundaries
  { _bUserExists      :: Bool
  , _bFailSetUploaded :: Bool
  , _bFailUpload      :: Bool
  , _bDownload        :: DownloadBoundary
  } deriving (Generic)

runTest :: Eff '[DocStorage, DataAccess, Logging, AppError, Identity] a -> Either Err a
runTest = runWithTest def

runWithTest :: Boundaries -> Eff '[DocStorage, DataAccess, Logging, AppError, Identity] a -> Either Err a
runWithTest boundaries = runIdentity . runM . runError . runTestColog . runPostgresqlTest boundaries . runS3StorageTest boundaries

runTestColog :: forall effs a. Eff (Logging ': effs) a
             -> Eff effs a
runTestColog req = do
    ((x, _), _) <- F.reinterpret2 go req & FS.runState [] & runWriter
    pure x
  where
    go :: Logging v
       -> Eff (FS.State [Text] ': FW.Writer [Text] ': effs) v
    go (LogInfo msg)  = tell ["INFO: "<>msg]
    go (LogError msg) = tell ["ERROR: "<>msg]

runPostgresqlTest :: Members '[ AppError ] effs => Boundaries -> Eff (DataAccess ': effs) a  -> Eff effs a
runPostgresqlTest Boundaries{..} =
    interpret $
        \case
            UserExists userId -> return _bUserExists
            SetDocumentUploaded _ _  -> when _bFailSetUploaded $ FE.throwError ERROR_UPLOADING_DOCUMENT


runS3StorageTest :: Members '[ AppError ] effs => Boundaries -> Eff (DocStorage ': effs) a -> Eff effs a
runS3StorageTest Boundaries{..} =
    interpret $
        \case
            UploadDocument _ -> if _bFailUpload
                                  then FE.throwError ERROR_UPLOADING_DOCUMENT
                                  else return $ DocRef "test_ref"

```
It is just a matter of implementing the right interpreters in the format that we want that allow us to run any test or combination we might need.

## Conclusions
**freer-simple** library is an *ergonomic* but not *easy* **Effect System** library that is based on Academic Research Papers and provide a Robust and Extensible way to build *Effectful Programs*.

On the other hand we have seen the flexibility this tooling has providing us with some built-in *effects* like `Reader`, `Writer`, `State` and so on.

Finally we have seen how we can quickly define *pure* interpreters or *Handlers* to escape from the *non-deterministic* context and be able to Test our programs. This interpretations are easy to built, avoiding the $$O(n^2)$$ problem on **mtl** instances without requiring **Monad** mock instances; and highly composable as well.

## Acknowledges

Thank you to [@monadplus](https://monadplus.pro/) who review this Article and help me to improve it.

## Appendix - Prerequisites for running code
The following are prerequisites for running examples here:

### GHC

- GHC 8.6.5 or above

### Language Extensions

```yaml
- DataKinds
- DeriveAnyClass
- DeriveGeneric
- DerivingStrategies
- DerivingVia
- GADTs
- GeneralizedNewtypeDeriving
- LambdaCase
- OverloadedStrings
- PartialTypeSignatures
- RecordWildCards
- Rank2Types
- RankNTypes
- ScopedTypeVariables
- TypeApplications
- TypeFamilies
- TypeOperators
```
### Dependencies

```yaml
- aeson
- amazonka
- amazonka-s3
- base
- base58-bytestring
- bytestring
- co-log
- conduit
- conduit-extra
- exceptions
- freer-simple
- lens
- text
```
And also extra deps for `servant`






