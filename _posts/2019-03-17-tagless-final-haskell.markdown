---
layout: post
title: "Tagless Final Encoding in Haskell"
author: Juan Pablo Royo Sales
date: 2019-03-17
excerpt: How to implement tagless final encoding in Haskell and making a testable program using Type Application
categories: [haskell, fp, functional, tagless, final, encoding, type, level, programming]
tags: [haskell, fp, functional, tagless, final, encoding, type, level, programming]
permalink: /posts/2019-03-17-tagless-final-haskell.html
---

## Source Code

You can find source code of the example described in this post [here](https://gist.github.com/jproyo/7127418371a6d6254ff2208bf26c0315)

## Introduction

In this post i am going to explore a simple technique for organizing our programs which is called **Tagless Final Encoding** to write testable programs in Haskell. I am also use **TypeApplication LANGUAGE** directive to write more readable and flexible test.

## Why Tagless Final?

Nowadays in Haskell Community there is an open discussion about using [Free Monads](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html), [mtl](http://hackage.haskell.org/package/mtl) or **Tagless Final Encoding** to write internal **DSL** (Domain Specific Language) for representing our programs in a descriptive and Functional way.

In my personal opinion i think all of these tools, theories and techniques are suitable to do it but depends on the context of the person, team or solution you are writing to decide which is more useful.

For example:

- **Free Monad**: I think it is great to have tools that are based on ***Category Theory*** concepts such as **Free Applicative**, **Free Monad** and so on. There is a great paper about this <cite>[Notions of Computation as Monoid][1]</cite>. In that sense **Free Monad** not only help us to describe our programs but also to have certain Math Properties in our toolbox to manipulate them. Although i have never benchmarked any of the **Free Monad** implementations out there, i know there are complains about their performance in the community. Beyond this i think for beginners it is a little difficult to implement.

- **Monad Transformers (mtl)**: Also great tool, based on simple **Monad** concept which is easier to understand for beginners and without performance penalties if you are using carefully. It is also the most used tool for dealing with different **Monads** in a single program from the beginning of Haskell. The only disadvantage i could pointed out is that is less readable and understandable in the code when you are stacking more than 3 Monads. Also a drawback for beginners.

- **Tagless Final**: You only need to define and implement **Typeclasses**. In the original post i have wrongly mentioned *"it is a technique not based on any paper or Math Theory"*, but thank you to [p-alik](https://www.reddit.com/user/p-alik) who pointed me out in reddit channel that there is paper for this here <cite>[Typed final (tagless-final) style][2]</cite>. The main advantage for me is it is beginner friendly, readable, easy to understand, test and extend.


[1]:https://www.fceia.unr.edu.ar/~mauro/pubs/Notions_of_Computation_as_Monoids.pdf
[2]:http://okmij.org/ftp/tagless-final/index.html#course-oxford

Having said that, I would like to talk about **Tagless Final** as an approach for Haskell beginners in order to help them to organize and describe programs; make them extensible and testable.

## What is Tagless Final Encoding?

**Tagless Final Encoding** is a technique for embedding a **DSL** (Domain Specific Language) in a **Functional Programming Language**. We need to define a ***Language*** for using it and an **Interpreter** to indicate how it should behave on each defined term. For this purpose we are going to use **Typeclasses**.

To sum up in **Tagless Final Encoding** style there are:

- **Typeclasses**: Set of operations over a Type.

- **Interpreter**: Instances of those Typeclasses for each specific Type

## Tagless Final Encoding in practice

We are going to build a basic program which request some user data. The program is going to do the following:

- First try to recover the data from a cache
- If data is found it is returned
- If there is no data in the cache, search user data in source repository and update cache

{% highlight haskell %}
type UserName = String

data DataResult = DataResult String
  deriving (Eq, Show)

requestData :: Monad m => UserName -> m [DataResult]
requestData userName = do
 cache  <- getFromCache userName
 result <- case cache of
   Just dataResult -> return dataResult
   Nothing         -> getFromSource userName
 storeCache result
 return result
{% endhighlight %}

Here it is our basic program which implements what we described above, but obviously this code doesn't work because we need to define functions such as `getFromCache`, `getFromSource` and `storeCache`.

For defining that we are going to use **Typeclasses** as we mentioned, in order to represent our program capabilities.

{% highlight haskell %}
class Monad m => Cache m where
  getFromCache :: String -> m (Maybe [DataResult])
  storeCache :: [DataResult] -> m ()

class Monad m => DataSource m where
  getFromSource :: String -> m [DataResult]

{% endhighlight %}

Why are we defining `Cache` and `DataSource` **Typeclasses** as `Monad` also? Basically because we want to combine and chain our **DSL** terms in a single program.

But we still need to change our program definition since we are constraining only on `Monad` and we want to use `Cache` and `DataSource` terms from the implicit context.

{% highlight haskell %}
requestData :: (Cache m, DataSource m) => UserName -> m [DataResult]
requestData userName = do
 cache  <- getFromCache userName
 result <- case cache of
   Just dataResult -> return dataResult
   Nothing         -> getFromSource userName
 storeCache result
 return result
{% endhighlight %}

Notice that we don't need anymore **`Monad`** Constraint in our signature because both `Cache` and `DataSource` are `Monad`s also.

The only thing left is to write our **Instances** to provide some implementation. We are going to provide a fake implementation for `IO`.

{% highlight haskell %}
instance Cache IO where
  getFromCache _ = return Nothing
  storeCache _ = return ()

instance DataSource IO where
  getFromSource user = return $ [DataResult $ "source: " <> user]
{% endhighlight %}

If we run our program from **ghci** we are going to see it is working:

{% highlight haskell %}
λx.x> import Data

λx.x> requestData "john"
[DataResult "source: john"]
{% endhighlight %}

## Provide and Test with different implementations using Type Application

One of the things I have announced on the beginning of this post is i am going to show how easy it is to test our programs using this technique combined with **TypeApplication** LANGUAGE extension. This combination enable us not only to test, but also to provide and interchange different instances of our **Typeclasses** in a straightforward way.

### Instances

In order to provide different instances of `Cache` and `DataSource`, and play around with different cases, for example when data is in cache or not, i am going to wrappe `IO` type in different `newtype` representations.


{% highlight haskell %}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

newtype NotInCache a = NotInCache { unNoCache :: IO a }
  deriving (Monad, Applicative, Functor)

instance Cache NotInCache where
  getFromCache _ = NotInCache $ return Nothing
  storeCache _ = NotInCache $ return ()

instance DataSource NotInCache where
  getFromSource user = return $ [DataResult $ "source: " <> user]

{% endhighlight %}

For the first instance we need to do enable **GeneralisedNewtypeDeriving** extension to allow us deriving `Functor`, `Monad` and `Applicative` because our **Typeclasses** `Cache` and `DataSource` are also `Monad` and we need to provide implementations of those **Typeclasses** for our custom type `NotInCache`

Now if we are trying to run this in **ghci** we are getting the following error:

{% highlight haskell %}
λx.x> requestData "john"

<interactive>:5:1: error:
    • Ambiguous type variable ‘m0’ arising from a use of ‘print’
      prevents the constraint ‘(Show
                                  (m0 [DataResult]))’ from being solved.
      Probable fix: use a type annotation to specify what ‘m0’ should be.
      These potential instances exist:
        instance (Show a, Show b) => Show (Either a b)
          -- Defined in ‘Data.Either’
        instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
        instance (Show a, Show b) => Show (a, b) -- Defined in ‘GHC.Show’
        ...plus 14 others
        ...plus 89 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of an interactive GHCi command: print it
{% endhighlight %}

Basically the compiler is saying us that it cannot find an unambiguous instance to use for our program. But also as the compiler is pointed out we can use **TypeApplication** extension to tell the compiler what instance should use and provide an explicit evidence of that.

{% highlight haskell %}
λx.x> :set -XTypeApplications

λx.x> :t requestData "john"
requestData "john"
  :: (Data.Cache m, DataSource m, Data.Logging m) => m [DataResult]
λx.x> :t requestData @NotInCache "john"
requestData @NotInCache "john" :: NotInCache [DataResult]

{% endhighlight %}

Here we've enabled extension and after that we are running our program with `NotInCache` type. Notice that now we need to call `unNoCache` to unwrap our underlying `IO` and effectively running in our **ghci** `IO` loop.

{% highlight haskell %}
x.x> unNoCache $ requestData "john"
[DataResult "source: john"]
{% endhighlight %}

We can also do it from our **.hs** file.

{% highlight haskell %}
main :: IO ()
main = (unNoCache $ requestData "john") >>= (putStrLn . show)
{% endhighlight %}

Now we are ready for different instances!!!

{% highlight haskell %}
newtype InCache a = InCache { unInCache :: IO a }
  deriving (Monad, Applicative, Functor)

instance Cache InCache where
  getFromCache user = InCache $ return $ Just [DataResult $ "cache: " <> user]
  storeCache _ = InCache $ return ()

instance DataSource InCache where
  getFromSource _ = undefined

main :: IO ()
main = do
  (unNoCache $ requestData "john") >>= (putStrLn . show)
  (unInCache $ requestData "john") >>= (putStrLn . show)
{% endhighlight %}

The outputs now look like this:

{% highlight haskell %}
λx.x> Data.main
[DataResult "source: john"]
[DataResult "cache: john"]
{% endhighlight %}

## Extensibility

One of the important aspects of **Tagless Final Encoding** is its extensibility property. It is extensible in 2 dimensions:

- **Vertical Extensibility**: It is what we have just done adding different implementations for the same **Typeclasses** without altering our program.

- **Horizontal Extensibility**: It is adding new capabilities to the program in order to extend some functionality inside it.

### Horizontal Extensibility

Our program capabilities beyond `Monad`, `Functor` and `Applicative` **Typeclasses** are `Cache` and `DataSource`. If we are saying that it is *Horizontal Extensible* we can add more capabilities apart from those mentioned. For example what about `Logging`?.

Let do it with our example:

{% highlight haskell %}

class Monad m => Logging m where
  logMsg :: String -> m ()

requestData :: (Cache m, DataSource m, Logging m) => UserName -> m [DataResult]
requestData userName = do
 cache  <- getFromCache userName
 result <- case cache of
   Just dataResult -> return dataResult
   Nothing         -> getFromSource userName
 storeCache result
 logMsg $ "Result data for user: " <> userName <> " - data: " <> show result
 return result

{% endhighlight %}

And now providing instances for `Logging`

{% highlight haskell %}
instance Logging NotInCache where
  logMsg = NotInCache . putStrLn

instance Logging InCache where
  logMsg = InCache . putStrLn

{% endhighlight %}

If we run the program we obtain the following:

{% highlight haskell %}
λx.x> Data.main
Result data for user: john - data: [DataResult "source: john"]
[DataResult "source: john"]
Result data for user: john - data: [DataResult "cache: john"]
[DataResult "cache: john"]
{% endhighlight %}

### Conclusion

As we can see, **Tagless Final Encoding** is a pretty good technique to build testable and extensible programs.

We have also demonstrated how easy is to interchange and provide different instances using **TypeApplication** extension.
