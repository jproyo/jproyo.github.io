---
layout: post
title: "Tagless Final in Scala: A Practical example"
author: Juan Pablo Royo Sales
date: 2019-02-07
categories: [scala, fp, functional, programming, tagless, final]
tags: [scala, fp, functional, programming, tagless, final]
excerpt: This is a practical example for beginners who want to understand better Tagless Final encoding in Scala.
---

## Source Code

To get a complete version of the source code presented here, go here [Github Repo](https://github.com/jproyo/imperative-to-fp/tree/demo-end).

> This post is based on **demo-end** branch and **NOT** in master

## Disclaimer

All the techniques and code that is going to be shown here are completely agnostic of any library. We are **not going** to use any **Scala FP** library such as **[cats](https://typelevel.org/cats/), [scalaz](https://scalaz.github.io/7/) or any other**. Obviously using any of those libraries could help us to implement these techniques without needing to write so much code, but the goal here is to shown how we can implement **Tagless Final Encoding** using only pure **Scala** language features.

## Introduction

This is my first blog post about Scala and i would like to describe a well known topic for the whole **Scala FP** community which is **Tagless Final Encoding**.

There are so many great code examples and blog post about the subject around there, but since there is always room to understand the technique from other perspective, I decided to take the chance and maybe help others to understand it in a more practical point of view.

I am going to deep dive in the technique with a real use case example and in the middle i will mix it with more theoretical concepts. This journey is going to start from an **imperative approach** until we get to a **Tagless Final FP approach**. So, fasten your seat belts and lets enjoy the ride!!!

## Problem example

I've created an example problem to be solved based on some things of my personal work, so everything in the example could be real production code. This is obviously a minimalist version of the problem.

We are going to describe a **Recommender Program** that given an **algorithm** and a **user** is going to generate recommendations for that **user** based on the selected **algorithm**. A **Recommender** system could be for example the most likely **recommendations** you received when visit any Marketplace such as *Amazon, Ebay, or any other* after searching for some products and navigate through these sites. For example: *"Since you have been searching for X cars, maybe you are interested in R, Y and Z also"*.

We have the following requirements defined in terms of **User Stories**:

- *As an user i want to get recommendations from an specific algorithm, but if there are no recommendations for this algorithm or i forgot to specify what algorithm should be use i would like to have default recommendations from the best algorithm the system has.*

- *As an user i want to get a message if recommendation's algorithm i requested is wrong.*

- *As an user i want to be able to be retrieve with a limited number of recommendations.*

## Imperative approach

{% highlight scala %}

  import DataSource._

  def getUser(userId: Option[Int]): Option[Int] =
    userId.filter(user => users.exists(_.userId == user))

  def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
    recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))


  def program(userId: Option[Int],
                recommenderId: Option[String] = None,
                limit: Option[Int] = None): Unit = {


      val user = getUser(userId)

      val algorithm = getAlgorithm(recommenderId)

      val result = algorithm.flatMap(_.run(UserId(user.get)))
                            .orElse(Some(emptyRecs(user.get)))

      val limitFilter = limit.getOrElse(limitDefault)

      val resultFiltered =
                result.map(_.copy(recs = recs.slice(0, limitFilter).toList))

      resultFiltered match {
        case Some(recs) => {
          println(s"\nRecommnedations for userId ${recs.userId}...")
          println(s"Algorithm ${algorithm.get.name}")
          println(s"Recs: ${recs.recs}")
        }
        case None => println(s"No recommendations found for userId $userId")
      }

  }

{% endhighlight %}

Although this imperative code version is fine, we can take the advantage of *for-comprehension* syntax sugar since we are manipulating `Option[+A]` type.

{% highlight scala %}

  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {

    val result = for {
      user           <- getUser(userId)
      algorithm      <- getAlgorithm(recommenderId)
      result         <- algorithm.run(UserId(user))
      limitFilter     = limit.getOrElse(limitDefault)
      resultFiltered  = result.copy(recs = recs.slice(0, limitFilter).toList)
    } yield Result(algorithm, resultFiltered)

    result match {
      case Some(algoRes) => {
        println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
        println(s"Algorithm ${algoRes.algorithm.name}")
        println(s"Recs: ${algoRes.recs.recs}")
      }
      case None => println(s"No recommendations found for userId $userId")
    }

  }

{% endhighlight %}

We can even go further and separate our program in 2 functions:

- `getRecommendations`: for-comprehension where the program logic takes place
- `printResults`: print results or errors to the user.

<a name="refactor-imperative"></a>
{% highlight scala %}

  def getRecommendations(userId: Option[Int],
                         recommenderId: Option[String],
                         limit: Option[Int]): Option[Result] = {
    val result = for {
      user           <- getUser(userId)
      algorithm      <- getAlgorithm(recommenderId)
      result         <- executeAlgorithm(user, algorithm)
      limitFilter     = limit.getOrElse(limitDefault)
      resultFiltered <- filterResults(result, limitFilter)
    } yield Result(algorithm, resultFiltered)
    result
  }


  def printResults(userId: Option[Int], result: Option[Result]): Unit = {
    result.fold(println(s"No recommendations found for userId $userId"))(algoRes => {
      println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
      println(s"Algorithm ${algoRes.algorithm.name}")
      println(s"Recs: ${algoRes.recs.recs}")
    })
  }

  private def getUser(userId: Option[Int]): Option[UserId] =
  userId.filter(user => users.exists(_.userId == user)).map(UserId)

  private def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
  recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))

  private def executeAlgorithm(user: UserId, algorithm: Algorithm): Option[UserRec] =
    algorithm.run(user)

  private def filterResults(result: UserRec, limitFilter: Int): Option[UserRec] =
    Some(result.copy(recs = recs.slice(0, limitFilter).toList))

{% endhighlight %}

Now we are done with our imperative code style and we can say it is in a good shape.

But we don't like imperative style, we love doing **Functional Programming** and applying all the Math science we have at our disposal, don't we? So, lets start to refactor our code with **Tagless Final Encoding** approach.

## Tagless Final Encoding

**Tagless Final Encoding** is a technique for embedding a **DSL** (Domain Specific Language) in a **Type Functional Language** such as Scala, Haskell, OCaml or other similar. Since we are embedding a DSL (a.k.a. Language) we are defining a *Language* in order to use its syntax and semantic **(Algebra)** in our program. We are defining also an *interpreter* of the Language to indicate how it should behave on each used term **(Interpreter)**.

> If you want to visit a more advanced approach of this, I would strongly recommend after this lecture to take a look on Free Monads.

We can say that in **Tagless Final** style there are:

- ***Algebra***s: Set of operations over a Structure. In a Programming language idiom could be a set of functions that operates over some Type.

- ***Interpreter***: The way of those operations behave according to an specific Type. In a Programming language idiom the implementation of those functions depending on the specific Type.

## Algebras

The first thing to do with this technique is to define our *Algebras*, our *operations* that are needed to solve our domain problem. If you already have some program in a good shape but with an imperative style approach like describe it [above](#imperative-approach), it is quite easy to define these operations because it is already provided by the semantic of the for-comprehension. In our case we have the following operations:

{% highlight scala %}

  def getUser(..)

  def getAlgorithm(..)

  def executeAlgorithm(..)

  def filter(...)

{% endhighlight %}

Now the only thing left to build our Algebra is to group those operation in different Algebras depending on the structure they are operating on:

- User: operations related to handle users
- Algorithm: operations related to handle algorithms
- Filter: operation for filtering results

Since our *Algebras* are only definition of our operations we are going to use *Trait* for defining this abstract representation.

{% highlight scala %}

  object algebras {

    import DataSource._

    trait UserRepo[F[_]] {
      def getUser(userId: Option[Int]): F[UserId]
    }

    object UserRepo {
      def apply[F[_]](implicit UserR: UserRepo[F]): UserRepo[F] = UserR
    }

    trait Filter[F[_]] {
      def filter(userRec: UserRec, limit: Int): F[UserRec]
    }

    object Filter {
      def apply[F[_]](implicit Fil: Filter[F]): Filter[F] = Fil
    }

    trait AlgorithmRepo[F[_]] {
      def getAlgorithm(recommenderId: Option[String]): F[Algorithm]
      def execute(algo: Algorithm, userId: UserId): F[UserRec]
    }

    object AlgorithmRepo {
      def apply[F[_]](implicit Algo: AlgorithmRepo[F]): AlgorithmRepo[F] = Algo
    }

  }

{% endhighlight %}

Here we have a couple of things perhaps new for the reader:

- We are defining our Algebras in Traits. So far so good. Nothing new or fancy.

- We are defining our Types with **Higher-Kinded Types** parameters (`F[_]`) to abstract out the Container Structure that it is going to be use in each *Interpreter*. A *Higher-Kinded Type* or Type Constructor is a Type which constructs a new Type based on a Type Parameter. For example `Option[+A]` is a Type constructor which takes a Type, for example `String` and constructs the final Type, for example `Option[String]`. You can probe this in a Scala console with *:kind* command:

{% highlight scala %}
scala> :k String
String's kind is A

scala> :k Option
Option's kind is F[+A]

scala> :k Option[String]
Option[String]'s kind is A
{% endhighlight %}

- A last thing we are adding here in companion objects are a technique called **Summoned values**, which allow us to get the implicit value using the Companion Object Trait's constructor.

With all this machinery in place we can add some utility functions to avoid calling companion objects and just calling **functions** from the client program:

{% highlight scala %}

  def getUser[F[_]: UserRepo](userId: Option[Int]): F[UserId] =
    UserRepo[F].getUser(userId)

  def filter[F[_]: Filter](userRec: UserRec, limit: Int): F[UserRec] =
    Filter[F].filter(userRec, limit)

  def getAlgorithm[F[_]: AlgorithmRepo](recommenderId: Option[String]): F[Algorithm] =
    AlgorithmRepo[F].getAlgorithm(recommenderId)

  def execute[F[_]: AlgorithmRepo](algo: Algorithm, userId: UserId): F[UserRec] =
    AlgorithmRepo[F].execute(algo, userId)

{% endhighlight %}

Now it is time to use our Algebras from `getRecommendations` program. In order to do that we are going to use [Context Bounds](https://docs.scala-lang.org/tutorials/FAQ/context-bounds.html) allowing the compiler to infer implicit values from context.

{% highlight scala %}

  def getRecommendations[F[_]: UserRepo: AlgorithmRepo: Filter]
      ( userId: Option[Int],
        recommenderId: Option[String],
        limit: Option[Int]): Option[Result] = {
    for {
      user           <- getUser(userId)
      algorithm      <- getAlgorithm(recommenderId)
      result         <- execute(user, algorithm)
      limitFilter     = limit.getOrElse(limitDefault)
      resultFiltered <- filter(result, limitFilter)
    } yield Result(algorithm, resultFiltered)
  }

{% endhighlight %}

## Algebra's Interpreter

Although it seems we have arrived to an acceptable solution we are not ready yet. We need to have at least one interpreter which is the real implementation of our Algebra. Recall that we are resting everything on `Option[+A]` for the moment, so our interpreter should be on `Option[+A]` Type.


One possible interpreter could be:

{% highlight scala %}
  object interpreter {

    import DataSource._
    import algebras._

    implicit object UserRepoOption extends UserRepo[Option] {
      override def getUser(userId: Option[Int]): Option[UserId] =
        userId.filter(user => users.exists(_.userId == user)).map(UserId)
    }

    implicit object AlgorithmRepoOption extends AlgorithmRepo[Option]{
      override def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
        recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))

      override def execute(algo: Algorithm, userId: UserId): Option[UserRec] =
        algo.run(userId)
    }

    implicit object FilterOption extends Filter[Option] {
      override def filter(userRec: UserRec, limit: Int): Option[UserRec] =
        Some(userRec.copy(recs = recs.slice(0, limit).toList))
    }

  }
{% endhighlight %}

We need to do for letting the compiler infer the implicit values it is just importing our implicit values on the context.

{% highlight scala %}

  import DataSource._
  import algebras._

  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {

   import interpreter._

   val result: Option[Result] = getRecommendations[Option](userId, recommenderId, limit)

   printResults(userId, result)

  }

{% endhighlight %}

Have you notice that we are calling `getRecommendations` with `Option[+A]` as the **Higher-Kinded Type** or **Type Constructor**?. Since we have indicated in `getRecommendations` that `F[_]` is context bounded by `UserRepo and AlgorithmRepo and Filter`, and

<center>$$F[\_] \cong Option[+A]$$</center>

the compiler should look for an `implicit` value for each Algebras whose `F` is `Option[+A]`. And that is what exactly we have provided to the compiler. But is this code compile and runs or only runs? Lets check

{% highlight scala %}
λx.x> $ sbt run
[error] value flatMap is not a member of type parameter F[program.DataSource.UserId]
[error]       user           <- getUser(userId)
[error]                                ^
[error] value flatMap is not a member of type parameter F[program.DataSource.Algorithm]
[error]       algorithm      <- getAlgorithm(recommenderId)
[error]                                     ^
[error] two errors found
{% endhighlight %}

Why is this happening? Because `getRecommendations` is **Context Bound** or **Constrained** on `UserRepo: AlgorithmRepo: Filter` and **for-comprehension** in *Scala* is only a *syntactic sugar* for `flatMap`, `map` and `withFilter`. Of course `Option[+A]` type implements those methods and can be used in a for-comprehension syntax, but *for-comprehension* used on `getRecommendations` doesn't know anything about `Option[+A]` or any other *Type* until it is bound in `program` function.

> Remember as i mentioned in **[Disclaimer](#disclaimer)** section that we are not using any extra FP library to do the job. If we are using cats or scalaz we can easily **Constraint** `getRecommendations` with `FlatMap` or `Monad` **Typeclasses**

### Program Syntax: For-Comprehension aware

We need a way to tell the compiler that `getRecommendations` supports **for-comprehension** syntax. We can do that creating an **Algebra** to support that syntax.

> I would like to point out that this part of the code is inspired on **[John De Goes](http://degoes.net/)** Talk **FP to the Max** and i would strongly encourage you to watch this [video](https://www.youtube.com/watch?v=sxudIMiOo68).

- In our Algebra we are going to have this:

{% highlight scala %}
  trait Program[F[_]] {
    def flatMap[A, B](fa: F[A], afb: A => F[B]): F[B]
    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  object Program {
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)
    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.flatMap(fa, afb)
  }

{% endhighlight %}

- An in our Interpreter we need resolution bind for `Option[+A]`:

{% highlight scala %}
  implicit object ProgramOption extends Program[Option] {
    override def flatMap[A, B](fa: Option[A], afb: A => Option[B]): Option[B] =
        fa.flatMap(afb)

    override def map[A, B](fa: Option[A], ab: A => B): Option[B] = fa.map(ab)

  }
{% endhighlight %}

- Last thing to do is to add `Program` **Constraint** to `getRecommendations`:

{% highlight scala %}
  def getRecommendations[F[_]: UserRepo: AlgorithmRepo: Filter: Program]
      (userId: Option[Int],
       recommenderId: Option[String],
       limit: Option[Int]): F[Result] = {
    for {
    ....
    }
  }
{% endhighlight %}

**Now the code compiles and run!!**

### Printing results: The right way

Recalling to our printing [results function](#refactor-imperative). In this case our `fold` over result is working because we are expecting an `Option[+A]` result, but `getRecommendations` is agnostic in that sense and `printResults` should also be.

{% highlight scala %}
  def printResults[F[_]: Program](userId: Option[Int], result: F[Result]): Unit = {
    result.fold[AppError, Unit](error => println(s"Error $error"), algoRes => {
      println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
      println(s"Algorithm ${algoRes.algorithm.name}")
      println(s"Recs: ${algoRes.recs.recs}")
    })
  }
{% endhighlight %}

This is a better version but our program doesn't compile because `Program` Algebra doesn't have defined a `fold` operation. So lets do it:

{% highlight scala %}
  trait Program[F[_]] {
    def flatMap[A, B](fa: F[A], afb: A => F[B]): F[B]
    def map[A, B](fa: F[A], ab: A => B): F[B]
    def fold[A, B, C](fa: F[A], first: B => C, second: A => C): C
  }

  object Program {
    def apply[F[_]](implicit Prog: Program[F]): Program[F] = Prog
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {

    def map[B](f: A => B)(implicit Prog: Program[F]): F[B] =
      Prog.map(fa, f)

    def flatMap[B](afb: A => F[B])(implicit Prog: Program[F]): F[B] =
      Prog.flatMap(fa, afb)

    def fold[B, C](first: B => C, second: A => C)(implicit Prog: Program[F]): C =
      Prog.fold(fa, first, second)
  }
{% endhighlight %}

Now we have defined our `fold` operation which is going to fold over `F`. It is time to add the interpretation of this operation:

{% highlight scala %}
  implicit object ProgramOption extends Program[Option] {
    override def flatMap[A, B](fa: Option[A], afb: A => Option[B]): Option[B] =
      fa.flatMap(afb)

    override def map[A, B](fa: Option[A], ab: A => B): Option[B] = fa.map(ab)

    override def fold[A, B, C](fa: Option[A], first: B => C, second: A => C): C =
      fa.fold(first(UnknownError.asInstanceOf[B]))(second(_))

  }
{% endhighlight %}

The code compiles and runs again with an abstract version of `printResults`. Lets see some examples:

{% highlight scala %}
λx.x> $ sbt run
[info] Done packaging.
[info] Running (fork) program.ToScalaFP
[info] Recommnedations for userId UserId(1)...
[info] Algorithm algo1
[info] Recs: List(Rec(a,0.054459512), Rec(b,0.8465745), Rec(c,0.656385),
Rec(d,0.13308495), Rec(e,0.7825986), Rec(f,0.29209626), Rec(g,0.4820329),
Rec(h,0.1532129), Rec(i,0.16719013), Rec(j,0.9551664))
[info] ------------------------------
[info] Recommnedations for userId UserId(2)...
[info] Algorithm algo2
[info] Recs: List(Rec(a,0.054459512), Rec(b,0.8465745), Rec(c,0.656385),
Rec(d,0.13308495), Rec(e,0.7825986))
[info] ------------------------------
[info] Error Unexpected Error
[info] ------------------------------
[info] Error Unexpected Error
[info] ------------------------------
[info] Error Unexpected Error
[info] ------------------------------
[info] Error Unexpected Error
[info] ------------------------------
[success] Total time: 9 s, completed Feb 17, 2019 8:45:15 PM
{% endhighlight %}

It doesn't look accurate, does it? Error cases such as *userId not found, no recommendations found and so on* are not displayed and the messages for the user are vague in those cases. This is because we are dealing with `Option[+A]` type and it doesn't give us the expressiveness we need to notify the user with the exact errors on our program.

## Handling Errors: Sum Type to Rescue

We need to alert program's user about what specific errors have been found during the execution. For that purpose it will be great to have our execution in terms of `Either[+A,+B]` instead of `Option[+A]`.

The cost of doing that with all the machinery we have defined until now is minimum because of the following:

1. `Either[+A,+B]` is a **Higher-Kinded Type** but with 2 Type parameters instead of 1 as our Algebras are requesting. We are going to see in a minute how to solve that problem.

2. We only need to write an **interpreter** for that Type, bind `getRecommendations` call with `Either[+A,+B]` and let the compiler use the correct interpreter on runtime for us.

### Lambda Types

Lets try to figure out our first problem which is how to bind a **Type Constructor** with 1 Type parameters (`Either[+A,+B]`) in a definition with only 1 Type parameter (`F[_]`)

<center>$$F[\_] \ncong Either[+A,+B]$$</center>

We can check this incongruence in **Scala** console very easily

{% highlight scala %}
scala> :kind Either
Either's kind is F[+A1,+A2]
{% endhighlight %}

As we can appreciate `Either[+A,+B]` has kind `F[+A,+B]` and we are asking a kind `F[+A]`, but we cannot change our Algebras to support `F[+A,+B]` because it is not going to accept anymore `Option[+A]` and we want to support both. Instead of changing our algebra we are going to adapt `Either[+A,+B]` to be a *Type Constructor* with 1 Type parameter. For that job we are going to use **Lambda Types**.

Basically a *Lambda Type* is similar to a Partially applied function but at a Type Level. We can **curry** our **2 parameter Type Constructor** to obtain another **1 parameter Type Constructor**. This is how it is done:

{% highlight scala %}
scala> :kind ({ type T[A] = Either[AppError, A] })#T
scala.util.Either[AppError,?]'s kind is F[+A]
{% endhighlight %}

As we can see in *Scala console* example we are fixing **Left** `Either` parameter type with `AppError` since all errors we are going to generate are subtypes of this, and let this phantom type be parameterized only in its **Right** value Type which is the Type it is going to change during execution.

> In our code we are going to use [kind-projector](https://github.com/non/kind-projector) compiler plugin to avoid this boilerplate syntax.

With **kind-projector** we can have a more readable *Lambda Type* like this:

{% highlight scala %}
scala> :kind Either[AppError, ?]
scala.util.Either[AppError,?]'s kind is F[+A]
{% endhighlight %}

### Interpreter for `Either[+A,+B]`

Now the only thing missing is to write our interpreter for this new type:

{% highlight scala %}
object interpreter {

  // Option Interpreters here

  implicit object UserRepoEither extends UserRepo[Either[AppError, ?]] {
    override def getUser(userId: Option[Int]): Either[AppError, UserId] = {
      for {
        userParam <- userId.map(UserId).toRight(UserNotProvided)
        userDb    <- users.find(_ == userParam).toRight(UserNotFound(userParam))
      } yield userDb
    }
  }

  implicit object AlgorithmRepoEither extends AlgorithmRepo[Either[AppError, ?]]{

  override def getAlgorithm(recommenderId: Option[String]):
    Either[AppError, Algorithm] =
      recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))
        .toRight(AlgorithmNotFound(recommenderId.getOrElse(algoDefault.get)))


  override def execute(algo: Algorithm, userId: UserId):
    Either[AppError, UserRec] =
      algo.run(userId).toRight(RecommendationsNotFound(userId, algo.name))
  }

  implicit object FilterEither extends Filter[Either[AppError, ?]] {
    override def filter(userRec: UserRec, limit: Int): Either[AppError, UserRec] = {
      Right(userRec.copy(recs = recs.slice(0, limit).toList))
    }
  }

  implicit object ProgramEither extends Program[Either[AppError, ?]] {
    override def flatMap[A, B](fa: Either[AppError, A],
                               afb: A => Either[AppError, B]): Either[AppError, B] =
      fa.flatMap(afb)

    override def map[A, B](fa: Either[AppError, A], ab: A => B): Either[AppError, B] =
      fa.map(ab)

    override def fold[A, B, C](fa: Either[AppError, A],
                               first: B => C,
                               second: A => C): C =
      fa.fold(error => first(error.asInstanceOf[B]), second(_))
  }
}
{% endhighlight %}

And we can execute both programs running on different interpreters at the same time:

{% highlight scala %}
  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {

    import interpreter._

    val resultEither =
      getRecommendations[Either[AppError, ?]](userId, recommenderId, limit)

    printResults[Either[AppError, ?]](userId, resultEither)

    val resultOption = getRecommendations[Option](userId, recommenderId, limit)

    printResults(userId, resultOption)
  }
{% endhighlight %}

If we run this program now we can compare detailed error when we interpret the Algebra with `Either[+A,+B]` against unknown errors with `Option[+A]`:

{% highlight scala %}
λx.x> $ sbt run
Recommnedations for userId UserId(1)...
Algorithm algo1
Recs: List(Rec(a,0.66836), Rec(b,0.8242624), Rec(c,0.74691266),
Rec(d,0.9902125), Rec(e,0.775927), Rec(f,0.015915632), Rec(g,0.19724733),
Rec(h,0.92668074), Rec(i,0.2997946), Rec(j,0.1962437))

Recommnedations for userId UserId(1)...
Algorithm algo1
Recs: List(Rec(a,0.66836), Rec(b,0.8242624), Rec(c,0.74691266),
Rec(d,0.9902125), Rec(e,0.775927), Rec(f,0.015915632), Rec(g,0.19724733),
Rec(h,0.92668074), Rec(i,0.2997946), Rec(j,0.1962437))
------------------------------


Recommnedations for userId UserId(2)...
Algorithm algo2
Recs: List(Rec(a,0.66836), Rec(b,0.8242624), Rec(c,0.74691266),
Rec(d,0.9902125), Rec(e,0.775927))

Recommnedations for userId UserId(2)...
Algorithm algo2
Recs: List(Rec(a,0.66836), Rec(b,0.8242624), Rec(c,0.74691266),
Rec(d,0.9902125), Rec(e,0.775927))
------------------------------

Error Algorithm not found for id algo5
Error Unexpected Error
------------------------------

Error User not found for id UserId(14)
Error Unexpected Error
------------------------------

Error User id must be provided
Error Unexpected Error
------------------------------

Error Recommendations not found for UserId(1) with algorithm 'algo3'
Error Unexpected Error
------------------------------


Process finished with exit code 0
{% endhighlight %}

## Testing

With this approach we can test our code in a straight forward way. We only need to provide a `Test` 1 parameter Type constructor and write our interpreters. Thats all.

To have an idea of this approach to that this could be done like this:

{% highlight scala %}

  // Provide Type parameter Test which wraps a value
  case class Test[+A](value: A)

  // Provide Interpreters for example for userRepo
   implicit object TestUserRepo extends UserRepo[Test] {
    override def getUser(userId: Option[Int]): Test[UserId] =
    Test(userId.map(UserId(_)).getOrElse(UserId(1)))
  }

{% endhighlight %}

### Conclusion

**Tagless Final Encoding** is a very good technique to encode DSL and separate the interpretation of DSL definition from implementation in a pure Functional way and as i pointed out on [Disclaimer](#disclaimer) section, implementing this technique with some **Scala FP** library such as **cats, scalaz or any other** do the work strait forward and easy, removing a lot of boilerplate code we are using here, specially **[Program Algebra](#program-syntax-for-comprehension-aware)**

