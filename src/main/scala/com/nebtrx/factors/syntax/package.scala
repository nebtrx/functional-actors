package com.nebtrx.factors

import cats.{Monad, MonadError}
import cats.effect._
import cats.syntax.flatMap._
import cats.syntax.apply._
import com.nebtrx.factors.actors.errors.ResultNotCollectedError

import scala.concurrent.duration._
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

package object syntax {
  implicit final class MonadErrorThrowableSyntax[F[_], A](private val fa: F[A]) extends AnyVal {
    def collect[B](pf: PartialFunction[A, B])(implicit F: MonadError[F, Throwable]): F[B] =
      fa.flatMap {
        pf.andThen(F.pure)
          .applyOrElse(_, (a: A) => {
            F.raiseError[B](ResultNotCollectedError(a))
          })
      }
  }

//  implicit final class TimerSintax[F[_]](val timer: Timer[F]) extends AnyVal {
//    def repeatAtFixedRate(period: FiniteDuration, task: F[Unit])(implicit monad: Monad[F]): F[Unit] = {
//      timer.clock.realTime(MILLISECONDS).flatMap { start =>
//        task *> timer.clock.realTime(MILLISECONDS).flatMap { finish =>
//          val nextDelay: Long = period.toMillis - (finish - start)
//          timer.sleep(nextDelay.millis) *> repeatAtFixedRate(period, task)
//        }
//      }
//    }
//
//    def repeatAtFixedRate(initialDelay: FiniteDuration, period: FiniteDuration, task: F[Unit])(implicit monad: Monad[F]): F[Unit] = {
//      timer.sleep(initialDelay) *> repeatAtFixedRate(period, task)
//    }
//  }

  implicit final class IOSintax[A](val timer: Timer[IO]) extends AnyVal {
    def repeatAtFixedRate(period: FiniteDuration, task: IO[Unit])
                         : IO[Unit] = {
      timer.clock.realTime(MILLISECONDS).flatMap { start =>
        task *> timer.clock.realTime(MILLISECONDS).flatMap { finish =>
          val nextDelay = period.toMillis - (finish - start)
          timer.sleep(nextDelay.millis) *> repeatAtFixedRate(period, task)
        }
      }
    }

    def repeatFiberAtFixedRate[B](period: FiniteDuration, task: IO[B])
                              (implicit cs: ContextShift[IO]): IO[Unit] = {
      for {
        start <- timer.clock.realTime(MILLISECONDS)
        f <- task.start
        finish <- timer.clock.realTime(MILLISECONDS)
        nextDelay: Long = period.toMillis - (finish - start)
        _ <- timer.sleep(nextDelay.millis)
        r <- repeatFiberAtFixedRate(period, task)
      } yield r
    }

    def repeatAtFixedRate(initialDelay: FiniteDuration, period: FiniteDuration, task: IO[Unit])
                         (implicit timer: Timer[IO]): IO[Unit] = {
      timer.sleep(initialDelay) *> repeatAtFixedRate(period, task)
    }
  }

}
