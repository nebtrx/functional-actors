package com.nebtrx.factors

import cats.effect._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.nebtrx.factors.actors.{Actor, MessageHandler}
import com.nebtrx.factors.syntax._
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import scala.concurrent.ExecutionContext.{global => globalContext}
import scala.concurrent.duration._

object Main extends IOApp {

  sealed trait Counter[+ _]

  case class Add(n: Int) extends Counter[Unit]

  case object Get extends Counter[Int]

  case object Print extends Counter[String]

  val handler: MessageHandler[IO, Counter, Int] = new MessageHandler[IO, Counter, Int] {
    private def messageAsync(actor: Actor[IO, Counter], message: Counter[_]): IO[Unit] = {
      for {
        _ <- (actor ! message).start
      } yield ()
    }

    def receive[A](state: Int, msg: Counter[A], actor: Actor[IO, Counter])
                  (implicit sender: Actor[IO, Counter], M: Concurrent[IO]): IO[(Int, A)] =
      msg match {
        case Add(n) => IO.sleep(500.milli) *>  IO { println(s"Adding 10 to $state")} *> IO.pure((state + n, ()))
        //      case Get    => (s ! Add(10).start
        case Get => (for {
          s <- IO.pure(actor)
          _ <- IO.timer(globalContext).repeatFiberAtFixedRate(2000.milli, messageAsync(s, Add(10))).foreverM.void.start
        } yield ()).map(_ => (state, state))
        case Print => IO(println(state)).flatMap(_ => IO.pure((state, state.toString)))
      }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit def unsafeLogger[F[_]: Sync]: SelfAwareStructuredLogger[F] =
      Slf4jLogger.unsafeCreate[F]

    for {
      counter <- Actor[IO, Counter, Int](0, handler)
      res <- counter ! Get
      _ <- IO(println(res))
      _ <- counter ! Add(1)
      _ <- counter ! Add(2)
      _ <- IO.sleep(10.second)
      res2 <- counter ! Print
      _ <- IO(println(res2))
      _ <- counter.stop()
    } yield ExitCode.Success
  }
}
