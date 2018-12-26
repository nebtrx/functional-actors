package com.nebtrx.factors

import cats.effect._
import com.nebtrx.factors.actors.{Actor, MessageHandler}

import scala.concurrent.duration._
import com.nebtrx.factors.syntax._

import scala.concurrent.ExecutionContext.{global => globalContext}

object Main extends IOApp {

  sealed trait Counter[+ _]

  case class Add(n: Int) extends Counter[Unit]

  case object Get extends Counter[Int]

  case object Print extends Counter[String]

  val handler: MessageHandler[IO, Counter, Int] = new MessageHandler[IO, Counter, Int] {
    private def messageAsync(actor: Actor[IO, Counter]): IO[Unit] = {
       for {
         _ <- (actor ! Add(10)).start
       } yield ()
    }

    def receive[A](state: Int, msg: Counter[A], sender: Actor[IO, Counter])
                  (implicit M: Concurrent[IO]): IO[(Int, A)] =
      msg match {
      case Add(n) => IO.pure((state + n, ()))
      //      case Get    => (s ! Add(10).start
      case Get => (for {
        s <- IO.pure(sender)
//        _ <- IO.timer(globalContext).repeatFiberAtFixedRate(1.second, messageAsync(s))
        _ <- IO.timer(globalContext).repeatFiberAtFixedRate(1.second, s ! Add(10))
//        _ <- messageAsync(s)
      } yield ()).map(_ => (state, state))
      case Print => IO(println(state)).flatMap(_ => IO.pure((state, state.toString)))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      counter <- Actor[IO, Counter, Int](0, handler)
      res <- counter ! Get
      _ <- IO(println(res))
      _ <- counter ! Add(1)
      _ <- counter ! Add(2)
      res2 <- counter ! Print
      _ <- IO.sleep(10.seconds)
      _ <- IO(println(res2))
      _ <- counter.stop()
    } yield ExitCode.Success
  }
}
