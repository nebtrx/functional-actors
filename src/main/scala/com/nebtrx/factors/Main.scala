package com.nebtrx.factors

import cats.effect.{ExitCode, IO, IOApp}
import com.nebtrx.factors.actors.{Actor, MessageHandler}

object Main extends IOApp {
  sealed trait Counter[+ _]
  case class Add(n: Int) extends Counter[Unit]
  case object Get extends Counter[Int]
  case object Print extends Counter[String]

  val handler: MessageHandler[IO, Counter, Int] = new MessageHandler[IO, Counter, Int] {
    def receive[A](state: Int, msg: Counter[A]): IO[(Int, A)] = msg match {
      case Add(n) => IO.pure((state + n, ()))
      case Get    => IO.pure((state, state))
      case Print  => IO(println(state)).flatMap(_ => IO.pure((state, state.toString)))
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
      _ <- IO(println(res2))
      _ <- counter.stop()
    } yield ExitCode.Success
  }
}