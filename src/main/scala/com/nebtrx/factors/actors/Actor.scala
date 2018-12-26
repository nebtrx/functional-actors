package com.nebtrx.factors.actors

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.syntax.concurrent._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.nebtrx.factors.syntax._
import fs2.concurrent.Queue

trait Actor[F[_], M[+ _]] {

  // TODO: make the sender more expressive
  def ![A](fa: M[A])(implicit sender: Actor[F, M] = this): F[A]

  def stop(): F[Unit]
}

object Actor {

  private type ActorMessage[F[_], M[+ _], A] = (M[A], Deferred[F, A], Actor[F, M])

  def apply[F[_], M[+ _], S](initialState: S, messageHandler: MessageHandler[F, M, S])
                            (implicit C: Concurrent[F])
                            : F[Actor[F, M]] = {

    def process[A](pendingMsg: ActorMessage[F, M, A], stateHolder: Ref[F, S])
                  (implicit C: Concurrent[F])
                  : F[Unit] =
      for {
        state <- stateHolder.get
        (msg, deferred, sender) = pendingMsg
        result <- messageHandler.receive(state, msg, sender)
        (newState, output) = result
        _ <- stateHolder.set(newState) *> deferred.complete(output)
      } yield ()

    for {
      state <- Ref.of[F, S](initialState)
      queue <- Queue.unbounded[F, ActorMessage[F, M, _]]
      consumer <- (for {
        msg <- queue.dequeue1
        _ <- process(msg, state)
      } yield ()).foreverM.void.start
    } yield
      new Actor[F, M] {
        def ![A](fa: M[A])(implicit sender: Actor[F, M]): F[A] =
          for {
            deferred <- Deferred[F, A]
            _ <- queue.offer1((fa, deferred, sender))
            output <- (consumer.join race deferred.get)
              .collect { case Right(o) => o }
          } yield output

        override def stop(): F[Unit] =
          consumer.cancel
      }
  }
}

