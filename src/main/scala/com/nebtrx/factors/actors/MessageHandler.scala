package com.nebtrx.factors.actors

import cats.effect.Concurrent

trait MessageHandler[F[_], M[+ _], S] {
  def receive[A](state: S, msg: M[A], sender: Actor[F, M])
                (implicit C: Concurrent[F]): F[(S, A)]
}
