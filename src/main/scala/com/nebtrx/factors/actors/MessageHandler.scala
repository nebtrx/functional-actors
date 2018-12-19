package com.nebtrx.factors.actors

import cats.effect.Concurrent

trait MessageHandler[F[_], M[+ _], S] {
  def receive[A](state: S, msg: M[A], sender: Option[Actor[F, M]])
                (implicit M: Concurrent[F]): F[(S, A)]
}
