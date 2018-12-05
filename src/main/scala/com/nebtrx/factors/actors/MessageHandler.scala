package com.nebtrx.factors.actors

trait MessageHandler[F[_], M[+ _], S] {
  def receive[A](state: S, msg: M[A]): F[(S, A)]
}
