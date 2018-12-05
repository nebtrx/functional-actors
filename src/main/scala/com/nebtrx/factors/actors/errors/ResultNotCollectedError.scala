package com.nebtrx.factors.actors.errors

final case class ResultNotCollectedError[A](a: A)
  extends RuntimeException(s"Value not collected: $a")
