package com.soulfiresoft.geometry.martinez

sealed trait EdgeType {
  def id: Int
}

object EdgeType {

  case object Normal extends EdgeType {
    val id = 0
  }

  case object NonContributing extends EdgeType {
    val id = 1
  }

  case object SameTransition extends EdgeType {
    val id = 2
  }

  case object DifferentTransition extends EdgeType {
    val id = 3
  }

}
