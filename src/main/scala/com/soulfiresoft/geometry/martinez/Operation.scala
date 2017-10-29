package com.soulfiresoft.geometry.martinez

sealed trait Operation {
  def id: Int
}

object Operation {

  case object Intersection extends Operation {
    val id = 0
  }

  case object Union extends Operation {
    val id = 1
  }

  case object Difference extends Operation {
    val id = 2
  }

  case object XOR extends Operation {
    val id = 3
  }

}
