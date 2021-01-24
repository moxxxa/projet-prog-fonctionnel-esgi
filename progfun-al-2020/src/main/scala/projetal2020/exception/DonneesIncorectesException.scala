package projetal2020

//  using exception dosen't respect wartremover rule

final case class DonneesIncorectesException(
    message: String
) extends Exception(message)
