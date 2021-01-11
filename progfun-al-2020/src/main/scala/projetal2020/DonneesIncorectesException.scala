package projetal2020

final case class DonneesIncorectesException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)
