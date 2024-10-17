case class PlayerNotFoundException(playerName: String) extends Exception(s"Spieler '$playerName' existiert nicht.")
case class Player(name: String, level: Int = 1, score: Int = 0) {
  def updateScore(points: Int): Player = {
    val newScore = score + points
    println(s"Spieler $name erhält $points Punkte. Aktueller Score: $newScore")
    copy(score = newScore)
  }

  def checkLevelUp(): Player = {
    val requiredPoints = if (level < 10) 8000 else 10000
    if (score >= requiredPoints) {
      val newScore = score - requiredPoints
      val newLevel = level + 1
      println(s"Du bist ein Level aufgestiegen! Aktuelles Level: $newLevel")
      copy(level = newLevel, score = newScore)
    } else {
      this
    }
  }
}