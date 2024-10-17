import scala.collection.mutable
import java.io.*
import scala.io.StdIn._
object PlayerManager {
  def loadPlayers(): mutable.Map[String, Player] = {
    val file = new File("players.txt")
    val players = mutable.Map[String, Player]()

    if (file.exists()) {
      val source = scala.io.Source.fromFile(file)
      source.getLines().foreach { line =>
        val Array(name, levelStr, scoreStr) = line.split(",")
        players += (name -> Player(name, levelStr.toInt, scoreStr.toInt))
      }
      source.close()
    }
    players
  }

  def savePlayers(players: mutable.Map[String, Player]): Unit = {
    val writer = new PrintWriter(new File("players.txt"))
    players.values.foreach { player =>
      writer.println(s"${player.name},${player.level},${player.score}")
    }
    writer.close()
  }

  def getPlayer(players: mutable.Map[String, Player]): (Option[Player], mutable.Map[String, Player]) = {
    println("Bitte gib den Spielernamen ein:")
    val playerName = readLine()

    players.get(playerName) match {
      case Some(existingPlayer) =>
        println(s"Spieler ${existingPlayer.name} existiert bereits mit Level ${existingPlayer.level} und Score ${existingPlayer.score}.")
        (Some(existingPlayer), players)
      case None =>
        val newPlayer = Player(playerName)
        players += (playerName -> newPlayer)
        println(s"Neuer Spieler ${newPlayer.name} wurde erstellt")
        (Some(newPlayer), players)

    }
  }

  def switchPlayer(players: mutable.Map[String, Player]): (Option[Player], mutable.Map[String, Player]) = {
    println("Möchtest du den Spieler wechseln? (j/n)")
    val input = readLine().toLowerCase()
    if (input == "j") {
      getPlayer(players)
    } else {
      println("Weiterspielen mit dem aktuellen Spieler.")
      (None, players)
    }
  }

  def updatePlayer(players: mutable.Map[String, Player], player: Player): mutable.Map[String, Player] = {
    players += (player.name -> player)
    players
  }

  def deletePlayer(players: mutable.Map[String, Player], adminPassword: String): mutable.Map[String, Player] = {
    println("Gib das Admin-Passwort ein:")
    val passwordInput = readLine()

    if (passwordInput == adminPassword) {
      println("Bitte gib den Namen des Spielers ein, den du löschen möchtest:")
      val playerName = readLine()
      if (players.contains(playerName)) {
        players -= playerName
        println(s"Spieler $playerName wurde erfolgreich gelöscht.")
      } else {
        throw PlayerNotFoundException(playerName) // Wirf eine Ausnahme, wenn der Spieler nicht existiert
      }
    } else {
      println("Falsches Passwort. Zugriff verweigert.")
    }

    players
  }
}