import scala.collection.mutable
import java.io.*
import scala.io.StdIn._
import scala.util.control.Breaks._
import scala.util.Random

//Player

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

//Playermanager
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
//Hauptspiel
val zahlenbereiche: Map[Int, (Int, Int)] = Map(
  1 -> (1, 10),
  2 -> (1, 100),
  3 -> (1, 200),
  4 -> (1, 500)
)

val schwierigkeit: Map[String, Double] = Map(
  "l" -> 0.8,
  "m" -> 0.6,
  "s" -> 0.4,
  "e" -> 0.1
)

// Funktion zur Abfrage des Zahlenbereichs
def zahlenbereichWaehlen(): (Int, Int) = {
  println("Wähle einen Zahlenbereich: ")
  println("1: 1 bis 10")
  println("2: 1 bis 100")
  println("3: 1 bis 200")
  println("4: 1 bis 500")

  var auswahl = 0
  while (!zahlenbereiche.contains(auswahl)) {
    println("Gib die Nummer des gewünschten Bereichs ein:")
    auswahl = readInt()

    if (!zahlenbereiche.contains(auswahl)) {
      println("Ungültige Auswahl, bitte erneut versuchen.")
    }
  }

  zahlenbereiche(auswahl)
}

// Funktion zur Abfrage der Schwierigkeit
def schwierigkeitWaehlen(): Double = {
  println("Wähle eine Schwierigkeit: ")
  println("l: leicht (80% der Versuche)")
  println("m: mittel (60% der Versuche)")
  println("s: schwer (40% der Versuche)")
  println("e: Experte (10% der Versuche)")

  var schwierigkeitsWahl = ""
  while (!schwierigkeit.contains(schwierigkeitsWahl)) {
    println("Gib den Buchstaben der Schwierigkeit ein (l, m, s, e):")
    schwierigkeitsWahl = readLine().toLowerCase()

    if (!schwierigkeit.contains(schwierigkeitsWahl)) {
      println("Ungültige Auswahl, bitte erneut versuchen.")
    }
  }

  schwierigkeit(schwierigkeitsWahl)
}

val random = new Random()
var players = PlayerManager.loadPlayers()
var player = PlayerManager.getPlayer(players)._1.get // Hole den Player

val adminPassword = "admin" // Beispiel für ein Admin-Passwort

breakable {
  while (true) {
    // Abfrage, ob Spieler gelöscht werden sollen
    println("Möchtest du einen Spieler löschen? (j/n)")
    if (readLine().toLowerCase() == "j") {
      try {
        players = PlayerManager.deletePlayer(players, adminPassword)
        PlayerManager.savePlayers(players) // Spieler speichern nach dem Löschen
      } catch {
        case e: PlayerNotFoundException => println(e.getMessage) // Ausnahmebehandlung für nicht existierenden Spieler
      }
      // Erneute Spielerabfrage nach dem Löschen
      val (newPlayer, updatedPlayers) = PlayerManager.getPlayer(players)
      players = updatedPlayers
      player = newPlayer.getOrElse(player) // Wenn ein neuer Spieler erstellt wurde, den aktuellen Spieler aktualisieren
    }

    val (untereGrenze, obereGrenze) = zahlenbereichWaehlen()
    val schwierigkeitsMultiplikator = schwierigkeitWaehlen()
    val versuche = ((obereGrenze - untereGrenze + 1) * schwierigkeitsMultiplikator).toInt

    val numbRandom = random.nextInt(obereGrenze - untereGrenze + 1) + untereGrenze
    var versuchscounter = 0

    println(s"Nenne eine Nummer von $untereGrenze bis $obereGrenze:")
    var numb = readInt()

    while (versuchscounter < versuche) {
      versuchscounter += 1

      if (numb == numbRandom) {
        println(s"Glückwunsch! Du hast es mit $versuchscounter Versuchen geschafft.")

        // Punkteberechnung
        if (versuchscounter <= versuche / 2) {
          player = player.updateScore(1000) // 1000 Punkte bei richtigem Raten unter der Hälfte der Versuche
        } else {
          player = player.updateScore(200) // 200 Punkte bei richtigem Raten allgemein
        }

        player = player.checkLevelUp()
        players = PlayerManager.updatePlayer(players, player)
        PlayerManager.savePlayers(players) // Spieler speichern
        break
      } else if (numb < numbRandom) {
        println("Zu niedrig, versuch es nochmal.")
      } else {
        println("Zu hoch, versuch es nochmal.")
      }

      if (versuchscounter < versuche) {
        println("Neue Nummer:")
        numb = readInt()
      }
    }

    println(s"Nicht erraten. Die richtige Nummer war $numbRandom.")

    // Spielerwechsel
    val (switchedPlayer, updatedPlayersAfterSwitch) = PlayerManager.switchPlayer(players)
    switchedPlayer.foreach { newPlayer =>
      players = PlayerManager.updatePlayer(players, newPlayer)
    }

    // Speicher die Spieler
    PlayerManager.savePlayers(players)
  }
}
