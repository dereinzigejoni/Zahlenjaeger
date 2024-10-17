import scala.io.StdIn._
import scala.util.control.Breaks._
import scala.util.Random

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


@main def main(): Unit = {
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
}
