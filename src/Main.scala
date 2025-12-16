/*
Rules:
* Use numbers for user input (no text, except for changing the movie title in admin mode).
* Validate all user inputs (ask again until a valid value is obtained).
* After each operation, the program must return to the main menu, except in case of explicit exit
or blocking after too many PIN attempts.
Structure:
*  The program must define a constant for the number of theaters, a variable named theaterCount
which contains the value 4. This value may be changed and the program must still work.
* Each theater has an internal identifier between 0 and theaterCount - 1. However, in the
menus the user must be able to select theaters from 1 up to theaterCount.
*You must use arrays to represent:
– the movies shown in each theater (an array of String);
– the number of seats still available for each theater and each screening (an array of Int
per time slot, therefore 3 in total);
– the PIN codes of the theaters (an array of String for the PIN codes).
All information concerning Theater 1 is stored in index 0 of all arrays (titles: 1 array, screenings:
3 arrays, PIN codes: 1 array), the information for Theater 2 is stored at index 1, and so on.
* When the program starts:
– no theater has a movie scheduled (the value "No movie scheduled");
– each theater has 50 available seats for each of the three screenings;
– all theaters use the default PIN code 2025.
* */
import scala.io.StdIn._
import scala.util.Random
//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
object Main {
  def main(args: Array[String]): Unit = {
    var movies: Array[String] = Array("The Mystery of the Forest", "Interstellar Journey", "No movie scheduled", "No movie scheduled")
    var seats14h: Array[Int] = Array(50, 50, 50, 50)
    var seats17h: Array[Int] = Array(50, 50, 50, 50)
    var seats20h: Array[Int] = Array(50, 50, 50, 50)
    var pins: Array[String] = Array("2025", "2025", "2025", "2025")
    var theaterCount: Int = 4
    val prices_Noon: Array[Double] = Array(4.0, 6.0, 10.0) // Prices for 14h screening
    val prices_Afternoon: Array[Double] = Array(6.0, 8.0, 12.0) // Prices for 17h screening
    val prices_Evening: Array[Double] = Array(7.0, 9.0, 13.0) // Prices for 20h screening

    var isProgramRunning: Boolean = true

    def stopProgram(): Unit = {
      isProgramRunning = false
      println("Exiting the program. Goodbye!")
    }

    do {
      showMainMenu()
      val choice = readLine(f"=> ").toInt
      if (choice < 1 || choice > 3) {
        println("Please enter a valid option (1-3).")
      } else if (choice == 3) {
        stopProgram()
      } else if (choice == 1) {
        clientMode()
      } else if (choice == 2) {
        adminModeAccess()
      }
    } while (isProgramRunning)

    def showMainMenu(): Unit = {
      println("Welcome to the Movie Theater Booking System")
      println("1. Client Mode")
      println("2. Admin Mode")
      println("3. Exit")
    }

    // Client Mode Section
    def clientMode(): Unit = {
      println("Client Mode")
      if (movies.isEmpty) {
        println("No movies scheduled in any theater.")
      } else {
        for (i <- 0 until theaterCount) {
          if (movies(i) == "No movie scheduled") {
            println(s"Theater ${i + 1}: No movie scheduled")
          } else {
            println(s"Theater ${i + 1}: ${movies(i)}")
          }
        }
      }
      println(s"Select a theater (1-$theaterCount):")
      var theaterChoice = readLine("=> ").toInt
      var idTheater: Int = theaterChoice - 1
      do {


        if (theaterChoice < 1 || theaterChoice > theaterCount) {
          println(s"Please enter a valid theater number (1-$theaterCount).")
          theaterChoice == readLine("=> ").toInt
        }else if (movies(idTheater) == "No movie scheduled" && movies(idTheater) == "") {
          println(s"Theater $theaterChoice has no movie scheduled. Please select another theater.")
          theaterChoice == readLine("=> ").toInt
           // Reset choice to continue the loop
        }else {
          println(s"You selected Theater $theaterChoice: ${movies(idTheater)}")
          println("Available screenings:")
          println(s"1. 14:00 - Seats available: ${seats14h(idTheater)} - Price: \n" +
            s" => Child price: $$${prices_Noon(0)}\n" +
            s" => Student price: $$${prices_Noon(1)}\n" +
            s" => Adult price: $$${prices_Noon(2)} \n")
          println(s"2. 17:00 - Seats available: ${seats17h(idTheater)} - Prices: \n" +
            s" => Child price :  $$${prices_Afternoon(0)} \n" +
            s" => Student price : $$${prices_Afternoon(1)} \n" +
            s" => Adult price: $$${prices_Afternoon(2)} \n")
          println(s"3. 20:00 - Seats available: ${seats20h(idTheater)} - Price: \n" +
            s" => Child price: $$${prices_Evening(0)} \n" +
            s" => Student price: $$${prices_Evening(1)} \n" +
            s" => Adult price: $$${prices_Evening(2)}")

        }
      } while (theaterChoice < 1 && theaterChoice > theaterCount)


      var screeningChoice: Int = -1
      do {
        println("Select a screening (1-3):")
        screeningChoice = readLine(f"=> ").toInt
        if (screeningChoice < 1 || screeningChoice > 3) {
          println("Please enter a valid screening number (1-3).")
        } else {
          val screeningIndex: Int = screeningChoice - 1
          if (screeningChoice == 1) {
            println(s"You selected the 14:00 screening.")
          } else if (screeningChoice == 2) {
            println(s"You selected the 17:00 screening.")
          } else if (screeningChoice == 3) {
            println(s"You selected the 20:00 screening.")
          }
        }
      } while (screeningChoice < 1 || screeningChoice > 3)
      var totalOrdering = selectSeats(screeningChoice, idTheater)
      println(s"Booking completed for $totalOrdering seats. Enjoy the movie!")
      println(totalOrdering)


    }

    def bookClient(
                    idTheater: Int,
                    films: Array[String],
                    seats14h: Array[Int],
                    seats17h: Array[Int],
                    seats20h: Array[Int]
                  ): Unit = {

    }


    def selectSeats(chosenScreening: Int, filmIndex: Int): Int = {
      val limit: Int = 5
      val idTheater:Int = filmIndex// This should be passed as a parameter or determined from context
      println("Select number of seats to book (max 5):")
      var seatsToBook: Int = -1
      do {
        seatsToBook = readLine(f"=> ").toInt
        if (seatsToBook < 1 || seatsToBook > limit) {
          println(s"Please enter a valid number of seats (1-${limit}).")
        }

      } while (seatsToBook < 1 || seatsToBook > limit)
      println(s"You selected to book $seatsToBook seats.")
      if (chosenScreening == 1) {
        if (seatsToBook > seats14h(idTheater)) {
          println(s"Not enough seats available. Only ${seats14h(idTheater)} seats left.")
          return selectSeats(chosenScreening, idTheater)
        } else {
          seats14h(idTheater) -= seatsToBook
        }
      } else if (chosenScreening == 2) {
        if (seatsToBook > seats17h(idTheater)) {
          println(s"Not enough seats available. Only ${seats17h(idTheater)} seats left.")
          return selectSeats(chosenScreening, idTheater)
        } else {
          seats17h(idTheater) -= seatsToBook
        }
      } else if (chosenScreening == 3) {
        if (seatsToBook > seats20h(idTheater)) {
          println(s"Not enough seats available. Only ${seats20h(idTheater)} seats left.")
          return selectSeats(chosenScreening, idTheater)
        } else {
          seats20h(idTheater) -= seatsToBook
        }
      }
      val (totalSeats, totalPriceAmount) = totalPrice(seatsToBook, chosenScreening)
      val transactionCode = generateTransactionCode()
      return totalSeats

    }
    def totalPrice(
                    seatsToBook: Int,
                    chosenScreening: Int
                  ): (Int, Double) = {
      var total: Double = 0.0
      for (i <- 1 to seatsToBook) {
        println(s"Select type for seat $i:")
        println("1. Child")
        println("2. Student")
        println("3. Adult")
        var seatTypeChoice: Int = -1
        do {
          seatTypeChoice = readLine(f"=> ").toInt
          if (seatTypeChoice < 1 || seatTypeChoice > 3) {
            println("Please enter a valid seat type (1-3).")
          } else {
            if (chosenScreening == 1) {
              total += prices_Noon(seatTypeChoice - 1)
            } else if (chosenScreening == 2) {
              total += prices_Afternoon(seatTypeChoice - 1)
            } else if (chosenScreening == 3) {
              total += prices_Evening(seatTypeChoice - 1)
            }
          }
        } while (seatTypeChoice < 1 || seatTypeChoice > 3)
      }
      println(f"Total price for $seatsToBook seats: $$${total}%.2f")
      return (seatsToBook, total)
    }

    def generateTransactionCode(): Int = {
      val random = new Random()
      val transactionCode = 100000 + random.nextInt(900000) // Generates a random 6-digit code
      println(s"Your transaction code is: $transactionCode")
      return transactionCode
    }

    // Admin Mode Section

    def adminModeAccess(): Unit = {
      println("Admin Mode with Password")
      println("Enter admin password:")
      val adminPassword: String = readLine(f"=> ")
      val correctPassword: String = "admin123" // Example password
      if (adminPassword == correctPassword) {
        println("Access granted.")
        adminMode()
      } else {
        println("Access denied. Incorrect password.")
      }
    }

    def adminMode(): Unit = {
      println("Admin Mode")

      var theaterChoice: Int = 0
      var idTheater: Int = -1

      // 1) Select theater (validated)
      do {
        println(s"Select a theater (1-$theaterCount):")
        val input: String = readLine("=> ")

        if (input.length > 0 && input.forall(_.isDigit)) theaterChoice = input.toInt
        else theaterChoice = 0

        if (theaterChoice < 1 || theaterChoice > theaterCount) {
          println(s"Please enter a valid theater number (1-$theaterCount).")
        }
      } while (theaterChoice < 1 || theaterChoice > theaterCount)

      idTheater = theaterChoice - 1

      // 2) Validate PIN (if fail => return to main menu)
      if (validatePin(idTheater, pins)) {

        var adminChoice: Int = 0

        do {
          // 3) Admin menu (3 options)
          println("\nAdmin Options:")
          println("1. Change theater")
          println("2. See available seats")
          println("3. Exit")

          val choiceInput: String = readLine("=> ")
          if (choiceInput.length > 0 && choiceInput.forall(_.isDigit)) adminChoice = choiceInput.toInt
          else adminChoice = 0

          if (adminChoice < 1 || adminChoice > 3) {
            println("Please enter a valid option (1-3).")

          } else if (adminChoice == 1) {
            // Change theater + re-auth
            println("\nChange Theater/Movie")
            // 1) Select theater (validated)
            do {
              println(s"Select a theater (1-$theaterCount):")
              val tInput: String = readLine("=> ")
              if (tInput.length > 0 && tInput.forall(_.isDigit)) theaterChoice = tInput.toInt
              else theaterChoice = 0
              if (theaterChoice < 1 || theaterChoice > theaterCount) {
                println(s"Please enter a valid theater number (1-$theaterCount).")
              }
            } while (theaterChoice < 1 || theaterChoice > theaterCount)
            idTheater = theaterChoice - 1
            // 2) Change movie + reset seats
            changeTheaterMovie(idTheater, movies, seats14h, seats17h, seats20h)
            // 3) Re-authenticate PIN (if fail => return to main menu)

            do {
              println(s"Select a theater (1-$theaterCount):")
              val tInput: String = readLine("=> ")

              if (tInput.length > 0 && tInput.forall(_.isDigit)) theaterChoice = tInput.toInt
              else theaterChoice = 0

              if (theaterChoice < 1 || theaterChoice > theaterCount) {
                println(s"Please enter a valid theater number (1-$theaterCount).")
              }
            } while (theaterChoice < 1 || theaterChoice > theaterCount)

            idTheater = theaterChoice - 1

            if (!validatePin(idTheater, pins)) {
              // blocked or failed -> leave admin mode (back to main menu)
              return
            }

          } else if (adminChoice == 2) {
            // See available seats for current theater
            println(s"\nTheater ${idTheater + 1}: ${movies(idTheater)}")
            println(s"14:00 seats: ${seats14h(idTheater)}")
            println(s"17:00 seats: ${seats17h(idTheater)}")
            println(s"20:00 seats: ${seats20h(idTheater)}")

          } else if (adminChoice == 3) {
            println("Leaving Admin Mode...")
          }

        } while (adminChoice != 3)
      }
    }

    def validatePin(idTheater: Int, pins: Array[String]): Boolean = {
      var attempts: Int = 0
      val maxAttempts: Int = 3
      do {
        println(s"Enter PIN for Theater ${idTheater + 1}:")
        val inputPin: String = readLine(f"=> ")
        if (inputPin == pins(idTheater)) {
          println("Authentication successful.")
          return true
        } else {
          attempts += 1
          println(s"Incorrect PIN. You have ${maxAttempts - attempts} attempts left.")
        }
      } while (attempts < maxAttempts)
      println("Too many incorrect attempts. Access blocked.")
      return false
    }

    def updatePin(idTheater: Int, pins: Array[String]): Unit = {
      println("Enter new PIN (4 digits):")
      var newPin: String = ""
      do {
        newPin = readLine(f"=> ")
        if (newPin.length != 4 || !newPin.forall(_.isDigit)) {
          println("Please enter a valid 4-digit PIN.")
        }
      } while (newPin.length != 4 || !newPin.forall(_.isDigit))
      pins(idTheater) = newPin
      println("PIN updated successfully.")
    }

    def changeTheaterMovie(
                            idTheater: Int,
                            films: Array[String],
                            seats14h: Array[Int],
                            seats17h: Array[Int],
                            seats20h: Array[Int]
                          ): Unit = {
      println("Enter new movie title:")
      val newMovieTitle: String = readLine(f"=> ")
      films(idTheater) = newMovieTitle
      seats14h(idTheater) = 50
      seats17h(idTheater) = 50
      seats20h(idTheater) = 50
      println(s"Movie for Theater ${idTheater + 1} updated to '$newMovieTitle' with 50 seats for each screening.")
    }


  }
}



