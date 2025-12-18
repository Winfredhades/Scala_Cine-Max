import scala.io.StdIn._
import scala.util.Random

object Main {

  // ==========================
  // Global data
  // ==========================
  var theaterCount: Int = 4

  var movies: Array[String] =
    Array("The Mystery of the Forest", "Interstellar Journey", "No movie scheduled", "No movie scheduled")

  var seats14h: Array[Int] = Array(50, 50, 50, 50)
  var seats17h: Array[Int] = Array(50, 50, 50, 50)
  var seats20h: Array[Int] = Array(50, 50, 50, 50)

  var pins: Array[String] = Array("2025", "2025", "2025", "2025")

  // Prices by screening (index 0=Child, 1=Student, 2=Adult)
  val prices_Noon: Array[Double] = Array(4.0, 6.0, 10.0)       // 14:00
  val prices_Afternoon: Array[Double] = Array(6.0, 8.0, 12.0)  // 17:00
  val prices_Evening: Array[Double] = Array(7.0, 9.0, 13.0)    // 20:00

  var isProgramRunning: Boolean = true

  // ==========================
  // Helpers
  // ==========================
  def stopProgram(): Unit = {
    isProgramRunning = false
    println("Exiting the program. Goodbye!")
  }


  /*
  def readIntInRange(prompt: String, min: Int, max: Int): Int = {
    var value: Int = min - 1
    do {
      print(prompt)
      val input = readLine()
      if (input != null && input.nonEmpty && input.forall(_.isDigit)) {
        value = input.toInt
        if (value < min || value > max) {
          println(s"Please enter a valid value ($min-$max).")
          value = min - 1
        }
      } else {
        println(s"Please enter a valid value ($min-$max).")
        value = min - 1
      }
    } while (value < min || value > max)
    value
  }
  */

  def generateTransactionCode(): Int = {
    val r = new Random()
    1000 + r.nextInt(9000) // 4 digits
  }

  def showMainMenu(): Unit = {
    println("================ Welcome To The Movie Theater Booking System CineMax ================")
    println("1) Client Mode")
    println("2) Admin Mode")
    println("3) Exit")
  }

  // ==========================
  // Client mode
  // ==========================

  def selectMovies(): Int = {
    println("Client mode")
    println("Please select a theater:")
    for (i <- 0 until theaterCount) {
      println(s"${i + 1}) Theater ${i + 1} - ${movies(i)}")
    }

    var theaterNumber: Int = -1
    do {
      println("Select a theater number:")
      theaterNumber = readLine(">").toInt
      if (theaterNumber < 1 || theaterNumber > theaterCount) {
        println("Invalid theater number. Please try again.")
        theaterNumber = -1
      } else {
        println(s"You selected Theater $theaterNumber - ${movies(theaterNumber - 1)}")
      }

      if (movies(theaterNumber - 1) == "No movie scheduled") {
        println(s"Theater $theaterNumber has no movie scheduled. Please select another theater.")
        theaterNumber = -1
      }
    } while (theaterNumber == -1)

    theaterNumber - 1 // return internal id (index)
  }

  def clientMode(): Unit = {
    // I could do it with for loop but this is clearer
    // And we saw _ synatx in the course
    if (movies.forall(_ == "No movie scheduled")) {
      println("No movies scheduled in any theater.")
    } else {
      val idTheater = selectMovies()
      val ok = bookClient(idTheater, movies, seats14h, seats17h, seats20h)
      if (!ok) {
        println("Returning to main menu.")
      }
    }
  }

  // ==========================
  // MANDATORY METHODS
  // ==========================

  // validatePin (single attempt only!)
  def validatePin(idTheater: Int, pins: Array[String]): Boolean = {
    println(s"Enter the PIN code for Theater ${idTheater + 1}:")
    val inputPin: String = readLine("> ")
    if (inputPin == pins(idTheater)) true else false
  }

  // updatePin
  def updatePin(idTheater: Int, pins: Array[String]): Unit = {
    println(s"Changing the PIN for Theater ${idTheater + 1}.")
    var newPin: String = ""
    do {
      println("Enter a new 4-digit PIN:")
      newPin = readLine("> ")
      if (newPin == null) newPin = ""
      if (newPin.length != 4 || !newPin.forall(_.isDigit)) {
        println("Invalid PIN. The PIN must contain exactly 4 digits.")
        newPin = ""
      }
    } while (newPin == "")

    pins(idTheater) = newPin
    println("PIN successfully updated.")
  }

  // bookClient (handles full booking for ONE theater)
  def bookClient(
                  idTheater: Int,
                  films: Array[String],
                  seats14h: Array[Int],
                  seats17h: Array[Int],
                  seats20h: Array[Int]
                ): Boolean = {

    if (films(idTheater) == "No movie scheduled") {
      println("Error: This theater has no movie scheduled.")
      return false
    }

    println("Please select your screening:")
    println(s"1) 14:00 (Reduced price: -2.00 CHF)")
    println("2) 17:00 (Normal price:)")
    println("3) 20:00 (Evening price: +1.00 CHF)")
    println("Please enter the number corresponding to your choice:")
    val chosenScreening = readLine("> ").toInt
    if (chosenScreening < 1 || chosenScreening > 3) {
      println("Error: Invalid screening selection.")
      return false
    }

    // Ask number of tickets
    println("How many tickets would you like? (1-5)")
    val tickets: Int = readLine("> ").toInt
    if (tickets < 1 || tickets > 5) {
      println("Error: You can only book between 1 and 5 tickets.")
      return false
    }

    // Check availability
    var available = 0
    if (chosenScreening == 1) available = seats14h(idTheater)
    else if (chosenScreening == 2) available = seats17h(idTheater)
    else available = seats20h(idTheater)

    if (tickets > available) {
      println(s"Error: Only $available seats are available for this screening.")
      return false
    }

    // Ask ticket types and compute total
    var total: Double = 0.0
    var i = 1
    while (i <= tickets) {
      println(s"Type for ticket $i:")
      println(s"1) Child")
      println("2) Student")
      println("3) Adult")
      val t: Int = readLine("> ").toInt
      if (t < 1 || t > 3) {
        println("Error: Invalid ticket type.")
        return false
      }

      // Convert (1..3) => price index (0..2): child(1)->0, student(2)->1, adult(3)->2
      val priceIndex = t - 1
      if (chosenScreening == 1) total += prices_Noon(priceIndex)
      else if (chosenScreening == 2) total += prices_Afternoon(priceIndex)
      else total += prices_Evening(priceIndex)

      i += 1
    }

    // Update seats (booking is possible)
    if (chosenScreening == 1) seats14h(idTheater) -= tickets
    else if (chosenScreening == 2) seats17h(idTheater) -= tickets
    else seats20h(idTheater) -= tickets

    // Summary
    var timeStr = ""
    if (chosenScreening == 1) timeStr = "14:00"
    else if (chosenScreening == 2) timeStr = "17:00"
    else timeStr = "20:00"

    val code = generateTransactionCode()

    println("\nSummary of your order:")
    println(s"Theater: ${idTheater + 1}")
    println(s"Movie: ${films(idTheater)}")
    println(s"Screening: $timeStr")
    println(s"Number of tickets: $tickets")
    println(f"Total price: $total%.2f CHF")
    println("Payment by credit card")
    println(s"Transaction code: $code")
    println("(Processing...)")
    println("Payment confirmed!")
    println("Your tickets have been successfully booked.")
    println("Enjoy the movie!")

    true
  }


  // ==========================
  // Admin mode
  // ==========================
  def selectTheaterForAdmin(): Int = {
    println("Select a theater for administration:")
    for (i <- 0 until theaterCount) {
      println(s"${i + 1}) Theater ${i + 1}")
    }
    val theaterNumber : Int = readLine("> ").toInt
    if (theaterNumber < 1 || theaterNumber > theaterCount) {
      println("Invalid theater number. Please try again.")
      return selectTheaterForAdmin()
    }
    theaterNumber - 1
  }

  def adminMenu(idTheater: Int): Unit = {
    var adminChoice: Int = 0
    do {
      println(s"\nAdmin menu - Theater ${idTheater + 1} - ${movies(idTheater)}")
      println("1) Display the status of all theaters")
      println(s"2) Change the movie of Theater ${idTheater + 1}")
      println(s"3) Change the PIN of Theater ${idTheater + 1}")
      println("4) Return to the main menu")

      adminChoice = readLine("> ").toInt

      if(adminChoice < 1 || adminChoice > 4 ) {
        println("Invalid choice. Please try again.")
      } else if (adminChoice == 1) {
        displayTheatersStatus(movies, seats14h, seats17h, seats20h)
      } else if (adminChoice == 2) {
        changeTheaterMovie(idTheater, movies, seats14h, seats17h, seats20h)
      } else if (adminChoice == 3) {
        updatePin(idTheater, pins)
      } else if (adminChoice == 4) {
        println("Returning to main menu...")
      }

    } while (adminChoice != 4)
  }

  def adminModeAccess(): Unit = {
    println("Admin mode")


    val idTheater = selectTheaterForAdmin()

    var attempts: Int = 0
    val maxAttempts: Int = 3
    var ok: Boolean = false

    do {
      ok = validatePin(idTheater, pins)
      if (!ok) {
        attempts += 1
        println(s"Incorrect PIN. You have ${maxAttempts - attempts} attempt(s) left.")
      }
    } while (!ok && attempts < maxAttempts)

    if (!ok) {
      println("Too many failed attempts. The program will terminate.")
    } else {
      println("Access granted.")
      adminMenu(idTheater)
    }
  }

  // displayTheatersStatus
  def displayTheatersStatus(
                             films: Array[String],
                             seats14h: Array[Int],
                             seats17h: Array[Int],
                             seats20h: Array[Int]
                           ): Unit = {
    println("Status of theaters:")
    for (i <- 0 until films.length) {
      println(s"Theater ${i + 1} - ${films(i)}")
      println(s"Screening 14:00 : ${seats14h(i)}/50 seats available")
      println(s"Screening 17:00 : ${seats17h(i)}/50 seats available")
      println(s"Screening 20:00 : ${seats20h(i)}/50 seats available")
      println()
    }
  }

  // changeTheaterMovie (only if no seat sold => all still 50)
  def changeTheaterMovie(
                          idTheater: Int,
                          films: Array[String],
                          seats14h: Array[Int],
                          seats17h: Array[Int],
                          seats20h: Array[Int]
                        ): Unit = {

    val soldSomething =
      seats14h(idTheater) != 50 || seats17h(idTheater) != 50 || seats20h(idTheater) != 50

    if (soldSomething) {
      println("Movie cannot be changed because seats have already been sold for this theater.")
    } else {
      println(s"Current movie for Theater ${idTheater + 1}: ${films(idTheater)}")
      println("Enter new movie title:")
      val newTitle = readLine("> ")
      films(idTheater) = newTitle
      println(s"Movie for Theater ${idTheater + 1} updated to '$newTitle'.")
    }
  }

  // ==========================
  // Main
  // ==========================
  def main(args: Array[String]): Unit = {
    do {
      showMainMenu()
      val choice: Int = readLine("> ").toInt

      if(choice < 1 || choice > 2){
        println("Invalid choice. Please try again.")
      } else if (choice == 1) {
        clientMode()
      } else if (choice == 2) {
        adminModeAccess()
      } else {
        stopProgram()
      }

    } while (isProgramRunning)
  }
}
