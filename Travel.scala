import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}
import scala.jdk.CollectionConverters._
import com.github.tototoshi.csv.{CSVReader, CSVFormat, defaultCSVFormat}
import edu.stanford.nlp.pipeline.{CoreDocument, StanfordCoreNLP}
import java.util.Properties

class CSVDataLoader(filePath: String)(using csvFormat: CSVFormat = defaultCSVFormat) {
  def loadCSV(): Try[List[Map[String, String]]] = Try {
    val reader = CSVReader.open(new java.io.File(filePath))(csvFormat)
    try {
      reader.allWithHeaders()
    } finally {
      reader.close()
    }
  }
}

class NLPProcessor {
  val props = new Properties()
  props.setProperty("annotators", "tokenize,ssplit,pos,lemma,ner")
  val pipeline = new StanfordCoreNLP(props)

  def extractEntities(input: String): List[String] = {
    val document = new CoreDocument(input)
    pipeline.annotate(document)
    document.tokens().asScala.toList.flatMap { token =>
      val ner = token.ner()
      if (ner != "O") Some(token.word().toLowerCase) else None
    }.distinct
  }
}

class Chatbot(data: List[Map[String, String]], nlpProcessor: NLPProcessor) {
  private var lastTopic: Option[String] = None

  def greetUser(): String = "Hello! I'm your travel assistant. How can I help you today?"

  def handleUserInput(input: String): String = {
  val entities = nlpProcessor.extractEntities(input)
  val keywords = input.toLowerCase.split("\\s+").toList

  keywords match {
    case List("how", "are", "you") => "I'm good, thank you! How can I assist you with your travels?"
    case List("thank", "you") | List("goodbye") => "You're welcome! Have a great day!"
    case List("what", "can", "you", "do") =>
      "I can help you find hotels, check weather, and provide travel tips for various destinations!"
    case _ =>
      (entities, keywords) match {
        case (_, "hello" :: Nil) => "Hi there! What can I help you with?"
        case (_, List("weather", location)) => s"Fetching weather for $location..."
        case (_, List("book", "hotel", location)) =>
          lastTopic = Some(location)
          s"Looking for hotels in $location..."
        case (_, List("hotels", "in", location)) =>
          lastTopic = Some(location)
          s"Looking for hotels in $location..."
        case (_, List("the", "total", "cost")) if lastTopic.isDefined =>
          generateTotalCostResponse(lastTopic.get)
        case (destination :: _, _) =>
          lastTopic = Some(destination)
          generateLocationBasedResponse(destination)
        case _ => "Can you tell me more about where you want to travel or what you need help with?"
      }
  }
}


  private def generateLocationBasedResponse(destination: String): String = {
    data.find(_.get("Destination").exists(_.equalsIgnoreCase(destination))) match {
      case Some(info) =>
        val accommodation = info.getOrElse("Accommodation type", "No information available")
        val cost = info.getOrElse("Accommodation cost", "No information available")
        val transportation = info.getOrElse("Transportation type", "No information available")
        s"Here is the information for $destination:\nAccommodation: $accommodation\nCost: $cost\nTransportation: $transportation"
      case None => "No information found for that location."
    }
  }

  private def generateTotalCostResponse(destination: String): String = {
    data.find(_.get("Destination").exists(_.equalsIgnoreCase(destination))) match {
      case Some(info) =>
        val cost = info.getOrElse("Accommodation cost", "No information available")
        val transportationCost = info.getOrElse("Transportation cost", "No information available")
        s"The total cost for staying and traveling to $destination is: Accommodation $cost and Transportation $transportationCost."
      case None => "I don't have recent information about costs for that location."
    }
  }
}


object Travel {
  def main(args: Array[String]): Unit = {
    val filePath = "C:\\Users\\20101\\Downloads\\Travel details dataset.csv"
    val loader = CSVDataLoader(filePath)
    val data = loader.loadCSV() match {
      case Success(data) => data
      case Failure(exception) =>
        println(s"Failed to load data: ${exception.getMessage}")
        return
    }
    val nlpProcessor = NLPProcessor()
    val chatbot = Chatbot(data, nlpProcessor)

    println(chatbot.greetUser())
    var continueChat = true
    while (continueChat) {
      val input = readLine()
      if (input.equalsIgnoreCase("exit")) {
        continueChat = false
        println("Thank you for using the travel assistant. Goodbye!")
      } else {
        println(chatbot.handleUserInput(input))
      }
    }
  }
}
