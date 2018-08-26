import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.io.StdIn
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol
import DefaultJsonProtocol._
import akka.http.scaladsl.model.headers.RawHeader
import spray.json._

object WebServer {
  val CURRENCY = "JPY"

  // Rtb request
  final case class Banner(w: Int, h: Int, pos: Int)
  implicit val bannerFormat = jsonFormat3(Banner)
  final case class Imp(id: String, banner: Banner, bidfloor: Int, bidfloorCur: String, native: Option[String], video: Option[String])
  implicit val impFormat = jsonFormat6(Imp)
  final case class Publisher(id: String, name: String, domain: String)
  implicit val publisherFormat = jsonFormat3(Publisher)
  final case class App(id: String, name: String, cat: List[String], ver: String, bundle: String, publisher: Publisher, storeurl: String)
  implicit val appFormat = jsonFormat7(App)
  final case class Device(ua: String, ip: String, model: String, os: String)
  implicit val deviceFormat = jsonFormat4(Device)
  final case class User(id: String, yob: Int, gender: String)
  implicit val userFormat = jsonFormat3(User)
  final case class RtbReq(id: String, imp: List[Imp], app: App, device: Device, user: User)
  implicit val rtbresFormat = jsonFormat5(RtbReq)

  // Rtb response
  final case class SeatBid(adid: String, adm: String, cid: String, crid: String, id: String, impid: String, iurl: String, nurl: String, price: Int)
  implicit val seatBidFormat = jsonFormat9(SeatBid)
  final case class Response(cur: String, id: String, seatbid: SeatBid)
  implicit val responseFormat = jsonFormat3(Response)

  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val rtbRequest =
      """
        |{
        |    "id": "IxexyLDIIk",
        |    "imp": [
        |        {
        |            "id": "1",
        |            "banner": {
        |                "w": 728,
        |                "h": 90,
        |                "pos": 1
        |            },
        |            "bidfloor": 50,
        |            "bidfloorCur": "JPY",
        |            "native": "nothing",
        |            "video": "nothign"
        |        }
        |    ],
        |    "app": {
        |        "id": "agltb3B1Yi1pbmNyDAsSA0FwcBiJkfIUDA",
        |        "name": "Yahoo Weather",
        |        "cat": [
        |            "IAB15"
        |        ],
        |        "ver": "1.0.2",
        |        "bundle": "628677149",
        |        "publisher": {
        |            "id": "agltb3B1Yi1pbmNyDAsSA0FwcBiJkfTUCV",
        |            "name": "yahoo",
        |            "domain": "www.yahoo.com"
        |        },
        |        "storeurl": "https://itunes.apple.com/id628677149"
        |    },
        |    "device": {
        |        "ua": "Mozilla/5.0 (iPhone; CPU iPhone OS 6_1 like Mac OS X) AppleWebKit/534.46 (KHTML, like Gecko) Version/5.1 Mobile/9A334 Safari/7534.48.3",
        |        "ip": "123.145.167.189",
        |        "model": "iPhone",
        |        "os": "iOS"
        |    },
        |    "user": {
        |        "id": "ffffffd5135596709273b3a1a07e466ea2bf4fff",
        |        "yob": 1984,
        |        "gender": "M"
        |    }
        |}
      """.stripMargin

    // Validates rtb request
    def validate(req: RtbReq): Either[String, Unit] = {
      // App's id should have name
      if (req.app.id == "") {
        return Left("app id should have a name")
      }

      // Imp should have entities at least one
      if (req.imp.isEmpty) {
        return Left("imp should have entities at least one")
      }

      // Currency should have valid currency name
      import util.control.Breaks._
      var err: Either[String, Unit] = Right()
      breakable {
        for (imp <- req.imp) {
          err = validateImp(imp)
          if (err.isLeft) break
        }
      }

      err
    }

    // Validates imp
    def validateImp(imp: Imp): Either[String, Unit] = {
      // Doesn't support native
      if (imp.native.isDefined) {
        return Left("doesn't support native")
      }

      // Doesn't support video
      if (imp.video.isDefined) {
        return Left("doesn't support video")
      }

      // Only support JPY
      if (imp.bidfloorCur != CURRENCY) {
        return Left("currency should have a valid currency name")
      }

      Right()
    }

    val response =
      """
        |{
        |    "cur": "JPY",
        |    "id": "IxexyLDIIk",
        |    "seatbid": [
        |        {
        |            "bid": [
        |                {
        |                    "adid": "1234",
        |                    "adm": "<a href=\"http://test.noadnolife.com/v1/click/12345?impid=${AUCTION_IMP_ID}&price=${AUCTION_PRICE}\"><img src=\"http://test.noadnolife.com/img/12345?impid=${AUCTION_IMP_ID}&price=${AUCTION_PRICE}\" width=\"728\" height=\"90\" border=\"0\" alt=\"Advertisement\" /></a>",
        |                    "cid": "1234",
        |                    "crid": "12345",
        |                    "id": "ed5dd1c0-05f1-4166-9370-4b07864d1136",
        |                    "impid": "1",
        |                    "iurl": "http://test.noadnolife.com/img/12345.png",
        |                    "nurl": "http://test.noadnolife.com/v1/win/12345?impid=${AUCTION_IMP_ID}&price=${AUCTION_PRICE}",
        |                    "price": 225
        |                }
        |            ]
        |        }
        |    ]
        |}
      """.stripMargin

    val route =
      get {
        path("rtb") {
          val jsonAst = rtbRequest.parseJson
          try {
            val rtbReq: RtbReq = jsonAst.convertTo[RtbReq]
            validate(rtbReq) match {
              case Right(_) =>
                val a = rtbReq.imp.map(Index.getIndex)
              case Left(a) => complete(StatusCodes.BadRequest, a)
            }

            val bid = SeatBid(
              "1234",
              "<a href=\"http://test.noadnolife.com/v1/click/12345?impid=${AUCTION_IMP_ID}&price=${AUCTION_PRICE}\"><img src=\"http://test.noadnolife.com/img/12345?impid=${AUCTION_IMP_ID}&price=${AUCTION_PRICE}\" width=\"728\" height=\"90\" border=\"0\" alt=\"Advertisement\" /></a>",
              "1234",
              "12345",
              "ed5dd1c0-05f1-4166-9370-4b07864d1136",
              "1",
              "http://localhost:8080/img/12345.png",
              "http://localhost:8080/win/12345?impid=1&price=200",
              225
            )
            val response = Response("JPY", "IxexyLDIIk", bid)
            respondWithHeader(RawHeader("x-openrtb-version", "2.3")) {
              complete(response)
            }

          } catch {
            case e: Exception => complete(StatusCodes.BadRequest, e)
          }
        }
      } ~
        get {
          path("win") {
            parameters("impid", "price") { (impid, price) =>
              def validate(impid: String, price: String): Either[String, Unit] = {
                try {
                  if (impid.isEmpty) {
                    return Left("impid shuold have an id")
                  }

                  if (price.toInt <= 0) {
                    return Left("price should be greater than zero")
                  }

                  Right()
                } catch {
                  case e: Exception => Left(e.getMessage)
                }
              }

              validate(impid, price) match {
                case Right(_) => complete("ok")
                case Left(a) => complete(StatusCodes.BadRequest, a)
              }
            }
          }
        }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}

object Index {

  case class Index(eCPM: Int, campaignID: String, creativeID: String, Nurl: String, Adm: String, Price: Int, PeCPM: Int)

  def getIndex(imp: WebServer.Imp): Option[Index] = {
    val index = Index(200, "1234", "12345", "http://xxxx.xxx/xxx/win", "", 250, 200)
    if (imp.bidfloor <= index.Price) {
      Some(index)
    } else {
      None
    }
  }

  def choiceBestAd() = ???

}