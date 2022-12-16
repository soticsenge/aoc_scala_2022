import spray.json._

import scala.annotation.tailrec

object JSON extends App {
  private def merge(j1: JsObject, j2: JsObject): JsObject = {
    val fields: Set[String] = (j1.fields ++ j2.fields).keySet
    fields.foldLeft(JsObject.empty)((acc: JsObject, e: String) => {
      (j1.fields.find(_._1 == e), j2.fields.find(_._1 == e)) match {
        case (None, Some(a)) => {
          val x: Map[String, JsValue] = acc.fields + a
          JsObject(x)
        }
        case (Some(a), None) => {
          val x: Map[String, JsValue] = acc.fields + a
          JsObject(x)
        }
        case (Some(a), Some(b)) => {
          if (b._2.isInstanceOf[JsBoolean] ||
            b._2.isInstanceOf[JsString] ||
            b._2.isInstanceOf[JsNumber]
          ) {
            val x: Map[String, JsValue] = acc.fields + b
            JsObject(x)
          } else b._2 match {
            case array: JsArray if a._2.isInstanceOf[JsArray] =>
              val x: Map[String, JsValue] = acc.fields + (a._1 -> JsArray(array.elements ++ b._2.asInstanceOf[JsArray].elements))
              JsObject(x)
            case _: JsArray =>
              val x: Map[String, JsValue] = acc.fields + b
              JsObject(x)
            case _ =>
              val x: Map[String, JsValue] = acc.fields + (a._1 -> merge(a._2.asJsObject, b._2.asJsObject))
              JsObject(x)
          }
        }
      }
    })
  }

  println(merge(
    new JsObject(
      Map("a" -> new JsString("a"),
        "b" -> new JsString("b"),
        "d" -> new JsString("b"),
        "e" -> new JsObject(
          Map("a" -> new JsString("cc"),
            "b" -> new JsString("ds"),
            "e" -> new JsString("dssd")))
        // NOT SURE HOW TO REPRESENT extra input
      )),
    new JsObject(
      Map("a" -> new JsString("c"),
        "b" -> new JsString("d"),
        "e" -> new JsObject(
          Map("a" -> new JsString("c"),
            "b" -> new JsString("d"),
            "e" -> new JsString("d"))),
        // NOT SURE HOW TO REPRESENT extra input
      ))))
}


/*
{a: b, c: d}
{a: b, e: f}

a,c,e

 */