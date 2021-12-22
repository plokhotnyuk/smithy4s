/*
 *  Copyright 2021 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package schematic

object SchematicRepr extends SchematicRepr {}

trait SchematicRepr
    extends Schematic.stdlib.Mixin[Repr]
    with struct.GenericAritySchematic[Repr] {

  def short: String = "short"

  def int: String = "int"

  def long: String = "long"

  def double: String = "double"

  def float: String = "float"

  def bigint: String = "bigint"

  def bigdecimal: String = "bigdecimal"

  def string: String = "string"

  def boolean: String = "boolean"

  def byte: String = "byte"

  def bytes: String = "byteArray"

  def timestamp: String = "timestamp"

  def instant: String = "instant"

  def localDate: String = "localDate"

  def offsetDateTime: String = "offsetDateTime"

  def list[S](fs: String): String = s"list[$fs]"

  def set[S](fs: String): String = s"set[$fs]"

  def vector[S](fs: String): String = s"vector[$fs]"

  def uuid: String = "uuid"

  def map[K, V](fk: String, fv: String): String = s"map[$fk, $fv]"

  def enumeration[A](
      to: A => (String, Int),
      fromName: Map[String, A],
      fromOrdinal: Map[Int, A]
  ): String =
    "enumeration"

  def genericStruct[S](
      list: Vector[Field[Repr, S, _]]
  )(f: Vector[Any] => S): String = {
    val fields = list
      .map {
        case r if r.isRequired => s"${r.label}: ${r.instance}"
        case o                 => s"${o.label}: Option[${o.instance}]"
      }
      .mkString(", ")
    s"struct${list.size}(${fields})"
  }

  def union[S](first: Alt[Repr, S, _], rest: Vector[Alt[Repr, S, _]])(
      total: S => Alt.WithValue[Repr, S, _]
  ): String = {
    val alts = (rest
      .:+(first))
      .map { alt =>
        s"${alt.label}:${alt.instance}"
      }
      .mkString(", ")
    s"oneOf(${alts})"

  }

  def suspend[A](f: => String): String = s"suspended($f)"

  def bijection[A, B](f: String, to: A => B, from: B => A) = f

  override def unit: Repr[Unit] = "unit"

}
