/*
 * Original implementation (C) 2009-2011 Debasish Ghosh
 * Adapted and extended in 2011 by Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cc.spray.json

/**
  * Provides additional JsonFormats and helpers.
 */
trait AdditionalFormats {
  /**
   * Constructs a JsonFormat from its two parts, JsonReader and JsonWriter.
   */
  def jsonFormat[T](reader: JsonReader[T], writer: JsonWriter[T]): JsonFormat[T] = new JsonFormat[T] {
    def write(obj: T) = writer.write(obj)
    def read(json: JsValue) = reader.read(json)
  }

  /**
   * Constructs a RootJsonFormat from its two parts, RootJsonReader and RootJsonWriter.
   */
  def rootJsonFormat[T](reader: RootJsonReader[T], writer: RootJsonWriter[T]): RootJsonFormat[T] =
    rootFormat(jsonFormat(reader, writer))

  /**
   * Turns a JsonWriter into a JsonFormat that throws an UnsupportedOperationException for reads.
   */
  def lift[T](writer :JsonWriter[T]): JsonFormat[T] = new JsonFormat[T] {
    def write(obj: T): JsValue = writer.write(obj)
    def read(value: JsValue) =
      Failure(new UnsupportedOperationException("JsonReader implementation missing"))
  }

  /**
   * Turns a RootJsonWriter into a RootJsonFormat that throws an UnsupportedOperationException for reads.
   */
  def lift[T](writer :RootJsonWriter[T]): RootJsonFormat[T] =
    rootFormat(lift(writer :JsonWriter[T]))

  /**
   * Turns a JsonReader into a JsonFormat that throws an UnsupportedOperationException for writes.
   */
  def lift[T <: AnyRef](reader :JsonReader[T]): JsonFormat[T] = new JsonFormat[T] {
    def write(obj: T): JsValue =
      throw new UnsupportedOperationException("No JsonWriter[" + obj.getClass + "] available")
    def read(value: JsValue) = reader.read(value)
  }

  /**
   * Turns a RootJsonReader into a RootJsonFormat that throws an UnsupportedOperationException for writes.
   */
  def lift[T <: AnyRef](reader :RootJsonReader[T]): RootJsonFormat[T] =
    rootFormat(lift(reader :JsonReader[T]))

  /**
   * Lazy wrapper around serialization. Useful when you want to serialize (mutually) recursive structures.
   */
  def lazyFormat[T](format: => JsonFormat[T]): JsonFormat[T] = new JsonFormat[T] {
    lazy val delegate = format;
    def write(x: T) = delegate.write(x);
    def read(value: JsValue) = delegate.read(value);
  }

  /**
   * Explicitly turns a JsonFormat into a RootJsonFormat.
   */
  def rootFormat[T](format: JsonFormat[T]): RootJsonFormat[T] = new RootJsonFormat[T] {
    def write(obj: T) = format.write(obj)
    def read(json: JsValue) = format.read(json)
  }

}