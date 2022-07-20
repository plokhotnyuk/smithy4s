/*
 *  Copyright 2021-2022 Disney Streaming
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

package smithy4s

/**
  * Natural transformation, turning a polymorphic type into another,
  * whilst keeping the type parameter intact.
  */
trait PolyFunction[F[_], G[_]] { self =>
  def apply[A](fa: F[A]): G[A]

  final def andThen[H[_]](other: PolyFunction[G, H]): PolyFunction[F, H] =
    new PolyFunction[F, H] {
      def apply[A](fa: F[A]): H[A] = other(self(fa))
    }

  final def mapK[H[_]](fk: PolyFunction[G, H]): PolyFunction[F, H] = andThen(fk)

  /**
    * Creates a memoised version of this function.
    *
    * Unsafe because it creates mutable state, which is a
    * non-referentially-transparent action (aka a side-effect).
    */
  final def unsafeMemoise: PolyFunction[F, G] =
    new PolyFunction[F, G] {
      private[this] val map = new java.util.HashMap[F[_], G[_]]

      def apply[A](fa: F[A]): G[A] =
        map.computeIfAbsent(fa, (fa: F[_]) => self(fa)).asInstanceOf[G[A]]
    }

  /**
   * Pre-computes the polyfunction by applying it on a vector
   * of possible inputs.
   *
   * Unsafe because calling the resulting polyfunction with an input that wasn't cached
   * will result in an exception.
   */
  final def unsafeCache(
      allPossibleInputs: Vector[Existential[F]]
  ): PolyFunction[F, G] =
    new PolyFunction[F, G] {
      private[this] val map = new java.util.HashMap[F[_], G[_]] {
        allPossibleInputs.foreach(input => put(input, self.apply(input)))
      }

      def apply[A](input: F[A]): G[A] = map.get(input).asInstanceOf[G[A]]
    }
}
