package net.virtualvoid.reflectionator

import scala.reflect._
import java.lang.reflect

object Reflect {
  def method0[U](code: Code[() => U]): reflect.Method
  def method1[T1, U](code: Code[T1 => U]): reflect.Method
  def method2[T1, T2, U](code: Code[(T1, T2) => U): reflect.Method
}
