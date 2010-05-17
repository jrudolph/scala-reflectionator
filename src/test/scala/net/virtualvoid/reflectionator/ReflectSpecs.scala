package net.virtualvoid.reflectionator

import _root_.org.specs._

object ReflectSpecs extends Specification {
	import scala.reflect.Code.lift

	import Reflect._
 
	"Fields should be correctly infered from scala.reflect.Code" in {
		"Qualified read-accesses" in {
			val field = Reflect.field(() => StaticVariableContainer.x)
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
		"imported read-accesses" in {
			import StaticVariableContainer.x
			val field = Reflect.field(() => x)
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
		"Write-accesses" in {
			val field = Reflect.field(StaticVariableContainer.x = (_:Int))
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
	}
	"Methods should be correctly infered from scala.reflect.Code tree" in {
	  "static methods of relatively qualified classes" in {
	    val m = Reflect.method1(java.lang.Double.valueOf(_:Double))
	    m.getName must be_==("valueOf")
	    m.getParameterTypes.length must be_==(1)
	  }
	}
}
