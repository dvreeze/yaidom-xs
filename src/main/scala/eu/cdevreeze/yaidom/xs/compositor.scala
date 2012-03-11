package eu.cdevreeze.yaidom
package xs

sealed trait Compositor

object Compositor {

  object CompositorAll extends Compositor
  object CompositorChoice extends Compositor
  object CompositorSequence extends Compositor
}
