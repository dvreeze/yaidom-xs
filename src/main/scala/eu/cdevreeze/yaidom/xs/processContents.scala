package eu.cdevreeze.yaidom
package xs

sealed trait ProcessContents

object ProcessContents {

  object ProcessContentsLax extends ProcessContents
  object ProcessContentsSkip extends ProcessContents
  object ProcessContentsStrict extends ProcessContents
}
