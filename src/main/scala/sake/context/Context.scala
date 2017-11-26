package sake.context

import sake.util._
import sake.target.Target  // TODO: Target and Context have a circular dependence

/**
 * The context passed to actions when they are invoked for targets.
 * @param target that is being built on this invocation.
 * @param settings by default the settings in the project.
 * @param properties other key-value pairs that have a narrower scope than the global settings.
 */
case class Context(target: Target[_], settings: Settings, properties: Map[String, Any])

