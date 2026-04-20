package configuration

object SystemPropertyReader extends ConfigurationReader {
  override def getConfigOption(key: String): Option[String] = Option(System.getProperty(key))

  override def getConfigOptionsWithPrefix(prefix: String): Map[String, String] =
    System
      .getProperties
      .stringPropertyNames()
      .toArray(new Array[String](0))
      .collect { case key if key.startsWith(prefix) => key -> System.getProperty(key) }
      .toMap
}
