package configuration

trait ConfigurationReader {
  def getConfigOption(key: String): Option[String]
  def getConfigOptionsWithPrefix(prefix: String): Map[String, String]
}
